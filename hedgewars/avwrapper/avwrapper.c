/*
 * Hedgewars, a free turn based strategy game
 * Copyright (c) 2004-2015 Andrey Korotaev <unC0Rr@gmail.com>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; version 2 of the License
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
 */

#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#include <string.h>
#include <stdarg.h>

#include "libavcodec/avcodec.h"
#include "libavformat/avformat.h"
#include "libavutil/avutil.h"
#include "libavutil/mathematics.h"

#if LIBAVCODEC_VERSION_MAJOR >= 59
#include <libavcodec/bsf.h>
#endif

#if (defined _MSC_VER)
#define AVWRAP_DECL __declspec(dllexport)
#elif ((__GNUC__ >= 3) && (!__EMX__) && (!sun))
#define AVWRAP_DECL __attribute__((visibility("default")))
#else
#define AVWRAP_DECL
#endif

#define UNUSED(x) (void)(x)

static AVFormatContext* g_pContainer;
static const AVOutputFormat* g_pFormat;
static AVStream* g_pAStream;
static AVStream* g_pVStream;
static AVFrame* g_pAFrame;
static AVFrame* g_pVFrame;
static const AVCodec* g_pACodec;
static const AVCodec* g_pVCodec;
static AVCodecContext* g_pAudio;
static AVCodecContext* g_pVideo;
#if LIBAVCODEC_VERSION_MAJOR >= 58
static AVPacket* g_pAPacket;
static AVPacket* g_pVPacket;
#endif

static int g_Width, g_Height;
static uint32_t g_Frequency, g_Channels;
static int g_VQuality;
static AVRational g_Framerate;

static FILE* g_pSoundFile;
#if LIBAVUTIL_VERSION_MAJOR < 53
static int16_t* g_pSamples;
#endif
static int g_NumSamples;
#if LIBAVCODEC_VERSION_MAJOR >= 53
static int64_t g_NextAudioPts;
#endif


// compatibility section
#if LIBAVCODEC_VERSION_MAJOR < 54
#define OUTBUFFER_SIZE 200000
static uint8_t g_OutBuffer[OUTBUFFER_SIZE];
#define avcodec_open2(x, y, z)              avcodec_open(x, y)
#endif

#if LIBAVCODEC_VERSION_MAJOR < 55
#define avcodec_default_get_buffer2(x, y ,z) avcodec_default_get_buffer(x, y)
#endif

#if LIBAVCODEC_VERSION_MAJOR < 56
#if LIBAVCODEC_VERSION_MAJOR < 55
#define av_frame_free                       av_freep
#else
#define av_frame_free                       avcodec_free_frame
#endif

#define av_frame_alloc                      avcodec_alloc_frame
#define av_frame_unref                      avcodec_get_frame_defaults
#define av_packet_rescale_ts                rescale_ts

static void rescale_ts(AVPacket *pkt, AVRational ctb, AVRational stb)
{
    if (pkt->pts != AV_NOPTS_VALUE)
        pkt->pts = av_rescale_q(pkt->pts, ctb, stb);
    if (pkt->dts != AV_NOPTS_VALUE)
        pkt->dts = av_rescale_q(pkt->dts, ctb, stb);
    if (pkt->duration > 0)
        pkt->duration = av_rescale_q(pkt->duration, ctb, stb);
}

#define avcodec_free_context(ctx)           do { avcodec_close(*ctx); av_freep(ctx); } while (0)
#endif

#ifndef AV_CODEC_CAP_DELAY
#define AV_CODEC_CAP_DELAY                  CODEC_CAP_DELAY
#endif
#ifndef AV_CODEC_CAP_VARIABLE_FRAME_SIZE
#define AV_CODEC_CAP_VARIABLE_FRAME_SIZE    CODEC_CAP_VARIABLE_FRAME_SIZE
#endif
#ifndef AV_CODEC_FLAG_GLOBAL_HEADER
#define AV_CODEC_FLAG_GLOBAL_HEADER         CODEC_FLAG_GLOBAL_HEADER
#endif
#ifndef AV_CODEC_FLAG_QSCALE
#define AV_CODEC_FLAG_QSCALE                CODEC_FLAG_QSCALE
#endif

#if LIBAVFORMAT_VERSION_MAJOR < 53
#define AVIO_FLAG_WRITE                     AVIO_WRONLY
#endif

#if LIBAVFORMAT_VERSION_MAJOR < 54
#define avformat_new_stream(x, y)           av_new_stream(x, y->type == AVMEDIA_TYPE_AUDIO)
#endif

#if LIBAVUTIL_VERSION_MAJOR < 54
#define AV_PIX_FMT_YUV420P                  PIX_FMT_YUV420P
#endif


// pointer to function from hwengine (uUtils.pas)
static void (*AddFileLogRaw)(const char* pString);

static int FatalError(const char* pFmt, ...)
{
    char Buffer[1024];
    va_list VaArgs;

    va_start(VaArgs, pFmt);
    vsnprintf(Buffer, 1024, pFmt, VaArgs);
    va_end(VaArgs);

    AddFileLogRaw("Error in av-wrapper: ");
    AddFileLogRaw(Buffer);
    AddFileLogRaw("\n");
    return(-1);
}

// Function to be called from libav for logging.
// Note: libav can call LogCallback from different threads
// (there is mutex in AddFileLogRaw).
static void LogCallback(void* p, int Level, const char* pFmt, va_list VaArgs)
{
    UNUSED(p);
    UNUSED(Level);

    char Buffer[1024];

    vsnprintf(Buffer, 1024, pFmt, VaArgs);
    AddFileLogRaw(Buffer);
}

static void Log(const char* pFmt, ...)
{
    char Buffer[1024];
    va_list VaArgs;

    va_start(VaArgs, pFmt);
    vsnprintf(Buffer, 1024, pFmt, VaArgs);
    va_end(VaArgs);

    AddFileLogRaw(Buffer);
}

#if LIBAVCODEC_VERSION_MAJOR >= 58
static int EncodeAndWriteFrame(
        const AVStream* pStream,
        AVCodecContext* pCodecContext,
        const AVFrame* pFrame,
        AVPacket* pPacket)
{
    int ret;

    ret = avcodec_send_frame(pCodecContext, pFrame);
    if (ret < 0)
        return FatalError("avcodec_send_frame failed: %d", ret);
    while (1)
    {
        ret = avcodec_receive_packet(pCodecContext, pPacket);
        if (ret == AVERROR(EAGAIN))
            return 1;
        else if (ret == AVERROR_EOF)
            return 0;
        else if (ret < 0)
            return FatalError("avcodec_receive_packet failed: %d", ret);

        av_packet_rescale_ts(pPacket, pCodecContext->time_base, pStream->time_base);

        // Write the compressed frame to the media file.
        pPacket->stream_index = pStream->index;
        ret = av_interleaved_write_frame(g_pContainer, pPacket);
        if (ret != 0)
            return FatalError("Error while writing frame: %d", ret);
    }
}
#endif

static void AddAudioStream()
{
    int ret;
    g_pAStream = avformat_new_stream(g_pContainer, g_pACodec);
    if(!g_pAStream)
    {
        Log("Could not allocate audio stream\n");
        return;
    }
    g_pAStream->id = 1;

#if LIBAVCODEC_VERSION_MAJOR >= 59
    g_pAudio = avcodec_alloc_context3(g_pACodec);
#else
    g_pAudio = g_pAStream->codec;

    avcodec_get_context_defaults3(g_pAudio, g_pACodec);
    g_pAudio->codec_id = g_pACodec->id;
#endif

    // put parameters
    g_pAudio->sample_fmt = AV_SAMPLE_FMT_S16;
    g_pAudio->sample_rate = g_Frequency;
#if LIBAVCODEC_VERSION_MAJOR >= 60
    const AVChannelLayout* pChLayout = g_pACodec->ch_layouts;
    if (pChLayout)
    {
        for (; pChLayout->nb_channels; pChLayout++)
        {
            if (pChLayout->nb_channels == g_Channels)
            {
                ret = av_channel_layout_copy(&g_pAudio->ch_layout, pChLayout);
                if (ret != 0)
                {
                    Log("Channel layout copy failed: %d\n", ret);
                    return;
                }
                break;
            }
        }
    }
    if (!g_pAudio->ch_layout.nb_channels)
    {
        // no suitable layout found
        g_pAudio->ch_layout.order = AV_CHANNEL_ORDER_UNSPEC;
        g_pAudio->ch_layout.nb_channels = g_Channels;
    }
#else
    g_pAudio->channels = g_Channels;
#endif

    // set time base as invers of sample rate
    g_pAudio->time_base.den = g_pAStream->time_base.den = g_Frequency;
    g_pAudio->time_base.num = g_pAStream->time_base.num = 1;

    // set quality
    g_pAudio->bit_rate = 160000;

    // for codecs that support variable bitrate use it, it should be better
    g_pAudio->flags |= AV_CODEC_FLAG_QSCALE;
    g_pAudio->global_quality = 1*FF_QP2LAMBDA;

    // some formats want stream headers to be separate
    if (g_pFormat->flags & AVFMT_GLOBALHEADER)
        g_pAudio->flags |= AV_CODEC_FLAG_GLOBAL_HEADER;

    // open it
    if (avcodec_open2(g_pAudio, g_pACodec, NULL) < 0)
    {
        Log("Could not open audio codec %s\n", g_pACodec->long_name);
        return;
    }

#if LIBAVCODEC_VERSION_MAJOR >= 58
    ret = avcodec_parameters_from_context(g_pAStream->codecpar, g_pAudio);
    if (ret < 0)
    {
        Log("Could not copy parameters from codec context: %d\n", ret);
        return;
    }
#endif

#if LIBAVCODEC_VERSION_MAJOR >= 54
    if (g_pACodec->capabilities & AV_CODEC_CAP_VARIABLE_FRAME_SIZE)
#else
    if (g_pAudio->frame_size == 0)
#endif
        g_NumSamples = 4096;
    else
        g_NumSamples = g_pAudio->frame_size;
    g_pAFrame = av_frame_alloc();
    if (!g_pAFrame)
    {
        Log("Could not allocate frame\n");
        return;
    }
#if LIBAVUTIL_VERSION_MAJOR >= 53
#if LIBAVCODEC_VERSION_MAJOR >= 60
    ret = av_channel_layout_copy(&g_pAFrame->ch_layout, &g_pAudio->ch_layout);
    if (ret != 0)
    {
        Log("Channel layout copy for frame failed: %d\n", ret);
        return;
    }
#else
    g_pAFrame->channels = g_pAudio->channels;
#endif
    g_pAFrame->format = g_pAudio->sample_fmt;
    g_pAFrame->sample_rate = g_pAudio->sample_rate;
    g_pAFrame->nb_samples = g_NumSamples;
    ret = av_frame_get_buffer(g_pAFrame, 1);
    if (ret < 0)
    {
        Log("Failed to allocate frame buffer: %d\n", ret);
        return;
    }
#else
    g_pSamples = (int16_t*)av_malloc(g_NumSamples*g_Channels*sizeof(int16_t));
#endif
#if LIBAVCODEC_VERSION_MAJOR >= 58
    g_pAPacket = av_packet_alloc();
    if (!g_pAPacket)
    {
        Log("Could not allocate audio packet\n");
        return;
    }
#endif
#if LIBAVCODEC_VERSION_MAJOR >= 53
    g_NextAudioPts = 0;
#endif
}

// returns non-zero if there is more sound, -1 in case of error
static int WriteAudioFrame()
{
    if (!g_pAStream)
        return 0;

    int ret;
    int16_t* pData;
#if LIBAVUTIL_VERSION_MAJOR >= 53
    ret = av_frame_make_writable(g_pAFrame);
    if (ret < 0)
        return FatalError("Could not make audio frame writable: %d", ret);
    pData = (int16_t*) g_pAFrame->data[0];
#else
    pData = g_pSamples;
#endif

    int NumSamples = fread(pData, 2*g_Channels, g_NumSamples, g_pSoundFile);

#if LIBAVCODEC_VERSION_MAJOR >= 53
    AVFrame* pFrame = NULL;
    if (NumSamples > 0)
    {
        g_pAFrame->nb_samples = NumSamples;
        g_pAFrame->pts = g_NextAudioPts;
        g_NextAudioPts += NumSamples;
#if LIBAVUTIL_VERSION_MAJOR < 53
        avcodec_fill_audio_frame(g_pAFrame, g_Channels, AV_SAMPLE_FMT_S16,
                                 (uint8_t*)pData, NumSamples*2*g_Channels, 1);
#endif
        pFrame = g_pAFrame;
    }
#endif

#if LIBAVCODEC_VERSION_MAJOR >= 58
    ret = EncodeAndWriteFrame(g_pAStream, g_pAudio, pFrame, g_pAPacket);
    if (ret < 0)
        return FatalError("Audio frame processing failed");
    return ret;
#else
    AVPacket Packet;
    av_init_packet(&Packet);
    Packet.data = NULL;
    Packet.size = 0;

#if LIBAVCODEC_VERSION_MAJOR >= 53
    // when NumSamples == 0 we still need to call encode_audio2 to flush
    int got_packet;
    if (avcodec_encode_audio2(g_pAudio, &Packet, pFrame, &got_packet) != 0)
        return FatalError("avcodec_encode_audio2 failed");
    if (!got_packet)
        return 0;

    av_packet_rescale_ts(&Packet, g_pAudio->time_base, g_pAStream->time_base);
#else
    if (NumSamples == 0)
        return 0;
    int BufferSize = OUTBUFFER_SIZE;
    if (g_pAudio->frame_size == 0)
        BufferSize = NumSamples*g_Channels*2;
    Packet.size = avcodec_encode_audio(g_pAudio, g_OutBuffer, BufferSize, pData);
    if (Packet.size == 0)
        return 1;
    if (g_pAudio->coded_frame && g_pAudio->coded_frame->pts != AV_NOPTS_VALUE)
        Packet.pts = av_rescale_q(g_pAudio->coded_frame->pts, g_pAudio->time_base, g_pAStream->time_base);
    Packet.flags |= AV_PKT_FLAG_KEY;
    Packet.data = g_OutBuffer;
#endif

    // Write the compressed frame to the media file.
    Packet.stream_index = g_pAStream->index;
    if (av_interleaved_write_frame(g_pContainer, &Packet) != 0)
        return FatalError("Error while writing audio frame");
    return 1;
#endif
}

// add a video output stream
static int AddVideoStream()
{
    int ret;
    g_pVStream = avformat_new_stream(g_pContainer, g_pVCodec);
    if (!g_pVStream)
        return FatalError("Could not allocate video stream");

#if LIBAVCODEC_VERSION_MAJOR >= 59
    g_pVideo = avcodec_alloc_context3(g_pVCodec);
#else
    g_pVideo = g_pVStream->codec;

    avcodec_get_context_defaults3(g_pVideo, g_pVCodec);
    g_pVideo->codec_id = g_pVCodec->id;
#endif

    // put parameters
    // resolution must be a multiple of two
    g_pVideo->width  = g_Width  & ~1; // make even (dimensions should be even)
    g_pVideo->height = g_Height & ~1; // make even
    /* time base: this is the fundamental unit of time (in seconds) in terms
       of which frame timestamps are represented. for fixed-fps content,
       timebase should be 1/framerate and timestamp increments should be
       identically 1. */
    g_pVideo->time_base.den = g_pVStream->time_base.den = g_Framerate.num;
    g_pVideo->time_base.num = g_pVStream->time_base.num = g_Framerate.den;

    g_pVideo->pix_fmt = AV_PIX_FMT_YUV420P;

    // set quality
    if (g_VQuality > 100)
        g_pVideo->bit_rate = g_VQuality;
    else
    {
        g_pVideo->flags |= AV_CODEC_FLAG_QSCALE;
        g_pVideo->global_quality = g_VQuality*FF_QP2LAMBDA;
    }

    // some formats want stream headers to be separate
    if (g_pFormat->flags & AVFMT_GLOBALHEADER)
        g_pVideo->flags |= AV_CODEC_FLAG_GLOBAL_HEADER;

#if LIBAVCODEC_VERSION_MAJOR < 53
    // for some versions of ffmpeg x264 options must be set explicitly
    if (strcmp(g_pVCodec->name, "libx264") == 0)
    {
        g_pVideo->coder_type = FF_CODER_TYPE_AC;
        g_pVideo->flags |= CODEC_FLAG_LOOP_FILTER;
        g_pVideo->crf = 23;
        g_pVideo->thread_count = 3;
        g_pVideo->me_cmp = FF_CMP_CHROMA;
        g_pVideo->partitions = X264_PART_I8X8 | X264_PART_I4X4 | X264_PART_P8X8 | X264_PART_B8X8;
        g_pVideo->me_method = ME_HEX;
        g_pVideo->me_subpel_quality = 7;
        g_pVideo->me_range = 16;
        g_pVideo->gop_size = 250;
        g_pVideo->keyint_min = 25;
        g_pVideo->scenechange_threshold = 40;
        g_pVideo->i_quant_factor = 0.71;
        g_pVideo->b_frame_strategy = 1;
        g_pVideo->qcompress = 0.6;
        g_pVideo->qmin = 10;
        g_pVideo->qmax = 51;
        g_pVideo->max_qdiff = 4;
        g_pVideo->max_b_frames = 3;
        g_pVideo->refs = 3;
        g_pVideo->directpred = 1;
        g_pVideo->trellis = 1;
        g_pVideo->flags2 = CODEC_FLAG2_BPYRAMID | CODEC_FLAG2_MIXED_REFS | CODEC_FLAG2_WPRED | CODEC_FLAG2_8X8DCT | CODEC_FLAG2_FASTPSKIP;
        g_pVideo->weighted_p_pred = 2;
    }
#endif

    // open the codec
    if (avcodec_open2(g_pVideo, g_pVCodec, NULL) < 0)
        return FatalError("Could not open video codec %s", g_pVCodec->long_name);

#if LIBAVCODEC_VERSION_MAJOR >= 58
    ret = avcodec_parameters_from_context(g_pVStream->codecpar, g_pVideo);
    if (ret < 0)
        return FatalError("Could not copy parameters from codec context: %d", ret);
#endif

    g_pVFrame = av_frame_alloc();
    if (!g_pVFrame)
        return FatalError("Could not allocate frame");
    av_frame_unref(g_pVFrame);

    g_pVFrame->width = g_Width;
    g_pVFrame->height = g_Height;
    g_pVFrame->format = AV_PIX_FMT_YUV420P;

#if LIBAVCODEC_VERSION_MAJOR >= 58
    g_pVPacket = av_packet_alloc();
    if (!g_pVPacket)
        return FatalError("Could not allocate packet");
#endif

    return avcodec_default_get_buffer2(g_pVideo, g_pVFrame, 0);
}

static int WriteFrame(AVFrame* pFrame)
{
    double AudioTime, VideoTime;
    int ret;
    // write interleaved audio frame
    if (g_pAStream)
    {
#if LIBAVCODEC_VERSION_MAJOR >= 58
        if (!g_pAPacket)
            return FatalError("Error while writing video frame: g_pAPacket does not exist");
#endif
        VideoTime = (double)g_pVFrame->pts * g_pVStream->time_base.num/g_pVStream->time_base.den;
        do
        {
            if (!g_pAFrame)
                return FatalError("Error while writing video frame: g_pAFrame does not exist");
            AudioTime = (double)g_pAFrame->pts * g_pAStream->time_base.num/g_pAStream->time_base.den;
            ret = WriteAudioFrame();
        }
        while (AudioTime < VideoTime && ret > 0);
        if (ret < 0)
            return ret;
    }

    if (!g_pVStream)
        return 0;

    g_pVFrame->pts++;
#if LIBAVCODEC_VERSION_MAJOR >= 58
    ret = EncodeAndWriteFrame(g_pVStream, g_pVideo, pFrame, g_pVPacket);
    if (ret < 0)
        return FatalError("Video frame processing failed");
    return ret;
#else
    AVPacket Packet;
    av_init_packet(&Packet);
    Packet.data = NULL;
    Packet.size = 0;

    if (g_pFormat->flags & AVFMT_RAWPICTURE)
    {
        /* raw video case. The API will change slightly in the near
           future for that. */
        Packet.flags |= AV_PKT_FLAG_KEY;
        Packet.stream_index = g_pVStream->index;
        Packet.data = (uint8_t*)pFrame;
        Packet.size = sizeof(AVPicture);

        if (av_interleaved_write_frame(g_pContainer, &Packet) != 0)
            return FatalError("Error while writing video frame");
        return 0;
    }
    else
    {
#if LIBAVCODEC_VERSION_MAJOR >= 54
        int got_packet;
        if (avcodec_encode_video2(g_pVideo, &Packet, pFrame, &got_packet) < 0)
            return FatalError("avcodec_encode_video2 failed");
        if (!got_packet)
            return 0;

        av_packet_rescale_ts(&Packet, g_pVideo->time_base, g_pVStream->time_base);
#else
        Packet.size = avcodec_encode_video(g_pVideo, g_OutBuffer, OUTBUFFER_SIZE, pFrame);
        if (Packet.size < 0)
            return FatalError("avcodec_encode_video failed");
        if (Packet.size == 0)
            return 0;

        if( g_pVideo->coded_frame->pts != AV_NOPTS_VALUE)
            Packet.pts = av_rescale_q(g_pVideo->coded_frame->pts, g_pVideo->time_base, g_pVStream->time_base);
        if( g_pVideo->coded_frame->key_frame )
            Packet.flags |= AV_PKT_FLAG_KEY;
        Packet.data = g_OutBuffer;
#endif
        // write the compressed frame in the media file
        Packet.stream_index = g_pVStream->index;
        if (av_interleaved_write_frame(g_pContainer, &Packet) != 0)
            return FatalError("Error while writing video frame");

        return 1;
    }
#endif
}

AVWRAP_DECL int AVWrapper_WriteFrame(uint8_t *buf)
{
    int x, y, stride = g_Width * 4;
    uint8_t *data[3];

    // copy pointers, prepare source
    memcpy(data, g_pVFrame->data, sizeof(data));
    buf += (g_Height - 1) * stride;

    // convert to YUV 4:2:0
    for (y = 0; y < g_Height; y++) {
        for (x = 0; x < g_Width; x++) {
            int r = buf[x * 4 + 0];
            int g = buf[x * 4 + 1];
            int b = buf[x * 4 + 2];

            int luma = (int)(0.299f * r +  0.587f * g + 0.114f * b);
            data[0][x] = av_clip_uint8(luma);

            if (!(x & 1) && !(y & 1)) {
                int r = (buf[x * 4 + 0]          + buf[(x + 1) * 4 + 0] +
                         buf[x * 4 + 0 - stride] + buf[(x + 1) * 4 + 0 - stride]) / 4;
                int g = (buf[x * 4 + 1]          + buf[(x + 1) * 4 + 1] +
                         buf[x * 4 + 1 - stride] + buf[(x + 1) * 4 + 1 - stride]) / 4;
                int b = (buf[x * 4 + 2]          + buf[(x + 1) * 4 + 2] +
                         buf[x * 4 + 2 - stride] + buf[(x + 1) * 4 + 2 - stride]) / 4;

                int cr = (int)(-0.14713f * r - 0.28886f * g + 0.436f   * b);
                int cb = (int)( 0.615f   * r - 0.51499f * g - 0.10001f * b);
                data[1][x / 2] = av_clip_uint8(128 + cr);
                data[2][x / 2] = av_clip_uint8(128 + cb);
            }
        }
        buf += -stride;
        data[0] += g_pVFrame->linesize[0];
        if (y & 1) {
            data[1] += g_pVFrame->linesize[1];
            data[2] += g_pVFrame->linesize[2];
        }
    }

    return WriteFrame(g_pVFrame);
}

AVWRAP_DECL int AVWrapper_Init(
         void (*pAddFileLogRaw)(const char*),
         const char* pFilename,
         const char* pDesc,
         const char* pSoundFile,
         const char* pFormatName,
         const char* pVCodecName,
         const char* pACodecName,
         int Width, int Height,
         int FramerateNum, int FramerateDen,
         int VQuality)
{
    int ret;
    AddFileLogRaw = pAddFileLogRaw;
    av_log_set_callback( &LogCallback );

    g_Width  = Width;
    g_Height = Height;
    g_Framerate.num = FramerateNum;
    g_Framerate.den = FramerateDen;
    g_VQuality = VQuality;

#if LIBAVCODEC_VERSION_MAJOR < 59
    // initialize libav and register all codecs and formats
    av_register_all();
#endif

    // find format
    g_pFormat = av_guess_format(pFormatName, NULL, NULL);
    if (!g_pFormat)
        return FatalError("Format \"%s\" was not found", pFormatName);

    // allocate the output media context
    g_pContainer = avformat_alloc_context();
    if (!g_pContainer)
        return FatalError("Could not allocate output context");

    g_pContainer->oformat = g_pFormat;

    // store description of file
    av_dict_set(&g_pContainer->metadata, "comment", pDesc, 0);

    // append extesnion to filename
    char ext[16];
    strncpy(ext, g_pFormat->extensions, 16);
    ext[15] = 0;
    size_t extLen = strcspn(ext, ",");
    ext[extLen] = 0;
#if LIBAVCODEC_VERSION_MAJOR >= 59
    // pFilename + dot + ext + null byte
    size_t urlLen = strlen(pFilename) + 1 + extLen + 1;
    g_pContainer->url = av_malloc(urlLen);
    snprintf(g_pContainer->url, urlLen, "%s.%s", pFilename, ext);
#else
    snprintf(g_pContainer->filename, sizeof(g_pContainer->filename), "%s.%s", pFilename, ext);
#endif
    // find codecs
    g_pVCodec = avcodec_find_encoder_by_name(pVCodecName);
    g_pACodec = avcodec_find_encoder_by_name(pACodecName);

    // add audio and video stream to container
    g_pVStream = NULL;
    g_pAStream = NULL;

    if (g_pVCodec)
    {
        ret = AddVideoStream();
        if (ret < 0)
            return ret;
    }
    else
        Log("Video codec \"%s\" was not found; video will be ignored.\n", pVCodecName);

    if (g_pACodec)
    {
        g_pSoundFile = fopen(pSoundFile, "rb");
        if (g_pSoundFile)
        {
            fread(&g_Frequency, 4, 1, g_pSoundFile);
            fread(&g_Channels, 4, 1, g_pSoundFile);
            AddAudioStream();
        }
        else
            Log("Could not open %s\n", pSoundFile);
    }
    else
        Log("Audio codec \"%s\" was not found; audio will be ignored.\n", pACodecName);

    if (!g_pAStream && !g_pVStream)
        return FatalError("No video, no audio, aborting...");

    // write format info to log
#if LIBAVCODEC_VERSION_MAJOR >= 59
    av_dump_format(g_pContainer, 0, g_pContainer->url, 1);
#else
    av_dump_format(g_pContainer, 0, g_pContainer->filename, 1);
#endif

    // open the output file, if needed
    if (!(g_pFormat->flags & AVFMT_NOFILE))
    {
#if LIBAVCODEC_VERSION_MAJOR >= 59
        if (avio_open(&g_pContainer->pb, g_pContainer->url, AVIO_FLAG_WRITE) < 0)
            return FatalError("Could not open output file (%s)", g_pContainer->url);
#else
        if (avio_open(&g_pContainer->pb, g_pContainer->filename, AVIO_FLAG_WRITE) < 0)
            return FatalError("Could not open output file (%s)", g_pContainer->filename);
#endif
    }

    g_pVFrame->pts = -1;

    // write the stream header, if any
    return avformat_write_header(g_pContainer, NULL);
}

AVWRAP_DECL int AVWrapper_Close()
{
    int ret;
    // output buffered frames
    if (g_pVCodec->capabilities & AV_CODEC_CAP_DELAY)
    {
        do
            ret = WriteFrame(NULL);
        while (ret > 0);
        if (ret < 0)
            return ret;
    }
    // output any remaining audio
    do
    {
        ret = WriteAudioFrame();
    }
    while(ret > 0);
    if (ret < 0)
        return ret;

    // write the trailer, if any.
    av_write_trailer(g_pContainer);

    // close the output file
    if (!(g_pFormat->flags & AVFMT_NOFILE))
        avio_close(g_pContainer->pb);

    // free everything
    if (g_pVStream)
    {
        avcodec_free_context(&g_pVideo);
        av_frame_free(&g_pVFrame);
#if LIBAVCODEC_VERSION_MAJOR >= 58
        av_packet_free(&g_pVPacket);
#endif
    }
    if (g_pAStream)
    {
        avcodec_free_context(&g_pAudio);
        av_frame_free(&g_pAFrame);
#if LIBAVCODEC_VERSION_MAJOR >= 58
        av_packet_free(&g_pAPacket);
#endif
#if LIBAVUTIL_VERSION_MAJOR < 53
        av_free(g_pSamples);
#endif
        fclose(g_pSoundFile);
    }

#if LIBAVCODEC_VERSION_MAJOR >= 59
    avformat_free_context(g_pContainer);
#else
    if (g_pVStream)
        av_free(g_pVStream);
    if (g_pAStream)
        av_free(g_pAStream);
    av_free(g_pContainer);
#endif
    return 0;
}
