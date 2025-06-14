(*
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
 *)

{$INCLUDE "options.inc"}

unit uChat;

interface
uses SDLh;

procedure initModule;
procedure freeModule;
procedure ReloadLines;
procedure CleanupInput;
procedure CloseChat;
procedure RestoreChat;
procedure AddChatString(s: shortstring);
procedure DrawChat;
procedure KeyPressChat(keysym: TSDL_Keysym);
procedure SendHogSpeech(s: shortstring);
procedure CopyToClipboard(var newContent: shortstring);
procedure TextInput(var event: TSDL_TextInputEvent);

implementation
uses uConsts, uInputHandler, uTypes, uVariables, uCommands, uUtils, uTextures, uRender, uIO, uScript, uRenderUtils, uStore, uLocale
     {$IFDEF USE_VIDEO_RECORDING}, uVideoRec{$ENDIF};

const MaxStrIndex = 27; // Max. possible string index
      MaxStrPartial = 7; // Max. displayed strings in normal mode
      MaxInputStrLen = 200;

type TChatLine = record
    Tex: PTexture;
    Time: Longword;
    Width: LongInt;
    s: shortstring;
    Color: TSDL_Color;
    end;
    TChatCmd = (ccQuit, ccPause, ccShowHistory, ccFullScreen);

var Strs: array[0 .. MaxStrIndex] of TChatLine;
    MStrs: array[0 .. MaxStrIndex] of shortstring;
    LocalStrs: array[0 .. MaxStrIndex] of shortstring;
    oldInput: shortstring;
    missedCount: LongWord;
    lastStr: LongWord;
    localLastStr: LongInt;
    history: LongInt;
    visibleCount: LongWord;
    InputStr: TChatLine;
    ChatReady: boolean;
    showAll: boolean;
    liveLua: boolean;
    ChatHidden: boolean;
    firstDraw: boolean;
    InputLinePrefix: TChatLine;
    // cursor
    cursorPos, cursorX, selectedPos, selectionDx: LongInt;
    LastKeyPressTick: LongWord;


const
    colors: array[#0..#9] of TSDL_Color = (
            (r:$FF; g:$FF; b:$00; a:$FF), // #0 warning message [Yellow]
            (r:$FF; g:$FF; b:$FF; a:$FF), // #1 chat message [White]
            (r:$FF; g:$00; b:$FF; a:$FF), // #2 action message [Purple]
            (r:$90; g:$FF; b:$90; a:$FF), // #3 join/leave message [Lime]
            (r:$FF; g:$FF; b:$A0; a:$FF), // #4 team message [Light Yellow]
            (r:$FF; g:$00; b:$00; a:$FF), // #5 error messages [Red]
            (r:$00; g:$FF; b:$FF; a:$FF), // #6 input line [Light Blue]
            (r:$FF; g:$80; b:$80; a:$FF), // #7 team gone [Light Red]
            (r:$FF; g:$D0; b:$80; a:$FF), // #8 team back [Light Orange]
            (r:$DF; g:$DF; b:$DF; a:$FF)  // #9 hog speech [Light Gray]
            );
    ChatCommandz: array [TChatCmd] of record
            ChatCmd: string[31];
            ProcedureCallChatCmd: string[31];
            end = (
            (ChatCmd: '/quit'; ProcedureCallChatCmd: 'halt'),
            (ChatCmd: '/pause'; ProcedureCallChatCmd: 'pause'),
            (ChatCmd: '/history'; ProcedureCallChatCmd: 'history'),
            (ChatCmd: '/fullscreen'; ProcedureCallChatCmd: 'fullscr')
            );


const PaddingFactor = 0.125; // relative to font size in pixels

var Padding, ClHeight: integer;
    LastChatScaleValue, LastUIScaleValue: real;
    SkipNextInput: boolean;

procedure UpdateInputLinePrefix(); forward;
procedure UpdateCursorCoords(); forward;

// relevant for UTF-8 handling
function IsFirstCharByte(c: char): boolean; 
begin
    // based on https://en.wikipedia.org/wiki/UTF-8#Description
    IsFirstCharByte:= (byte(c) and $C0) <> $80;
end;

function charIsForHogSpeech(c: char): boolean;
begin
exit((c = '"') or (c = '''') or (c = '-'));
end;

procedure ResetSelection();
begin
    selectedPos:= -1;
end;

procedure AdjustToUIScale();
var fntSize, fntSizePx: integer;
begin
    // don't do anything if no change
    if (ChatScaleValue = LastChatScaleValue) and (UIScaleValue = LastUIScaleValue) then
        exit;

    LastChatScaleValue:= ChatScaleValue;
    LastUIScaleValue:= UIScaleValue;

    fntSize:= max(1, round(UIScaleValue * ChatScaleValue * cBaseChatFontHeight));

    if Fontz[fntChat].Height <> fntSize then
        begin
        // adjust associated heights
        Fontz[fntChat].Height:= fntSize;
        Fontz[CJKfntChat].Height:= fntSize;
        // reload if initialized already
        if Fontz[fntChat].Handle <> nil then
            LoadFont(fntChat);
        if Fontz[CJKfntChat].Handle <> nil then
            LoadFont(CJKfntChat);
        end;

    // adjust line height etc.
    fntSizePx:= round(cFontPxToPtRatio * fntSize);
    Padding:= max(1, round(PaddingFactor * fntSizePx));

    ClHeight:= 2 * Padding + fntSizePx;

    // clear cache of already rendered lines
    ReloadLines();
    UpdateInputLinePrefix();
    UpdateCursorCoords();
end;

procedure ChatSizeInc(pxprecise: boolean);
var fntSize: integer;
begin
if pxprecise then
    begin
    fntSize:= Fontz[fntChat].Height;
    inc(fntSize);
    ChatScaleValue:= 1.0 * fntSize / cBaseChatFontHeight;
    end
else
    ChatScaleValue:= ChatScaleValue * (1.0 + cChatScaleRelDelta);
if ChatScaleValue > cMaxChatScaleValue then
    ChatScaleValue:= cMaxChatScaleValue;
AdjustToUIScale();
end;

procedure ChatSizeDec(pxprecise: boolean);
var fntSize: integer;
begin
if pxprecise then
    begin
    fntSize:= Fontz[fntChat].Height;
    dec(fntSize);
    ChatScaleValue:= 1.0 * fntSize / cBaseChatFontHeight;
    end
else
    ChatScaleValue:= ChatScaleValue / (1.0 + cChatScaleRelDelta);
if ChatScaleValue < cMinChatScaleValue then
    ChatScaleValue:= cMinChatScaleValue;
AdjustToUIScale();
end;

procedure chatSizeReset();
begin
ChatScaleValue:= cDefaultChatScale;
AdjustToUIScale();
end;

function GetChatFont(str: shortstring): THWFONT;
begin
    GetChatFont:= CheckCJKFont(ansistring(str), fntChat);
end;

procedure UpdateCursorCoords();
var font: THWFont;
    str : shortstring;
    coff, soff: LongInt;
begin
    AdjustToUIScale();

    if cursorPos = selectedPos then
        ResetSelection();

    // calculate cursor offset

    str:= InputStr.s;
    font:= GetChatFont(str);

    // get only substring before cursor to determine length
    // SetLength(str, cursorPos); // makes pas2c unhappy
    str[0]:= char(cursorPos);
    // get render size of text
    TTF_SizeUTF8(Fontz[font].Handle, Str2PChar(str), @coff, nil);
    cursorX:= Padding + coff;

    // calculate selection width on screen
    if selectedPos >= 0 then
        begin
        if selectedPos > cursorPos then
            str:= InputStr.s;
        // SetLength(str, selectedPos); // makes pas2c unhappy
        str[0]:= char(selectedPos);
        TTF_SizeUTF8(Fontz[font].Handle, Str2PChar(str), @soff, nil);
        selectionDx:= soff - coff;
        end
    else
        selectionDx:= 0;
end;


procedure ResetCursor();
begin
    ResetSelection();
    cursorPos:= 0;
    UpdateCursorCoords();
end;

(* This procedure [re]renders a texture showing str for the chat line cl.
 * It will use the color stored in cl and update width
 *)
procedure RenderChatLineTex(var cl: TChatLine; var str: shortstring);
var strSurface, tmpSurface,
    resSurface: PSDL_Surface;
    dstrect   : TSDL_Rect; // destination rectangle for blitting
    font      : THWFont;
const
    shadowint  = $80 shl AShift;
begin

FreeAndNilTexture(cl.Tex);

font:= GetChatFont(str);

// get render size of text
TTF_SizeUTF8(Fontz[font].Handle, Str2PChar(str), @cl.Width, nil);

// calculate and save size
cl.Width := cl.Width  + 2 * Padding;

// create surface to draw on
resSurface:= SDL_CreateRGBSurface(
                0, toPowerOf2(cl.Width), toPowerOf2(ClHeight),
                32, RMask, GMask, BMask, AMask);

// define area we want to draw in
dstrect.x:= 0;
dstrect.y:= 0;
dstrect.w:= cl.Width;
dstrect.h:= ClHeight;

// draw background
SDL_FillRect(resSurface, @dstrect, shadowint);

// create and blit text
tmpSurface:= nil;
strSurface:= TTF_RenderUTF8_Blended(Fontz[font].Handle, Str2PChar(str), cl.color);
// fix format
if strSurface <> nil then tmpSurface:= SDL_ConvertSurface(strSurface, resSurface^.format, 0);
SDL_FreeSurface(strSurface);
//SDL_UpperBlit(strSurface, nil, resSurface, @dstrect);
if tmpSurface <> nil then copyToXY(tmpSurface, resSurface, Padding, Padding);
SDL_FreeSurface(tmpSurface);

cl.Tex:= Surface2Tex(resSurface, false);

SDL_FreeSurface(resSurface)
end;

const ClDisplayDuration = 12500;

procedure SetLine(var cl: TChatLine; str: shortstring; isInput: boolean);
var color  : TSDL_Color;
begin
if isInput then
    begin
    cl.s:= str;
    color:= colors[#6];
    str:= str + ' ';
    end
else
    begin
    if str[1] <= High(colors) then
        begin
        color:= colors[str[1]];
        delete(str, 1, 1);
        end
    // fallback if invalid color
    else
        color:= colors[Low(colors)];

    cl.s:= str;
    end;

cl.color:= color;

// set texture, note: variables cl.s and str will be different here if isInput
RenderChatLineTex(cl, str);

cl.Time:= RealTicks + ClDisplayDuration;
end;

// For uStore texture recreation
procedure ReloadLines;
var i: LongWord;
begin
    // also reload empty input line (as chat size/scaling might have changed)
    //if InputStr.s <> '' then
    SetLine(InputStr, InputStr.s, true);
    for i:= 0 to MaxStrIndex do
        if Strs[i].s <> '' then
            begin
            RenderChatLineTex(Strs[i], Strs[i].s);
            end;
end;

procedure AddChatString(s: shortstring);
begin
if not ChatReady then
    begin
    if MissedCount < MaxStrIndex - 1 then
        MStrs[MissedCount]:= s
    else if MissedCount < MaxStrIndex then
        MStrs[MissedCount]:= #5 + '[...]';
    inc(MissedCount);
    exit
    end;

lastStr:= (lastStr + 1) mod (MaxStrIndex + 1);

SetLine(Strs[lastStr], s, false);

inc(visibleCount)
end;

procedure UpdateInputLinePrefix();
begin
if liveLua then
    begin
    InputLinePrefix.color:= colors[#1];
    InputLinePrefix.s:= '[Lua] >';
    end
else
    begin
    InputLinePrefix.color:= colors[#6];
    InputLinePrefix.s:= UserNick + '>';
    end;

FreeAndNilTexture(InputLinePrefix.Tex);
end;

procedure DrawChat;
var i, t, left, top, cnt: LongInt;
    selRect: TSDL_Rect;
    c: char;
begin
AdjustToUIScale();

ChatReady:= true; // maybe move to somewhere else?

if ChatHidden and (not showAll) then
    visibleCount:= 0;

// draw chat lines with some distance from screen border
left:= 4 - cScreenWidth div 2;
{$IFDEF USE_TOUCH_INTERFACE}
i:= 55;
{$ELSE}
i:= 10;
{$ENDIF}
top := i + visibleCount * ClHeight; // we start with input line (if any)
if top > cScreenHeight - ClHeight - 60 then
    begin
    top:= cScreenHeight - ClHeight - 60;
    top:= i + top - (top mod ClHeight);
    end;

// draw chat input line first and under all other lines
if isInChatMode and (InputStr.Tex <> nil) then
    begin

    if InputLinePrefix.Tex = nil then
        RenderChatLineTex(InputLinePrefix, InputLinePrefix.s);

    DrawTexture(left, top, InputLinePrefix.Tex);
    inc(left, InputLinePrefix.Width);
    DrawTexture(left, top, InputStr.Tex);

    if firstDraw then
        begin
        UpdateCursorCoords();
        firstDraw:= false;
        end;

    if selectedPos < 0 then
        begin
        // draw cursor
        if ((RealTicks - LastKeyPressTick) and 512) < 256 then
            DrawLineOnScreen(left + cursorX, top + Padding, left + cursorX, top + ClHeight - Padding, max(2, round(UIScaleValue * ChatScaleValue * 2.0)), $00, $FF, $FF, $FF);
        end
    else // draw selection
        begin
        selRect.y:= top + Padding;
        selRect.h:= clHeight - 2 * Padding;
        if selectionDx < 0 then
            begin
            selRect.x:= left + cursorX + selectionDx;
            selRect.w:= -selectionDx;
            end
        else
            begin
            selRect.x:= left + cursorX;
            selRect.w:= selectionDx;
            end;

        DrawRect(selRect, $FF, $FF, $FF, $40, true);
        end;

    dec(left, InputLinePrefix.Width);


    if (Length(InputStr.s) > 0) and ((CursorPos = 1) or (CursorPos = 2)) then
        begin
        c:= InputStr.s[1];
        if charIsForHogSpeech(c) then
            begin
            SpeechHogNumber:= 0;
            if Length(InputStr.s) > 1 then
                begin
                c:= InputStr.s[2];
                if (c > '0') and (c < '9') then
                    SpeechHogNumber:= byte(c) - 48;
                end;
            // default to current hedgehog (if own) or first hedgehog
            if SpeechHogNumber = 0 then
                begin
                if (not CurrentTeam^.ExtDriven) and (not CurrentHedgehog^.Unplaced) then
                    SpeechHogNumber:= CurrentTeam^.CurrHedgehog + 1
                else
                    SpeechHogNumber:= 1;
                end;
            end;
        end
    else
        SpeechHogNumber:= -1;
    end
else
    SpeechHogNumber:= -1;

// draw chat lines
if ((not ChatHidden) or showAll) and (UIDisplay <> uiNone) then
    begin
    if MissedCount <> 0 then // there are chat strings we missed, so print them now
        begin
        for i:= 0 to MissedCount - 1 do
            AddChatString(MStrs[i]);
        MissedCount:= 0;
        end;
    i:= lastStr;

    cnt:= 0; // count of lines displayed
    t  := 1; // # of current line processed

    // draw lines in reverse order
    while (((t < MaxStrPartial) and (Strs[i].Time > RealTicks)) or ((t <= MaxStrIndex + 1) and showAll))
    and (Strs[i].Tex <> nil) do
        begin
        top:= top - ClHeight;
        // draw chatline only if not offscreen
        if top > 0 then
            DrawTexture(left, top, Strs[i].Tex);

        if i = 0 then
            i:= MaxStrIndex
        else
            dec(i);

        inc(cnt);
        inc(t)
        end;

    visibleCount:= cnt;
    end;
end;

procedure SendHogSpeech(s: shortstring);
begin
SendIPC('h' + s);
ParseCommand('/hogsay '+s, true)
end;

procedure SendConsoleCommand(s: shortstring);
begin
    Delete(s, 1, 1);
    SendIPC('~' + s)
end;

procedure AcceptChatString(s: shortstring);
var i: TWave;
    j: TChatCmd;
    c, t: LongInt;
    x: byte;
begin
if s <> LocalStrs[localLastStr] then
    begin
    // put in input history
    localLastStr:= (localLastStr + 1) mod MaxStrIndex;
    LocalStrs[localLastStr]:= s;
    end;

t:= LocalTeam;
x:= 0;
// speech bubble
if (s[1] = '"') and (s[Length(s)] = '"')
    then x:= 1

// thinking bubble
else if (s[1] = '''') and (s[Length(s)] = '''') then
    x:= 2

// yelling bubble
else if (s[1] = '-') and (s[Length(s)] = '-') then
    x:= 3;

if (not CurrentTeam^.ExtDriven) and (x <> 0) then
    for c:= 0 to Pred(TeamsCount) do
        if (TeamsArray[c] = CurrentTeam) then
            t:= c;

if x <> 0 then
    begin
    if t = -1 then
        ParseCommand('/say ' + copy(s, 2, Length(s)-2), true)
    else
        SendHogSpeech(char(x) + char(t) + copy(s, 2, Length(s)-2));
    exit
    end;

if (s[1] = '/') then
    begin
    if (Length(s) <= 1) then
        begin
        // empty chat command
        AddChatString(#0 + shortstring(trcmd[sidCmdUnknown]));
        exit;
        end;

    // Ignore message-type commands with empty argument list
    if (copy(s, 2, 2) = 'me') and (Length(s) = 3) then
        exit;
    if ((copy(s, 2, 3) = 'hsa') or (copy(s, 2, 3) = 'hta') or (copy(s, 2, 3) = 'hya')) and (Length(s) = 4) then
        exit;
    if ((copy(s, 2, 4) = 'team') or (copy(s, 2, 4) = 'clan')) and (Length(s) = 5) then
        exit;

    // Speech bubble, but on next attack
    if (copy(s, 2, 4) = 'hsa ') then
        begin
        if CurrentTeam^.ExtDriven then
            ParseCommand('/say ' + copy(s, 6, Length(s)-5), true)
        else
            SendHogSpeech(#4 + copy(s, 6, Length(s)-5));
        exit
        end;

    // Thinking bubble, but on next attack
    if (copy(s, 2, 4) = 'hta ') then
        begin
        if CurrentTeam^.ExtDriven then
            ParseCommand('/say ' + copy(s, 6, Length(s)-5), true)
        else
            SendHogSpeech(#5 + copy(s, 6, Length(s)-5));
        exit
        end;

    // Yelling bubble, but on next attack
    if (copy(s, 2, 4) = 'hya ') then
        begin
        if CurrentTeam^.ExtDriven then
            ParseCommand('/say ' + copy(s, 6, Length(s)-5), true)
        else
            SendHogSpeech(#6 + copy(s, 6, Length(s)-5));
        exit
        end;

    // "/clan" or "/team" ("/team" is an alias for "/clan")
    if ((copy(s, 2, 5) = 'clan ') or (copy(s, 2, 5) = 'team ')) then
        begin
        if (Length(s) > 6) then
            ParseCommand('team ' + copy(s, 7, Length(s) - 6), true);
        exit
        end;

    if (copy(s, 2, 3) = 'me ') then
        begin
        ParseCommand('/say ' + s, true);
        exit
        end;

    if (copy(s, 2, 10) = 'togglechat') then
        begin
        ChatHidden:= (not ChatHidden);
        if ChatHidden then
           showAll:= false;
        exit
        end;

    if (copy(s, 2, 3) = 'ff ') then
    begin
       ParseCommand(s, true);
       exit
    end;

    if (copy(s, 2, 3) = 'sff') then
    begin
       ParseCommand(s, true);
       exit
    end;

    // debugging commands
    if (copy(s, 2, 7) = 'debugvl') then
        // This command intentionally not documented in /help
        begin
        cViewLimitsDebug:= (not cViewLimitsDebug);
        UpdateViewLimits();
        exit
        end;

    if (copy(s, 2, 3) = 'lua') then
        begin
        LuaCmdUsed:= true;
        AddFileLog('/lua issued');
{$IFDEF USE_VIDEO_RECORDING}
        if flagPrerecording then
            begin
            AddFileLog('Force-stopping prerecording! Lua commands can not be recorded');
            StopPreRecording;
            end;
{$ENDIF}
        if gameType <> gmtNet then
            begin
            liveLua:= (not liveLua);
            if liveLua then
                begin
                AddFileLog('[Lua] chat input string parsing enabled');
                AddChatString(#3 + shortstring(trmsg[sidLuaParsingOn]));
                end
            else
                begin
                AddFileLog('[Lua] chat input string parsing disabled');
                AddChatString(#3 + shortstring(trmsg[sidLuaParsingOff]));
                end;
            UpdateInputLinePrefix();
            end
        else
            AddChatString(#5 + shortstring(trmsg[sidLuaParsingDenied]));
        exit
        end;

    // Help commands
    if (copy(s, 2, 11) = 'help taunts') then
        begin
        AddChatString(#3 + shortstring(trcmd[sidCmdHeaderTaunts]));
        AddChatString(#3 + shortstring(trcmd[sidCmdSpeech]));
        AddChatString(#3 + shortstring(trcmd[sidCmdThink]));
        AddChatString(#3 + shortstring(trcmd[sidCmdYell]));
        AddChatString(#3 + shortstring(trcmd[sidCmdSpeechNumberHint]));
        AddChatString(#3 + shortstring(trcmd[sidCmdHsa]));
        AddChatString(#3 + shortstring(trcmd[sidCmdHta]));
        AddChatString(#3 + shortstring(trcmd[sidCmdHya]));
        AddChatString(#3 + shortstring(trcmd[sidCmdHappy]));
        AddChatString(#3 + shortstring(trcmd[sidCmdWave]));
        AddChatString(#3 + shortstring(trcmd[sidCmdHurrah]));
        AddChatString(#3 + shortstring(trcmd[sidCmdShrug]));
        AddChatString(#3 + shortstring(trcmd[sidCmdSad]));
        AddChatString(#3 + shortstring(trcmd[sidCmdIlovelotsoflemonade]));
        AddChatString(#3 + shortstring(trcmd[sidCmdJuggle]));
        AddChatString(#3 + shortstring(trcmd[sidCmdRollup]));
        AddChatString(#3 + shortstring(trcmd[sidCmdBubble]));
        exit
        end;

    if (copy(s, 2, 9) = 'help room') then
        begin
        if (gameType = gmtNet) then
            SendConsoleCommand('/help')
        else
            AddChatString(#0 + shortstring(trcmd[sidCmdHelpRoomFail]));
        exit;
        end;

    if (copy(s, 2, 4) = 'help') then
        begin
        AddChatString(#3 + shortstring(trcmd[sidCmdHeaderBasic]));
        if gameType = gmtNet then
            AddChatString(#3 + shortstring(trcmd[sidCmdPauseNet]))
        else
            AddChatString(#3 + shortstring(trcmd[sidCmdPause]));
        AddChatString(#3 + shortstring(trcmd[sidCmdFullscreen]));
        AddChatString(#3 + shortstring(trcmd[sidCmdQuit]));
        if gameType <> gmtNet then
            AddChatString(#3 + shortstring(trcmd[sidLua]));
        // history and help commands needs to be close to the end because they are always visible
        // with a short chat history length.
        AddChatString(#3 + shortstring(trcmd[sidCmdTeam]));
        AddChatString(#3 + shortstring(trcmd[sidCmdMe]));
        AddChatString(#3 + shortstring(trcmd[sidCmdTogglechat]));
        AddChatString(#3 + shortstring(trcmd[sidCmdHistory]));
        AddChatString(#3 + shortstring(trcmd[sidCmdHelp]));
        AddChatString(#3 + shortstring(trcmd[sidCmdHelpTaunts]));
        if gameType = gmtNet then
            AddChatString(#3 + shortstring(trcmd[sidCmdHelpRoom]));
        exit
        end;

    // hedghog animations/taunts and engine commands
    for i:= Low(TWave) to High(TWave) do
        if (s = Wavez[i].cmd) then
            begin
            // only works for local non-bot teams
            if (not CurrentTeam^.ExtDriven) and (CurrentTeam^.Hedgehogs[0].BotLevel = 0) then
                ParseCommand('/taunt ' + char(i), true);
            exit;
            end;

    for j:= Low(TChatCmd) to High(TChatCmd) do
        if (s = ChatCommandz[j].ChatCmd) then
            begin
            ParseCommand(ChatCommandz[j].ProcedureCallChatCmd, true);
            exit
            end;

    if (gameType = gmtNet) then
        SendConsoleCommand(s)
    else
        AddChatString(#0 + shortstring(trcmd[sidCmdUnknown]));
    end
else
    begin
    if liveLua then
        LuaParseString(s)
    else
        ParseCommand('/say ' + s, true);
    end;
end;

procedure CleanupInput;
begin
    FreezeEnterKey;
    history:= 0;
    SDL_StopTextInput();
    //SDL_EnableKeyRepeat(0,0);
    isInChatMode:= false;
    ResetKbd;
end;

procedure OpenChat(s: shortstring);
var i: Integer;
begin
    if GameState = gsConfirm then
        ParseCommand('quit', true);
    isInChatMode:= true;
    SDL_StopTextInput();
    SDL_StartTextInput();
    //Make REALLY sure unexpected events are flushed (1 time is insufficient as of SDL 2.0.7)
    for i := 1 to 2 do
    begin
        SDL_PumpEvents();
        SDL_FlushEvent(SDL_TEXTINPUT);
    end;
    if length(s) = 0 then
        SetLine(InputStr, '', true)
    else
        begin
        SetLine(InputStr, s, true);
        cursorPos:= length(s);
        UpdateCursorCoords();
        end;
end;

procedure CloseChat;
begin
    oldInput:= InputStr.s;
    SetLine(InputStr, '', true);
    ResetCursor();
    CleanupInput();
end;

procedure RestoreChat;
begin
    if length(oldInput) > 0 then
        OpenChat(oldInput);
    oldInput:= '';
end;

procedure DelBytesFromInputStrBack(endIdx: integer; count: byte);
var startIdx: integer;
begin
    // nothing to do if count is 0
    if count = 0 then
        exit;

    // first byte to delete
    startIdx:= endIdx - (count - 1);

    // delete bytes from string
    Delete(InputStr.s, startIdx, count);

    SetLine(InputStr, InputStr.s, true);
end;

procedure MoveCursorToPreviousChar();
begin
    if cursorPos > 0 then
        repeat
            dec(cursorPos);
        until ((cursorPos = 0) or IsFirstCharByte(InputStr.s[cursorPos + 1]));
end;

procedure MoveCursorToNextChar();
var len: integer;
begin
    len:= Length(InputStr.s);
    if cursorPos < len then
        repeat
            inc(cursorPos);
        until ((cursorPos = len) or IsFirstCharByte(InputStr.s[cursorPos + 1]));
end;

procedure DeleteLastUTF8CharFromStr(var s: shortstring);
var l: byte;
begin
    l:= Length(s);

    while (l > 1) and (not IsFirstCharByte(s[l])) do
        begin
        dec(l);
        end;

    if l > 0 then
        dec(l);

    s[0]:= char(l);
end;

procedure DeleteSelected();
begin
    if (selectedPos >= 0) and (cursorPos <> selectedPos) then
        begin
        DelBytesFromInputStrBack(max(cursorPos, selectedPos), abs(selectedPos-cursorPos));
        cursorPos:= min(cursorPos, selectedPos);
        end;
    ResetSelection();
    UpdateCursorCoords();
end;

procedure HandleSelection(enabled: boolean);
begin
if enabled then
    begin
    if selectedPos < 0 then
        selectedPos:= cursorPos;
    end
else
    ResetSelection();
end;

type TCharSkip = ( none, wspace, numalpha, special );

function GetInputCharSkipClass(index: LongInt): TCharSkip;
var  c: char;
begin
    c:= InputStr.s[index];

    // non-ascii counts as letter
    if c > #127 then
        exit(numalpha);

    // low-ascii whitespaces and DEL
    if (c < #33) or (c = #127) then
        exit(wspace);

    // low-ascii special chars
    if c < #48 then
        exit(special);

    // digits
    if c < #58 then
        exit(numalpha);

    // make c upper-case
    if c > #96 then
        c:= char(byte(c) - 32);

    // letters
    if (c > #64) and (c < #90) then
        exit(numalpha);

    // remaining ascii are special chars
    exit(special);
end;

// skip from word to word, similar to Qt
procedure SkipInputChars(skip: TCharSkip; backwards: boolean);
begin
if backwards then
    begin
    // skip trailing whitespace, similar to Qt
    while (skip = wspace) and (cursorPos > 0) do
        begin
        skip:= GetInputCharSkipClass(cursorPos);
        if skip = wspace then
            MoveCursorToPreviousChar();
        end;
    // skip same-type chars
    while (cursorPos > 0) and (GetInputCharSkipClass(cursorPos) = skip) do
        MoveCursorToPreviousChar();
    end
else
    begin
    // skip same-type chars
    while cursorPos < Length(InputStr.s) do
        begin
        MoveCursorToNextChar();
        if (GetInputCharSkipClass(cursorPos) <> skip) then
            begin
            MoveCursorToPreviousChar();
            break;
            end;
        end;
    // skip trailing whitespace, similar to Qt
    while cursorPos < Length(InputStr.s) do
        begin
        MoveCursorToNextChar();
        if (GetInputCharSkipClass(cursorPos) <> wspace) then
            begin
            MoveCursorToPreviousChar();
            break;
            end;
        end;
    end;
end;

procedure CopyToClipboard(var newContent: shortstring);
begin
    // SDL2 clipboard
    SDL_SetClipboardText(Str2PChar(newContent));
end;

procedure CopySelectionToClipboard();
var selection: shortstring;
begin
    if selectedPos >= 0 then
        begin
        selection:= copy(InputStr.s, min(CursorPos, selectedPos) + 1, abs(CursorPos - selectedPos));
        CopyToClipboard(selection);
        end;
end;

procedure InsertIntoInputStr(s: shortstring);
var limit: integer;
begin
    // we check limit for trailing stuff before insertion limit for a reason
    // (possible remaining space after too long UTF8-insertion has been shortened)

    // length limit for stuff to that will trail the insertion
    limit:= max(cursorPos, MaxInputStrLen-Length(s));

    while Length(InputStr.s) > limit do
        begin
        DeleteLastUTF8CharFromStr(InputStr.s);
        end;

    // length limit for stuff to insert
    limit:= max(0, MaxInputStrLen-cursorPos);

    if limit = 0 then
        s:= ''
    else while Length(s) > limit do
        begin
        DeleteLastUTF8CharFromStr(s);
        end;

    if Length(s) > 0 then
        begin
        // insert string truncated to safe length
        Insert(s, InputStr.s, cursorPos + 1);

        if Length(InputStr.s) > MaxInputStrLen then
            InputStr.s[0]:= char(MaxInputStrLen);

        SetLine(InputStr, InputStr.s, true);

        // move cursor to end of inserted string
        inc(cursorPos, Length(s));
        UpdateCursorCoords();
        end;
end;

procedure PasteFromClipboard();
var clip: PChar;
begin
    // use SDL2 clipboard functions
    if SDL_HasClipboardText() then
        begin
        clip:= SDL_GetClipboardText();
        // returns NULL if not enough memory for a copy of clipboard content 
        if clip <> nil then
            begin
            InsertIntoInputStr(shortstring(clip));
            SDL_free(Pointer(clip));
            end;
        end;
end;

procedure KeyPressChat(keysym: TSDL_Keysym);
const nonStateMask = (not (KMOD_NUM or KMOD_CAPS));
var i, index: integer;
    selMode, ctrl, ctrlonly: boolean;
    skip: TCharSkip;
    Scancode: TSDL_Scancode;
    Modifier: Word;
begin
    Scancode:= keysym.scancode;
    Modifier:= keysym.modifier;

    LastKeyPressTick:= RealTicks;

    selMode:= (modifier and (KMOD_LSHIFT or KMOD_RSHIFT)) <> 0;
    ctrl:= (modifier and (KMOD_LCTRL or KMOD_RCTRL)) <> 0;
    ctrlonly:= ctrl and ((modifier and nonStateMask and (not (KMOD_LCTRL or KMOD_RCTRL))) = 0);
    skip:= none;

    case Scancode of
        SDL_SCANCODE_BACKSPACE:
            begin
            if selectedPos < 0 then
                begin
                HandleSelection(true);

                // delete more if ctrl is held
                if ctrl then
                    SkipInputChars(GetInputCharSkipClass(cursorPos), true)
                else
                    MoveCursorToPreviousChar();

                end;

            DeleteSelected();
            UpdateCursorCoords();
            end;
        SDL_SCANCODE_DELETE:
            begin
            if selectedPos < 0 then
                begin
                HandleSelection(true);

                // delete more if ctrl is held
                if ctrl then
                    SkipInputChars(GetInputCharSkipClass(cursorPos), false)
                else
                    MoveCursorToNextChar();

                end;

            DeleteSelected();
            UpdateCursorCoords();
            end;
        SDL_SCANCODE_ESCAPE:
            begin
            if Length(InputStr.s) > 0 then
                begin
                SetLine(InputStr, '', true);
                ResetCursor();
                end
            else
                CleanupInput;
            oldInput:= '';
            end;
        SDL_SCANCODE_RETURN, SDL_SCANCODE_KP_ENTER:
            begin
            if Length(InputStr.s) > 0 then
                begin
                AcceptChatString(InputStr.s);
                SetLine(InputStr, '', false);
                ResetCursor();
                end;
            CleanupInput
            end;
        SDL_SCANCODE_UP, SDL_SCANCODE_DOWN:
            begin
            if (Scancode = SDL_SCANCODE_UP) and (history < localLastStr) then inc(history);
            if (Scancode = SDL_SCANCODE_DOWN) and (history > 0) then dec(history);
            index:= localLastStr - history + 1;
            if (index > localLastStr) then
                begin
                SetLine(InputStr, '', true);
                end
            else
                begin
                SetLine(InputStr, LocalStrs[index], true);
                end;
            cursorPos:= Length(InputStr.s);
            ResetSelection();
            UpdateCursorCoords();
            end;
        SDL_SCANCODE_HOME:
            begin
            if cursorPos > 0 then
                begin
                HandleSelection(selMode);
                cursorPos:= 0;
                end
            else if (not selMode) then
                ResetSelection();

            UpdateCursorCoords();
            end;
        SDL_SCANCODE_END:
            begin
            i:= Length(InputStr.s);
            if cursorPos < i then
                begin
                HandleSelection(selMode);
                cursorPos:= i;
                end
            else if (not selMode) then
                ResetSelection();

            UpdateCursorCoords();
            end;
        SDL_SCANCODE_LEFT:
            begin
            if cursorPos > 0 then
                begin

                if ctrl then
                    skip:= GetInputCharSkipClass(cursorPos);

                if selMode or (selectedPos < 0) then
                    begin
                    HandleSelection(selMode);
                    // go to end of previous utf8-char
                    MoveCursorToPreviousChar();
                    end
                else // if we're leaving selection mode, jump to its left end
                    begin
                    cursorPos:= min(cursorPos, selectedPos);
                    ResetSelection();
                    end;

                if ctrl then
                    SkipInputChars(skip, true);

                end
            else if (not selMode) then
                ResetSelection();

            UpdateCursorCoords();
            end;
        SDL_SCANCODE_RIGHT:
            begin
            if cursorPos < Length(InputStr.s) then
                begin

                if selMode or (selectedPos < 0) then
                    begin
                    HandleSelection(selMode);
                    MoveCursorToNextChar();
                    end
                else // if we're leaving selection mode, jump to its right end
                    begin
                    cursorPos:= max(cursorPos, selectedPos);
                    ResetSelection();
                    end;

                if ctrl then
                    SkipInputChars(GetInputCharSkipClass(cursorPos), false);

                end
            else if (not selMode) then
                ResetSelection();

            UpdateCursorCoords();
            end;
        SDL_SCANCODE_PAGEUP, SDL_SCANCODE_PAGEDOWN:
            begin
            // ignore me!!!
            end;
        // TODO: figure out how to determine those keys better
        SDL_SCANCODE_a:
            begin
            // select all
            if ctrlonly then
                begin
                ResetSelection();
                cursorPos:= 0;
                HandleSelection(true);
                cursorPos:= Length(InputStr.s);
                UpdateCursorCoords();
                end
            end;
        SDL_SCANCODE_c:
            begin
            // copy
            if ctrlonly then
                CopySelectionToClipboard()
            end;
        SDL_SCANCODE_v:
            begin
            // paste
            if ctrlonly then
                begin
                DeleteSelected();
                PasteFromClipboard();
                end
            end;
        SDL_SCANCODE_x:
            begin
            // cut
            if ctrlonly then
                begin
                CopySelectionToClipboard();
                DeleteSelected();
                end
            end;
            // make chat bigger
        SDL_SCANCODE_KP_PLUS, SDL_SCANCODE_EQUALS:
            begin
            if ctrl then
                begin
                ChatSizeInc(selMode);
                SkipNextInput:= true;
                end;
            end;
            // make chat smaller
        SDL_SCANCODE_KP_MINUS, SDL_SCANCODE_MINUS:
            begin
            if ctrl then
                begin
                ChatSizeDec(selMode);
                SkipNextInput:= true;
                end;
            end;
            // revert chat size to default
        SDL_SCANCODE_KP_0, SDL_SCANCODE_0:
            begin
            if ctrl then
                begin
                ChatSizeReset();
                SkipNextInput:= true;
                end;
            end;
        end;
end;

procedure TextInput(var event: TSDL_TextInputEvent);
var s: shortstring;
    l: byte;
    isl: integer;
begin
    if SkipNextInput then
        begin
        SkipNextInput:= false;
        exit;
        end;

    DeleteSelected();

    s:= '';
    l:= 0;
    // fetch all bytes of character/input
    while event.text[l] <> #0 do
        begin
        if Length(s) < 255 then
            begin
            s[l + 1]:= event.text[l];
            inc(l)
            end
        end;
    if l > 0 then
        begin
        isl:= Length(InputStr.s);
        // check if user is typing a redundant closing hog-speech quotation mark
        if (l = 1) and (isl >= 2) and (cursorPos = isl - 1) and charIsForHogSpeech(s[1])
          and (s[1] = InputStr.s[1]) and (s[1] = InputStr.s[isl]) then
            begin
            MoveCursorToNextChar();
            UpdateCursorCoords();
            end
        else
            begin
            // don't add input that doesn't fit
            if isl + l > MaxInputStrLen then exit;
            s[0]:= char(l);
            InsertIntoInputStr(s);

            // add closing hog speech quotation marks automagically
            if (l = 1) and (Length(InputStr.s) = 1) and charIsForHogSpeech(s[1]) then
                begin
                InsertIntoInputStr(s);
                MoveCursorToPreviousChar();
                UpdateCursorCoords();
                end;
            end;

        end
end;


procedure chChatMessage(var s: shortstring);
begin
    AddChatString(s)
end;

procedure chSay(var s: shortstring);
begin
    SendIPC('s' + s);

    if copy(s, 1, 4) = '/me ' then
        s:= #2 + '* ' + UserNick + ' ' + copy(s, 5, Length(s) - 4)
    else
        s:= #1 + Format(shortstring(trmsg[sidChat]), UserNick, s);

    AddChatString(s)
end;

procedure chTeamSay(var s: shortstring);
begin
    SendIPC('b' + s);

    s:= #4 + Format(shortstring(trmsg[sidChatTeam]), UserNick, s);

    AddChatString(s)
end;

procedure chHistory(var s: shortstring);
var i: LongInt;
begin
    s:= s; // avoid compiler hint
    showAll:= not showAll;
    // immediatly recount
    visibleCount:= 0;
    if showAll or (not ChatHidden) then
        for i:= 0 to MaxStrIndex do
            begin
            if (Strs[i].Tex <> nil) and (showAll or (Strs[i].Time > RealTicks)) then
                inc(visibleCount);
            end;
end;

procedure chChat(var s: shortstring);
begin
    s:= s; // avoid compiler hint
    if length(s) = 0 then
        OpenChat('')
    else
        OpenChat('/clan ');
end;

procedure initModule;
var i: ShortInt;
begin
    RegisterVariable('chatmsg', @chChatMessage, true);
    RegisterVariable('say', @chSay, true);
    RegisterVariable('team', @chTeamSay, true);
    RegisterVariable('history', @chHistory, true );
    RegisterVariable('chat', @chChat, true );

    lastStr:= 0;
    localLastStr:= 0;
    history:= 0;
    visibleCount:= 0;
    showAll:= false;
    ChatReady:= false;
    missedCount:= 0;
    liveLua:= false;
    ChatHidden:= false;
    firstDraw:= true;

    LastChatScaleValue:= 0;
    LastUIScaleValue:= 0;
    SkipNextInput:= false;

    InputLinePrefix.Tex:= nil;
    UpdateInputLinePrefix();
    inputStr.s:= '';
    inputStr.Tex := nil;
    for i:= 0 to MaxStrIndex do
        Strs[i].Tex := nil;

    LastKeyPressTick:= 0;
    ResetCursor();
    SDL_StopTextInput();
end;

procedure freeModule;
var i: ShortInt;
begin
    FreeAndNilTexture(InputLinePrefix.Tex);
    FreeAndNilTexture(InputStr.Tex);
    for i:= 0 to MaxStrIndex do
        FreeAndNilTexture(Strs[i].Tex);
end;

end.
