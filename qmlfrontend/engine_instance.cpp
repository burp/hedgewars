#include "engine_instance.h"

#include <QDebug>
#include <QLibrary>
#include <QOpenGLFunctions>
#include <QSurface>

static QOpenGLContext* currentOpenglContext = nullptr;
extern "C" void* getProcAddress(const char* fn) {
  if (!currentOpenglContext)
    return nullptr;
  else
    return reinterpret_cast<void*>(currentOpenglContext->getProcAddress(fn));
}

EngineInstance::EngineInstance(const QString& libraryPath, const QString&dataPath, QObject* parent)
    : QObject(parent), m_instance{nullptr, nullptr} {
  QLibrary hwlib(libraryPath);

  if (!hwlib.load())
    qWarning() << "Engine library not found" << hwlib.errorString();

  hedgewars_engine_protocol_version =
      reinterpret_cast<Engine::hedgewars_engine_protocol_version_t*>(
          hwlib.resolve("hedgewars_engine_protocol_version"));
  start_engine =
      reinterpret_cast<Engine::start_engine_t*>(hwlib.resolve("start_engine"));
  generate_preview = reinterpret_cast<Engine::generate_preview_t*>(
      hwlib.resolve("generate_preview"));
  dispose_preview = reinterpret_cast<Engine::dispose_preview_t*>(
      hwlib.resolve("dispose_preview"));
  cleanup = reinterpret_cast<Engine::cleanup_t*>(hwlib.resolve("cleanup"));

  send_ipc = reinterpret_cast<Engine::send_ipc_t*>(hwlib.resolve("send_ipc"));
  read_ipc = reinterpret_cast<Engine::read_ipc_t*>(hwlib.resolve("read_ipc"));

  setup_current_gl_context =
      reinterpret_cast<Engine::setup_current_gl_context_t*>(
          hwlib.resolve("setup_current_gl_context"));
  render_frame =
      reinterpret_cast<Engine::render_frame_t*>(hwlib.resolve("render_frame"));
  advance_simulation = reinterpret_cast<Engine::advance_simulation_t*>(
      hwlib.resolve("advance_simulation"));
  move_camera =
      reinterpret_cast<Engine::move_camera_t*>(hwlib.resolve("move_camera"));
  simple_event =
      reinterpret_cast<Engine::simple_event_t*>(hwlib.resolve("simple_event"));
  long_event =
      reinterpret_cast<Engine::long_event_t*>(hwlib.resolve("long_event"));
  positioned_event = reinterpret_cast<Engine::positioned_event_t*>(
      hwlib.resolve("positioned_event"));

  m_isValid = hedgewars_engine_protocol_version && start_engine &&
              generate_preview && dispose_preview && cleanup && send_ipc &&
              read_ipc && setup_current_gl_context && render_frame &&
              advance_simulation && move_camera && simple_event && long_event &&
              positioned_event;

  emit isValidChanged(m_isValid);

  if (isValid()) {
    qDebug() << "Loaded engine library with protocol version"
             << hedgewars_engine_protocol_version();

    m_instance = std::unique_ptr<Engine::EngineInstance, Engine::cleanup_t*>(
        start_engine(reinterpret_cast<const int8_t*>(dataPath.toUtf8().data())),
        cleanup);
  } else {
    qDebug("Engine library load failed");
  }
}

EngineInstance::~EngineInstance() = default;

void EngineInstance::sendConfig(const GameConfig& config) {
  for (auto b : config.config()) {
    send_ipc(m_instance.get(), reinterpret_cast<uint8_t*>(b.data()),
             static_cast<size_t>(b.size()));
  }
}

void EngineInstance::advance(quint32 ticks) {
  advance_simulation(m_instance.get(), ticks);
}

void EngineInstance::moveCamera(const QPoint& delta) {
  move_camera(m_instance.get(), delta.x(), delta.y());
}

void EngineInstance::simpleEvent(Engine::SimpleEventType event_type) {
  simple_event(m_instance.get(),
               static_cast<hwengine::SimpleEventType>(event_type));
}

void EngineInstance::longEvent(Engine::LongEventType event_type,
                               Engine::LongEventState state) {
  long_event(m_instance.get(), static_cast<hwengine::LongEventType>(event_type),
             static_cast<hwengine::LongEventState>(state));
}

void EngineInstance::positionedEvent(Engine::PositionedEventType event_type,
                                     qint32 x, qint32 y) {
  positioned_event(m_instance.get(),
                   static_cast<hwengine::PositionedEventType>(event_type), x,
                   y);
}

void EngineInstance::renderFrame() { render_frame(m_instance.get()); }

void EngineInstance::setOpenGLContext(QOpenGLContext* context) {
  currentOpenglContext = context;

  const auto size = context->surface()->size();
  setup_current_gl_context(m_instance.get(), static_cast<quint16>(size.width()),
                           static_cast<quint16>(size.height()),
                           &getProcAddress);
}

QImage EngineInstance::generatePreview() {
  Engine::PreviewInfo pinfo;

  generate_preview(m_instance.get(), &pinfo);

  QVector<QRgb> colorTable;
  colorTable.resize(256);
  for (int i = 0; i < 256; ++i) colorTable[i] = qRgba(255, 255, 0, i);

  QImage previewImage(pinfo.land, static_cast<int>(pinfo.width),
                      static_cast<int>(pinfo.height), QImage::Format_Indexed8);
  previewImage.setColorTable(colorTable);

  // Cannot use it here, since QImage refers to original bytes
  // dispose_preview(m_instance.get());

  return previewImage;
}

bool EngineInstance::isValid() const { return m_isValid; }
