cmake_minimum_required(VERSION 3.8)

project(qmlfrontend LANGUAGES CXX)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

set(CMAKE_INCLUDE_CURRENT_DIR ON)
set(CMAKE_AUTOMOC ON)
set(CMAKE_AUTORCC ON)

find_package(Qt6 COMPONENTS Core Quick REQUIRED)

add_executable(${PROJECT_NAME} "main.cpp" "qml.qrc"
    "hwengine.cpp" "hwengine.h"
    "game_config.cpp" "game_config.h"
    "game_view.cpp" "game_view.h"
    "team.cpp" "team.h"
    "engine_instance.cpp" "engine_instance.h"
    "preview_image_provider.cpp" "preview_image_provider.h"
    "engine_interface.h"
    "preview_acceptor.cpp" "preview_acceptor.h"
    "net_session.cpp" "net_session.h"
    "players_model.cpp" "players_model.h"
    "rooms_model.cpp" "rooms_model.h"
    )

target_link_libraries(${PROJECT_NAME} Qt6::Core Qt6::Network Qt6::Quick)
