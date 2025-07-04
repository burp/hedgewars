cmake_minimum_required(VERSION 3.12.0)

#initialise cmake environment
foreach(hwpolicy CMP0003 CMP0012 CMP0017 CMP0018 CMP0048)
    if(POLICY ${hwpolicy})
        cmake_policy(SET ${hwpolicy} NEW)
    endif()
endforeach()

foreach(hwpolicy CMP0026 CMP0068)
    if(POLICY ${hwpolicy})
        cmake_policy(SET ${hwpolicy} NEW)
    endif()
endforeach()

project(hedgewars VERSION 1.1.0)

set(CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}/cmake_modules")
include(${CMAKE_MODULE_PATH}/utils.cmake)

#possible cmake configuration
option(NOSERVER "Disable gameServer build (off)" OFF)
if(NOT WIN32)
    option(NOPNG "Disable screenshoot compression (off)" OFF)
endif()
option(NOVIDEOREC "Disable video recording (off)" OFF)

#libraries are built shared unless explicitly added as a static
option(BUILD_SHARED_LIBS "Build libraries as shared modules (on)" ON)

if(WIN32 OR APPLE)
    option(LUA_SYSTEM "Use system Lua (off)" OFF)
else()
    option(LUA_SYSTEM "Use system Lua (on)" ON)
endif()

option(BUILD_ENGINE_LIBRARY "Enable hwengine library (off)" OFF)
option(ANDROID "Enable Android build (off)" OFF)

option(MINIMAL_FLAGS "Respect system flags as much as possible (off)" OFF)
option(NOAUTOUPDATE "Disable OS X Sparkle update checking (off)" OFF)
option(SKIPBUNDLE "Do not create relocate bundle (off)" OFF)

option(BUILD_ENGINE_C "Compile hwengine as native C (off)" OFF)
option(BUILD_ENGINE_JS "Compile hwengine as JavaScript (off)" OFF)
option(GL2 "Enable OpenGL 2 rendering, only use if you know what you're doing (off)" OFF)

set(GHFLAGS "" CACHE STRING "Additional Haskell flags")
if(UNIX AND NOT APPLE)
    set(DATA_INSTALL_DIR "share/hedgewars" CACHE STRING "Resource folder path")
endif()

option(NOVERSIONINFOUPDATE "Disable update of version_info.txt. To be used if source is in a git/repo that is NOT the hedgewars repo" OFF)


if(BUILD_ENGINE_C AND NOT NOVIDEOREC)
    if((CMAKE_BUILD_TYPE STREQUAL "Release") OR (CMAKE_BUILD_TYPE STREQUAL "RelWithDebInfo"))
        message("NOTE: Video recorder support disabled. It's incompatible with BUILD_ENGINE_C")
        set(BUILD_ENGINE_C ON CACHE STRING "Required for BUILD_ENGINE_JS" FORCE)
    else()
        message("WARNING: Video recorder support is currently incompatible with BUILD_ENGINE_C, the video recorder won't work (but demos are fine)! See <https://issues.hedgewars.org/show_bug.cgi?id=722>.")
    endif()
endif()
if(BUILD_ENGINE_JS)
    if(NOT CMAKE_TOOLCHAIN_FILE)
        message(FATAL_ERROR "Missing emscripten toolchain file\nClean your cache and rerun cmake with -DCMAKE_TOOLCHAIN_FILE=${CMAKE_SOURCE_DIR}/cmake_modules/Platform/Emscripten.cmake")
    endif()

    set(BUILD_ENGINE_C ON CACHE STRING "Required for BUILD_ENGINE_JS" FORCE)
    set(BUILD_ENGINE_LIBRARY ON CACHE STRING "Required for BUILD_ENGINE_JS" FORCE)
    set(NOAUTOUPDATE ON CACHE STRING "Required for BUILD_ENGINE_JS" FORCE)
    set(LUA_SYSTEM OFF CACHE STRING "Required for BUILD_ENGINE_JS" FORCE)
    set(NOVIDEOREC ON CACHE STRING "Required for BUILD_ENGINE_JS" FORCE)
    set(NOSERVER ON CACHE STRING "Required for BUILD_ENGINE_JS" FORCE)
    set(GL2 ON CACHE STRING "Required for BUILD_ENGINE_JS" FORCE)
    set(BUILD_SHARED_LIBS OFF CACHE STRING "Required for BUILD_ENGINE_JS" FORCE)

    set(target_binary_install_dir "bin" CACHE PATH "install dest for binaries")
    set(target_library_install_dir "lib" CACHE PATH "install dest for libs")
endif()

if("${CMAKE_SIZEOF_VOID_P}" EQUAL "4" AND UNIX AND NOT APPLE)
    set(BUILD_ENGINE_C ON CACHE STRING "PAS2C force-enabled due to a freepascal 32 bit alignment bug" FORCE)
endif()

#system paths for finding required fonts (see share/hedgewars/Data/fonts)
#subdirectories will NOT be searched.
#all fonts that can't be found will be bundled with hedgewars
set(FONTS_DIRS "" CACHE STRING "Additional paths to folders where required fonts can be found ( ; is separator)")

#versioning
set(HEDGEWARS_PROTO_VER 60)
if((CMAKE_BUILD_TYPE STREQUAL "Release") OR (CMAKE_BUILD_TYPE STREQUAL "RelWithDebInfo"))
    set(HEDGEWARS_VERSION "${CMAKE_PROJECT_VERSION_MAJOR}.${CMAKE_PROJECT_VERSION_MINOR}.${CMAKE_PROJECT_VERSION_PATCH}")
else()
    set(HEDGEWARS_VERSION "${CMAKE_PROJECT_VERSION_MAJOR}.${CMAKE_PROJECT_VERSION_MINOR}.${CMAKE_PROJECT_VERSION_PATCH}-dev")
endif()
include(${CMAKE_MODULE_PATH}/revinfo.cmake)

message(STATUS "Building ${HEDGEWARS_VERSION}-r${HEDGEWARS_REVISION} (${HEDGEWARS_HASH})")

#io library paths
include(${CMAKE_MODULE_PATH}/paths.cmake)
#general utilities
include(${CMAKE_MODULE_PATH}/utils.cmake)
#platform specific init code
include(${CMAKE_MODULE_PATH}/platform.cmake)

#when build type is not specified, assume Debug/Release according to build version information
if(CMAKE_BUILD_TYPE)
    if(NOT((CMAKE_BUILD_TYPE STREQUAL "Release") OR
           (CMAKE_BUILD_TYPE STREQUAL "Debug") OR
           (CMAKE_BUILD_TYPE STREQUAL "RelWithDebInfo")))
        set(CMAKE_BUILD_TYPE ${default_build_type} CACHE STRING "Build type (Debug/Release/RelWithDebInfo)" FORCE)
        message(STATUS "Unknown build type ${CMAKE_BUILD_TYPE}, using default (${default_build_type})")
    endif()
else(CMAKE_BUILD_TYPE)
    set(CMAKE_BUILD_TYPE ${default_build_type} CACHE STRING "Build type (Debug/Release/RelWithDebInfo)" FORCE)
endif(CMAKE_BUILD_TYPE)


#perform safe check that enable/disable compilation features
#skip when crosscompiling to javascript
if(NOT BUILD_ENGINE_JS)
    include(${CMAKE_MODULE_PATH}/compilerchecks.cmake)
endif()

#set default compiler flags
if(WIN32 AND VCPKG_TOOLCHAIN)
    add_flag_append(CMAKE_C_FLAGS "/DWIN32_VCPKG /Wall")
    add_flag_append(CMAKE_C_FLAGS_RELEASE "/Ox")
    add_flag_append(CMAKE_C_FLAGS_DEBUG "/Od")
    add_flag_append(CMAKE_CXX_FLAGS "/DWIN32_VCPKG /Wall")
    add_flag_append(CMAKE_CXX_FLAGS_RELEASE "/Ox")
    add_flag_append(CMAKE_CXX_FLAGS_DEBUG "/Od")    
else()
    add_flag_append(CMAKE_C_FLAGS "-Wall -pipe")
    add_flag_append(CMAKE_C_FLAGS_RELEASE "-O3")
    add_flag_append(CMAKE_C_FLAGS_DEBUG "-Wextra -O0")
    add_flag_append(CMAKE_CXX_FLAGS "-Wall -pipe")
    add_flag_append(CMAKE_CXX_FLAGS_RELEASE "-O3")
    add_flag_append(CMAKE_CXX_FLAGS_DEBUG "-Wextra -O0")    
endif()       

#CMake adds a lot of additional configuration flags, so let's clear them up
if(MINIMAL_FLAGS)
    unset(CMAKE_C_FLAGS_RELEASE)
    unset(CMAKE_C_FLAGS_DEBUG)
    unset(CMAKE_CXX_FLAGS_RELEASE)
    unset(CMAKE_CXX_FLAGS_DEBUG)
endif()


#parse additional parameters
if(GHFLAGS)
    if(${CMAKE_VERSION} VERSION_GREATER 2.6)
        separate_arguments(ghflags_parsed UNIX_COMMAND ${GHFLAGS})
    else()
        message("*** GHFLAGS are available only when using CMake >= 2.8 ***")
    endif()
endif()


if(GHC_DYNAMIC)
  list(APPEND haskell_flags "-dynamic")
endif()

#get BUILD_TYPE and enable/disable optimisation
message(STATUS "Using ${CMAKE_BUILD_TYPE} configuration")
if(CMAKE_BUILD_TYPE STREQUAL "Debug")
    list(APPEND haskell_flags "-Wall"       # all warnings
                              "-debug"      # debug mode
                              "-fno-warn-unused-do-bind"
                              "-O0"
                              )
    set(USE_DEBUG_LIBRARIES TRUE)
else()
    list(APPEND haskell_flags "-w" # no warnings
                              "-O2"
                              )
endif()


#build engine without freepascal
if(BUILD_ENGINE_C AND NOT BUILD_ENGINE_JS)
    find_package(Clang REQUIRED)

    if(${CLANG_VERSION} VERSION_LESS "3.0")
        message(FATAL_ERROR "LLVM/Clang compiler required version is 3.0 but version ${CLANG_VERSION} was found!")
    endif()

    set(CMAKE_C_COMPILER ${CLANG_EXECUTABLE})
    set(CMAKE_CXX_COMPILER ${CLANG_EXECUTABLE})
endif()


#server
if(NOT NOSERVER)
    add_subdirectory(gameServer)
endif()


#lua discovery
if(LUA_SYSTEM)
    if(NOT LUA_LIBRARY OR NOT LUA_INCLUDE_DIR)
        find_package(Lua)
    endif()

    if(LUA_LIBRARY AND LUA_INCLUDE_DIR)
        #use an IMPORTED tharget so that we can just use 'lua' to link
        add_library(lua UNKNOWN IMPORTED)
        set_target_properties(lua PROPERTIES IMPORTED_LOCATION ${LUA_LIBRARY})
    else()
        message(FATAL_ERROR "Missing Lua! Rerun cmake with -DLUA_SYSTEM=off to build the internal version")
    endif()
else()
    if(NOT LUA_LIBRARY OR NOT LUA_INCLUDE_DIR)
        message(STATUS "LUA will be provided by the bundled sources")
    endif()
    set(lua_output_name "hwlua")
    add_subdirectory(misc/liblua)
endif()


#physfs discovery
if(NOT PHYSFS_LIBRARY OR NOT PHYSFS_INCLUDE_DIR)
    find_package(PhysFS)
endif()

find_file(physfs_h physfs.h ${PHYSFS_INCLUDE_DIR})
if(physfs_h)
    file(STRINGS ${physfs_h} physfs_majorversion REGEX "PHYSFS_VER_MAJOR[\t' ']+[0-9]+")
    file(STRINGS ${physfs_h} physfs_minorversion REGEX "PHYSFS_VER_MINOR[\t' ']+[0-9]+")
    file(STRINGS ${physfs_h} physfs_patchversion REGEX "PHYSFS_VER_PATCH[\t' ']+[0-9]+")
    string(REGEX MATCH "([0-9]+)" physfs_majorversion "${physfs_majorversion}")
    string(REGEX MATCH "([0-9]+)" physfs_minorversion "${physfs_minorversion}")
    string(REGEX MATCH "([0-9]+)" physfs_patchversion "${physfs_patchversion}")
    set(physfs_detected_ver "${physfs_majorversion}.${physfs_minorversion}.${physfs_patchversion}")

    if(${physfs_detected_ver} VERSION_LESS 3.0.0)
        message(FATAL_ERROR "PhysFS version is too old (detected ${physfs_detected_ver}, required 3.0.0)\n"
                            "Perform an update of PhysFS to fix this.")
    endif()
endif()

if(PHYSFS_LIBRARY AND PHYSFS_INCLUDE_DIR)
    #use an IMPORTED tharget so that we can just use 'physfs' to link
    add_library(physfs UNKNOWN IMPORTED)
    if (DEFINED PHYSFS_LIBRARY_RELEASE)
        if (${USE_DEBUG_LIBRARIES})
            set_target_properties(physfs PROPERTIES IMPORTED_LOCATION ${PHYSFS_LIBRARY_DEBUG})
        else()    
            set_target_properties(physfs PROPERTIES IMPORTED_LOCATION ${PHYSFS_LIBRARY_RELEASE})
        endif()
    else()
        set_target_properties(physfs PROPERTIES IMPORTED_LOCATION ${PHYSFS_LIBRARY})
    endif()
else()
    message(FATAL_ERROR "Missing PhysFS! Install PhysFS to fix this.")
endif()

find_package_or_disable_msg(LIBAV NOVIDEOREC "Video recording will not be built")


#physfs helper library
add_subdirectory(misc/libphyslayer)


#rust libraries
add_subdirectory(tools/corrosion)
corrosion_import_crate(MANIFEST_PATH rust/lib-hwengine-future/Cargo.toml)
corrosion_install(TARGETS hwengine_future EXPORT hwengine_future_install_target)
install(EXPORT hwengine_future_install_target DESTINATION ${target_library_install_dir})

#maybe this could be merged inside hedgewars/CMakeLists.txt
if(BUILD_ENGINE_C)
    #pascal to c converter
    add_subdirectory(tools/pas2c)
    add_subdirectory(project_files/hwc)
else()
    #main pascal engine
    add_subdirectory(hedgewars)
endif()

#Android related build scripts
#TODO: when ANDROID, BUILD_ENGINE_LIBRARY should be set
if(ANDROID)
    add_subdirectory(project_files/Android-build)
else(ANDROID)
    #skip frontend for javascript
    if(NOT BUILD_ENGINE_JS)
        add_subdirectory(QTfrontend)
        add_subdirectory(share)
    endif()

    add_subdirectory(bin)
    add_subdirectory(tools)
endif(ANDROID)

include(${CMAKE_MODULE_PATH}/cpackvars.cmake)

enable_testing()

add_custom_target(test_normal  COMMAND ${CMAKE_CTEST_COMMAND} -E '^todo/' --timeout 300 --schedule-random)
add_custom_target(test_verbose COMMAND ${CMAKE_CTEST_COMMAND} -E '^todo/' --timeout 300 --schedule-random -V)

set(LUATESTS_DIR "${CMAKE_SOURCE_DIR}/tests/lua")
set(TESTSDATA_DIR "${CMAKE_SOURCE_DIR}/share/hedgewars/Data")

# set set this to "" if you want to see what's going on
# TODO: engine should do this implicitly when running tests,
# unless some env var like HWENGINE_SHOWTESTS is set or something
set(STATSONLYFLAG "--stats-only")

# add all lua tests
file(GLOB_RECURSE luatests RELATIVE "${LUATESTS_DIR}" "${LUATESTS_DIR}/*.lua")
foreach(luatest ${luatests})
    add_test("${luatest}" "bin/hwengine" "--prefix" "${TESTSDATA_DIR}" "--nosound" "--nomusic" "${STATSONLYFLAG}" "--lua-test" "${LUATESTS_DIR}/${luatest}")
endforeach(luatest)

