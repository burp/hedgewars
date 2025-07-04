find_package(SDL2 REQUIRED CONFIG)

include_directories(${SDL2_INCLUDE_DIRS})
include_directories(${PHYSFS_INCLUDE_DIR})
include_directories(${LUA_INCLUDE_DIR})


set(PHYSLAYER_SRCS
    physfscompat.c
    physfsrwops.c
    physfslualoader.c
    hwpacksmounter.c
)

#compiles and links actual library
add_library (physlayer ${PHYSLAYER_SRCS})
set_target_properties(physlayer PROPERTIES
                          VERSION 1.0
                          SOVERSION 1.0)
if(WIN32 AND VCPKG_TOOLCHAIN)                          
    target_link_libraries(physlayer SDL2::SDL2 lua physfs)
else()
    target_link_libraries(physlayer ${SDL2_LIBRARIES} lua physfs)
endif()
install(TARGETS physlayer RUNTIME DESTINATION ${target_binary_install_dir}
                          LIBRARY DESTINATION ${target_library_install_dir}
                          ARCHIVE DESTINATION ${target_library_install_dir})


## added standard variables (FORCE or cmake won't pick 'em)
set(PHYSLAYER_INCLUDE_DIR ${CMAKE_CURRENT_SOURCE_DIR} CACHE STRING "Physlayer include dir" FORCE)
set(PHYSLAYER_LIBRARY physlayer CACHE STRING "Physlayer library" FORCE)

if(BUILD_ENGINE_JS)
    set_target_properties(physlayer PROPERTIES SUFFIX ".bc")
endif()

