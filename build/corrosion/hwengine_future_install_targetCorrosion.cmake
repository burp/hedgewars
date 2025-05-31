
add_library(hwengine_future-shared SHARED IMPORTED)
set_target_properties(hwengine_future-shared
    PROPERTIES
    IMPORTED_LOCATION "${PACKAGE_PREFIX_DIR}/lib/libhwengine_future.so"
)
