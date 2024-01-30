# Get the macros and functions we'll need
include(FetchContent)

# Fetch the proper content
FetchContent_Declare(
    forimage
    GIT_REPOSITORY "https://github.com/gha3mi/forimage"
    GIT_TAG main
)

FetchContent_MakeAvailable(forimage)

if (WIN32)
    if (BUILD_SHARED_LIBS)
        add_custom_command(
            TARGET ${PROJECT_NAME} POST_BUILD
            COMMAND ${CMAKE_COMMAND} -E copy_if_different
            $<TARGET_FILE:forimage>
            $<TARGET_FILE_DIR:${PROJECT_NAME}
        )
    endif()
endif()

set(forimage_INCLUDE_DIR ${forimage_BINARY_DIR}/include)
set(forimage_INCLUDE_DIR ${forimage_INCLUDE_DIR} PARENT_SCOPE)

# Make a parent-scope variable for the library
set(forimage_LIBRARY forimage)
set(forimage_LIBRARY ${forimage_LIBRARY} PARENT_SCOPE)
