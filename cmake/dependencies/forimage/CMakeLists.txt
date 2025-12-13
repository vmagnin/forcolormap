# Get the macros and functions we'll need
include(FetchContent)

# Fetch the proper content
FetchContent_Declare(
    forimage
    GIT_REPOSITORY "https://github.com/gha3mi/forimage"
    GIT_TAG main
)

FetchContent_MakeAvailable(forimage)


set(forimage_INCLUDE_DIR ${forimage_BINARY_DIR}/include)
set(forimage_INCLUDE_DIR ${forimage_INCLUDE_DIR} PARENT_SCOPE)

# Make a parent-scope variable for the library
set(forimage_LIBRARY forimage)
set(forimage_LIBRARY ${forimage_LIBRARY} PARENT_SCOPE)
