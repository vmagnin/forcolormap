cmake_minimum_required(VERSION 3.24)
project(
    forcolormap
    LANGUAGES Fortran
    VERSION 0.9.0
)

# Get useful macros
include("${PROJECT_SOURCE_DIR}/cmake/helper.cmake")

# Set a default build type if none was specified
if(NOT CMAKE_BUILD_TYPE AND NOT CMAKE_CONFIGURATION_TYPES)
  message(STATUS "Setting build type to 'Release' as none was specified.")
  set(CMAKE_BUILD_TYPE Release CACHE STRING "Choose the type of build." FORCE)
  # Set the possible values of build type for cmake-gui
  set_property(CACHE CMAKE_BUILD_TYPE PROPERTY STRINGS "Debug" "Release")
endif()

# By default, static library
option(BUILD_SHARED_LIBS "Build shared libraries" OFF)

# Export all symbols on Windows when building libraries
set(CMAKE_WINDOWS_EXPORT_ALL_SYMBOLS TRUE)

# Utilize the GNU installation structure
include(GNUInstallDirs)

# Locate the local include directory
set(PROJECT_INCLUDE_DIR ${PROJECT_BINARY_DIR}/include)

# Dependencies
add_subdirectory(cmake/dependencies)

# Build
add_subdirectory(src)
add_fortran_library(
  ${PROJECT_NAME}
  ${PROJECT_INCLUDE_DIR}
  ${CMAKE_INSTALL_INCLUDEDIR}
  ${PROJECT_VERSION}
  ${PROJECT_VERSION_MAJOR}
  ${FORCOLORMAP_SOURCES}
)
link_library(${PROJECT_NAME} ${forimage_LIBRARY} ${forimage_INCLUDE_DIR})

# Installation
add_subdirectory(cmake/install)

#===============================================================================
# Uninstall target:
# Generic code adapted from
# https://gitlab.kitware.com/cmake/community/-/wikis/FAQ#can-i-do-make-uninstall-with-cmake
#===============================================================================
if(NOT TARGET uninstall_forcolormap)
    # configure_file() copies a file and substitutes @VAR@ or ${VAR}
    # @ONLY means only @VAR@ will be sustituted
    # https://cmake.org/cmake/help/latest/command/configure_file.html
    configure_file(
        "${CMAKE_CURRENT_SOURCE_DIR}/cmake/cmake_uninstall.cmake.in"
        "${CMAKE_CURRENT_BINARY_DIR}/cmake_uninstall.cmake"
        @ONLY)

    add_custom_target(uninstall_forcolormap
        COMMAND ${CMAKE_COMMAND} -P ${CMAKE_CURRENT_BINARY_DIR}/cmake_uninstall.cmake)
endif()

# Testing
option(BUILD_TESTING "Build tests")
include(CTest)
message(STATUS "Build FORCOLORMAP tests: ${BUILD_TESTING}")
if (BUILD_TESTING)
    enable_testing()
    add_subdirectory(test)
endif()

# Examples
option(BUILD_FORCOLORMAP_EXAMPLES "Build examples")
message(STATUS "Build FORCOLORMAP examples: ${BUILD_FORCOLORMAP_EXAMPLES}")
if (BUILD_FORCOLORMAP_EXAMPLES)
    add_subdirectory(example)
endif()
