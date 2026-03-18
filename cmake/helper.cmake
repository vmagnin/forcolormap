# helper.cmake
#
# A collection of macros and functions making life with CMake and Fortran a
# bit simpler.

# Use instead of add_library.
function(forcolormap_add_fortran_library lib_name mod_dir include_install_dir version major)
    message(STATUS ">>> include_install_dir=${include_install_dir}")
    add_library(${lib_name} ${ARGN})
    set_target_properties(
        ${lib_name}
        PROPERTIES
            POSITION_INDEPENDENT_CODE TRUE
            OUTPUT_NAME ${lib_name}
            VERSION ${version}
            SOVERSION ${major}
            Fortran_MODULE_DIRECTORY ${mod_dir}
    )
    target_include_directories(
        ${lib_name}
        PUBLIC
        $<BUILD_INTERFACE:${mod_dir}>
        $<INSTALL_INTERFACE:${include_install_dir}>
    )
endfunction()

# Installs the library
function(forcolormap_install_library lib_name lib_install_dir bin_install_dir mod_dir install_dir)
    install(
        TARGETS ${lib_name}
        EXPORT ${lib_name}Targets
        RUNTIME DESTINATION ${bin_install_dir}
        LIBRARY DESTINATION ${lib_install_dir}
        ARCHIVE DESTINATION ${lib_install_dir}
        INCLUDES DESTINATION ${install_dir}
#        INCLUDES DESTINATION ${install_dir}/include
    )
    message(STATUS ">>> install_dir=${install_dir}")

    install(
        DIRECTORY ${mod_dir}/
        DESTINATION ${install_dir}
    )
endfunction()

# Links the supplied library
function(forcolormap_link_library targ lib include_dir)
    target_link_libraries(${targ} ${lib})
    target_include_directories(${targ} PUBLIC $<BUILD_INTERFACE:${include_dir}>)
endfunction()

# ------------------------------------------------------------------------------
# Helpful Macros
macro(forcolormap_print_all_variables)
    message(STATUS "---------- CURRENTLY DEFINED VARIABLES -----------")
    get_cmake_property(varNames VARIABLES)
    foreach(varName ${varNames})
        message(STATUS ${varName} = ${${varName}})
    endforeach()
    message(STATUS "---------- END ----------")
endmacro()
