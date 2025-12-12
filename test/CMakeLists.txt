function(build_test test_name src)
    add_executable(${test_name} ${src})
    target_link_libraries(${test_name} PRIVATE ${PROJECT_NAME})
    target_include_directories(${test_name} PUBLIC $<BUILD_INTERFACE:${PROJECT_INCLUDE_DIR}>)
    add_test(
        NAME ${test_name}
        WORKING_DIRECTORY ${CMAKE_LIBRARY_OUTPUT_DIRECTORY}
        COMMAND $<TARGET_FILE:${test_name}>
    )

    if (${BUILD_SHARED_LIBS} AND WIN32)
        add_custom_command(
            TARGET ${test_name}
            POST_BUILD
            COMMAND ${CMAKE_COMMAND} -E copy
            $<TARGET_RUNTIME_DLLS:${test_name}>
            $<TARGET_FILE_DIR:${test_name}>
            COMMAND_EXPAND_LISTS
        )
    endif()
endfunction()

build_test(check check.f90)