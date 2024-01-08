function(build_test test_name src)
    add_executable(${test_name} ${src})
    target_link_libraries(${test_name} PRIVATE ${PROJECT_NAME})
    target_include_directories(${test_name} PUBLIC $<BUILD_INTERFACE:${PROJECT_INCLUDE_DIR}>)
    add_test(
        NAME ${test_name}
        WORKING_DIRECTORY ${CMAKE_LIBRARY_OUTPUT_DIRECTORY}
        COMMAND $<TARGET_FILE:${test_name}>
    )
endfunction()

build_test(check check.f90)