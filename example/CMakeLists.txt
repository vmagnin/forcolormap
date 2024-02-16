# DEMO_REVERSE
add_executable(demo_reverse demo_reverse.f90)
target_link_libraries(demo_reverse ${PROJECT_NAME})

# DEMO
add_executable(demo demo.f90)
target_link_libraries(demo ${PROJECT_NAME})

# EXAMPLE1
add_executable(example1 example1.f90)
target_link_libraries(example1 ${PROJECT_NAME})

# EXTRACT
add_executable(extract extract.f90)
target_link_libraries(extract ${PROJECT_NAME})

# INFO
add_executable(info info.f90)
target_link_libraries(info ${PROJECT_NAME})

# MODIFY
add_executable(modify modify.f90)
target_link_libraries(modify ${PROJECT_NAME})

# CREATE
add_executable(create create.f90)
target_link_libraries(create ${PROJECT_NAME})
