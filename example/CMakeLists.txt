# DEMO_REVERSE
add_executable(demo_reverse
  demo_reverse.f90)
target_link_libraries(demo_reverse PRIVATE forcolormap)

# DEMO
add_executable(demo
  demo.f90)
target_link_libraries(demo PRIVATE forcolormap)

# EXAMPLE1
add_executable(example1
  example1.f90)
target_link_libraries(example1 PRIVATE forcolormap)

# EXTRACT
add_executable(extract
  extract.f90)
target_link_libraries(extract PRIVATE forcolormap)

# INFO
add_executable(info
  info.f90)
target_link_libraries(info PRIVATE forcolormap)

# MODIFY
add_executable(modify
  modify.f90)
target_link_libraries(modify PRIVATE forcolormap)

# CREATE
add_executable(create
  create.f90)
target_link_libraries(create PRIVATE forcolormap)

# COLORMAPS_LIST
add_executable(colormaps_list
  colormaps_list.f90)
target_link_libraries(colormaps_list PRIVATE forcolormap)