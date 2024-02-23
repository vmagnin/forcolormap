set(dir ${CMAKE_CURRENT_SOURCE_DIR})

set(FORCOLORMAP_SOURCES
    ${dir}/colormap_class.f90
    ${dir}/forcolormap_utils.f90
    ${dir}/colormap_parameters.f90
    ${dir}/colormaps_info.f90
    ${dir}/matplotlib_colormaps.f90
    ${dir}/miscellaneous_colormaps.f90
    ${dir}/scientific_colour_maps.f90
)
set(FORCOLORMAP_SOURCES ${FORCOLORMAP_SOURCES} PARENT_SCOPE)
