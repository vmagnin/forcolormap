set(dir ${CMAKE_CURRENT_SOURCE_DIR})

set(FORCOLORMAP_SOURCES
    ${dir}/forcolormap.f90
    ${dir}/forcolormap_parameters.f90
    ${dir}/forcolormap_utils.f90
    ${dir}/forcolormap_info.f90
    ${dir}/forcolormap_cm_matplotlib.f90
    ${dir}/forcolormap_cm_miscellaneous.f90
    ${dir}/forcolormap_cm_scientific.f90
)
set(FORCOLORMAP_SOURCES ${FORCOLORMAP_SOURCES} PARENT_SCOPE)
