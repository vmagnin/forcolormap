program example1
    use forcolormap
    use forimage
    implicit none

    type(Colormap) :: custom_cmap
    type(format_pnm) :: ex1_colormap, ex1_colorbar

    ! Create ppm files
    call custom_cmap%load("test_map_to_load.txt", 0.0_wp, 2.0_wp)
    call custom_cmap%test("a_loaded_colormap_ascii", 'ascii')
    call custom_cmap%print()

    ! Import ascii ppm files
    call ex1_colormap%import_pnm('a_loaded_colormap_ascii_test','ppm', 'ascii')
    call ex1_colorbar%import_pnm('a_loaded_colormap_ascii_colorbar','ppm', 'ascii')
   
    ! Change colormap and colorbar colors
    ex1_colormap%pixels = ex1_colormap%pixels * (1.6)
    ex1_colorbar%pixels = ex1_colorbar%pixels * (1.6)

    ! Export binary ppm files
    call ex1_colormap%export_pnm('a_loaded_colormap_binary_test_m', 'binary')
    call ex1_colorbar%export_pnm('a_loaded_colormap_binary_colorbar_m', 'binary')

    ! Deallocate
    call ex1_colormap%finalize()
    call ex1_colorbar%finalize()
end program example1