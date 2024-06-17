module stl_reader
    use iso_fortran_env, only: wp => real64
    implicit none

    contains
        ! Subroutine to read STL file and extract vertices and normals
        subroutine read_stl(stl_file, vertices, normals)
            character(len=:), allocatable :: stl_file
            real(wp), allocatable, intent(out) :: vertices(:,:), normals(:,:)
            integer :: unit, ierr, num_vertices, i, vertex_idx
            character(len=256) :: line

            ! Open the STL file for reading
            print *, "Attempting to open STL file: ", trim(stl_file)
            open(newunit=unit, file=trim(stl_file), status='old', action='read', iostat=ierr)
            if (ierr /= 0) then
                print *, "Error opening STL file: ", trim(stl_file)
                stop
            end if

            ! Count the number of vertices in the STL file
            num_vertices = 0
            do
                read(unit, '(A)', iostat=ierr) line
                if (ierr /= 0) exit
                if (index(trim(line), 'vertex') > 0) num_vertices = num_vertices + 1
            end do

            ! Allocate arrays for vertices and normals
            allocate(vertices(3, num_vertices / 3))
            allocate(normals(3, num_vertices / 3))

            ! Rewind the file to read the vertices and normals
            rewind(unit)
            i = 0
            vertex_idx = 0
            do
                read(unit, '(A)', iostat=ierr) line
                if (ierr /= 0) exit
                if (index(trim(line), 'facet normal') > 0) then
                    i = i + 1
                    read(line(7:), '(3F8.5)', iostat=ierr) normals(:, i)
                    if (ierr /= 0) then
                        print *, "Error reading normal from line: ", trim(line)
                        stop
                    end if
                end if
                if (index(trim(line), 'vertex') > 0) then
                    vertex_idx = vertex_idx + 1
                    read(line(8:), '(3F8.5)', iostat=ierr) vertices(:, vertex_idx)
                    if (ierr /= 0) then
                        print *, "Error reading vertex from line: ", trim(line)
                        stop
                    end if
                end if
            end do

            ! Close the STL file
            close(unit)

            ! Debugging output
            print *, "Read STL file: ", stl_file
            print *, "Number of vertices: ", num_vertices
            print *, "Vertices: ", vertices
            print *, "Normals: ", normals
        end subroutine read_stl

end module stl_reader
