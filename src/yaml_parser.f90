module yaml_parser
    use iso_fortran_env, only: wp => real64
    implicit none

    contains
        ! Main subroutine to parse the YAML file
        subroutine parse_yaml(file_name, mach_numbers, aoa, stl_file)
            character(len=*), intent(in) :: file_name
            real(wp), allocatable, intent(out) :: mach_numbers(:)
            real(wp), allocatable, intent(out) :: aoa(:)
            character(len=:), allocatable, intent(out) :: stl_file

            integer :: unit, ierr
            character(len=256) :: line, key, value

            ! Open the YAML file for reading
            open(newunit=unit, file=trim(file_name), status='old', action='read', iostat=ierr)
            if (ierr /= 0) then
                print *, "Error opening file: ", trim(file_name)
                stop
            end if

            ! Read the YAML file line by line
            do
                read(unit, '(A)', iostat=ierr) line
                if (ierr /= 0) exit  ! Exit loop if end of file or read error

                ! Process non-empty lines
                if (trim(line) /= "") then
                    ! Parse the line into key and value
                    call parse_line(line, key, value)
                    ! Process the key-value pair
                    select case (trim(adjustl(key)))
                    case ('stl_file')
                        stl_file = trim(adjustl(value))  ! Extract STL file name
                    case ('mach_numbers')
                        call parse_array(trim(adjustl(value)), mach_numbers)  ! Extract Mach numbers
                    case ('angles_of_attack')
                        call parse_array(trim(adjustl(value)), aoa)  ! Extract angles of attack
                    end select
                end if
            end do

            ! Close the YAML file
            close(unit)

            ! Print parsed values for debugging
            print *, "Parsed STL File: ", stl_file
            print *, "Parsed Mach Numbers: ", mach_numbers
            print *, "Parsed Angles of Attack: ", aoa
        end subroutine parse_yaml

        ! Subroutine to parse a single line and extract key-value pairs
        subroutine parse_line(line, key, value)
            character(len=*), intent(in) :: line
            character(len=256), intent(out) :: key, value
            integer :: pos

            ! Find the position of the colon character
            pos = index(line, ':')
            if (pos > 0) then
                ! Extract the key and value based on the position of the colon
                key = adjustl(line(1:pos-1))
                value = adjustl(line(pos+1:))
            else
                key = ''
                value = ''
            end if

            ! Debugging output
            print *, "Parsed line - Key: ", key, " Value: ", value
        end subroutine parse_line

        ! Subroutine to parse an array from a YAML value
        subroutine parse_array(value, array)
            character(len=*), intent(in) :: value
            real(wp), allocatable, intent(out) :: array(:)
            character(len=:), allocatable :: trimmed_value
            character(len=256) :: num_str
            integer :: i, num_start, num_end, num_count, ierr

            ! Debugging statement
            print *, "Original value: ", value

            ! Check for surrounding brackets and trim them
            if (len(value) > 1 .and. value(1:1) == '[' .and. value(len(value):len(value)) == ']') then
                trimmed_value = value(2:len(value)-1)
            else
                print *, "Error: Array value not properly formatted: ", value
                stop
            end if

            ! Debugging statement
            print *, "Trimmed value: ", trimmed_value

            ! Count the number of values (comma-separated)
            num_count = 1
            do i = 1, len(trimmed_value)
                if (trimmed_value(i:i) == ',') num_count = num_count + 1
            end do
            allocate(array(num_count))

            ! Extract and convert each number
            num_start = 1
            do i = 1, num_count
                num_end = index(trimmed_value(num_start:), ',')
                if (num_end == 0) num_end = len(trimmed_value) - num_start + 1  ! Last value
                num_str = adjustl(trim(trimmed_value(num_start:num_start+num_end-1)))
                print *, "Reading value: ", num_str  ! Debugging statement
                read(num_str, *, iostat=ierr) array(i)
                if (ierr /= 0) then
                    print *, "Error reading value: ", num_str
                    stop
                end if
                num_start = num_start + num_end + 1
            end do

            ! Print parsed array for debugging
            print *, "Parsed array: ", array
        end subroutine parse_array

end module yaml_parser
