! unique.f90 --
!     Construct an array consisting of unique elements
!
module unique_elements
    implicit none

contains
recursive function unique( array ) result(unique_array)
    character(len=*), dimension(:) :: array
    character(len=len(array)), allocatable :: unique_array(:)

    if ( size(array) > 0 ) then
        unique_array = [array(1), unique( pack(array(2:), array(2:) /= array(1)) )]
    else
        allocate( unique_array(0) )
    endif
end function unique

end module unique_elements
program test_unique
    use unique_elements
    implicit none

    character(len = 2) :: array(8) = [ 'AA',  'BB', 'CB', 'AA',  'BB', 'CB', 'DD', 'AA' ]
    character(len = len(array)), allocatable :: unique_values(:)

    unique_values = unique(array)

    write(*,'(10a3)') unique_values
end program test_unique

