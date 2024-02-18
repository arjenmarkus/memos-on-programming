! set_implementation.f90 --
!     Use the idea by John Burkhardt to determine an array of unique elements
!
!     Limit to integers!
!
module set_implementation
    implicit none

contains
subroutine unique( array, unique_elements )
    integer, intent(in)               :: array(:)
    integer, intent(out), allocatable :: unique_elements(:)

    logical, allocatable              :: work_array(:)

    integer                           :: i, min_value, max_value

    min_value = minval( array )
    max_value = maxval( array )

    allocate( work_array(min_value:max_value) )
    work_array = .false.

    ! Use the input array as index
    work_array(array) = .true.

    unique_elements = pack( [ (i, i = min_value, max_value)], work_array )
end subroutine unique
end module set_implementation

! Test program
!
program test_set_implementation
    use set_implementation
    implicit none

    integer, allocatable :: array(:)
    integer, allocatable :: unique_elements(:)

    array = [10, 2, 3, 2, 1, 1, 10]
    call unique( array, unique_elements )

    write(*,*) unique_elements
end program test_set_implementation
