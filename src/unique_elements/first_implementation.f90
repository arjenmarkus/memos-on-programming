! first_implementation.f90 --
!     Construct an array consisting of unique elements
!
module unique_elements
    implicit none

contains
subroutine unique( array, unique_elements )
    character(len=*), intent(in)           :: array(:)
    character(len=len(array)), allocatable :: unique_elements(:)

    character(len=len(array)), allocatable :: work_array(:)
    integer                                :: i, n

    if ( size(array) <= 1 ) then
        !
        ! Take care of the trivial cases
        !
        unique_elements = array
    else
        !
        ! Scan the elements and store the ones that we encounter
        ! for the first time.
        !
        work_array = array

        n = 1
        do i = 2,size(array)
            if ( all( work_array(i) /= work_array(1:n) ) ) then
               n = n + 1
               work_array(n) = work_array(i)
            endif
        enddo

        unique_elements = work_array(1:n)
    endif
end subroutine unique
end module unique_elements

program test_unique
    use unique_elements
    implicit none

    character(len = 2) :: array(8) = [ 'AA',  'BB', 'CB', 'AA',  'BB', 'CB', 'DD', 'AA' ]
    character(len = len(array)), allocatable :: unique_values(:)

    call unique( array, unique_values )

    write(*,'(10a3)') unique_values
end program test_unique
