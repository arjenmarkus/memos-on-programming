! recursive_unlimited.f90 --
!     Determine the sums of squares that equal a given number
!     This time: no limit on the number of squares
!
!     If N=1000 and you set maxvalue to 2, the program fails.
!     There is no error message, but it seems likely that the
!     stack is overflowing.
!
program recursive_unlimited
    implicit none

    character(len=20) :: string
    integer           :: number, maxvalue, count
    integer           :: values(0)

    if ( command_argument_count() < 1 ) then
        write( *, * ) 'What is the number?'
        read( *, * )  number
    else
        call get_command_argument( 1, string )
        read( string, * ) number
    endif

    write( *, * ) 'Number to be written as a sum of squares:', number

    maxvalue = isqrt(number)  ! Make sure we have the largest possible integer - rounding errors?

    count = 0

    maxvalue = 2 ! Try to get to the end - for N=1000 we already have an enormous number of possibilities

    call determine_solutions( number, maxvalue, count, values )

    write(*,*) 'Total number of solutions: ', count
contains

recursive subroutine determine_solutions( number, maxvalue, count, values )
    integer, intent(in)    :: number
    integer, intent(in)    :: maxvalue
    integer, intent(inout) :: count
    integer, intent(in)    :: values(:)

    integer                :: i
    integer                :: maxnext

    do i = maxvalue,1,-1

        if ( number == i**2 ) then
            count = count + 1
            write(*,*) count, ':', [values, i]

        elseif ( number > i**2 ) then
            maxnext = isqrt(i**2)
            call determine_solutions( number-i**2, maxnext, count, [values, i] )
        endif
    enddo
end subroutine determine_solutions

integer function isqrt( n )
    integer, intent(in) :: n

    isqrt = int( sqrt(real(n)) )

    !
    ! Take care of possible rounding errors (if n is very large)
    ! Hm, not quite there yet?
    !
    if ( (isqrt+1)**2 == n ) then
        isqrt = isqrt + 1
    endif
end function isqrt

end program recursive_unlimited
