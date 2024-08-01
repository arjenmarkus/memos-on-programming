! iterative_unlimited.f90 --
!     An iterative solution to the problem of determining how many sums of squares add up to a given number.
!
!     Determine the sums of squares that equal a given number
!     This time: no limit on the number of squares
!
!     If N=1000 and you set maxvalue to 2, the program fails.
!     There is no error message, but it seems likely that the
!     stack is overflowing.
!
program iterative_unlimited
    implicit none

    character(len=20)    :: string
    integer              :: idx, number, maxvalue, count, residue
    integer, allocatable :: values(:)

    integer :: loop

    if ( command_argument_count() < 1 ) then
        write( *, * ) 'What is the number?'
        read( *, * )  number
    else
        call get_command_argument( 1, string )
        read( string, * ) number
    endif

    write( *, * ) 'Number to be written as a sum of squares:', number

    allocate( values(number) ) ! Provide the storage
    maxvalue = isqrt(number)   ! Make sure we have the largest possible integer - rounding errors?

    count = 0
    idx   = 1

    values    = 0
    values(1) = maxvalue

    do
        residue  = number - sum(values(1:idx)**2)

        if  ( residue > 0 ) then
            maxvalue = min( isqrt(residue), values(idx) )


            if ( maxvalue > 0 ) then
                idx = idx + 1
                values(idx) = maxvalue
            endif

        elseif ( residue == 0 ) then
            count = count + 1
            write(*,*) count, ':', values(1:idx)

            do while ( values(idx) > 0 )
                values(idx) = values(idx) - 1

                if ( values(idx) <= 0 ) then
                    idx = idx - 1
                else
                    exit
                endif
            enddo

        else
            if ( values(idx) > 1 ) then
                values(idx) = values(idx) - 1
            else
                idx = idx - 1
            endif
        endif

        if ( idx <= 0 ) then
            exit
        endif
    enddo

    write(*,*) 'Total number of solutions: ', count
contains

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

end program iterative_unlimited
