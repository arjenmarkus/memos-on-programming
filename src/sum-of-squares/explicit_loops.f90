! explicit_loops.f90 --
!     Determine the sums of atmost four squares that equal a given number
!
program explicit_loops
    implicit none

    character(len=20) :: string
    integer           :: number, maxi, maxj, maxk, maxl, count
    integer           :: i, j, k, l

    if ( command_argument_count() < 1 ) then
        write( *, * ) 'What is the number?'
        read( *, * )  number
    else
        call get_command_argument( 1, string )
        read( string, * ) number
    endif

    write( *, * ) 'Number to be written as a sum of squares:', number

    maxl = isqrt(number)  ! Make sure we have the largest possible integer - rounding errors?

    count = 0
    do l = maxl,1,-1

        if ( l**2 == number ) then
            count = count + 1
            write(*,*) count, ':', l
            cycle
        endif

        maxk = isqrt(l**2)
        do k = maxk,1,-1

            if ( l**2 + k**2 == number ) then
                count = count + 1
                write(*,*) count, ':', l, k
                cycle
            endif

            maxj = isqrt(k**2)
            do j = maxj,1,-1

                if ( l**2 + k**2 + j**2 == number ) then
                    count = count + 1
                    write(*,*) count, ':', l, k, j
                    cycle
                endif

                maxi = isqrt(j**2)
                do i = maxi,1,-1

                    if ( l**2 + k**2 + j**2 + i**2 == number ) then
                        count = count + 1
                        write(*,*) count, ':', l, k, j, i
                    endif
                enddo
            enddo
        enddo
    enddo

    write(*,*) 'Total number of solutions: ', count
contains

integer function isqrt( n )
    integer, intent(in) :: n

    isqrt = int( sqrt(real(n)) )

    !
    ! Take care of possible rounding errors (if n is very large)
    !
    if ( (isqrt+1)**2 == n ) then
        isqrt = isqrt + 1
    endif
end function isqrt

end program explicit_loops
