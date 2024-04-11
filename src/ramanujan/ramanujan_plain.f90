! ramanujan_plain.f90 --
!     Straightforward implementation to determine the first few Ramanujan numbers
!
!     Note: this does not determine all Ramanujan numbers up to 2 million.
!     Only the ones for which the cubes do not exceed 100.
!
program ramanujan_plain
    implicit none

    integer, parameter :: max_ij  = 100
    integer, parameter :: max_sum = 2 * max_ij ** 3
    integer            :: cnt(max_sum)
    integer            :: i, j, k

    !
    ! Determine the sums of two cubes
    !
    cnt = 0

    do j = 1,max_ij
        do i = 1,j
            k = i ** 3 + j ** 3

            cnt(k) = cnt(k) + 1
        enddo
    enddo

    !
    ! Write out the numbers for which there are at least two
    ! combinations i,j that give these numbers
    !
    do k = 1,max_sum
        if ( cnt(k) > 1 ) then
            write(*,*) k
        endif
    enddo
end program ramanujan_plain
