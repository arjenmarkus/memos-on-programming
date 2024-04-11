! ramanujan_vector.f90 --
!     Implementation to determine the first few Ramanujan numbers via array operations
!
!     Note: this does not determine all Ramanujan numbers up to 2 million.
!     Only the ones for which the cubes do not exceed 100.
!
program ramanujan_vector
    implicit none

    integer, parameter   :: max_ij  = 100
    integer, parameter   :: max_sum = 2 * max_ij ** 3
    integer              :: cnt(1:max_sum)
    integer, allocatable :: sums(:)
    integer              :: i, j, k

    !
    ! Determine the sums of two cubes
    !
    sums = [((i**3+j**3, i = 1,j), j = 1,max_ij)]
    cnt  = 0
    cnt(sums) = cnt(sums) + 1

    !
    ! Write out the numbers for which there are at least two
    ! combinations i,j that give these numbers
    !
!!    do k = 1,max_sum
!!        if ( cnt(k) > 1 ) then
!!            write(*,*) k
!!        endif
!!    enddo

    write(*,*) pack( [(k, k = 1,max_sum)], cnt > 1)
end program ramanujan_vector
