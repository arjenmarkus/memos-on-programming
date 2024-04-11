! ramanujan_single.f90 --
!     A silly implementation using a single statement
!     that does not rely on a large array
!
!     Note: this does not determine all Ramanujan numbers up to 2 million.
!     Only the ones for which the cubes do not exceed 100.
!
program ramanujan_single
    implicit none

    integer, parameter   :: max_ij  = 13
!   integer, parameter   :: max_ij  = 100
    integer, parameter   :: max_sum = 2 * max_ij ** 3
    integer              :: i, j, k

    integer, allocatable :: dummy(:)

    !
    ! Determine the sums of two cubes and print the ones
    ! that appear multiple times
    !
    write(*,*) pack( [ (k, k = 1,max_sum)], &
                     [ ( count( [((i**3+j**3, i = 1,j), j = 1,max_ij)] == k ) > 1, k = 1,max_sum )] )

    !
    ! The code below is much faster:
    !
    ! dummy = [((i**3+j**3, i = 1,j), j = 1,max_ij)]
    ! write(*,*) pack( [ (k, k = 1,max_sum)], &
    !                  [ ( count( dummy == k ) > 1, k = 1,max_sum )] )

end program ramanujan_single
