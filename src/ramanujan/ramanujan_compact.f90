! ramanujan_compact.f90 --
!     Use an implementation to determine the first few Ramanujan numbers
!     that does not rely on a large array
!
!     Note: this does not determine all Ramanujan numbers up to 2 million.
!     Only the ones for which the cubes do not exceed 100.
!
program ramanujan_compact
    implicit none

    integer, parameter   :: max_ij  = 100
    integer, allocatable :: saved_sums(:)
    integer, allocatable :: sums(:)
    integer              :: i, j, value

    !
    ! Determine the sums of two cubes
    !
    sums = [((i**3+j**3, i = 0,j), j = 0,max_ij)]
    allocate( saved_sums(0) )

    do while ( size(sums) > 0 )
        value = sums(1)

        if ( count( sums == value ) > 1 ) then
            saved_sums = [saved_sums, value]
        endif

        sums = pack( sums, sums /= value )
    enddo

    !
    ! Write out the numbers for which there are at least two
    ! combinations i,j that give these numbers
    !
    write(*,*) saved_sums
end program ramanujan_compact
