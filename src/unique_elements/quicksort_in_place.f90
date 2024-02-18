! quicksort_in_place.f90 --
!     Quicksort algorithm with in-place sorting
!
module quicksort_in_place
    implicit none

contains
recursive subroutine quicksort( array )
    integer, intent(inout) :: array(:)

    integer                :: i, pivot, tmp

    pivot = 1                                                           !    104
    do i = 2,size(array)
        if ( array(i) <= array(pivot) ) then                            !    841
            !
            ! Pivot and current element adjacent?
            ! Then exchange these two. Else we
            ! need to shuffle three elements, as
            ! element pivot+1 is definitely larger than
            ! element pivot
            !
            if ( pivot == i - 1 ) then                                  !    331
                tmp            = array(i)                               !    157
                array(i)       = array(pivot)
                array(pivot)   = tmp
            else
                tmp            = array(i)                               !    174
                array(i)       = array(pivot+1)
                array(pivot+1) = array(pivot)
                array(pivot)   = tmp
            endif
            pivot = pivot + 1                                           !    331
        endif
    enddo

    if ( size(array) > 2 ) then                                         !    104
        call quicksort( array(1:pivot-1) )                              !     51
        call quicksort( array(pivot+1:)  )
    endif
end subroutine quicksort
end module quicksort_in_place

! test program --
!     Simple test for the quicksort implementation
!
program test_quicksort
    use quicksort_in_place
    implicit none

    integer, allocatable :: array(:)
    real                 :: r(100)

    array = [7, 6, 1, 10, 4, 5, 3]

    call quicksort( array )                                             !      1

    write( *, '(10i4)' ) array

    !
    ! A bigger example
    !
    call random_number( r )

    array = int(100 * r )

    call quicksort( array )

    write( *, '(10i4)' ) array

    if ( any(r) > 2.0 ) then
        write(*,*) 'Odd values in r!'                                   !      0
    endif
    if ( any(r) > 2.0 ) &
        write(*,*) 'Odd values in r!'                                   !      0

end program test_quicksort
