! second_implementation.f90 --
!     Construct an array consisting of unique elements:
!     iterative, using pack()
!
!     Adapted from @eelis' contribution to AOC 2023
!     -- aoc2023/7/aoc7_part1.f90 at 9edcf5f4387bad10d22f6153d339b0e7355f59af - ettaka/aoc2023 - GitHub
!
!     https://github.com/ettaka/aoc2023/blob/9edcf5f4387bad10d22f6153d339b0e7355f59af/7/aoc7_part1.f90
!
module unique_elements
    implicit none

contains
subroutine unique( array, unique_elements )
    character(len=*), intent(in)           :: array(:)
    character(len=len(array)), allocatable :: unique_elements(:)

    character(len=len(array)), allocatable :: work_array(:)
    integer                                :: i, n

    allocate(unique_elements(0))
    work_array = array(:)

    do while (size(work_array) > 0)
        unique_elements = [unique_elements, work_array(1)]
        work_array = pack(work_array,mask=(work_array(:) /= work_array(1)))
    end do
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
