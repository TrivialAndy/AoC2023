module aoc2023__day10
   implicit none
   private

   public :: day10

contains

   !> Run the days solution for part <part> on the dataset numbered <dataset>
   !> Prints the correct answer to the terminal
   subroutine day10(part, dataset)
      integer, intent(in) :: part, dataset
   end subroutine day10

   !> Parse the input for this problem
   subroutine parse(dataset)
      integer, intent(in) :: dataset
   end subroutine parse

   !> Solve part 1
   subroutine part1(input)
      character(len=50), dimension(:), intent(in) :: input

   end subroutine part1
end module aoc2023__day10
