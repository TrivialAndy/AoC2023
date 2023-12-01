module aoc2023__day09
   implicit none
   private

   public :: day09

contains

   !> Run the days solution for part <part> on the dataset numbered <dataset>
   !> Prints the correct answer to the terminal
   subroutine day09(part, dataset)
      integer, intent(in) :: part, dataset
   end subroutine day09

   !> Parse the input for this problem
   subroutine parse(dataset)
      integer, intent(in) :: dataset
   end subroutine parse

   !> Solve part 1
   subroutine part1(input)
      character(len=50), dimension(:), intent(in) :: input

   end subroutine part1
end module aoc2023__day09
