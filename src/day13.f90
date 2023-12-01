module aoc2023__day13
   implicit none
   private

   public :: day13

contains

   !> Run the days solution for part <part> on the dataset numbered <dataset>
   !> Prints the correct answer to the terminal
   subroutine day13(part, dataset)
      integer, intent(in) :: part, dataset
   end subroutine day13

   !> Parse the input for this problem
   subroutine parse(dataset)
      integer, intent(in) :: dataset
   end subroutine parse

   !> Solve part 1
   subroutine part1(input)
      character(len=50), dimension(:), intent(in) :: input

   end subroutine part1
end module aoc2023__day13
