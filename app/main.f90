program main
   use, intrinsic :: iso_fortran_env, only: real64
   use aoc2023__day01
   use aoc2023__day02
   use aoc2023__day03
   use aoc2023__day04
   use aoc2023__day05
   use aoc2023__day06
   use aoc2023__day07
   use aoc2023__day08
   use aoc2023__day09
   use aoc2023__day10
   use aoc2023__day11
   use aoc2023__day12
   use aoc2023__day13
   use aoc2023__day14
   use aoc2023__day15
   use aoc2023__day16
   use aoc2023__day17
   use aoc2023__day18
   use aoc2023__day19
   use aoc2023__day20
   use aoc2023__day21
   use aoc2023__day22
   use aoc2023__day23
   use aoc2023__day24
   use aoc2023__day25
   implicit none

   !> Number of cli arguments (expecting 3)
   integer :: num_args
   !> Temporary storage for cli args
   character(len=20), dimension(:), allocatable :: args
   !> The defining values for the problem to solve
   integer :: day, part, dataset
   ! Get input (day, part, dataset)

   !> Time markers for start and end
   real(kind=real64) :: start, finish

   !> Loop counter
   integer :: i

   num_args = command_argument_count()
   if (num_args < 3) then
      print*, "Not enough args! What day, part, and dataset should we solve?"
      stop
   end if
   allocate(args(num_args))
   do i=1,3
      call get_command_argument(i, args(i))
   end do

   read(args(1), *) day
   read(args(2), *) part
   read(args(3), *) dataset

   if (part < 1 .or. part > 2) then
      print*, "Hold up, AoC days only have 2 parts!"
      stop
   end if

   call cpu_time(start)
   select case(day)
    case (1)
      call day01(part, dataset)
    case (2)
      call day02(part, dataset)
    case (3)
      call day03(part, dataset)
    case (4)
      call day04(part, dataset)
    case (5)
      call day05(part, dataset)
    case (6)
      call day06(part, dataset)
    case (7)
      call day07(part, dataset)
    case (8)
      call day08(part, dataset)
    case (9)
      call day09(part, dataset)
    case (10)
      call day10(part, dataset)
    case (11)
      call day11(part, dataset)
    case (12)
      call day12(part, dataset)
    case (13)
      call day13(part, dataset)
    case (14)
      call day14(part, dataset)
    case (15)
      call day15(part, dataset)
    case (16)
      call day16(part, dataset)
    case (17)
      call day17(part, dataset)
    case (18)
      call day18(part, dataset)
    case (19)
      call day19(part, dataset)
    case (20)
      call day20(part, dataset)
    case (21)
      call day21(part, dataset)
    case (22)
      call day22(part, dataset)
    case (23)
      call day23(part, dataset)
    case (24)
      call day24(part, dataset)
    case (25)
      call day25(part, dataset)
    case default
      print *, "Day not in advent! Are you solving problems from another date?!"
   end select
   call cpu_time(finish)

   print '(a, f0.4, a)', "Solved in ", finish - start, " seconds"

end program main
