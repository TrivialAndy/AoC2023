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

   !> Unit for the input dataset
   integer :: fileunit
   !> The filename for the inpput dataset
   character(len=50) :: filename

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


   write (filename, '(a,i2.2,a,i0,a)') "data/day", day, "_", dataset, ".txt"
   print '(a, i0, a, i0, a, a)', "Solving problem ", day, ".", part, " for input: ", filename
   open(newunit=fileunit, file=filename)

   call cpu_time(start)
   select case(day)
    case (1)
      call day01(fileunit, part)
    case (2)
      call day02(fileunit, part)
    case (3)
      call day03(fileunit, part)
    case (4)
      call day04(fileunit, part)
    case (5)
      call day05(fileunit, part)
    case (6)
      call day06(fileunit, part)
    case (7)
      call day07(fileunit, part)
    case (8)
      call day08(fileunit, part)
    case (9)
      call day09(fileunit, part)
    case (10)
      call day10(fileunit, part)
    case (11)
      call day11(fileunit, part)
    case (12)
      call day12(fileunit, part)
    case (13)
      call day13(fileunit, part)
    case (14)
      call day14(fileunit, part)
    case (15)
      call day15(fileunit, part)
    case (16)
      call day16(fileunit, part)
    case (17)
      call day17(fileunit, part)
    case (18)
      call day18(fileunit, part)
    case (19)
      call day19(fileunit, part)
    case (20)
      call day20(fileunit, part)
    case (21)
      call day21(fileunit, part)
    case (22)
      call day22(fileunit, part)
    case (23)
      call day23(fileunit, part)
    case (24)
      call day24(fileunit, part)
    case (25)
      call day25(fileunit, part)
    case default
      print *, "Day not in advent! Are you solving problems from another date?!"
   end select
   call cpu_time(finish)

   close(fileunit)

   print '(a, f0.4, a)', "Solved in ", finish - start, " seconds"

end program main
