module aoc2023__day04
   use, intrinsic :: iso_fortran_env, only: iostat_end
   implicit none
   private

   public :: day04

contains

   !> Solve the problem
   subroutine day04(input, part)
      !> THe unit to read input from
      integer, intent(in) :: input
      !> Which part are we solving?
      integer, intent(in) :: part

      !> Buffer for reading a line of input
      character(len=300) :: line
      !> Stat of io calls
      integer :: stat
      !> Length of the input
      integer :: length

      !> Winning numbers
      integer, allocatable, dimension(:) :: winning
      !> Multiplier for each card
      integer, allocatable, dimension(:) :: mult
      !> Number of winning and playing numbers
      integer :: num_win, num_play
      !> Number of wins in a card
      integer :: wins

      !> Score so far
      integer :: card_score, tot_score

      !> Temporary and working integer storage
      integer :: tmp, tmp2, working, colon, bar
      !> Loop counters
      integer :: i
      !> Temporary string
      character(len=10) :: substring

      tot_score = 0

      read(input, '(a)', iostat=stat) line
      length = len_trim(line)
      colon = index(line, ":")
      bar = index(line, "|")
      num_win = (bar - 2 - colon) / 3
      num_play = (length - bar) / 3

      allocate(winning(num_win))
      if (part == 2) then
         allocate(mult(num_win + 1))
         mult = 1
      end if

      do while (stat == 0)
         tmp = colon + 1
         do i=1,num_win
            substring = adjustl(line(tmp:tmp+2))
            read(substring, *) winning(i)
            tmp = tmp + 3
         end do

         card_score = 1
         wins = 0

         tmp = bar + 1
         do i=1,num_play
            substring = adjustl(line(tmp:tmp+2))
            read(substring, *) working
            tmp2 = findloc(winning, working, dim=1)
            if (tmp2 /= 0) then
               winning(tmp2) = -1
               wins = wins + 1
            end if
            tmp = tmp + 3
         end do

         if (wins > 0 .and. part == 1) then
            tot_score = tot_score + 2**(wins - 1)
         elseif (part == 2) then
            tot_score = tot_score + mult(1)
            if (wins > 0) then
               do i=2,wins+1
                  mult(i) = mult(i) + mult(1)
               end do
            end if
            do i=1,num_win
               mult(i) = mult(i+1)
            end do
            mult(num_win + 1) = 1
         end if

         read(input, '(a)', iostat=stat) line
      end do

      if (stat /= iostat_end) print*, "uh oh... file read error"

      print *, "This elf got ", tot_score, " points!"
   end subroutine day04
end module aoc2023__day04
