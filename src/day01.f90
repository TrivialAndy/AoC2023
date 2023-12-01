module aoc2023__day01
   use, intrinsic :: iso_fortran_env, only: iostat_end
   implicit none
   private

   public :: day01

contains

   !> Run the days solution for part <part> on the dataset numbered <dataset>
   !> Prints the correct answer to the terminal
   subroutine day01(part, dataset)
      !> The problem to solve
      integer, intent(in) :: part, dataset
      !> Unit for the input dataset
      integer :: fileunit
      !> The filename for the inpput dataset
      character(len=50) :: filename

      write (filename, '(a,i0,a)') "data/day01_", dataset, ".txt"
      open(newunit=fileunit, file=filename)

      if (part < 3 .and. part > 0) then
         call solve(fileunit, part)
      else
         print*, "Hold up, we're out of parts'!"
      end if
   end subroutine day01

   !> Solve
   subroutine solve(input, part)
      !> Input open file unit
      integer, intent(in) :: input
      !> Which part are we solving
      integer, intent(in) :: part

      !> buffer for holding a line from the input
      character(len=100) :: line
      !> Length of the line
      integer :: length
      !> Status for a read
      integer :: stat

      !> Running total for the calibration values
      integer :: total

      !> First and last integer values encountered
      integer :: first, last, last_idx

      !> Loop counter
      integer :: i

      print *, "Calibrating..."
      total = 0
      stat = 0
      do
         read(input, *, iostat = stat) line
         select case(stat)
          case (0)
            ! Continue execution
          case (iostat_end)
            exit
          case default
            print *, "Read error!"
            stop
         end select

         first = -1
         last = -1
         last_idx = 1
         length = len_trim(line)
         do i=1,length
            if (first < 0) then
               read(line(i:i), '(i1)', iostat=stat) first
               if (part == 2 .and. stat == 0) call first_digit(line(1:i-1), first)
            end if
            read(line(i:i), '(i1)', iostat=stat) last
            if (part == 2 .and. stat == 0) last_idx = i
         end do
         if (part == 2) call last_digit(line(last_idx+1:length), last)
         total = total + 10*first + last
      end do
      print *, "Calibration value is: ", total
   end subroutine solve

   !> Find the first digit written as text in a string
   subroutine first_digit(line, output)
      !> The line to scan for text
      character(len=*), intent(in) :: line
      !> The value to return if no string is found
      integer, intent(inout) :: output
      !> The first digit found
      integer :: tmp

      !> Length of line
      integer :: length

      !> Loop counter
      integer :: i

      length = len_trim(line)
      tmp = -1

      do i=1,(length-2)
         if (length - i + 1 >= 5) call five_digit_number(line(i:i+4), tmp)
         if (length - i + 1 >= 4) call four_digit_number(line(i:i+3), tmp)
         if (length - i + 1 >= 3) call three_digit_number(line(i:i+2), tmp)
         if (tmp >= 0) then
            output = tmp
            exit
         end if
      end do
   end subroutine first_digit
   
   !> Find the last digit written as text in a string
   subroutine last_digit(line, output)
      !> The line to scan for text
      character(len=*), intent(in) :: line
      !> The value to return if no string is found
      integer, intent(inout) :: output
      !> The first digit found
      integer :: tmp

      !> Length of line
      integer :: length

      !> Loop counter
      integer :: i

      length = len_trim(line)
      tmp = -1

      do i=length,3,-1
         if (i >= 5) call five_digit_number(line(i-4:i), tmp)
         if (i >= 4) call four_digit_number(line(i-3:i), tmp)
         if (i >= 3) call three_digit_number(line(i-2:i), tmp)
         if (tmp >= 0) then
            output = tmp
            exit
         end if
      end do
   end subroutine last_digit

   !> Parse a 5 digit number
   subroutine five_digit_number(line, output)
      !> The line to check
      character(len=5), intent(in) :: line
      !> The output to set
      integer, intent(inout) :: output
      if (line == "three") output = 3
      if (line == "seven") output = 7
      if (line == "eight") output = 8
   end subroutine five_digit_number

   !> Parse a 4 digit number
   subroutine four_digit_number(line, output)
      !> The line to check
      character(len=4), intent(in) :: line
      !> The output to set
      integer, intent(inout) :: output
      if (line == "zero") output = 0
      if (line == "four") output = 4
      if (line == "five") output = 5
      if (line == "nine") output = 9
   end subroutine four_digit_number

   !> Parse a 3 digit number
   subroutine three_digit_number(line, output)
      !> The line to check
      character(len=3), intent(in) :: line
      !> The output to set
      integer, intent(inout) :: output
      if (line == "one") output = 1
      if (line == "two") output = 2
      if (line == "six") output = 6
   end subroutine three_digit_number



end module aoc2023__day01
