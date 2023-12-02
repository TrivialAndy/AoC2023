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

      call solve(fileunit, part)
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
      integer :: first, last

      !> Loop counter
      integer :: i

      print *, "Calibrating..."
      total = 0
      stat = 0
      do
         read(input, *, iostat = stat) line
         if (stat /= 0) then
            if (stat == iostat_end) exit
            print *, "Read error!"
            stop
         end if
         first = -1
         last = -1
         length = len_trim(line)

         do i=1,length
            read(line(i:i), '(i1)', iostat=stat) first
            if (part == 2 .and. first < 0) then
               if (length - i + 1 >= 5) call five_digit_number(line(i:i+4), first)
               if (length - i + 1 >= 4) call four_digit_number(line(i:i+3), first)
               if (length - i + 1 >= 3) call three_digit_number(line(i:i+2), first)
            end if
            if (first >= 0) exit
         end do

         do i=length,1,-1
            read(line(i:i), '(i1)', iostat=stat) last
            if (part == 2 .and. last < 0) then
               if (i >= 5) call five_digit_number(line(i-4:i), last)
               if (i >= 4) call four_digit_number(line(i-3:i), last)
               if (i >= 3) call three_digit_number(line(i-2:i), last)
            end if
            if (last >= 0) exit
         end do

         total = total + 10*first + last

      end do
      print *, "Calibration value is: ", total
   end subroutine solve

   !> Parse a 5 digit number
   subroutine five_digit_number(line, output)
      !> The line to check
      character(len=5), intent(in) :: line
      !> The output to set
      integer, intent(inout) :: output
      select case (line)
       case ("three")
         output = 3
       case ("seven")
         output = 7
       case ("eight")
         output = 8
      end select
   end subroutine five_digit_number

   !> Parse a 4 digit number
   subroutine four_digit_number(line, output)
      !> The line to check
      character(len=4), intent(in) :: line
      !> The output to set
      integer, intent(inout) :: output
      select case (line)
       case ("zero")
         output = 0
       case("four")
         output = 4
       case("five")
         output = 5
       case("nine")
         output = 9
      end select
   end subroutine four_digit_number

   !> Parse a 3 digit number
   subroutine three_digit_number(line, output)
      !> The line to check
      character(len=3), intent(in) :: line
      !> The output to set
      integer, intent(inout) :: output
      select case (line)
       case ("one")
         output = 1
       case ("two")
         output = 2
       case ("six")
         output = 6
      end select
   end subroutine three_digit_number



end module aoc2023__day01
