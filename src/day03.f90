module aoc2023__day03
   use, intrinsic :: iso_fortran_env, only: iostat_end
   implicit none
   private

   public :: day03

contains

   !> Run the days solution for part <part> on the dataset numbered <dataset>
   !> Prints the correct answer to the terminal
   subroutine day03(part, dataset)
      !> The problem to solve
      integer, intent(in) :: part, dataset
      !> Unit for the input dataset
      integer :: fileunit
      !> The filename for the inpput dataset
      character(len=50) :: filename

      write (filename, '(a,i0,a)') "data/day03_", dataset, ".txt"
      open(newunit=fileunit, file=filename)

      call solve(fileunit, part)
   end subroutine day03

   !> Solve the problem!
   subroutine solve(input, part)
      !> Open unit for reading the input
      integer, intent(in) :: input
      !> Which part are we solving?!
      integer, intent(in) :: part

      !> Buffers for input lines
      character(len=300), dimension(3) :: lines
      !> Status of io calls
      integer :: stat

      !> The next numbers to consider
      integer :: working, working2
      !> The running total
      integer :: total
      !> Did something get changed?
      logical :: updated

      !> Loop counters
      integer :: i, j
      !> Either end of the number in the string
      integer :: start, finish
      !> Length of the input line
      integer :: length

      total = 0

      read (input, '(a)', iostat=stat) lines(2)
      length = len_trim(lines(2))
      lines(1) = repeat(".", length)
      read (input, '(a)', iostat=stat) lines(3)
      do
         i = 1
         do
            if (part == 1 .and. lines(2)(i:i) >= "0" .and. lines(2)(i:i) <= "9") then

               start = i - 1
               finish = start + non_int_index(lines(2)(i:length), .true.)
               if (finish == start) finish = length + 1

               if (contains_symbol(lines(1)(1:length), start, finish) &
               & .or. contains_symbol(lines(2)(1:length), start, finish) &
               & .or. contains_symbol(lines(3)(1:length), start, finish)) then
                  read(lines(2)(start+1:finish-1), *) working
                  total = total + working
               end if

               i = finish

            elseif (part == 2 .and. lines(2)(i:i) == "*") then
               working = -1
               working2 = -1
               do j=1,3
                  call update_working(lines(j), i, working, working2, updated)
                  if (.not. updated) then
                     call update_working(lines(j), i-1, working, working2, updated)
                     call update_working(lines(j), i+1, working, working2, updated)
                  end if
               end do
               if (working > 0 .and. working2 > 0) total = total + working*working2
               i = i + 1
            else
               i = i + 1
            end if
            if (i > length) exit
         end do

         lines(1) = lines(2)
         lines(2) = lines(3)
         if (stat /= 0) exit
         read(input, '(a)', iostat=stat) lines(3)
         if (stat /= 0) lines(3) = repeat(".", length)
      end do

      print *, "The parts add up to ", total

   end subroutine solve

   !> Update the working values for part 2. Set values to -2 if not possible
   subroutine update_working(line, idx, working, working2, updated)
      !> The line to read
      character(len=*), intent(in) :: line
      !> Index to start the search from
      integer, intent(in) :: idx
      !> working values
      integer, intent(inout) :: working, working2
      !> Did anything get changed
      logical, intent(out) :: updated

      !> Length of the string
      integer :: length
      !> temporary value to read into
      integer :: tmp
      !> Indices for the start and finish of the int
      integer:: start, finish

      length = len_trim(line)
      updated = .false.

      if (is_int(line(idx:idx))) then
         updated = .true.
      
         start = non_int_index(line(1:idx-1), .false.) + 1
         finish = idx + non_int_index(line(idx+1:length), .true.) - 1
         if (finish == idx - 1) finish = length

         read(line(start:finish), *) tmp

         if (working == -1) then
            working = tmp
         elseif (working2 == -1) then
            working2 = tmp
         else
            working = -2
            working2 = -2
         end if
      end if
   end subroutine

   !> Check if a symbol is in the string between the given indexes
   function contains_symbol(line, start, finish) result(found)
      !> The buffer to check for symbols
      character(len=*), intent(in) :: line
      !> The bounds of the buffer to check for a symbol
      integer, intent(in) :: start, finish
      !> Working copies of start and finish
      integer :: w_start, w_finish

      !> Whether a symbol has been found
      logical :: found

      !> THe length of the input string
      integer :: length
      !> Loop counter
      integer :: i

      length = len_trim(line)

      found = .false.
      w_start = max(2, start)
      w_finish = min(length, finish)

      do i=w_start,w_finish
         if (line(i:i) /= "." .and. .not. is_int(line(i:i))) then
            found = .true.
            return
         end if
      end do
   end function contains_symbol

   !> Find the index of the next symbol that is not an integer
   function non_int_index(line, forward) result(idx)
      !> The line buffer to check
      character(len=*), intent(in) :: line
      !> The direction of traversal
      logical, intent(in) :: forward

      !> THe index of the next non int symbol
      integer :: idx

      if (forward) then
         do idx=1,len_trim(line)
            if (.not. is_int(line(idx:idx))) exit
         end do
      else
         do idx=len_trim(line),1,-1
            if (.not. is_int(line(idx:idx))) exit
         end do
      end if
   end function non_int_index

   !> Test if a character is an integer
   function is_int(symbol) result(retval)
      !> The symbol to check
      character(len=1), intent(in) :: symbol
      !> The verdict of the test
      logical :: retval

      retval = symbol >= "0" .and. symbol <= "9"
   end function is_int

end module aoc2023__day03
