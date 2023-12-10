module aoc2023__day09
   use, intrinsic :: iso_fortran_env, only: iostat_end
   implicit none
   private

   public :: day09

contains

   !> Solve the problem
   subroutine day09(input, part)
      !> The unit to read input from
      integer, intent(in) :: input
      !> Which part are we solving?
      integer, intent(in) :: part

      !> Line buffer
      character(len=300) :: line

      !> Work array
      integer, allocatable, dimension(:) :: work

      !> Sum of added values
      integer, dimension(2) :: total

      !> Status of io calls
      integer :: stat
      !> Number of elements
      integer :: count

      !> Loop counter
      integer :: i

      total = 0
      read(input, '(a)', iostat=stat) line
      do while (stat /= iostat_end)
         count = 1
         do i=1,len_trim(line)
            if (line(i:i) == " ") count = count + 1
         end do

         allocate(work(count + 2))
         read(line, *) work(2:count+1)

         call find_next(work)
         total(1) = total(1) + work(count + 2)
         total(2) = total(2) + work(1)

         deallocate(work)

         read(input, '(a)', iostat=stat) line
      end do

      print '(a, i0, a)', "We've added ", total(part), " to the pot"
   end subroutine day09

   !> Find the next and previous element in the series
   recursive subroutine find_next(work)
      !> The series to find the next element of
      integer, dimension(:), intent(inout) :: work

      !> Temp array for the differences in the series
      integer, dimension(size(work)-1) :: diffs

      !> Number of elems in diffs
      integer :: count
      !> Loop counter
      integer :: i

      count = size(diffs)
      do i=2,count-1
         diffs(i) = work(i+1)-work(i)
      end do

      if (all(diffs(2:count-1) == 0)) then
         diffs(1) = 0
         diffs(count) = 0
      else
         call find_next(diffs)
      end if

      work(1) = work(2) - diffs(1)
      work(count + 1) = work(count) + diffs(count)

   end subroutine find_next


end module aoc2023__day09
