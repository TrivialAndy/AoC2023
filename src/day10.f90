module aoc2023__day10
   use, intrinsic :: iso_fortran_env, only: iostat_end
   use :: dynamic_array, only: grow, shrink
   implicit none
   private

   public :: day10

contains

   !> Solve the problem
   subroutine day10(input, part)
      !> The unit to read input from
      integer, intent(in) :: input
      !> Which part are we solving?
      integer, intent(in) :: part

      !> Line buffer
      character(len=300) :: line
      !> Status of io calls
      integer :: stat

      !> The grid of characters, converted into ints:
      !> "."=-1, "S"=0, "|"=1, "-"=2, "F"=3, "7"=4, "J"=5, "L"=6
      integer, allocatable, dimension(:,:) :: grid
      !> Grid for marking whether points are in, out, or on the loop
      integer, allocatable, dimension(:,:) :: inout
      !> Position of left and right moving pointers (and the previous ones)
      integer, dimension(2) :: left, left_prev, right, right_prev
      !> The next step to take
      integer, dimension(2) :: delta
      !> The number of steps taken
      integer :: steps

      !> Extent of the grid
      integer, dimension(2) :: length

      !> Loop counters
      integer :: i, j

      read(input, '(a)', iostat=stat) line
      length(1) = len_trim(line)+2
      length(2) = 2
      allocate(grid(length(1), 8))
      do while(stat /= iostat_end)
         length(2) = length(2) + 1
         if (length(2) > size(grid, dim=2)) call grow(grid, 2)
         do i=2,length(1)-1
            grid(i, length(2)-1) = char2int(line(i-1:i-1))
            if (grid(i,length(2)-1) == 0) then
               left = [i, length(2)-1]
               right = [i, length(2)-1]
            end if
         end do
         read(input, '(a)', iostat=stat) line
      end do

      grid(1, :) = -1
      grid(length(1), :) = -1
      grid(:, 1) = -1
      grid(:, length(2)) = -1

      call shrink(grid, 2, length(2))

      ! do i=1,length(2)
      !    print '(*(a))', int2char(grid(:,i))
      ! end do

      left_prev = left
      right_prev = right
      if (any(grid(left(1), left(2)-1) == [1,3,4])) then
         delta = [0, -1]
      elseif (any(grid(left(1), left(2)+1) == [1,5,6])) then
         delta = [0, 1]
      elseif (any(grid(left(1)-1,left(2)) == [2,3,6])) then
         delta = [-1, 0]
      end if
      left = left + delta
      if (any(grid(right(1)+1,right(2)) == [2,4,5])) then
         delta = [1, 0]
      elseif (any(grid(right(1)-1,right(2)) == [2,3,6])) then
         delta = [-1, 0]
      elseif (any(grid(right(1), right(2)+1) == [1,5,6])) then
         delta = [0, 1]
      end if
      right = right + delta
      steps = 1

      if (part == 2) then
         allocate(inout(length(1), length(2)))
         inout = 0
         inout(1,:) = -1
         inout(length(1), :) = -1
         inout(:, 1) = -1
         inout(:, length(2)) = -1
      end if

      do
         if (part == 2) then
            call update_inout(inout, left, left_prev)
            call update_inout(inout, right_prev, right)
         end if
         if (all(left == right)) exit
         call next_pos(grid, left_prev, left)
         if (all(left == right)) exit
         call next_pos(grid, right_prev, right)
         steps = steps + 1
      end do

      if (part == 2) then
         ! do i=1,length(2)
         !    print '(*(i3))', inout(:,i)
         ! end do

         steps = 0
         stat = 0
         do i=2,length(1)-1
            do j=2,length(2)-1
               if (inout(i, j) == 1) cycle
               if (inout(i-1,j) == -1 .or. inout(i+1,j) == -1 &
               & .or. inout(i,j-1) == -1 .or. inout(i,j+1) == -1) then
                  if (stat == 0 .and. inout(i, j) == 2) stat = 3
                  if (stat == 0 .and. inout(i, j) == 3) stat = 2
                  inout(i,j) = -1
               elseif (inout(i-1,j) == 2 .or. inout(i+1,j) == 2 &
               & .or. inout(i,j-1) == 2 .or. inout(i,j+1) == 2) then
                  inout(i,j) = 2
               elseif (inout(i-1,j) == 3 .or. inout(i+1,j) == 3 &
               & .or. inout(i,j-1) == 3 .or. inout(i,j+1) == 3) then
                  inout(i,j) = 3
               end if
               if (stat /= 0 .and. inout(i, j) == stat) steps = steps + 1
            end do
         end do
         ! print*, "Inside: ", stat

         ! do i=1,length(2)
         !    print '(*(i3))', inout(:,i)
         ! end do
      end if

      print '(a, i0, a)', "Phew! That took ", steps, " steps..."
   end subroutine day10

   !> Convert the input chars to ints
   elemental function char2int(c) result(i)
      !> The char to convert
      character(len=1), intent(in) :: c
      !> The integer value for the char
      integer :: i
      select case(c)
       case (".")
         i = -1
       case ("S")
         i = 0
       case ("|")
         i = 1
       case ("-")
         i = 2
       case ("F")
         i = 3
       case ("7")
         i = 4
       case ("J")
         i = 5
       case ("L")
         i = 6
      end select
   end function char2int

   !> Convert the ints to input char (for testing)
   elemental function int2char(i) result(c)
      !> The int to convert
      integer, intent(in) :: i
      !> The char for the int
      character(len=1) :: c
      select case(i)
       case (-1)
         c = "."
       case (0)
         c = "S"
       case (1)
         c = "|"
       case (2)
         c = "-"
       case (3)
         c = "F"
       case (4)
         c = "7"
       case (5)
         c = "J"
       case (6)
         c = "L"
      end select

   end function int2char

   !> Advance the pointers to the next step
   subroutine next_pos(grid, prev, cur)
      !> THe grid to check
      integer, dimension(:,:), intent(in) :: grid
      !> The current and previous position
      integer, dimension(2), intent(inout) :: prev, cur

      !> THe step to change by
      integer, dimension(2) :: delta

      select case(grid(cur(1), cur(2)))
       case (1)
         if (prev(2) /= cur(2) - 1) then
            delta = [0, -1]
         else
            delta = [0, 1]
         end if
       case (2)
         if (prev(1) /= cur(1) - 1) then
            delta = [-1, 0]
         else
            delta = [1, 0]
         end if
       case (3)
         if (prev(1) /= cur(1) + 1) then
            delta = [1, 0]
         else
            delta = [0, 1]
         end if
       case (4)
         if (prev(1) /= cur(1) - 1) then
            delta = [-1, 0]
         else
            delta = [0, 1]
         end if
       case (5)
         if (prev(1) /= cur(1) - 1) then
            delta = [-1, 0]
         else
            delta = [0, -1]
         end if
       case (6)
         if (prev(1) /= cur(1) + 1) then
            delta = [1, 0]
         else
            delta = [0, -1]
         end if
      end select
      prev = cur
      cur = cur + delta
   end subroutine next_pos

   !> Update the status of inout for the given values
   !> 1 = on line, 2 = left of line, 3 = right of line
   subroutine update_inout(inout, cur, prev)
      !> The grid to update
      integer, dimension(:,:), intent(inout) :: inout
      !> The current and previous points on the grid
      integer, dimension(2), intent(in) :: cur, prev

      !> The step from prev to cur and the delta for
      integer, dimension(2) :: delta, perp

      inout(prev(1), prev(2)) = 1
      inout(cur(1), cur(2)) = 1
      delta = cur - prev
      perp = [delta(2), -delta(1)]
      delta = prev + perp
      if (inout(delta(1), delta(2)) == 0) then
         inout(delta(1), delta(2)) = 2
      end if
      delta = cur + perp
      if (inout(delta(1), delta(2)) == 0) then
         inout(delta(1), delta(2)) = 2
      end if
      delta = prev - perp
      if (inout(delta(1), delta(2)) == 0) then
         inout(delta(1), delta(2)) = 3
      end if
      delta = cur - perp
      if (inout(delta(1), delta(2)) == 0) then
         inout(delta(1), delta(2)) = 3
      end if

   end subroutine update_inout
end module aoc2023__day10
