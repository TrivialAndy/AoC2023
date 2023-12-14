module aoc2023__day14
   use, intrinsic :: iso_fortran_env, only: iostat_end, output_unit
   use dynamic_array, only: grow, shrink
   implicit none
   private

   public :: day14

contains

   !> Solve the problem
   subroutine day14(input, part)
      !> The unit to read input from
      integer, intent(in) :: input
      !> Which part are we solving?
      integer, intent(in) :: part

      !> Line buffer
      character(len=300) :: line
      !> Width and height
      integer :: width, height
      !> Grid of square rock locations
      logical, allocatable, dimension(:,:) :: grid
      !> Boulder locations
      logical, allocatable, dimension(:,:) :: boulders
      !> A history of boulder positions
      logical, allocatable, dimension(:,:,:) :: hist, hist2

      !> The number of boulders
      integer :: count

      !> The total load on the beams
      integer :: load

      !> Status of io calls
      integer :: stat
      !> Loop counters
      integer :: i, j

      read(input, '(a)', iostat=stat) line
      width = len_trim(line) + 2
      height = 2
      count = 0
      allocate(grid(8,width), boulders(8,width))

      do while (stat /= iostat_end)
         height = height + 1
         if (size(grid, 1) < height) then
            call grow(boulders, 1)
            call grow(grid, 1)
         end if

         do i=1,width-2
            select case (line(i:i))
             case("O")
               boulders(height-1, i+1) = .true.
               grid(height-1, i+1) = .false.
               count = count + 1
             case("#")
               boulders(height-1, i+1) = .false.
               grid(height-1, i+1) = .true.
             case(".")
               boulders(height-1, i+1) = .false.
               grid(height-1, i+1) = .false.
            end select
         end do
         read(input, '(a)', iostat=stat) line
      end do

      grid(1, :) = .true.
      grid(height, :) = .true.
      grid(:, 1) = .true.
      grid(:, width) = .true.

      call shrink(boulders, 1, height)
      call shrink(grid, 1, height)

      ! call print_grid(boulders, grid)
      if (part == 1) then
         call roll(boulders, grid, [-1, 0])
      else
         allocate(hist(50,size(boulders, 1),size(boulders, 2)))
         hist(1,:,:) = boulders
         count = 0
         do i=1,1000000000
            call roll(boulders, grid, [-1, 0])
            call roll(boulders, grid, [ 0,-1])
            call roll(boulders, grid, [ 1, 0])
            call roll(boulders, grid, [ 0, 1])

            do j=1,size(hist,1)
               if (all(hist(modulo(i+j-1,size(hist,1))+1,:,:) .eqv. boulders)) then
                  count = modulo(-j,size(hist,1)) + 1
                  exit
               end if
            end do
            
            if (count > 0) exit

            hist(modulo(i,size(hist,1))+1,:,:) = boulders
         end do

         ! Loop is in hist from modulo(i-count,20)+1 to modulo(i-1,20)+1
         allocate(hist2(count,size(boulders,1),size(boulders,2)))
         do j=1,count
            hist2(j,:,:) = hist(modulo(i-count+j-1,size(hist,1))+1,:,:)
            ! call print_grid(hist2(j,:,:), grid)
            ! print*, get_load(hist2(j,:,:), [-1, 0])
            ! print*, ""
         end do

         j = modulo(1000000000-i, count)+1
         ! print*, j
         boulders = hist2(j,:,:)
      end if
      
      load = get_load(boulders, [-1, 0])
      print '(a, i0, a)', "Hopefully the beams can hold ", load, " tonnes!"
      ! call print_grid(boulders, grid)

   end subroutine day14

   !> Move the boulders to the correct place after rolling
   subroutine roll(boulders, grid, direction)
      !> The grid of boulders to adjust
      logical, dimension(:,:), intent(inout) :: boulders
      !> The grid of immovable rocks
      logical, dimension(:,:), intent(in) :: grid
      !> The direction to roll
      integer, dimension(2), intent(in) :: direction

      !> Loop counters
      integer :: i, new_i, j, new_j, k

      do i=2,size(boulders, 1)-1
         new_i = merge(size(boulders, 1) - i + 1, i, direction(1) > 0)
         do j=2,size(boulders,2)-1
            new_j = merge(size(boulders, 2) - j + 1, j, direction(2) > 0)
            if (.not. boulders(new_i, new_j)) cycle
            k = 1
            do while (.not. (boulders(new_i+k*direction(1),new_j+k*direction(2)) &
            & .or. grid(new_i+k*direction(1),new_j+k*direction(2))))
               k = k + 1
            end do
            k = k - 1
            boulders(new_i, new_j) = .false.
            boulders(new_i+k*direction(1),new_j+k*direction(2)) = .true.
         end do
      end do
   end subroutine

   !> Calculate the load on the beams in the given direction
   function get_load(boulders, direction) result(load)
      !> The grid of boulders
      logical, dimension(:,:), intent(in) :: boulders
      !> The direction of the beam to check
      integer, dimension(2), intent(in) :: direction

      !> The total load on the beam
      integer :: load

      !> Loop counters
      integer :: i, new_i, j, new_j
      load = 0
      do i=2,size(boulders, 1)-1
         new_i = merge(size(boulders, 1) - i + 1, i, direction(1) < 0)
         do j=2,size(boulders, 2)-1
            new_j = merge(size(boulders, 2) - j + 1, j, direction(2) < 0)
            if (boulders(i, j)) then
               load = load + abs(direction(1))*(new_i-1) + abs(direction(2))*(new_j-1)
            end if
         end do
      end do
   end function get_load

   !> Print the grid to the terminal
   subroutine print_grid(boulders, grid)
      !> The grid of boulders
      logical, dimension(:,:), intent(in) :: boulders
      !> The grid of immovable rocks
      logical, dimension(:,:), intent(in) :: grid

      !> Loop counters
      integer :: i, j

      do i=2,size(boulders,1)-1
         do j=2,size(boulders,2)-1
            if (boulders(i, j)) then
               write(output_unit, '(a)', advance='no') "O"
            elseif(grid(i,j)) then
               write(output_unit, '(a)', advance='no') "#"
            else
               write(output_unit, '(a)', advance='no') "."
            end if
         end do
         write(output_unit, '(a)') ""
      end do
   end subroutine print_grid
end module aoc2023__day14
