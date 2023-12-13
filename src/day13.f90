module aoc2023__day13
   use, intrinsic :: iso_fortran_env, only: iostat_end, output_unit
   use dynamic_array, only: grow, shrink
   implicit none
   private

   public :: day13

contains

   !> Solve the problem
   subroutine day13(input, part)
      !> The unit to read input from
      integer, intent(in) :: input
      !> Which part are we solving?
      integer, intent(in) :: part

      !> Line buffer
      character(len=100) :: line
      !> Actual line length
      integer :: length
      !> Number of lines in a grid
      integer :: rows

      !> Grid
      logical, dimension(:,:), allocatable :: grid
      !> Possible reflection points (vertically and horizontally)
      integer, dimension(:), allocatable :: refl

      !> Number of blemishes on the mirror
      integer :: blemishes

      !> Score fore a single grid
      integer :: score
      !> Score for the whole puzzle
      integer :: total

      !> Loop counters
      integer :: i,j,k
      !> Status of io calls
      integer :: stat
      !> msg from failed io calls
      character(len=50) :: msg

      read(input, '(a)', iostat=stat) line

      total = 0
      if (part == 1) then
         blemishes = 0
      else
         blemishes = 1
      end if

      ! Loop over grids
      do while (stat /= iostat_end)

         length = len_trim(line)
         allocate(refl(length - 1))
         allocate(grid(8,length))
         refl = 0
         rows = 0
         score = 0

         ! Read grid and check for vert reflections
         do while (stat /= iostat_end .and. line /= "")
            rows = rows + 1
            if (rows > size(grid, 1)) call grow(grid, 1)
            do i=1,length
               grid(rows, i) = line(i:i) == '#'
               do j=1,i-1,2
                  if (grid(rows, i) .neqv. grid(rows, i-j)) refl(i-(j+1)/2) = refl(i-(j+1)/2) + 1
               end do
            end do
            read(input, '(a)', iostat=stat, iomsg=msg) line
            if (stat /= 0 .and. stat /= iostat_end) then
               print*, msg
               stop
            end if
         end do

         do i=1,size(refl)
            if (refl(i) == blemishes) then
               score = i
               exit
            end if
         end do

         deallocate(refl)

         ! Check for horizontal reflections
         if (score == 0) then
            allocate(refl(rows))
            refl = 0
            do i=1,rows
               do j=1,i-1,2
                  do k=1,length
                     if (grid(i, k) .neqv. grid(i-j, k)) refl(i-(j+1)/2) = refl(i-(j+1)/2) + 1
                  end do
               end do
            end do

            do i=1,size(refl)
               if (refl(i) == blemishes) then
                  score = i*100
                  exit
               end if
            end do

            deallocate(refl)
         end if

         deallocate(grid)

         total = total + score
         if (stat /= iostat_end) read(input, '(a)', iostat=stat) line
      end do

      print '(a, i0)', "All of the reflections add up... to ", total

   end subroutine day13

   !> Print the grid and mark the reflection point
   subroutine print_grid(grid, refl, vertical)
      !> The grid to print
      logical, dimension(:,:), intent(in) :: grid
      !> The reflection
      logical, dimension(:), intent(in) :: refl
      !> Is the reflection vertical (or horizontal)
      logical, intent(in) :: vertical

      !> Index of refl
      integer :: idx
      !> Loop counters
      integer :: i, j

      do i=1,size(refl)
         if (refl(i)) exit
         idx = idx + 1
      end do

      if (vertical) print '(a,a,a)', repeat(" ", idx-1), "><"

      do i=1,size(grid, 1)
         if (.not. vertical) then
            if (idx == size(grid, 1)) then
               write(output_unit, '(a)', advance='NO') 'v'
            elseif (idx + 1 == size(grid, 1)) then
               write(output_unit, '(a)', advance='NO') '^'
            else
               write(output_unit, '(a)', advance='NO') ' '
            end if
         end if
         do j=1,size(grid, 2)
            if (grid(i,j)) write(output_unit, '(a)', advance='NO') '#'
            if (.not.grid(i,j)) write(output_unit, '(a)', advance='NO') '.'
         end do
         print*, " "
      end do

   end subroutine print_grid

end module aoc2023__day13
