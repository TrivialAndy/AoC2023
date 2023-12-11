module aoc2023__day11
   use, intrinsic :: iso_fortran_env, only: iostat_end, int32, int64
   use dynamic_array, only: grow, shrink
   implicit none
   private

   public :: day11

contains

   !> Solve the problem
   subroutine day11(input, part)
      !> The unit to read input from
      integer, intent(in) :: input
      !> Which part are we solving?
      integer, intent(in) :: part

      !> Line buffer
      character(len=300) :: line
      !> The location of the galaxies
      integer(int64), allocatable, dimension(:,:) :: galaxies
      !> Columns which have no galaxies in
      logical, allocatable, dimension(:) :: empty

      !> The number of galaxies
      integer :: galaxy_count

      !> The total sum of the shortest paths
      integer(int64) :: total

      !> Status of io calls
      integer :: stat
      !> Loop counters
      integer :: i, j


      read(input, '(a)', iostat=stat) line

      allocate(galaxies(8,2))
      allocate(empty(len_trim(line)))
      empty = .true.

      j = 0
      galaxy_count = 0
      do while (stat /= iostat_end)
         j = j + 1
         do i=1,len_trim(line)
            if (line(i:i) == "#") then
               galaxy_count = galaxy_count + 1
               if (size(galaxies, dim=1) < galaxy_count) call grow(galaxies, 1)
               galaxies(galaxy_count, :) = [i, j]
               empty(i) = .false.
            end if
         end do
         if (part == 1) then
            if (galaxies(galaxy_count, 2) /= j) j = j + 1
         else
            if (galaxies(galaxy_count, 2) /= j) j = j + 999999
         end if
         read(input, '(a)', iostat=stat) line
      end do

      ! do i=1,galaxy_count
      !    print '(*(i3))', galaxies(i,:)
      ! end do

      do i=1,galaxy_count
         total = 0
         do j=1,int(galaxies(i,1), int32)
            if (part == 1) then
               if (empty(j)) total = total + 1
            else
               if (empty(j)) total = total + 999999
            end if
         end do
         galaxies(i,1) = galaxies(i,1) + total
      end do

      total = 0
      do i=2,galaxy_count
         do j=1,i
            total = total + abs(galaxies(i,1) - galaxies(j,1))
            total = total + abs(galaxies(i,2) - galaxies(j,2))
         end do
      end do

      print '(a, i0, a)', "Space is so big, there's ", total, " gaps between the nearest galaxies"

   end subroutine day11
end module aoc2023__day11
