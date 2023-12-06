module aoc2023__day05
   use, intrinsic :: iso_fortran_env, only: iostat_end, int64, int32
   implicit none
   private

   public :: day05

contains

   !> Solve the problem
   subroutine day05(input, part)
      !> The unit to read input from
      integer, intent(in) :: input
      !> Which part are we solving?
      integer, intent(in) :: part

      if (part == 1) call part1(input)
      if (part == 2) call part2(input)

   end subroutine day05

   !> Solve part 1
   subroutine part1(input)
      !> The unit to read input from
      integer, intent(in) :: input

      !> The seed values to translate
      integer(kind=int64), allocatable, dimension(:) :: seeds
      !> Flag for if seeds has been updated in this loop
      logical, allocatable, dimension(:) :: updated

      !> Working values for the translation
      integer(kind=int64) :: from, to, length

      !> Line buffer for text
      character(len=300) :: line
      !> Stat of io calls
      integer :: stat
      !> Loop counter
      integer :: i

      read(input, '(a)', iostat=stat) line
      length = 0
      do i=6,len_trim(line)
         if (line(i:i) == " ") length = length + 1
      end do

      allocate(seeds(length))
      allocate(updated(length))
      read(line(7:len_trim(line)), *) seeds

      stat = 0
      ! Blank line and first header
      read(input, '(a)') line
      read(input, '(a)') line
      ! print*, "Starting vals are: ", seeds
      do while (stat /= iostat_end)
         stat = 0
         updated = .false.
         read(input, *, iostat=stat) to, from, length
         do while (stat == 0)
            ! print '(a,i0,a,i0,a,i0)', "Rule: ", to, " ", from, " ", length
            do i=1,size(seeds)
               if (.not. updated(i) .and. seeds(i) >= from .and. seeds(i) < from + length) then
                  seeds(i) = seeds(i) - from + to
                  updated(i) = .true.
               end if
            end do
            read(input, *, iostat=stat) to, from, length
         end do
      end do
      ! print *, "Final values are: ", seeds

      print '(a, i0)', "Closest location is plot ", minval(seeds)

   end subroutine part1

   !> Solve part 2
   subroutine part2(input)
      !> The unit to read input from
      integer, intent(in) :: input

      !> The seed values to translate
      integer(kind=int64), allocatable, dimension(:) :: seeds, tmp
      !> Flag for if seeds has been updated in this loop
      logical, allocatable, dimension(:) :: updated, tmp2
      !> The number of ranges to track
      integer :: ranges

      !> Working values for the translation
      integer(kind=int64) :: from, to, length

      !> Line buffer for text
      character(len=300) :: line
      !> Stat of io calls
      integer :: stat
      !> Loop counter
      integer :: i

      read(input, '(a)', iostat=stat) line
      length = 0
      do i=6,len_trim(line)
         if (line(i:i) == " ") length = length + 1
      end do

      ranges = int(length, kind=int32) / 2
      ! Anticipate increasing number of ranges
      allocate(seeds(length * 2))
      allocate(updated(length))
      seeds = -1
      read(line(7:len_trim(line)), *) seeds(1:length)
      do i=1,ranges
         seeds(i*2) = seeds(i*2 - 1) + seeds(i*2) - 1
      end do

      stat = 0
      ! Blank line and first header
      read(input, '(a)') line
      read(input, '(a)') line
      do while (stat /= iostat_end)
         stat = 0
         updated = .false.
         read(input, *, iostat=stat) to, from, length
         do while (stat == 0)
            do i=1,ranges
               if (.not. updated(i) &
               & .and. (seeds(i*2-1) < from .and. from < seeds(i*2) &
               & .or. seeds(i*2-1) < from + length - 1 .and. from + length - 1 < seeds(i*2) &
               & .or. seeds(i*2-1) < from + length - 1 .and. seeds(i*2) > from)) then
                  if (seeds(i*2-1) < from) then
                     ! New range
                     ! - seeds(i*2-1) to from-1
                     if (ranges == size(seeds)/2) then
                        ! Extend array
                        allocate(tmp(ranges*2))
                        tmp = seeds
                        deallocate(seeds)
                        allocate(seeds(ranges*4))
                        seeds(1:ranges*2) = tmp
                        seeds(ranges*2+1:ranges*4) = -1
                        deallocate(tmp)
                        allocate(tmp2(ranges))
                        tmp2 = updated
                        deallocate(updated)
                        allocate(updated(ranges*2))
                        updated(1:ranges) = tmp2
                        updated(ranges+1:ranges*2) = .false.
                        deallocate(tmp2)
                     end if

                     ranges = ranges + 1
                     seeds(ranges*2 - 1) = seeds(i*2 - 1)
                     seeds(ranges*2) = from - 1
                     updated(ranges) = .false.
                  end if
                  if (seeds(i*2) > from + length - 1) then
                     ! New range
                     ! - from+length to seeds(i*2)
                     if (ranges == size(seeds)/2) then
                        ! Extend array
                        allocate(tmp(ranges*2))
                        tmp = seeds
                        deallocate(seeds)
                        allocate(seeds(ranges*4))
                        seeds(1:ranges*2) = tmp
                        seeds(ranges*2+1:ranges*4) = -1
                        deallocate(tmp)
                        allocate(tmp2(ranges))
                        tmp2 = updated
                        deallocate(updated)
                        allocate(updated(ranges*2))
                        updated(1:ranges) = tmp2
                        updated(ranges+1:ranges*2) = .false.
                        deallocate(tmp2)
                     end if
                     ranges = ranges + 1
                     seeds(ranges*2 - 1) = from + length
                     seeds(ranges*2) = seeds(i*2)
                     updated(ranges) = .false.
                  end if
                  ! Update range
                  seeds(i*2 - 1) = max(from, seeds(i*2 -1)) - from + to
                  seeds(i*2) = min(from + length - 1, seeds(i*2)) - from + to
                  updated(i) = .true.
               end if
            end do
            read(input, *, iostat=stat) to, from, length
         end do

      end do

      print '(a, i0)', "Closest location is plot ", minval(seeds(1:ranges*2))

   end subroutine part2

   !> Simplify a set of ranges (combining where necessary)
   subroutine simplify(ranges, num_ranges)
      !> The ranges to simplify
      integer(kind=int64), dimension(:), allocatable, intent(inout) :: ranges
      !> The number of ranges in ranges
      integer, intent(inout) :: num_ranges

      !> Did an update happen?
      logical :: updated

      !> Loop counters
      integer :: i, j

      ! Merge any overlapping ranges
      do i=1,num_ranges-1
         if (ranges(i*2) < 0) exit
         do j=i,num_ranges
            updated = .false.
            if (ranges(i*2-1) > ranges(j*2-1) .and. ranges(i*2-1) < ranges(j*2)) then
               ranges(i*2 - 1) = ranges(j*2 - 1)
               updated = .true.
            end if
            if (ranges(i*2) > ranges(j*2-1) .and. ranges(i*2) < ranges(j*2)) then
               ranges(i*2) = ranges(j*2)
               updated = .true.
            end if
            if (updated) then
               ranges(j*2 - 1) = -1
               ranges(j*2) = -1
            end if
         end do
      end do

      ! Remove any unnecessary ranges
      j=2
      do i=2,num_ranges
         if (ranges(i*2) >= 0) then
            ranges(j*2 - 1) = ranges(i*2 - 1)
            ranges(j*2) = ranges(i*2)
            j = j + 1
         end if
      end do

      num_ranges = j - 1
   end subroutine simplify

end module aoc2023__day05
