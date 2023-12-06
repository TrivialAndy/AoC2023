module aoc2023__day06
   use, intrinsic :: iso_fortran_env, only: real64, int64
   implicit none
   private

   public :: day06

contains

   !> Solve the problem
   subroutine day06(input, part)
      !> The unit to read input from
      integer, intent(in) :: input
      !> Which part are we solving?
      integer, intent(in) :: part

      !> Line and string buffers for reading
      character(len=100) :: line, substr

      !> The times to solve for
      integer(kind=int64), allocatable, dimension(:) :: times
      !> The distance to beat
      integer(kind=int64), allocatable, dimension(:) :: dist
      !> Solution vector
      integer, allocatable, dimension(:) :: soln
      !> Working REAL vector
      real(kind=real64), allocatable, dimension(:) :: work
      !> The number of races
      integer :: races

      !> Array indexes
      integer :: idx, idx2
      !> Length of input str
      integer :: length

      read(input, '(a)') line
      length = len_trim(line)

      if (part == 1) then
         races = 0
         do idx=6,length-1
            if (line(idx:idx) == " " .and. line(idx+1:idx+1) /= " ") races = races + 1
         end do
      else
         races = 1
      end if


      allocate(times(races))
      allocate(dist(races))
      allocate(soln(races))
      allocate(work(races))
      idx = 6
      idx2 = 1
      do while (idx < length)
         do while(line(idx:idx) == " ")
            idx = idx + 1
         end do
         if (part == 1) then
            read(line(idx:length), *) times(idx2)
            idx2 = idx2 + 1
         end if
         do while (line(idx:idx) /= " ")
            if (part == 2) then
               line(idx2:idx2) = line(idx:idx)
               idx2 = idx2 + 1
            end if
            idx = idx + 1
         end do
      end do
      if (part == 2) read(line(1:idx2-1), *) times

      read(input, '(a)') line
      length = len_trim(line)

      idx = 10
      idx2 = 1
      do while (idx < length)
         do while(line(idx:idx) == " ")
            idx = idx + 1
         end do
         if (part == 1) then
            read(line(idx:length), *) dist(idx2)
            idx2 = idx2 + 1
         end if
         do while (line(idx:idx) /= " ")
            if (part == 2) then
               line(idx2:idx2) = line(idx:idx)
               idx2 = idx2 + 1
            end if
            idx = idx + 1
         end do
      end do
      if (part == 2) read(line(1:idx2-1), *) dist

      ! Require:
      !   x(t-x)>d
      !   x**2 - xt + d < 0
      !   (x-t/2)**2 -t**2/4 + d < 0
      !   (x-t/2)**2 < t**2/4 - d
      !   x-t/2 < +- sqrt(t**2/4 - d)
      !   -sqrt(t**2/4 -d) + t/2 < x < sqrt(t**2/4 - d) + t/2

      work = sqrt(real(times**2, kind=real64)/4 - real(dist, kind=real64))
      soln = floor(real(times,kind=real64)/2 + work - 0.0001) - ceiling(real(times,kind=real64)/2 - work + 0.0001) + 1

      ! print*, "Time:    ", times
      ! print*, "Dist:    ", dist
      ! print*, "Work:    ", work
      ! print*, "Floor:   ", floor(real(times,kind=real64)/2 + work - 0.0001)
      ! print*, "Ceiling: ", ceiling(real(times,kind=real64)/2 - work + 0.0001)
      ! print*, "Soln:    ", soln
      print*, "Product of number of integer solutions: ", product(soln)

   end subroutine day06
end module aoc2023__day06
