module aoc2023__day02
   use, intrinsic :: iso_fortran_env, only: iostat_end
   implicit none
   private

   public :: day02

contains

   !> Solve the problem
   subroutine day02(input, part)
      !> Open file unit for the input
      integer, intent(in) :: input
      !> Which part are we solving?
      integer, intent(in) :: part

      !> One line from the file
      character(len=300) :: line
      !> Length of the input
      integer :: length
      !> A single round within a game
      integer :: round_start, round_end
      !> Vars to parse into
      integer :: game_id, red, green, blue
      !> IO status
      integer :: stat
      !> IO message
      character(len=20) :: msg

      !> Total to calculate
      integer :: soln
      !> The maximum number of cubes for part 1
      integer, dimension(3) :: part1_max
      !> FLag for whether game is possible
      logical :: possible

      !> Cubes revealed
      integer, dimension(3) :: revealed
      !> Minimum number of cubes required
      integer, dimension(3) :: min_req

      soln = 0
      part1_max = [12, 13, 14]
      
      do
         read(input, '(a)', iostat=stat, iomsg=msg) line
         if (stat /= 0) then
            if (stat == iostat_end) exit
            print*, "Oops, can't read file...", msg
            stop
         end if
         
         length = len_trim(line)
         
         round_end = index(line, ":")
         read(line(6:round_end -1), *) game_id
         round_start = round_end + 1
         
         possible = .true.
         min_req = [0, 0, 0]
         
         do
            round_end = round_start + index(line(round_start:length), ";") - 2
            if (round_end < round_start) round_end = length
            revealed = min_possible(line(round_start:round_end))
            if (part == 1) possible = possible .and. all(revealed <= part1_max)
            if (part == 2) then
               min_req(1) = max(min_req(1), revealed(1))
               min_req(2) = max(min_req(2), revealed(2))
               min_req(3) = max(min_req(3), revealed(3))
            end if
            if (round_end == length) exit
            round_start = round_end + 3
         end do
         if (part == 1 .and. possible) then
            soln = soln + game_id
         elseif (part == 2) then
            soln = soln + product(min_req)
         end if
      end do

      print*, "They cheated! Max score was ", soln


   end subroutine day02

   !> Get the minimum number of cubes for the game
   function min_possible(round) result(revealed)
      !> The round to check
      character(len=*), intent(in) :: round
      !> Number of dice of each colour
      integer, dimension(3) :: revealed
      
      !> Status of read
      integer :: stat

      !> Indexs in string
      integer :: last_comma, next_comma
      !> Length of string
      integer :: length

      length = len_trim(round)
      revealed = [0, 0, 0]

      last_comma = 1
      next_comma = length
      do
         next_comma = last_comma + index(round(last_comma:length), ",") - 2
         if (next_comma < last_comma) next_comma = length
         select case (round(next_comma-2:next_comma))
          case ("red")
            read(round(last_comma:next_comma), *, iostat=stat) revealed(1)
          case ("een")
            read(round(last_comma:next_comma), *, iostat=stat) revealed(2)
          case ("lue")
            read(round(last_comma:next_comma), *, iostat=stat) revealed(3)
         end select
         if (next_comma >= length) exit
         last_comma = next_comma + 3
      end do

   end function min_possible

end module aoc2023__day02
