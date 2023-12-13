module aoc2023__day12
   use, intrinsic :: iso_fortran_env, only: iostat_end, int64
   implicit none
   private

   public :: day12

   type :: cache
      integer, dimension(:), allocatable :: broken
      character(len=:), allocatable :: map
      integer(int64) :: total
   end type cache

contains

   !> Solve the problem
   subroutine day12(input, part)
      !> The unit to read input from
      integer, intent(in) :: input
      !> Which part are we solving?
      integer, intent(in) :: part

      !> Input buffers
      character(len=300) :: map
      !> Index for broken ranges portion of input
      integer :: broken_idx
      !> Status of io calls
      integer :: stat
      !> The broken ranges
      integer, allocatable, dimension(:) :: broken

      !> Total number of paths
      integer(int64) :: total

      !> Counter
      integer :: i, count

      !> Number of times to unfold
      integer :: unfold

      !> Lru cache
      type(cache), dimension(16) :: lru_cache
      !> Lru index
      integer :: lru_index

      lru_index = 1
      do i=1,size(lru_cache)
         lru_cache(i)%broken=[1]
         lru_cache(i)%map=""
         lru_cache(i)%total=0
      end do

      if (part == 1) then
         unfold = 1
      else
         unfold = 5
      end if

      total = 0
      read(input, '(a)', iostat=stat) map
      do while (stat /= iostat_end)
         count = 1
         do i=1,len_trim(map)
            if (map(i:i) == " ") broken_idx = i+1
            if (map(i:i) == ",") count = count + 1
         end do
         allocate(broken(unfold*count))
         do i=0,unfold-1
            read(map(broken_idx:len_trim(map)), *) broken(i*count+1:(i+1)*count)
         end do
         do i=1,unfold-1
            map(i*(broken_idx-1):i*(broken_idx-1)) = "?"
            map(i*(broken_idx-1)+1:(i+1)*(broken_idx-1)-1) = map(1:broken_idx-2)
         end do
         broken_idx = (broken_idx-1)*unfold - 1
         ! print*, trim(map)
         ! print*, map(1:broken_idx)
         ! print '(*(i3))', broken
         total = total + count_possible(map(1:broken_idx), broken, lru_cache, lru_index)
         ! print*, "New total = ", total
         deallocate(broken)

         read(input, '(a)', iostat=stat) map
      end do

      print '(a,i0,a)', "We'll never solve this. There're ", total, " options"
   end subroutine day12

   !!!! TODO !!!!
   ! Rewrite to work on the blocks and remove the need for checking every char
   ! map = #?????, broken = 3 1 -> #??? (###.) can be removed so map = ??, broken = 1
   ! now the first can be ignored (consume ?) or accepted (consume ??)
   !> Count the number of possible permutations of the broken parts
   recursive function count_possible(map, broken, lru_cache, lru_index) result(count)
      !> The map of parts with some missing information
      character(len=*), intent(in) :: map
      !> The list of runs of broken parts
      integer, dimension(:), intent(in) :: broken
      !> Lru cache
      type(cache), dimension(:), intent(inout) :: lru_cache
      !> Lru index
      integer, intent(inout) :: lru_index

      !> The final count of the perms
      integer(int64) :: count

      !> Loop counter
      integer :: i

      ! print '(a,a,a,i0,a)', "Count: Input map = ", map, " (len=",len(map),")"
      ! print*, "Count: Input broken = ", broken
      ! End of recursion!
      if (size(broken) == 0) then
         count = 1
         do i=1,len(map)
            if (map(i:i) == "#") count = 0
         end do
         ! print*, "Count: Output = ", count
         return
      end if

      ! Not enough space to get any more possible paths
      if (sum(broken) + size(broken) - 1 > len(map)) then
         count = 0
         ! print '(a,i0,a,i0)', "Not enough space! Sum = ", sum(broken), ", size = ", size(broken)
         ! print*, "Count: Output = ", count
         return
      end if

      count = 0
      select case(map(1:1))
       case ("#")
         do i=1,broken(1)
            if (map(i:i) == ".") then
               count = 0
               ! print*, "Count: Output = ", count
               return
            end if
         end do
         if (len(map) > broken(1)) then
            if (map(broken(1)+1:broken(1)+1) == "#") then
               count = 0
               ! print*, "Count: Output = ", count
               return
            end if
         end if
         count = count_possible(map(broken(1)+2:len(map)), broken(2:size(broken)), lru_cache, lru_index)
       case (".")
         i = 1
         do
            if (i > len(map)) exit
            if (map(i:i) /= ".") exit
            i = i + 1
         end do

         count = count_possible(map(i:len(map)), broken, lru_cache, lru_index)
       case("?")
         do i=1,size(lru_cache)
            if (size(lru_cache(i)%broken) == size(broken) .and. lru_cache(i)%map == map) then
               if (all(lru_cache(i)%broken == broken)) then
                  count = lru_cache(i)%total
                  return
               end if
            end if
         end do
         count = count_possible(map(2:len(map)), broken, lru_cache, lru_index)
         i = index(map(1:broken(1)), ".")
         if (i < 1) then
            i = broken(1)+1
            if (size(broken) > 1) then
               if (map(i:i) == "#") then
                  i = -1
               else
                  i = i + 1
               end if
            end if
            if (i > 0) then
               count = count + count_possible(map(i:len(map)), broken(2:size(broken)), lru_cache, lru_index)
            end if
         end if
         lru_cache(lru_index)%map = map
         lru_cache(lru_index)%broken = broken
         lru_cache(lru_index)%total = count
         lru_index = mod(lru_index, size(lru_cache)) + 1
      end select
      ! print*, "Count: Output = ", count

   end function count_possible

end module aoc2023__day12
