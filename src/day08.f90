module aoc2023__day08
   use, intrinsic ::iso_fortran_env, only: iostat_end, iostat_eor, int64
   use stdlib_sorting, only: sort_index, int_size
   use stdlib_ascii, only: reverse
   use dynamic_array, only: grow, shrink
   implicit none
   private

   public :: day08

contains

   !> Solve the problem
   subroutine day08(input, part)
      !> The unit to read input from
      integer, intent(in) :: input
      !> Which part are we solving?
      integer, intent(in) :: part

      !> The route to take (L = .true.)
      logical, allocatable, dimension(:) :: route
      !> The length of the route
      integer :: route_len

      !> The name of each index
      character(len=3), allocatable, dimension(:) :: lookup
      !> An index to speed up searching
      integer, dimension(27) :: lookup_index

      !> Unparsed list of children
      character(len=6), dimension(:), allocatable :: children
      !> The left and right children of each index
      integer(int_size), dimension(:), allocatable :: left, right

      !> Buffer for file input
      character(len=50) :: line

      !> Total steps take
      integer(int64) :: steps

      !> Status of io calls
      integer :: stat
      !> Number of items
      integer :: count
      !> Loop counters
      integer :: i


      ! Read LR (using buffer)
      stat = 0
      route_len = 0
      allocate(route(8))

      do while (stat /= iostat_eor)
         read(input, '(a50)', advance='NO', size=count, iostat=stat) line
         do while (size(route) < route_len + count )
            call grow(route)
         end do
         do i=1,count
            route_len = route_len + 1
            route(route_len) = line(i:i) == "L"
         end do
      end do

      ! Read graph into lookup, children
      allocate(lookup(8), children(8))
      count = 0
      read(input, '(a)') line
      read(input, '(a)', iostat=stat) line
      do while (stat /= iostat_end)
         if (size(lookup) <= count) then
            call grow(lookup)
            call grow(children)
         end if
         count = count + 1
         lookup(count) = reverse(line(1:3))
         children(count) = reverse(line(8:10)) // reverse(line(13:15))

         read(input, '(a)', iostat=stat) line
      end do

      ! Sort lookup (and children)
      allocate(left(count), right(count))
      call sort_index(lookup(1:count), left)
      children(1:count) = children(left)

      ! Create index
      stat = 0  ! counter for index in lookup_index
      lookup_index = -1
      do i=1,count
         do while (achar(64+stat) /= lookup(i)(1:1))
            stat = stat + 1
            lookup_index(stat) = i
         end do
      end do
      lookup_index(27) = count

      ! Set left and right
      do i=1,count
         left(i) = find(lookup, lookup_index, children(i)(1:3))
         right(i) = find(lookup, lookup_index, children(i)(4:6))
      end do

      steps = 0
      if (part == 1) then
         ! Traverse
         i = 1
         do while (lookup(i) /= "ZZZ")
            steps = steps + 1
            if (route(mod(steps-1, route_len)+1)) then
               i = left(i)
            else
               i = right(i)
            end if
         end do
      else
         steps = part2(lookup(1:count), lookup_index, left(1:count), right(1:count), route(1:route_len))
      end if
      print '(a,i0,a)', "Phew! That took ", steps, " steps!"
   end subroutine day08

   !> Find an elem in a lookup table
   function find(lookup, lookup_index, elem) result(l)
      !> The lookup table
      character(len=3), dimension(:), intent(in) :: lookup
      !> An index on the first character
      integer, dimension(27), intent(in) :: lookup_index
      !> The element to find
      character(len=3), intent(in) :: elem

      !> Left and right indices for binary search
      integer :: l, r

      l = lookup_index(iachar(elem(1:1)) - 64)
      r = lookup_index(iachar(elem(1:1)) - 63)
      do while (lookup(l) /= elem)
         if (lookup((l+r)/2) == elem) then
            l = (l+r)/2
         elseif (lookup((l+r)/2) < elem) then
            l = (l+r)/2 + 1
         else
            r = (l+r)/2 - 1
         end if
      end do
   end function

   ! Part 2 - Create combined_head_list and combined_loop list
   !        - for each ??A
   !        - follow route and track the history from step j-count*route_len to step j
   !        - store any indices of ??Z elems in head_list
   !        - keep going until lookup(i) == hist(j-k*route_len) for int k then scan hist and create a loop_list of ??Z indices
   !        - eg for route_len=50. BCA - 5 (MNZ), 8 (PLZ), 17(HJZ), 54(PLZ), 61(LLZ), 67(HJZ). head_list=5,8,17 loop_list=37,7,6
   !        - Loop through head_list (adding elems from loop list to extend if needed)
   !        - if head_list(n) or combined_head_list(m) is part of a match, put the result in a new combined_head_list (or if new head list is empty)
   !        - once looping with just tail lists, go through longest (sum(tail_list)) 2 times put the diffs between matches in new_tail list. Dont add the last val if it's a match
   !        - Repeat for each new ??A until all have been merged in.
   !
   ! For a head_list, loop_list for QWA and SDA
   ! if head_lists never match, loop_lists will match every x
   ! route_len=6, count=10
   ! hl1=1,5 hl2=2 tl1=2,4,7,5 tl2=5,1
   ! all 1 =  1,5,   7,11,18,23,     25,29,36,41,    43,47,54,59,   61,65,72,77
   ! all 2 =    2,   7,8,  13,14,  19,20,  25,26,  31,32,  37,38,  43,44, 49,50, 55,56, 61,62
   ! match =  7,25,43,61
   ! new hl = 7
   ! new tl  =  18
   function part2(lookup, lookup_index, left, right, route) result(steps)
      !> The lookup table for the elements
      character(len=3), dimension(:), intent(in) :: lookup
      !> The index for each starting letter for the elements
      integer, dimension(27), intent(in) :: lookup_index
      !> The left and right children of each element
      integer(int_size), dimension(:), intent(in) :: left, right
      !> The route to take
      logical, dimension(:), intent(in) :: route

      !> The number of steps
      integer(int64) :: steps

      !> Loop counter
      integer :: i

      integer, allocatable, dimension(:) :: starts
      integer(int64), allocatable, dimension(:) :: head
      integer(int64), allocatable, dimension(:) :: loop

      allocate(starts(lookup_index(2)-1))
      do i=1,size(starts)
         starts(i) = i
      end do
      call common_loops(starts, lookup, lookup_index, left, right, route, head, loop)
      steps = head(1)

   end function part2

   !> Find the common loops from the starting point
   recursive subroutine common_loops(starts, lookup, lookup_index, left, right, route, head, loop)
      !> The indexes to start from
      integer, dimension(:), intent(in) :: starts
      !> The lookup table for the elements
      character(len=3), dimension(:), intent(in) :: lookup
      !> The index for each starting letter for the elements
      integer, dimension(27), intent(in) :: lookup_index
      !> The left and right children of each element
      integer(int_size), dimension(:), intent(in) :: left, right
      !> The route to take
      logical, dimension(:), intent(in) :: route

      !> The number of steps to non-looping ??Z elements
      integer(int64), allocatable, dimension(:), intent(out) :: head
      !> The number of steps between each looping ??Z element
      integer(int64), allocatable, dimension(:), intent(out) :: loop

      !> Head from each half of starts
      integer(int64), allocatable, dimension(:) :: head1, head2
      !> Loop for each half of starts
      integer(int64), allocatable, dimension(:) :: loop1, loop2

      !> Indexes into each heads and loops
      integer :: ih, ih1, ih2, il, il1, il2
      !> Sums for each loop
      integer(int64) :: tot, tot1, tot2, tot_max

      !> last head in loops calc
      integer(int64) :: hist

      !> The index to split starts at
      integer :: m

      if (size(starts) == 1) then
         call detect_loops(starts(1), lookup, lookup_index, left, right, route, head, loop)
         return
      end if

      m = size(starts) / 2
      call common_loops(starts(1:m), lookup, lookup_index, left, right, route, head1, loop1)
      call common_loops(starts(m+1:size(starts)), lookup, lookup_index, left, right, route, head2, loop2)

      tot1 = sum(loop1)
      tot2 = sum(loop2)
      tot_max = tot2
      do while (mod(tot_max,tot1) /= 0)
         tot_max = tot_max + tot2
      end do

      allocate(head(8))

      ih = 0
      ih1 = 1
      ih2 = 1
      il = 0
      il1 = 0
      il2 = 0
      do while (.not. allocated(loop))
         if (head1(ih1) == head2(ih2)) then
            if (ih >= size(head)) call grow(head)
            ih = ih + 1
            head(ih) = head1(ih1)
            ih1 = ih1 + 1
            ih2 = ih2 + 1
         elseif (head1(ih1) < head2(ih2)) then
            ih1 = ih1 + 1
         else
            ih2 = ih2 + 1
         end if

         if (ih1 > size(head1) .and. ih2 > size(head2)) then
            allocate(loop(8))
         elseif (ih1 > size(head1)) then
            ih1 = ih1 - 1
            il1 = mod(il1, size(loop1)) + 1
            head1(ih1) = head1(ih1) + loop1(il1)
         elseif (ih2 > size(head2)) then
            ih2 = ih2 - 1
            il2 = mod(il2, size(loop2)) + 1
            head2(ih2) = head2(ih2) + loop2(il2)
         end if
      end do

      hist = head(ih)
      ih1 = ih1 - 1
      ih2 = ih2 - 1
      
      il1 = mod(il1, size(loop1)) + 1
      head1(ih1) = head1(ih1) + loop1(il1)

      tot = 0
      do while (mod(tot, tot_max) /= 0 .or. tot == 0)
         if (head1(ih1) < head2(ih2)) then
            il1 = mod(il1, size(loop1)) + 1
            head1(ih1) = head1(ih1) + loop1(il1)
         elseif (head2(ih2) < head1(ih1)) then
            il2 = mod(il2, size(loop2)) + 1
            head2(ih2) = head2(ih2) + loop2(il2)
         endif

         if (head1(ih1) == head2(ih2)) then
            if (il >= size(loop)) call grow(loop)
            il = il + 1
            loop(il) = head1(ih1) - hist
            tot = tot + loop(il)
            hist = head1(ih1)
            il1 = mod(il1, size(loop1)) + 1
            il2 = mod(il2, size(loop2)) + 1
            head1(ih1) = head1(ih1) + loop2(il1)
            head2(ih2) = head2(ih2) + loop2(il2)
         end if
      end do

      deallocate(head1, head2, loop1, loop2)
      call shrink(head, ih)
      call shrink(loop, il)
   end subroutine common_loops


   subroutine detect_loops(start, lookup, lookup_index, left, right, route, head, loop)
      !> The index to start from
      integer, intent(in) :: start
      !> The lookup table for the elements
      character(len=3), dimension(:), intent(in) :: lookup
      !> The index for each starting letter for the elements
      integer, dimension(27), intent(in) :: lookup_index
      !> The left and right children of each element
      integer(int_size), dimension(:), intent(in) :: left, right
      !> The route to take
      logical, dimension(:), intent(in) :: route

      !> The number of steps to non-looping ??Z elements
      integer(int64), allocatable, dimension(:), intent(out) :: head
      !> The number of steps between each looping ??Z element
      integer(int64), allocatable, dimension(:), intent(out) :: loop

      !> The idx for each value in head
      integer, allocatable, dimension(:) :: head_val
      !> The last index of head and loop
      integer :: head_idx, loop_idx
      !> The current element to check
      integer :: idx
      !> The number of steps taken
      integer :: steps

      !> Loop counter
      integer :: i, j

      allocate(head(8), head_val(8))
      steps = 0
      head_idx = 0
      idx = start

      ! Find a loop
      do while (.not. allocated(loop))
         if (idx >= lookup_index(26)) then
            head_idx = head_idx + 1
            if (head_idx > size(head)) then
               call grow(head)
               call grow(head_val)
            end if
            head(head_idx) = steps
            head_val(head_idx) = idx
            do i=1,head_idx-1
               if (head_val(i) == idx .and. mod(steps - head(i), size(route)) == 0) then
                  ! loop!
                  allocate(loop(8))
                  exit
               end if
            end do
         end if

         steps = steps + 1
         if (route(mod(steps-1, size(route)) + 1)) then
            idx = left(idx)
         else
            idx = right(idx)
         end if
      end do

      ! Backtrack to find loop
      loop_idx = 0
      do while (i < head_idx)
         loop_idx = loop_idx + 1
         if (loop_idx >= size(loop)) call grow(loop)
         loop(loop_idx) = head(i+1) - head(i)
         i = i + 1
      end do
      head_idx = head_idx - loop_idx

      call shrink(head, head_idx)
      call shrink(loop, loop_idx)
   end subroutine detect_loops
end module aoc2023__day08

! Email poppy
