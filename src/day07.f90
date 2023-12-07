module aoc2023__day07
   use, intrinsic :: iso_fortran_env, only: iostat_end
   use stdlib_sorting, only: sort_index, int_size
   implicit none
   private

   public :: day07

contains

   !> Solve the problem
   subroutine day07(input, part)
      !> The unit to read input from
      integer, intent(in) :: input
      !> Which part are we solving?
      integer, intent(in) :: part

      !> The hands to compare (and temp array for resizing)
      character(len=6), allocatable, dimension(:) :: hands, tmp_hands
      !> The bids for each hand (and temp array for resizing)
      integer, allocatable, dimension(:) :: bids, tmp_bids
      !> The number of hands each hand beats
      integer(kind=int_size), allocatable, dimension(:) :: scores
      !> The number of hands to compare
      integer :: count

      !> Total score!
      integer :: score

      !> Loop counters
      integer :: i,j,k
      !> Status of io calls
      integer :: stat

      count = 1
      allocate(hands(8), bids(8))

      do
         if (size(hands) == count) then
            allocate(tmp_hands(count*2), tmp_bids(count*2))
            tmp_hands(1:count) = hands
            tmp_bids(1:count) = bids
            call move_alloc(tmp_hands, hands)
            call move_alloc(tmp_bids, bids)
         end if
         read(input, *, iostat=stat) hands(count)(2:6), bids(count)
         if (stat /= 0) then
            if (stat == iostat_end) exit
            print*, "Ooops file read error!"
            stop
         end if
         call rewrite_hand(hands(count), part)
         count = count + 1
      end do

      count = count - 1

      allocate(scores(count))

      call sort_index(hands(1:count), scores)

      score = 0
      bids(1:count) = bids(scores)
      do i=1,count
         score = score + i * bids(i)
      end do

      print '(a,i0)', "Final score... ", score
   end subroutine day07

   !> Get the type of the hand:
   !> High card = 1
   !> One pair = 2
   !> Two pair = 3
   !> Three of a kind = 4
   !> Full house = 5
   !> Four of a kind = 6
   !> Five of a kind = 7
   subroutine rewrite_hand(hand, part)
      !> The hand to test
      character(len=6), intent(inout) :: hand
      !> Which part are we solving
      integer, intent(in) :: part
      !> The type of the hand
      integer :: type

      !> Count of each card
      integer, dimension(13) :: cards
      !> Max and second max number of matches
      integer, dimension(2) :: best

      !> Loop counter
      integer :: i
      !> Tmp card value
      integer :: val

      cards = 0
      do i=2,6
         val = card_value(hand(i:i), part)
         cards(val) = cards(val) + 1
         hand(i:i) = char(64+val)
      end do

      best = 1
      if (part == 1) then
         val = 1
      else
         val = 2
      end if
      do i=13,val,-1
         if (cards(i) > best(1)) then
            best(2) = best(1)
            best(1) = cards(i)
         elseif (cards(i) > best(2)) then
            best(2) = cards(i)
         end if
      end do

      if (part == 2) best(1) = best(1) + cards(1)
      if (best(1) > 5) best(1) = 5
      select case(best(1))
       case(1)
         hand(1:1) = "1"
       case(2)
         if (best(2) == 1) then
            hand(1:1) = "2"
         else
            hand(1:1) = "3"
         end if
       case(3)
         if (best(2) == 1) then
            hand(1:1) = "4"
         else
            hand(1:1) = "5"
         end if
       case(4)
         hand(1:1) = "6"
       case(5)
         hand(1:1) = "7"
      end select
   end subroutine rewrite_hand

   !> Convert a card character to a value
   function card_value(card, part) result(val)
      !> The card to convert
      character(len=1), intent(in) :: card
      !> Which part are we solving
      integer, intent(in) :: part
      !> The value of the card
      integer :: val
      select case(card)
       case("2")
         val = 1
       case("3")
         val = 2
       case("4")
         val = 3
       case("5")
         val = 4
       case("6")
         val = 5
       case("7")
         val = 6
       case("8")
         val = 7
       case("9")
         val = 8
       case("T")
         val = 9
       case("J")
         val = 10
       case("Q")
         val = 11
       case("K")
         val = 12
       case("A")
         val = 13
      end select
      if (part == 2) then
         if (val == 10) then
            val = 1
         elseif (val < 10) then
            val = val + 1
         end if
      end if
   end function card_value

end module aoc2023__day07
