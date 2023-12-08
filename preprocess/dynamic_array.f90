module dynamic_array
   use, intrinsic :: iso_fortran_env, only: int64
   implicit none
   private
   public :: grow, shrink

   interface grow
#:for dtype in [ 'integer', 'integer64', 'logical', 'character' ]
      procedure grow_${dtype}$
#:endfor
   end interface

   interface shrink
   #:for dtype in [ 'integer', 'integer64', 'logical', 'character' ]
         procedure shrink_${dtype}$
   #:endfor
   end interface
contains
#:for dtype in [ 'integer', 'integer64', 'logical', 'character' ]
   !> Double the size of an allocatable array
   subroutine grow_${dtype}$(arr)
      #:if dtype in [ 'integer', 'logical' ]
      !> The array to grow
      ${dtype}$, allocatable, dimension(:), intent(inout) :: arr
      !> A temp array
      ${dtype}$, allocatable, dimension(:) :: tmp
      #:elif dtype == 'integer64'
      !> The array to grow
      integer(int64), allocatable, dimension(:), intent(inout) :: arr
      !> A temp array
      integer(int64), allocatable, dimension(:) :: tmp
      #:elif dtype == 'character'
      !> The array to grow
      character(len=*), allocatable, dimension(:), intent(inout) :: arr
      !> A temp array
      character(len=len(arr(1))), allocatable, dimension(:) :: tmp
      #:endif
      !> The size of the array
      integer :: length

      length = size(arr)
      allocate(tmp(length*2))
      tmp(1:length) = arr(1:length)
      deallocate(arr)
      call move_alloc(tmp, arr)
   end subroutine grow_${dtype}$

   !> Reduce an allocatable array to the given size
   subroutine shrink_${dtype}$(arr, length)
      !> The size of the array
      integer, intent(in) :: length
      #:if dtype in [ 'integer', 'logical' ]
      !> The array to shrink
      ${dtype}$, allocatable, dimension(:), intent(inout) :: arr
      !> A temp array
      ${dtype}$, allocatable, dimension(:) :: tmp
      #:elif dtype == 'integer64'
      !> The array to grow
      integer(int64), allocatable, dimension(:), intent(inout) :: arr
      !> A temp array
      integer(int64), allocatable, dimension(:) :: tmp
      #:elif dtype == 'character'
      !> The array to shrink
      character(len=*), allocatable, dimension(:), intent(inout) :: arr
      !> A temp array
      character(len=len(arr(1))), allocatable, dimension(:) :: tmp
      #:endif

      allocate(tmp(length))
      tmp(1:length) = arr(1:length)
      deallocate(arr)
      call move_alloc(tmp, arr)
      
   end subroutine shrink_${dtype}$

#:endfor
end module dynamic_array
