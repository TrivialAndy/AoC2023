module dynamic_array
   use, intrinsic :: iso_fortran_env, only: int32, int64
   implicit none
   private
   public :: grow, shrink

   interface grow
#:for dtype in [ 'integer', 'integer64', 'logical', 'character' ]
      procedure grow_${dtype}$
      procedure grow_2d_${dtype}$
#:endfor
   end interface

   interface shrink
   #:for dtype in [ 'integer', 'integer64', 'logical', 'character' ]
      procedure shrink_${dtype}$
      procedure shrink_2d_${dtype}$
   #:endfor
   end interface
contains
#:for dt_name, dt_keyword in [ ('integer', 'integer(kind=int32)'), ('integer64', 'integer(kind=int64)'), ('logical', 'logical'), ('character', 'character(len=*)') ]
   !> Double the size of an allocatable array
   subroutine grow_${dt_name}$(arr)
      !> The array to grow
      ${dt_keyword}$, allocatable, dimension(:), intent(inout) :: arr

      !> A temp array
      #:if dt_name != 'character'
      ${dt_keyword}$, allocatable, dimension(:) :: tmp
      #:else
      character(len=len(arr(1))), allocatable, dimension(:) :: tmp
      #:endif

      !> The size of the array
      integer :: length

      length = size(arr)
      allocate(tmp(length*2))
      tmp(1:length) = arr(1:length)
      deallocate(arr)
      call move_alloc(tmp, arr)
   end subroutine grow_${dt_name}$

   !> Double the size of a 2d allocatable array
   subroutine grow_2d_${dt_name}$(arr, dim)
      !> The array to grow
      ${dt_keyword}$, allocatable, dimension(:,:), intent(inout) :: arr
      !> The dimension to grow along
      integer, intent(in) :: dim

      !> A temp array
      #:if dt_name != 'character'
      ${dt_keyword}$, allocatable, dimension(:,:) :: tmp
      #:else
      character(len=len(arr(1,1))), allocatable, dimension(:,:) :: tmp
      #:endif

      !> The size of the array
      integer, dimension(2) :: length

      length = shape(arr)
      length(dim) = length(dim)*2
      allocate(tmp(length(1),length(2)))
      length(dim) = length(dim)/2
      tmp(1:length(1),1:length(2)) = arr(1:length(1),1:length(2))
      deallocate(arr)
      call move_alloc(tmp, arr)
   end subroutine grow_2d_${dt_name}$

   !> Reduce an allocatable array to the given size
   subroutine shrink_${dt_name}$(arr, length)
      !> The array to grow
      ${dt_keyword}$, allocatable, dimension(:), intent(inout) :: arr
      !> The size of the array
      integer, intent(in) :: length

      !> A temp array
      #:if dt_name != 'character'
      ${dt_keyword}$, allocatable, dimension(:) :: tmp
      #:else
      character(len=len(arr(1))), allocatable, dimension(:) :: tmp
      #:endif

      allocate(tmp(length))
      tmp(1:length) = arr(1:length)
      deallocate(arr)
      call move_alloc(tmp, arr)
      
   end subroutine shrink_${dt_name}$
   
   !> Double the size of a 2d allocatable array
   subroutine shrink_2d_${dt_name}$(arr, dim, length)
      !> The array to grow
      ${dt_keyword}$, allocatable, dimension(:,:), intent(inout) :: arr
      !> The dimension to grow along
      integer, intent(in) :: dim
      !> The size of the array
      integer, intent(in) :: length

      !> A temp array
      #:if dt_name != 'character'
      ${dt_keyword}$, allocatable, dimension(:,:) :: tmp
      #:else
      character(len=len(arr(1,1))), allocatable, dimension(:,:) :: tmp
      #:endif
      !> The size of the array
      integer, dimension(2) :: arr_shape

      arr_shape = shape(arr)
      arr_shape(dim) = length
      allocate(tmp(arr_shape(1),arr_shape(2)))
      tmp(1:arr_shape(1),1:arr_shape(2)) = arr(1:arr_shape(1),1:arr_shape(2))
      deallocate(arr)
      call move_alloc(tmp, arr)

   end subroutine shrink_2d_${dt_name}$

#:endfor
end module dynamic_array
