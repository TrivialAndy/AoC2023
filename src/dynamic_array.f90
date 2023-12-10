module dynamic_array
   use, intrinsic :: iso_fortran_env, only: int64
   implicit none
   private
   public :: grow, shrink

   interface grow
      procedure grow_integer
      procedure grow_2d_integer
      procedure grow_integer64
      procedure grow_2d_integer64
      procedure grow_logical
      procedure grow_2d_logical
      procedure grow_character
      procedure grow_2d_character
   end interface

   interface shrink
      procedure shrink_integer
      procedure shrink_2d_integer
      procedure shrink_integer64
      procedure shrink_2d_integer64
      procedure shrink_logical
      procedure shrink_2d_logical
      procedure shrink_character
      procedure shrink_2d_character
   end interface
contains
   !> Double the size of an allocatable array
   subroutine grow_integer(arr)
      !> The array to grow
      integer, allocatable, dimension(:), intent(inout) :: arr
      !> A temp array
      integer, allocatable, dimension(:) :: tmp
      !> The size of the array
      integer :: length

      length = size(arr)
      allocate(tmp(length*2))
      tmp(1:length) = arr(1:length)
      deallocate(arr)
      call move_alloc(tmp, arr)
   end subroutine grow_integer

   !> Double the size of a 2d allocatable array
   subroutine grow_2d_integer(arr, dim)
      !> The dimension to grow along
      integer, intent(in) :: dim
      !> The array to grow
      integer, allocatable, dimension(:,:), intent(inout) :: arr
      !> A temp array
      integer, allocatable, dimension(:,:) :: tmp
      !> The size of the array
      integer, dimension(2) :: length

      length = shape(arr)
      length(dim) = length(dim)*2
      allocate(tmp(length(1),length(2)))
      length(dim) = length(dim)/2
      tmp(1:length(1),1:length(2)) = arr(1:length(1),1:length(2))
      deallocate(arr)
      call move_alloc(tmp, arr)
   end subroutine grow_2d_integer

   !> Reduce an allocatable array to the given size
   subroutine shrink_integer(arr, length)
      !> The size of the array
      integer, intent(in) :: length
      !> The array to shrink
      integer, allocatable, dimension(:), intent(inout) :: arr
      !> A temp array
      integer, allocatable, dimension(:) :: tmp

      allocate(tmp(length))
      tmp(1:length) = arr(1:length)
      deallocate(arr)
      call move_alloc(tmp, arr)
      
   end subroutine shrink_integer
   
   !> Double the size of a 2d allocatable array
   subroutine shrink_2d_integer(arr, dim, length)
      !> The dimension to grow along
      integer, intent(in) :: dim
      !> The size of the array
      integer, intent(in) :: length
      !> The array to grow
      integer, allocatable, dimension(:,:), intent(inout) :: arr
      !> A temp array
      integer, allocatable, dimension(:,:) :: tmp
      !> The size of the array
      integer, dimension(2) :: arr_shape

      arr_shape = shape(arr)
      arr_shape(dim) = length
      allocate(tmp(arr_shape(1),arr_shape(2)))
      tmp(1:arr_shape(1),1:arr_shape(2)) = arr(1:arr_shape(1),1:arr_shape(2))
      deallocate(arr)
      call move_alloc(tmp, arr)

   end subroutine shrink_2d_integer

   !> Double the size of an allocatable array
   subroutine grow_integer64(arr)
      !> The array to grow
      integer(int64), allocatable, dimension(:), intent(inout) :: arr
      !> A temp array
      integer(int64), allocatable, dimension(:) :: tmp
      !> The size of the array
      integer :: length

      length = size(arr)
      allocate(tmp(length*2))
      tmp(1:length) = arr(1:length)
      deallocate(arr)
      call move_alloc(tmp, arr)
   end subroutine grow_integer64

   !> Double the size of a 2d allocatable array
   subroutine grow_2d_integer64(arr, dim)
      !> The dimension to grow along
      integer, intent(in) :: dim
      !> The array to grow
      integer(int64), allocatable, dimension(:,:), intent(inout) :: arr
      !> A temp array
      integer(int64), allocatable, dimension(:,:) :: tmp
      !> The size of the array
      integer, dimension(2) :: length

      length = shape(arr)
      length(dim) = length(dim)*2
      allocate(tmp(length(1),length(2)))
      length(dim) = length(dim)/2
      tmp(1:length(1),1:length(2)) = arr(1:length(1),1:length(2))
      deallocate(arr)
      call move_alloc(tmp, arr)
   end subroutine grow_2d_integer64

   !> Reduce an allocatable array to the given size
   subroutine shrink_integer64(arr, length)
      !> The size of the array
      integer, intent(in) :: length
      !> The array to grow
      integer(int64), allocatable, dimension(:), intent(inout) :: arr
      !> A temp array
      integer(int64), allocatable, dimension(:) :: tmp

      allocate(tmp(length))
      tmp(1:length) = arr(1:length)
      deallocate(arr)
      call move_alloc(tmp, arr)
      
   end subroutine shrink_integer64
   
   !> Double the size of a 2d allocatable array
   subroutine shrink_2d_integer64(arr, dim, length)
      !> The dimension to grow along
      integer, intent(in) :: dim
      !> The size of the array
      integer, intent(in) :: length
      !> The array to grow
      integer(int64), allocatable, dimension(:,:), intent(inout) :: arr
      !> A temp array
      integer(int64), allocatable, dimension(:,:) :: tmp
      !> The size of the array
      integer, dimension(2) :: arr_shape

      arr_shape = shape(arr)
      arr_shape(dim) = length
      allocate(tmp(arr_shape(1),arr_shape(2)))
      tmp(1:arr_shape(1),1:arr_shape(2)) = arr(1:arr_shape(1),1:arr_shape(2))
      deallocate(arr)
      call move_alloc(tmp, arr)

   end subroutine shrink_2d_integer64

   !> Double the size of an allocatable array
   subroutine grow_logical(arr)
      !> The array to grow
      logical, allocatable, dimension(:), intent(inout) :: arr
      !> A temp array
      logical, allocatable, dimension(:) :: tmp
      !> The size of the array
      integer :: length

      length = size(arr)
      allocate(tmp(length*2))
      tmp(1:length) = arr(1:length)
      deallocate(arr)
      call move_alloc(tmp, arr)
   end subroutine grow_logical

   !> Double the size of a 2d allocatable array
   subroutine grow_2d_logical(arr, dim)
      !> The dimension to grow along
      integer, intent(in) :: dim
      !> The array to grow
      logical, allocatable, dimension(:,:), intent(inout) :: arr
      !> A temp array
      logical, allocatable, dimension(:,:) :: tmp
      !> The size of the array
      integer, dimension(2) :: length

      length = shape(arr)
      length(dim) = length(dim)*2
      allocate(tmp(length(1),length(2)))
      length(dim) = length(dim)/2
      tmp(1:length(1),1:length(2)) = arr(1:length(1),1:length(2))
      deallocate(arr)
      call move_alloc(tmp, arr)
   end subroutine grow_2d_logical

   !> Reduce an allocatable array to the given size
   subroutine shrink_logical(arr, length)
      !> The size of the array
      integer, intent(in) :: length
      !> The array to shrink
      logical, allocatable, dimension(:), intent(inout) :: arr
      !> A temp array
      logical, allocatable, dimension(:) :: tmp

      allocate(tmp(length))
      tmp(1:length) = arr(1:length)
      deallocate(arr)
      call move_alloc(tmp, arr)
      
   end subroutine shrink_logical
   
   !> Double the size of a 2d allocatable array
   subroutine shrink_2d_logical(arr, dim, length)
      !> The dimension to grow along
      integer, intent(in) :: dim
      !> The size of the array
      integer, intent(in) :: length
      !> The array to grow
      logical, allocatable, dimension(:,:), intent(inout) :: arr
      !> A temp array
      logical, allocatable, dimension(:,:) :: tmp
      !> The size of the array
      integer, dimension(2) :: arr_shape

      arr_shape = shape(arr)
      arr_shape(dim) = length
      allocate(tmp(arr_shape(1),arr_shape(2)))
      tmp(1:arr_shape(1),1:arr_shape(2)) = arr(1:arr_shape(1),1:arr_shape(2))
      deallocate(arr)
      call move_alloc(tmp, arr)

   end subroutine shrink_2d_logical

   !> Double the size of an allocatable array
   subroutine grow_character(arr)
      !> The array to grow
      character(len=*), allocatable, dimension(:), intent(inout) :: arr
      !> A temp array
      character(len=len(arr(1))), allocatable, dimension(:) :: tmp
      !> The size of the array
      integer :: length

      length = size(arr)
      allocate(tmp(length*2))
      tmp(1:length) = arr(1:length)
      deallocate(arr)
      call move_alloc(tmp, arr)
   end subroutine grow_character

   !> Double the size of a 2d allocatable array
   subroutine grow_2d_character(arr, dim)
      !> The dimension to grow along
      integer, intent(in) :: dim
      !> The array to grow
      character(len=*), allocatable, dimension(:,:), intent(inout) :: arr
      !> A temp array
      character(len=len(arr(1,1))), allocatable, dimension(:,:) :: tmp
      !> The size of the array
      integer, dimension(2) :: length

      length = shape(arr)
      length(dim) = length(dim)*2
      allocate(tmp(length(1),length(2)))
      length(dim) = length(dim)/2
      tmp(1:length(1),1:length(2)) = arr(1:length(1),1:length(2))
      deallocate(arr)
      call move_alloc(tmp, arr)
   end subroutine grow_2d_character

   !> Reduce an allocatable array to the given size
   subroutine shrink_character(arr, length)
      !> The size of the array
      integer, intent(in) :: length
      !> The array to shrink
      character(len=*), allocatable, dimension(:), intent(inout) :: arr
      !> A temp array
      character(len=len(arr(1))), allocatable, dimension(:) :: tmp

      allocate(tmp(length))
      tmp(1:length) = arr(1:length)
      deallocate(arr)
      call move_alloc(tmp, arr)
      
   end subroutine shrink_character
   
   !> Double the size of a 2d allocatable array
   subroutine shrink_2d_character(arr, dim, length)
      !> The dimension to grow along
      integer, intent(in) :: dim
      !> The size of the array
      integer, intent(in) :: length
      !> The array to grow
      character(len=*), allocatable, dimension(:,:), intent(inout) :: arr
      !> A temp array
      character(len=len(arr(1,1))), allocatable, dimension(:,:) :: tmp
      !> The size of the array
      integer, dimension(2) :: arr_shape

      arr_shape = shape(arr)
      arr_shape(dim) = length
      allocate(tmp(arr_shape(1),arr_shape(2)))
      tmp(1:arr_shape(1),1:arr_shape(2)) = arr(1:arr_shape(1),1:arr_shape(2))
      deallocate(arr)
      call move_alloc(tmp, arr)

   end subroutine shrink_2d_character

end module dynamic_array
