module user_mod
  !! ## Example of using several data types in a list
  !!
  !! Contains definition of user-types and wrapper functions
  use dll_mod
  use dllnode_mod, only : dllnode_t, dllnode_read
  use common_mod, only : mold, DATA_KIND
  implicit none

  integer, parameter :: MAXDIM=4
  type :: testone_t
    integer :: a(MAXDIM)
  end type testone_t

  type :: testtwo_t
    type(testone_t), pointer :: ptr
  end type testtwo_t

  interface win
    !! Wrapper to transform the actual value to an array of integers
    module procedure one2dat
    module procedure two2dat
    module procedure int2dat
    module procedure real2dat
  end interface

contains

  pure function one2dat(one) result(dat)
    type(testone_t), intent(in) :: one
    integer(DATA_KIND), allocatable :: dat(:)
    dat = transfer(one,mold)
  end function

  pure function two2dat(one) result(dat)
    type(testtwo_t), intent(in) :: one
    integer(DATA_KIND), allocatable :: dat(:)
    dat = transfer(one,mold)
  end function

  pure function int2dat(one) result(dat)
    integer, intent(in) :: one
    integer(DATA_KIND), allocatable :: dat(:)
    dat = transfer(one,mold)
  end function

  pure function real2dat(one) result(dat)
    real, intent(in) :: one
    integer(DATA_KIND), allocatable :: dat(:)
    dat = transfer(one,mold)
  end function


  pure function dat2one(dat) result(one)
    !! Wrapper to transform array of integers back to user type
    integer(DATA_KIND), intent(in) :: dat(:)
    type(testone_t) :: one
    one = transfer(dat,one)
  end function

  pure function dat2two(dat) result(one)
    !! Wrapper to transform array of integers back to user type
    integer(DATA_KIND), intent(in) :: dat(:)
    type(testtwo_t) :: one
    one = transfer(dat,one)
  end function

  pure function dat2int(dat) result(one)
    !! Wrapper to transform array of integers back to its type
    integer(DATA_KIND), intent(in) :: dat(:)
    integer :: one
    one = transfer(dat,one)
  end function

  pure function dat2real(dat) result(one)
    !! Wrapper to transform array of integers back to its type
    integer(DATA_KIND), intent(in) :: dat(:)
    real :: one
    one = transfer(dat,one)
  end function

end module user_mod
