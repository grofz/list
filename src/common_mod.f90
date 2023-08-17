module common_mod
  !*
  !
  !
  use iso_fortran_env, only : int64
  implicit none
  private

  integer, parameter, public :: DATA_KIND=int64
  !!  Kind of integer array to store node data

  integer(DATA_KIND), public :: mold(1)
  !* This variable can be used as _mold_ argument in `transfer` function
  !  to cast the user type variable to the type accepted in argument of
  !  `dllnode_*` subroutines and functions

  abstract interface
    function compare_fun(adat, bdat) result(ires)
      !* An user function to compare value of two nodes and return:
      !
      ! * -1 if A is less than B;
      !
      ! *  0 if A equals B;
      !
      ! * +1 if A is greater than B
      import :: DATA_KIND, mold
      implicit none
      integer(DATA_KIND), dimension(:), intent(in) :: adat, bdat
      integer :: ires
    end function
  end interface
  public compare_fun

end module common_mod