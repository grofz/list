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
  !  `dllnode_*` or `rbnode_*` subroutines and functions

  abstract interface
    function compare_fun(adat, bdat) result(ires)
      !* An user function to compare value of two nodes and return:
      !
      ! * -1 if A is less than B;
      !
      ! *  0 if A equals B;
      !
      ! * +1 if A is greater than B
      import :: DATA_KIND
      implicit none
      integer(DATA_KIND), dimension(:), intent(in) :: adat, bdat
      integer :: ires
    end function

    function get_node_label_fun(dat) result(label)
      !* An user function to provide a string used as a node label. It is
      !  used in `graohviz`, for example.
      import DATA_KIND
      implicit none
      integer(DATA_KIND), intent(in) :: dat(:)
      character(len=:), allocatable :: label
    end function get_node_label_fun

    function select_fun(dat) result(is_selected)
      import DATA_KIND
      implicit none
      integer(DATA_KIND), intent(in) :: dat(:)
      logical :: is_selected
    end function select_fun
  end interface
  public compare_fun, get_node_label_fun, select_fun

end module common_mod
