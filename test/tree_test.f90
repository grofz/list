module tree_test_mod
  use common_mod, only : mold, DATA_KIND
  use rbtrnode_mod
  implicit none

contains

  subroutine tree_test_basic()
    type(rbtrnode_t), pointer :: root, current, output
    integer :: ierr, i
    integer, parameter, dimension(*) :: DATA=[10, 3, 4, 20, 1, 3]

    root => null()

    do i=1, size(DATA)
      call rbtrnode_insert(root, &
                           rbtrnode_t(transfer(DATA(i),mold)), &
                           tree_test_basic_comp, ierr)
      print *, 'Insert ierr = ',ierr
    end do

    ! traversing
    current=>rbtrnode_leftmost(root)
    do
      if (.not. associated(current)) exit
      print *, transfer(rbtrnode_read(current),i)
      current => rbtrnode_nextnode(current)
    end do

  end subroutine


  integer function tree_test_basic_comp(a,b) result(comp)
    integer(DATA_KIND), intent(in), dimension(:) :: a, b
    integer :: aval, bval

    aval = transfer(a,aval)
    bval = transfer(b,bval)
    if (aval<bval) then
      comp = -1
    else if (aval>bval) then
      comp = 1
    else
      comp = 0
    end if
  end function tree_test_basic_comp

end module tree_test_mod