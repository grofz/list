module tree_test_mod
  use common_mod, only : mold, DATA_KIND
  use rbnode_mod
  implicit none

contains

  subroutine tree_test_basic()
    integer, parameter, dimension(*) :: DATA=[10, 5, 7, 8, 9, 11, 12, 13]
    integer, parameter :: NSIZE = 150000

    type(rbnode_t), pointer :: current, output
    type(rbbasetree_t) :: tree
    integer :: ierr, i, nblacks
    integer, allocatable :: y1(:), y2(:)
    logical :: isvalid

    y1 = get_array(NSIZE)
    y2 = shuffle_array(y1)

    do i=1, size(y2)
      !call rbnode_insert(root, &
      !                     rbnode_t(transfer(DATA(i),mold)), &
      !                     tree_test_basic_comp, ierr)
      call rbnode_insert(tree, &
                           rbnode_t(transfer(y2(i),mold)), &
                           tree_test_basic_comp, ierr)
      if (ierr/=0) print *, 'Insert ierr = ',ierr
    end do

    ! traversing
    current=>rbnode_leftmost(tree%root)
    do
      if (.not. associated(current)) exit
      !write(*,'(i0,l2,2x)',advance='no') transfer(rbnode_read(current),i), current%is_node_black()
      current => rbnode_nextnode(current)
    end do
    write(*,*)

    ! Root is
    if (associated(tree%root)) then
      print *, "Root is: ", transfer(rbnode_read(tree%root),i), tree%root%is_node_black()
    else
      print *, "Root is null:"
    end if
    ! Validation
    call rbnode_validate(tree%root, tree_test_basic_comp, isvalid, nblacks)
    print '("Is tree valid ?",L2, " black nodes count = ",i0)',isvalid, nblacks

    print '("Is tree valid ?",L2)', tree%isvalid(tree_test_basic_comp)

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


  pure function get_array(n) result(y)
    integer, allocatable :: y(:)
    integer, intent(in) :: n
    integer :: i
    allocate(y(n))
    y = [(i, i=1,n)]
  end function get_array


  function shuffle_array(yin) result(yout)
    integer, intent(in) :: yin(:)
    integer :: yout(size(yin))

    integer :: i, j, n, ytmp
    real :: xran

    yout = yin
    n = size(yin)
    do i=1, n-1
      call random_number(xran)
      ! j is in the range 
      ! * i==1: 1...n, i==2: 2..n, i==3: 3..n, i==n-1: n-1..n
      j = i-1 + int(xran*(n-i+1))+1
      ytmp = yout(j)
      yout(j) = yout(i)
      yout(i) = ytmp
    end do
  end function shuffle_array

end module tree_test_mod
