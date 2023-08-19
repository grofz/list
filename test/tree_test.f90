module tree_test_mod
  use common_mod, only : mold, DATA_KIND
  use rbnode_mod
  implicit none

contains

  subroutine tree_test_join()
    type(rbbasetree_t) :: tree_a, tree_b, tree_ab
    integer :: i, ierr
    integer, parameter :: IMID=4, ISPLIT=4, NTOT=12
    !logical :: key_in_tree
    type(rbnode_t), pointer :: key_in_tree

    do i=2, IMID-1, 2
      call rbnode_insert(tree_a, rbnode_t(transfer(i,mold)), tree_test_basic_comp)
    end do
    print '("Insertion L - Valid? ",L2," black height is ",i0)', &
        tree_a%isvalid(tree_test_basic_comp), tree_a%blackheight()

    do i=NTOT, IMID+1, -2
    !do i=NMID+1, NRIGHT
      call rbnode_insert(tree_b, rbnode_t(transfer(i,mold)), tree_test_basic_comp, ierr)
      if (ierr/=0) print *, 'Insert ierr = ',ierr
    end do
    print '("Insertion R - Valid? ",L2," black height is ",i0)', &
        tree_b%isvalid(tree_test_basic_comp), tree_b%blackheight()

    call dump_graphviz('our_a', tree_a)
    call dump_graphviz('our_b', tree_b)
    print *
    print *, 'Traverse A'
    call traverse(tree_a)
    print *
    print *, 'Traverse B'
    call traverse(tree_b)

    tree_ab%root => join(tree_a%root, transfer(IMID,mold), tree_b%root)
    print '("Join L+R    - Valid? ",L2," black height is ",i0)', &
        tree_ab%isvalid(tree_test_basic_comp), tree_ab%blackheight()

    call dump_graphviz('our_ab', tree_ab)
    print *
    print *, 'Traverse AB'
    call traverse(tree_ab)

    ! Split
    print *
    print *, 'SPLIT'
    nullify(tree_a%root, tree_b%root)
    call split(tree_a%root,key_in_tree,tree_b%root, &
        tree_ab%root, transfer(ISPLIT,mold), tree_test_basic_comp)
    print '("Split: key_in_tree ",L2)', associated(key_in_tree)
    if (associated(key_in_tree)) then
      print *, 'Key =',transfer(rbnode_read(key_in_tree),i)
      call rbnode_free(key_in_tree)
    end if
    print '("Insertion L - Valid? ",L2," black height is ",i0)', &
        tree_a%isvalid(tree_test_basic_comp), tree_a%blackheight()
    print '("Insertion R - Valid? ",L2," black height is ",i0)', &
        tree_b%isvalid(tree_test_basic_comp), tree_b%blackheight()
    call dump_graphviz('our_c', tree_a)
    call dump_graphviz('our_d', tree_b)
    print *
    print *, 'Traverse B'
    call traverse(tree_b)
    print *
    print *, 'Traverse A'
    call traverse(tree_a)

    ! Delete everything
    print *
    print *, 'DELETE'
   !do i=1,NRIGHT
   !  call rbnode_delete(tree_ab, rbnode_find(tree_ab%root, transfer(i,mold), tree_test_basic_comp))
   !end do
    do i=2,min(ISPLIT-1,NTOT),2
      call rbnode_delete(tree_a, rbnode_find(tree_a%root, transfer(i,mold), tree_test_basic_comp))
    end do
    do i=ISPLIT+2-mod(ISPLIT,2), NTOT,2
      call rbnode_delete(tree_b, rbnode_find(tree_b%root, transfer(i,mold), tree_test_basic_comp))
    end do

   !print '("Empty tree    - Valid? ",L2," black height is ",i0)', &
   !    tree_ab%isvalid(tree_test_basic_comp), tree_ab%blackheight()
    print '("Empty tree    - Valid? ",L2," black height is ",i0)', &
        tree_a%isvalid(tree_test_basic_comp), tree_a%blackheight()
    print '("Empty tree    - Valid? ",L2," black height is ",i0)', &
        tree_b%isvalid(tree_test_basic_comp), tree_b%blackheight()
    print *, 'Allocated nodes zero?  =', allocation_counter

  end subroutine tree_test_join

  subroutine tree_test_basic()
    integer, parameter, dimension(*) :: DATA=[10, 5, 7, 8, 9, 11, 12, 13]
    integer, parameter :: NSIZE = 52
   !integer, parameter :: NSIZE = 150000

    type(rbnode_t), pointer :: current, output
    type(rbbasetree_t) :: tree
    integer :: ierr, i, nblacks
    integer, allocatable :: y1(:), y2(:)
    logical :: isvalid

    y1 = get_array(NSIZE)
    y2 = shuffle_array(y1)

    do i=1, size(y2)
      !call rbnode_insert(tree, rbnode_t(transfer(DATA(i),mold)), tree_test_basic_comp, ierr)
      call rbnode_insert(tree, rbnode_t(transfer(y2(i),mold)), tree_test_basic_comp, ierr)
      if (ierr/=0) print *, 'Insert ierr = ',ierr
    end do
    print '("Is tree valid after inserion?",L2)', &
      tree%isvalid(tree_test_basic_comp)
    call dump_graphviz('our_tree', tree)

    ! traversing
    current=>rbnode_leftmost(tree%root)
    do
      if (.not. associated(current)) exit
      write(*,'("[",i0,l2,": ",i0,"]",3x)',advance='no') &
          transfer(rbnode_read(current),i), current%is_node_black(), rbnode_blackheight(current)
      current => current%nextnode()
    end do
    write(*,*)

    ! Deletion
    do i=1, size(y2)
      call rbnode_delete(tree, rbnode_find(tree%root, &
        transfer(y2(i),mold), tree_test_basic_comp) )
      if (mod(i,10000)/=0) cycle
      isvalid = tree%isvalid(tree_test_basic_comp)
      !print *, 'removed ',y2(i), isvalid
      if (.not. isvalid) stop 'tree is not valid'
    end do

    ! Root is
    if (associated(tree%root)) then
      print *, "Root is: ", transfer(rbnode_read(tree%root),i), tree%root%is_node_black()
    else
      print *, "Root is null:"
    end if

    ! Validation
    call rbnode_validate(tree%root, tree_test_basic_comp, isvalid, nblacks)
    print '("Is tree valid ?",L2, " black nodes count = ",i0)',isvalid, nblacks

  end subroutine


  subroutine traverse(t)
    type(rbbasetree_t), intent(in) :: t
    type(rbnode_t), pointer :: current
    integer :: i
    ! traversing
    current=>rbnode_leftmost(t%root)
    do
      if (.not. associated(current)) exit
      write(*,'("[",i0,l2,": ",i0,"]",3x)',advance='no') &
          transfer(rbnode_read(current),i), current%is_node_black(), rbnode_blackheight(current)
      current => current%nextnode()
    end do
    write(*,*)
  end subroutine traverse

  subroutine dump_graphviz(basename, tree)
    character(len=*), intent(in) :: basename
    type(rbbasetree_t), intent(in) :: tree

    character(len=*), parameter :: SUFTXT='gv.txt', SUFPNG='.png'
    integer :: fid, cmdstat, exitstat
    character(len=200) cmdmsg

    if (.not. associated(tree%root)) then
      print *, 'Warning: graphviz dump skiped for empty tree'
      return
    end if

    ! open a temporary file
    open(newunit=fid, file=basename//SUFTXT, status='replace')
    write(fid,'(a,/,a)') 'digraph {','node [fontname="Arial"];'
    call visit_nodes(tree%root, fid)
    write(fid,'(a)') '}'
    flush(fid)
    call execute_command_line('dot -Tpng < '//basename//SUFTXT//' > '//basename//SUFPNG, &
        exitstat=exitstat, cmdstat=cmdstat, cmdmsg=cmdmsg)
    if (exitstat/=0 .or. cmdstat/=0) print *, exitstat, cmdstat, cmdmsg
    close(fid, status='delete')
  end subroutine dump_graphviz


  recursive subroutine visit_nodes(current, fid)
    type(rbnode_t), pointer, intent(in) :: current
    integer, intent(in) :: fid

    type(rbnode_t), pointer :: par, left, right
    character(len=11) bufcur, bufpar

    write(bufcur,'(i11)') transfer(rbnode_read(current),fid)
    if (.not. current%is_node_black()) then
      write(fid,'(a)') trim(adjustl(bufcur))//' [color=red]'
    end if

    par => current%upnode()
    if (associated(par)) then
      write(bufpar,'(i11)') transfer(rbnode_read(par),fid)
      write(fid,'(a)') trim(adjustl(bufpar))//' -> '//trim(adjustl(bufcur))
    end if

    left => current%leftnode()
    right => current%rightnode()
    if (associated(left)) call visit_nodes(left, fid)
    if (associated(right)) call visit_nodes(right, fid)
  end subroutine visit_nodes


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
