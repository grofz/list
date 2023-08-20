module tree_test_mod
  use common_mod, only : mold, DATA_KIND
  use rbnode_mod
  use quicksort_module
  use mergesort_module
  implicit none

contains

  subroutine tree_test_union()
    integer, parameter :: NMAX=2000000, N1=1000000, N2=0500000
    !integer, parameter :: NMAX=100, N1=20, N2=35

    integer, allocatable :: x1(:), x2(:), x12(:)
    type(rbbasetree_t) :: t1, t2, t12
    type(rbnode_t), pointer :: found
    integer :: i, j, last
    logical :: passed 
    real :: time(2)

    800 format(a,": nodes = ",i0,"  black_h = ",i0,"  valid? ",l2)

    print '("------------------")'
    print '("Running Union test       total size ",i0,"k")', (N1+N2)/1000
    print '("------------------")'

    ! Prepare input trees
    x1 = get_array(NMAX)
    x2 = get_array(NMAX)
    x1 = shuffle_array(x1)
    x2 = shuffle_array(x2)
    
    call start_stopwatch('inserting nodes',time)
    do i=1, max(N1, N2) ! Insert nodes
      if (i<=N1) call rbnode_insert(t1, rbnode_t(transfer(x1(i),mold)), tree_test_basic_comp)
      if (i<=N2) call rbnode_insert(t2, rbnode_t(transfer(x2(i),mold)), tree_test_basic_comp)
    end do
    call end_stopwatch(time)

    print 800, 'Tree 1', t1%size(), t1%blackheight(), t1%isvalid(tree_test_basic_comp)
    print 800, 'Tree 2', t2%size(), t2%blackheight(), t2%isvalid(tree_test_basic_comp)
    passed = t1%isvalid(tree_test_basic_comp) .and. t2%isvalid(tree_test_basic_comp)
    print '("Allocated nodes counter ",i0)', allocation_counter

    call t1%graphviz('tree_1', get_node_label)
    call t2%graphviz('tree_2', get_node_label)
    call traverse(t1, '1')
    call traverse(t2, '2')

    ! Make independent union via arrays
    call start_stopwatch('making verification arrays',time)
    call quicksort(x1(1:N1))
    call quicksort(x2(1:N2))
    allocate(x12(N1+N2))
    call merge(x1(1:N1), x2(1:N2), x12)

    j = 0
    last = -huge(last)
    do i=1, size(x12)
      ! skip duplicit elements
      if (x12(i)==last) cycle
      last = x12(i)
      j = j + 1
      if (i/=j) x12(j) = x12(i)
    end do
    call end_stopwatch(time)

    print '("N1 = ",i0,"  N2 = ",i0,"  N12 = ",i0,"  commons = ",i0)', &
        N1, N2, j, N1+N2-j

    ! Union of two red-black trees
    call start_stopwatch('calling union',time)
    t12%root => union(t1%root, t2%root, tree_test_basic_comp)
    call end_stopwatch(time)

    print 800,'Tree 1+2', t12%size(), t12%blackheight(), t12%isvalid(tree_test_basic_comp)
    print '("Allocated nodes counter ",i0)', allocation_counter
    call t12%graphviz('tree_1+2', get_node_label)
    call traverse(t12,'1+2')
    passed = passed .and. t12%isvalid(tree_test_basic_comp)

    ! Now compare nodes in T12 with x12 by trying to remove them all
    call start_stopwatch('deleting nodes',time)
    do i=1,j ! Remove nodes
      found => rbnode_find(t12%root, transfer(x12(i),mold), tree_test_basic_comp)
      if (associated(found)) then
        call rbnode_delete(t12, found)
      else
        passed = .false.
        print *, 'missing item = ',x12(i)
      end if
    end do
    call end_stopwatch(time)

    print 800, 'After deleting', t12%size(), t12%blackheight(), t12%isvalid(tree_test_basic_comp)
    print '("Allocated nodes counter ",i0)', allocation_counter

    passed = passed .and. .not. associated(t12%root) .and. allocation_counter==0
    if (passed) then
      print '("Union test: PASS")'
    else
      print '("Union test: FAIL")'
    end if
  end subroutine tree_test_union


  subroutine tree_test_joinsplit()
    type(rbbasetree_t) :: tree_a, tree_b, tree_ab
    integer :: i, ierr
    integer, parameter :: IMID=4000, ISPLIT=3914080, NTOT=4120000
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

    call tree_a%graphviz('our_a', get_node_label)
    call tree_b%graphviz('our_b', get_node_label)
    call traverse(tree_a,'tree_a')
    call traverse(tree_b,'tree_b')

    tree_ab%root => join(tree_a%root, transfer(IMID,mold), tree_b%root)
    print '("Join L+R    - Valid? ",L2," black height is ",i0)', &
        tree_ab%isvalid(tree_test_basic_comp), tree_ab%blackheight()

    call tree_ab%graphviz('our_ab', get_node_label)
    call traverse(tree_ab,'tree_ab')

    ! Split
    print *
    print *, 'SPLIT'
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
    call tree_a%graphviz('our_c', get_node_label)
    call tree_b%graphviz('our_d', get_node_label)
    call traverse(tree_b,'tree_b')
    call traverse(tree_a,'tree_a')

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

  end subroutine tree_test_joinsplit


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
   !call rbbasetree_graphviz('our_tree', tree)

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
  end subroutine tree_test_basic


  subroutine tree_test_playground()
   !integer, parameter, dimension(*) :: &
   !   DAT1=[10, 1, 5, 13], & ! these gave memory leak union (but should be fixed)
   !   DAT2=[1, 9, 10, 6, 13]
    type(rbbasetree_t) :: t
    integer, parameter, dimension(*) :: DAT = [2, 10, 1, 3, 23, 54, 13]
    integer :: i

    do i=1, size(DAT)
      call rbnode_insert(t, rbnode_t(transfer(DAT(i),mold)), tree_test_basic_comp)
    end do
    print '("Tree size ",i0)', t%size()
    call t%graphviz('tree_play',get_node_label)
  end subroutine tree_test_playground


  ! =====================================
  ! Plug-in user functions for rbnode_mod
  ! =====================================

  function get_node_label(dat) result(label)
    !! Plug-in function for graphviz method of rbbasetree_t
    integer(DATA_KIND), intent(in) :: dat(:)
    character(len=:), allocatable :: label
    allocate(character(len=11) :: label)
    write(label,'(i11)') transfer(dat,1)
  end function get_node_label


  integer function tree_test_basic_comp(a,b) result(comp)
    !! Plug-in function for rbnode_t nodes comparison
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


  ! ====================
  ! Misceleanous helpers
  ! ====================

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


  subroutine traverse(t, label)
    type(rbbasetree_t), intent(in) :: t
    character(len=*), intent(in) :: label
    integer, parameter :: MAX_TREE_SIZE = 400
    type(rbnode_t), pointer :: current
    integer :: i, n

    ! traversing
    if (.not. associated(t%root)) then
      write(*,'(a)') 'Tree "'//label//'": empty tree'
      return
    end if

    n = t%size()
    if (n > MAX_TREE_SIZE) then
      write(*,'(a,i0)') 'Tree "'//label//'": tree of size ',n
      return
    end if

    current=>rbnode_leftmost(t%root)
    write(*,'(a,i0,a)',advance='no') 'Tree "'//label//'" (size=',n,'): '
    do
      if (.not. associated(current)) exit
      write(*,'("[",i0,l2,": ",i0,"]",3x)',advance='no') &
          transfer(rbnode_read(current),i), current%is_node_black(), rbnode_blackheight(current)
      current => current%nextnode()
    end do
    write(*,*)
  end subroutine traverse


  subroutine start_stopwatch(task, time)
    character(len=*), intent(in) :: task
    real, intent(inout) :: time(2)
    call cpu_time(time(1))
    write(*,'(a)',advance='no') task//'...  '
  end subroutine

  subroutine end_stopwatch(time)
    real, intent(inout) :: time(2)
    call cpu_time(time(2))
    print '("time=",f6.3," s.")', time(2)-time(1)
  end subroutine

end module tree_test_mod
