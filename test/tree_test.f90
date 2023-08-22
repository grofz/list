module tree_test_mod
  use common_mod, only : mold, DATA_KIND
  use rbnode_mod
  use quicksort_module
  use mergesort_module
  implicit none

  integer, parameter :: FLAG_DUPLICATE=1, FLAG_OK=0, FLAG_DUPLICATE_FREED=2
  integer, parameter :: FLAG_NOTFOUND=1, FLAG_JUSTREMOVED=-1

contains

  subroutine tree_test_union(copy_before_operation)
    !integer, parameter :: NMAX=15000000, N1=10000000, N2=4000000
    !integer, parameter :: NMAX=100, N1=20, N2=35
    integer, parameter :: NMAX=2000000, N1=800000, N2=350000
    logical, intent(in) :: copy_before_operation

    integer, allocatable :: x1(:), x2(:), x12(:)
    type(rbbasetree_t) :: t1, t2, t12
    integer :: i, j, last
    logical :: passed 
    real :: time(2)

    print '("------------------")'
    print '("Running Union test       total size ",i0,"k")', (N1+N2)/1000
    print '("------------------")'

    ! Prepare input trees
    passed = .true.
    x1 = get_array(NMAX)
    x2 = get_array(NMAX)
    x1 = shuffle_array(x1)
    x2 = shuffle_array(x2)
    call start_stopwatch('inserting nodes',time)
    call build_tree_via_insert(t1, x1(1:N1), passed, to_validate=.true.)
    call build_tree_via_insert(t2, x2(1:N2), passed, to_validate=.true.)
    call end_stopwatch(time)

    !call display_tree_information(t1, 'T1', passed)
    !call display_tree_information(t2, 'T2', passed)

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
    if (copy_before_operation) then
      call start_stopwatch('calling union with copy',time)
      t12%root => union(rbnode_t(t1%root), rbnode_t(t2%root), compare_integers)
    else
      ! original trees will be destroyed during operation
      call start_stopwatch('calling union',time)
      t12%root => union(t1%root, t2%root, compare_integers)
    end if
    call end_stopwatch(time)

    call display_tree_information(t12, 'T1_union_T2', passed)

    ! Now compare nodes in T12 with x12
    call start_stopwatch('traversing nodes in union tree',time)
    passed = passed .and. verify_nodes_order(t12, x12(1:j))
    call end_stopwatch(time)

    call start_stopwatch('deleting nodes from union tree',time)
    x12(1:j) = shuffle_array(x12(1:j))
    passed = passed .and. verify_nodes_and_delete(t12, x12(1:j))
    call end_stopwatch(time)

    if (copy_before_operation) then
      ! Verify source trees are still traversable
      passed = passed .and. t1%isvalid(compare_integers) .and. t2%isvalid(compare_integers)
      x1(1:N1) = shuffle_array(x1(1:N1))
      x2(1:N2) = shuffle_array(x2(1:N2))
      call start_stopwatch('deleting nodes from original trees',time)
      passed = passed .and. verify_nodes_and_delete(t1, x1(1:N1))
      passed = passed .and. verify_nodes_and_delete(t2, x2(1:N2))
      call end_stopwatch(time)
    end if

    ! Assert no leaking memory
    passed = passed .and. .not. associated(t12%root) .and. allocation_counter==0

    if (passed) then
      print '("Union test: PASS")'
    else
      print '("Union test: FAIL")'
    end if
  end subroutine tree_test_union


  subroutine tree_test_joinsplit()
    type(rbbasetree_t) :: t1, t2, t12, k
    type(rbnode_t), pointer :: cursor1, cursor2, cursork
    integer, allocatable :: x(:)
    integer :: i, isplit
    logical :: passed, rejoined
    real :: time(2), xr
    integer, parameter :: N=9000000

    print '("-----------------------")'
    print '("Running Join/Split test       size ",i0,"k")', (N)/1000
    print '("-----------------------")'

    ! Prepare the tree
    passed = .true.
    x = get_array(2*N)
    x = shuffle_array(x)
    call start_stopwatch('inserting nodes',time)
    call build_tree_via_insert(t12, x(1:N), passed, to_validate=.true.)
    call end_stopwatch(time)

    call quicksort(x(1:N))

    ! Split the tree
    call random_number(xr)
    isplit = int((2*N*xr)+1)
    call start_stopwatch('spliting tree',time)
    call split(t1%root, k%root, t2%root, t12%root, transfer(isplit,mold), compare_integers)
    call end_stopwatch(time)

    call display_tree_information(t1, 'T_left', passed)
    call display_tree_information(t2, 'T_right', passed)
    call display_tree_information(k, 'K_node', passed)

    ! verify split did not loose any nodes
    call start_stopwatch('verifying splits',time)
    cursor1 => t1%leftmost()
    cursor2 => t2%leftmost()
    cursork => k%leftmost()
    do i=1, N
      if (x(i)<isplit) then
        if (associated(cursor1)) then
          passed = passed .and. transfer(rbnode_read(cursor1),1)==x(i)
          cursor1 => rbnode_successor(cursor1)
          cycle
        end if
      else if (x(i)>isplit) then
        if (associated(cursor2)) then
          passed = passed .and. transfer(rbnode_read(cursor2),1)==x(i)
          cursor2 => rbnode_successor(cursor2)
          cycle
        end if
      else ! x(i)==isplit
        if (associated(cursork)) then
          passed = passed .and. transfer(rbnode_read(cursork),1)==x(i)
          cursork => rbnode_successor(cursork)
          cycle
        end if
      end if
      passed = .false.
    end do
    ! all cursors must be byond the last node
    passed = passed .and. (.not. associated(cursor1)) .and. (.not. associated(cursor2)) .and. (.not. associated(cursork))
    call end_stopwatch(time)

    ! JOIN trees again (if split was not in tree, we skip this part)
    if (k%size()>0) then
      call start_stopwatch('re-joining trees',time)
      t12%root => join(t1%root, k%root, t2%root)
      call end_stopwatch(time)

      call display_tree_information(t12, 're-joined', passed)

      ! check that all nodes are present in the joined tree
      call start_stopwatch('verifying re-join',time)
      cursor1 => t12%leftmost()
      do i=1, N
        if (associated(cursor1)) then
          passed = passed .and. transfer(rbnode_read(cursor1),1)==x(i)
          cursor1 => rbnode_successor(cursor1)
          cycle
        end if
        passed = .false.
      end do
      ! cursors must be byond the last node
      passed = passed .and. (.not. associated(cursor1))
      call end_stopwatch(time)
      rejoined = .true.
    else
      print '("join operation skipped, rerun test if needed")'
      rejoined = .false.
    end if

    ! Delete all trees
    call start_stopwatch('deleting nodes', time)
    if (rejoined) then
      call rbnode_freetree(t12%root)
    else
      call rbnode_freetree(t1%root)
      call rbnode_freetree(t2%root)
    end if
    call end_stopwatch(time)

    ! Memory leak check
    passed = passed .and. allocation_counter==0

    if (passed) then
      print '("Join/Split test: PASS")'
    else
      print '("Join/Split test: FAIL")'
    end if
  end subroutine tree_test_joinsplit

  
  subroutine tree_test_basic()
    !! TODO test import/export and copy
  end subroutine tree_test_basic


  subroutine tree_test_playground()
    type(rbbasetree_t) :: ta, tb, tab, k 
    integer, parameter, dimension(*) :: &
      x1 = [20, 30, 40, 50, 60, 70, 80], &
      x2 = [4, 30, 35, 40]
    integer, parameter :: KEY=31, KSPLIT=31
    integer :: i 
    logical :: passed
   !integer, allocatable :: starts(:)
   !integer(DATA_KIND), allocatable :: values(:)
    
    passed = .true.
    call build_tree_via_insert(ta, x1, passed, to_validate=.true.)
    passed = passed .and. verify_nodes_order(ta, x1)
    print *, 'test_order ', passed

    stop

    ! BUILD FROM RAW DATA (if "x" are sorted)
    ta%root=>rbnode_t(int(x1,DATA_KIND), [(i,i=1,size(x1))])
    tb%root=>rbnode_t(int(x2,DATA_KIND), [(i,i=1,size(x2))])

    call ta%graphviz('tree_a',get_node_label)
    call tb%graphviz('tree_b',get_node_label)
    call traverse(ta,'ta')
    call traverse(tb,'tb')
    print '("Black height of ta/tb: ",l2,1x,i0," | ",l2,1x,i0)', &
        ta%isvalid(compare_integers), ta%blackheight(), tb%isvalid(compare_integers), tb%blackheight()
    print '("Allocated nodes counter ",i0)', allocation_counter

    ! SET OPERATIONS
    !tab%root => union2(ta%root, tb%root, compare_integers)
    tab%root => union(rbnode_t(ta%root), rbnode_t(tb%root), compare_integers)
    !t%root => intersection2(tl%root, tr%root, compare_integers)
    call traverse(tab,'tab')
    call tab%graphviz('tree_ab',get_node_label)
    print '("Black height of tab: ",l2,1x,i0)', tab%isvalid(compare_integers), tab%blackheight()

    ! Check originals are intact
   !call ta%graphviz('tree_a',get_node_label)
   !call tb%graphviz('tree_b',get_node_label)
    call traverse(ta,'ta')
    call traverse(tb,'tb')
    print '("Black height of ta/tb: ",l2,1x,i0," | ",l2,1x,i0)', &
        ta%isvalid(compare_integers), ta%blackheight(), tb%isvalid(compare_integers), tb%blackheight()
    print '("Allocated nodes counter ",i0)', allocation_counter

    ! EXPORT
!   values = rbnode_export(tab%root, starts)
!   print *, 'values ='
!   print '(*(i0,1x))', values
!   print *, 'starts ='
!   print '(*(i0,1x))', starts
!   tcopy%root => rbnode_t(values, starts)
!   call traverse(tcopy,'tcopy')
!   print *, tcopy%isvalid(compare_integers)

    ! Delete all trees
    call rbnode_freetree(tab%root)
    call rbnode_freetree(k%root)
    call rbnode_freetree(ta%root)
    call rbnode_freetree(tb%root)
    print '("Allocated nodes counter ",i0)', allocation_counter
  end subroutine tree_test_playground


  ! ==========================================
  ! Test insert / remove / traverse operations
  ! ==========================================

  subroutine display_tree_information(tree, label, passing_pipe)
    type(rbbasetree_t), intent(in) :: tree
    character(len=*), intent(in) :: label
    logical, intent(inout) :: passing_pipe

    800 format(a,": nodes = ",i0,"  black_h = ",i0,"  valid? ",l2)

    print 800, label, tree%size(), tree%blackheight(), tree%isvalid(compare_integers)
    passing_pipe = passing_pipe .and. tree%isvalid(compare_integers)
    call tree%graphviz(label, get_node_label)
    call traverse(tree, label)
  end subroutine display_tree_information


  function run_delete_test(nsize, inloop_validation, seed) result(passed)
    integer, intent(in) :: nsize
    integer, intent(in), optional :: seed
    logical, intent(in) :: inloop_validation
    logical :: passed

    type(rbbasetree_t) :: tree
    type(rbnode_t), pointer :: node
    integer, allocatable :: y(:)
    integer :: ierr, i, nodes

    y = get_array(nsize)
    y = shuffle_array(y, seed)
    nodes = allocation_counter
    passed = .true.
    call build_tree_via_insert(tree, y, passed, to_validate=.true.)

    y = shuffle_array(y)
    ! Step 1 - Find and delete nodes from the tree until it is empty
    do i=1,nsize
      if (.not. passed) exit
      if (mod(i,2)==0) then
        call rbnode_delete(tree, rbnode_find(tree%root, transfer(y(i),mold), compare_integers), ierr=ierr)
      else
        call rbnode_delete(tree, rbnode_find(tree%root, transfer(y(i),mold), compare_integers), deleted_output=node)
        call rbnode_free(node)
        ierr = FLAG_OK
      end if
      if (ierr /= FLAG_OK) passed = .false.
      ! validate tree when it gets sufficiently small or "inloop_validation" is on
      if (inloop_validation .or. nsize-i < 1000) &
          passed = passed .and. tree%isvalid(compare_integers)

      ! try to remove node that does not exists
      call rbnode_delete(tree, rbnode_find(tree%root, transfer(y(i),mold), compare_integers), ierr=ierr)
      if (ierr /= FLAG_NOTFOUND) passed = .false.
    end do
    if (nodes /= allocation_counter) passed = .false.
  end function run_delete_test


  function run_insert_test(nsize, seed) result(passed)
    integer, intent(in) :: nsize
    integer, intent(in), optional :: seed
    logical :: passed

    type(rbbasetree_t) :: tree
    integer, allocatable :: y(:)
    integer :: ierr, i, nodes

    y = get_array(nsize)
    y = shuffle_array(y, seed)

    ! Step 1 - just insert nodes in an empty tree
    passed = .true.
    nodes = allocation_counter
    call build_tree_via_insert(tree, y, passed, to_validate=.true.)
    if (nodes+nsize /= allocation_counter) passed = .false.

    ! Step 2 - try to insert duplicate nodes, assert the insertion fails
    do i=1, size(y)
      if (.not. passed) exit
      call rbnode_insert(tree, rbnode_t(transfer(y(i),mold)), compare_integers, &
          ierr=ierr)
      if (ierr == FLAG_DUPLICATE_FREED) cycle
      ! failing test
      passed = .false.
    end do
    if (nodes+nsize /= allocation_counter) passed = .false.

    call rbnode_freetree(tree%root)
    if (nodes /= allocation_counter) passed = .false.
  end function run_insert_test


  subroutine build_tree_via_insert(tree, y, passing_pipe, to_validate)
    type(rbbasetree_t), intent(inout) :: tree
    integer, intent(in)    :: y(:)
    logical, intent(inout) :: passing_pipe
    logical, intent(in)    :: to_validate

    integer :: i, ierr
    type(rbnode_t), pointer :: node

    do i=1,size(y)
      call rbnode_insert(tree, rbnode_t(transfer(y(i),mold)), compare_integers, &
          ierr=ierr, new_output=node)
      if (ierr==FLAG_OK) cycle
      ! insert error, probably duplicit elements
      passing_pipe = .false.
      if (ierr==FLAG_DUPLICATE) call rbnode_free(node)
    end do

    if (to_validate) passing_pipe = passing_pipe .and. tree%isvalid(compare_integers)
  end subroutine build_tree_via_insert


  function verify_nodes_order(tree, y) result(passed)
    type(rbbasetree_t), intent(in) :: tree
    integer, intent(in) :: y(:)
    logical :: passed

    type(rbnode_t), pointer :: cursor
    integer :: i

    passed = .true.
    cursor => rbnode_leftmost(tree%root)
    do i=1, size(y)
      if (.not. associated(cursor)) exit
      if (transfer(rbnode_read(cursor),1) /= y(i)) passed = .false.
      cursor => rbnode_successor(cursor)
    end do
    if (i/=size(y)+1 .or. associated(cursor)) passed = .false.

    ! traverse in oposite direction
    cursor => rbnode_rightmost(tree%root)
    do i=size(y), 1, -1
      if (.not. associated(cursor)) exit
      if (transfer(rbnode_read(cursor),1) /= y(i)) passed = .false.
      cursor => rbnode_predecessor(cursor)
    end do
    if (i/=0 .or. associated(cursor)) passed = .false.
  end function verify_nodes_order


  function verify_nodes_and_delete(tree, y) result(passed)
    type(rbbasetree_t), intent(inout) :: tree
    integer, intent(in) :: y(:)
    logical :: passed

    type(rbnode_t), pointer :: found
    integer :: i

    passed = .true.
    do i=1, size(y)
      found => rbnode_find(tree%root, transfer(y(i),mold), compare_integers)
      if (associated(found)) then
        call rbnode_delete(tree, found)
        cycle
      end if
      passed = .false.
    end do
  end function verify_nodes_and_delete


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


  integer function compare_integers(a,b) result(comp)
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
  end function compare_integers


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


  function shuffle_array(yin, seed) result(yout)
    integer, intent(in) :: yin(:)
    integer, intent(in), optional :: seed
    integer :: yout(size(yin))

    integer :: i, j, n, ytmp, seed_size
    integer, allocatable :: seed_arr(:)
    real :: xran

    ! set seed to make shuffle deterministic
    if (present(seed)) then
      call random_seed(size=seed_size)
      allocate(seed_arr(seed_size), source=seed)
      call random_seed(put=seed_arr)
    end if

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
          transfer(rbnode_read(current),i), current%isblack_f(), rbnode_blackheight(current)
      current => rbnode_successor(current)
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
