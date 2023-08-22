module rbnode_mod
  !* Red-black tree implementation
  !  Algorithms based on this Wikipedia articles:
  !  - (https://en.wikipedia.org/wiki/Red%E2%80%93black_tree)
  !  - (https://en.wikipedia.org/wiki/Join-based_tree_algorithms)
  !
  use common_mod, only : DATA_KIND, mold, compare_fun, get_node_label_fun
  use iso_fortran_env, only : int8
  implicit none
  private

  type, public :: rbnode_t
    !! Red-black tree node
    private
    integer(kind=DATA_KIND), allocatable :: dat(:)
    logical(int8) :: isblack = .false.
    type(rbnode_t), pointer :: left => null()
    type(rbnode_t), pointer :: right => null()
    type(rbnode_t), pointer :: parent => null()
  contains
    procedure :: isblack_f
    procedure :: leftnode, rightnode, upnode
   !procedure :: nextnode => nextnode_tbp
  end type rbnode_t
  interface rbnode_t
    !* Signatures recognized by the constructor
    !
    !   * rank-1 array: to allocate a single node
    !
    !   * rank-1 array + meta-data: to build a tree from an array
    !
    !   * pointer to a node: to deep copy the sub-tree rooted by the node
    module procedure rbnode_new
    module procedure rbnode_importsorted
    module procedure rbnode_copytree
  end interface


  type, public :: rbbasetree_t
    type(rbnode_t), pointer :: root => null()
  contains
    procedure :: isvalid => rbbasetree_isvalid
    procedure :: blackheight => rbbasetree_blackheight
    procedure :: size => rbbasetree_size
    procedure :: graphviz => rbbasetree_graphviz
    procedure :: leftmost => rbbasetree_leftmost
  end type rbbasetree_t


  integer, parameter :: LEFT_CHILD=1, RIGHT_CHILD=2, NO_PARENT=0
  logical(int8), parameter :: RED_COLOUR=.false., BLACK_COLOUR=.true.

  integer, parameter :: MAX_SAFE_DEPTH=1000000
    !! Defensive test to avoid infinite loops in the code

  interface join
    !! Join operation with or without the middle node
    module procedure join_3
    module procedure join_2
  end interface join

  public rbnode_update, rbnode_read, rbnode_free
  public rbnode_find
  public rbnode_leftmost, rbnode_rightmost, rbnode_successor, rbnode_predecessor
  public rbnode_insert, rbnode_delete, rbnode_freetree
  public rbnode_validate, rbnode_blackheight
  public rbnode_export
  public join, split, union, intersection, difference

  integer, public, save :: allocation_counter = 0
    !! temporary, just for mem.leakage debuging

contains

  ! =======================
  ! Simple rbnode_t getters
  ! =======================
  logical function isblack_f(this)
    class(rbnode_t), intent(in) :: this
    isblack_f = this%isblack
  end function

  function leftnode(this) result(ln)
    class(rbnode_t), intent(in) :: this
    type(rbnode_t), pointer :: ln
    ln => this%left
  end function

  function rightnode(this) result(rn)
    class(rbnode_t), intent(in) :: this
    type(rbnode_t), pointer :: rn
    rn => this%right
  end function

  function upnode(this) result(un)
    class(rbnode_t), intent(in) :: this
    type(rbnode_t), pointer :: un
    un => this%parent
  end function


  ! =======================
  ! Simple rbbasetree_t ...
  ! =======================
  function rbbasetree_leftmost(this) result(ln)
    class(rbbasetree_t), intent(in) :: this
    type(rbnode_t), pointer :: ln
    ln => null()
    if (associated(this%root)) ln => rbnode_leftmost(this%root)
  end function


  ! ================================
  ! Allocate new node
  ! Update node data (Future work - isolated nodes only?)
  ! Read data from node
  ! Free node
  ! ================================

  function rbnode_new(dat) result(new)
    !! Allocate new node, fill it with data
    integer(DATA_KIND), intent(in) :: dat(:)
    type(rbnode_t), pointer :: new

    integer :: ierr

allocation_counter=allocation_counter+1
    allocate(new, stat=ierr)
    if (ierr /= 0) &
        error stop 'could not allocate new rbtr node'
    allocate(new%dat(size(dat)), stat=ierr)
    if (ierr /= 0) &
        error stop 'could not allocate data in new rbtr node'
    new%dat = dat
    new%left => null()
    new%right => null()
    new%parent => null()
  end function rbnode_new


  subroutine rbnode_update(node, newdata, update_fail)
    !! Update the data content of the node by newdata
    !! Node must be isolated
    type(rbnode_t), intent(in), pointer :: node
    integer(DATA_KIND), intent(in) :: newdata(:)
    logical, intent(out), optional :: update_fail

    integer :: ierr

    if (.not. associated(node)) &
        error stop 'could not update data: node is null'
    ! to be on the safe side, we do not allow modifying node in tree
    if (associated(node%left) .or. associated(node%right) .or. associated(node%parent)) &
        error stop 'update: works with isolated nodes only'
! TODO - check successor/predecessor and allow update to proceed
! only if the order is not affected by the change (future work?)

    if (allocated(node%dat)) then
      ierr = 0
      if (size(newdata) /= size(node%dat)) deallocate(node%dat, stat=ierr)
      if (ierr /= 0) &
          error stop 'could not deallocate data during update'
    end if
    ierr = 0
    if (.not. allocated(node%dat)) allocate(node%dat(size(newdata)), stat=ierr)
    if (ierr /= 0) &
        error stop 'could not allocate data during update'
    node%dat = newdata
  end subroutine rbnode_update


  function rbnode_read(node) result(dat)
    !! Return node data
    type(rbnode_t), intent(in), pointer :: node
    integer(DATA_KIND), allocatable :: dat(:)
    if (.not. associated(node)) &
        error stop 'could not read: node is null'
    if (.not. allocated(node%dat)) &
        error stop 'could not read: node contains no data'
    allocate(dat(size(node%dat)))
    dat = node%dat
  end function rbnode_read


  subroutine rbnode_free(deleted)
    !! Deallocae the node from memory
    type(rbnode_t), pointer, intent(inout) :: deleted

    integer :: ierr

    if (.not. associated(deleted)) &
        error stop 'could not free node: null pointer'
    ierr = 0
    if (allocated(deleted%dat)) deallocate(deleted%dat, stat=ierr)
    if (ierr /= 0) &
        error stop 'could not free node: data deallocation failed'
    deallocate(deleted, stat=ierr)
    if (ierr /= 0) &
        error stop 'could not free node: node deallocation failed'
allocation_counter=allocation_counter-1
  end subroutine rbnode_free


  ! =================================================
  ! Get related nodes (sibling, uncle, grandparent)
  ! Move within the tree, in-order traversal
  ! Tree rotations
  ! =================================================

  function rbnode_whichchild(node) result(we)
    type(rbnode_t), intent(in), pointer :: node
    integer :: we

    if (.not. associated(node)) &
        error stop 'whichchild: null pointer as input'

    if (associated(node%parent)) then
      if (associated(node, node%parent%left)) then
        we = LEFT_CHILD
      else if (associated(node, node%parent%right)) then
        we = RIGHT_CHILD
      else
        error stop 'whichchild: parent not related to node!'
      end if
    else
      we = NO_PARENT
    end if
  end function rbnode_whichchild


  function rbnode_isblack(node) result(isblack)
    !! For the convenience, we want be able query nil nodes as well
    type(rbnode_t), pointer, intent(in) :: node
    logical :: isblack ! leaves are assumed black
    isblack = .true.
    if (associated(node)) isblack = node%isblack
  end function rbnode_isblack


  function rbnode_grandparent(node) result(grandparent)
    type(rbnode_t), intent(in), pointer :: node
    type(rbnode_t), pointer :: grandparent

    if (.not. associated(node)) &
        error stop 'grandparent: null node as input'

    if (associated(node%parent)) then
      grandparent => node%parent%parent
    else
      grandparent => null()
    end if
  end function rbnode_grandparent


  function rbnode_sibling(node, node_is_which) result(sibling)
    type(rbnode_t), intent(in), pointer :: node
    integer, intent(out), optional :: node_is_which
    type(rbnode_t), pointer :: sibling

    if (.not. associated(node)) &
        error stop 'sibling: null node as input'

    associate (we => rbnode_whichchild(node))
      select case(we)
      case(LEFT_CHILD)
        sibling => node%parent%right
      case(RIGHT_CHILD)
        sibling => node%parent%left
      case(NO_PARENT)
        sibling => null()
      case default
        error stop 'sibling: invalid output from which_child'
      end select
      if (present(node_is_which)) node_is_which = we
    end associate
  end function rbnode_sibling


  function rbnode_uncle(node, parent_is_which) result(uncle)
    type(rbnode_t), intent(in), pointer :: node
    integer, intent(out), optional :: parent_is_which
    type(rbnode_t), pointer :: uncle

    if (.not. associated(node)) &
        error stop 'uncle: null node as input'

    if (associated(node%parent)) then
      uncle => rbnode_sibling(node%parent, parent_is_which)
    else
      uncle => null()
      if (present(parent_is_which)) parent_is_which=NO_PARENT
    end if
  end function rbnode_uncle


  subroutine fix_link_at_parent(oldchild, newchild, tree)
    ! Old child (pivot) exchanged place with new child (rotator) during the
    ! rotation. Here, we repair the "parent->child" link.
    ! The other way link ("child->parent") should have been updated in
    ! rotate_left/right already.
    type(rbnode_t), pointer, intent(in) :: oldchild, newchild
    type(rbbasetree_t), intent(inout) :: tree

    type(rbnode_t), pointer :: parent

    parent => newchild%parent

    if (associated(parent)) then
      ! Assert old child was really one of parents children
      if (associated(parent%left, oldchild)) then
        parent%left => newchild
      else if (associated(parent%right, oldchild)) then
        parent%right => newchild
      else
        error stop 'fix_link_at_parent - old child not recognized by parent'
      end if
    else
      ! Pivot was root, and did not have any parent. Root-pointer in tree 
      ! structure must be repaired instead
      if (.not. associated(tree%root, oldchild)) &
          error stop 'fix_link_at_parent - unexpected association of root pointer'
      tree%root => newchild
    end if
  end subroutine fix_link_at_parent


  function rotate_left(piv, tree) result(rot)
    !! Rotate left.
    !! Fails if pivot is leaf or if pivot's right child (rotator) is leaf
    type(rbnode_t), intent(in), pointer :: piv
    type(rbbasetree_t), intent(inout) :: tree
    type(rbnode_t), pointer :: rot

    if (.not. associated(piv)) &
        error stop 'rotate_left: pivot is null'
    rot => piv%right
    if (.not. associated(rot)) &
        error stop 'rotate_left: rotator is null'

    ! rotator is the right child of pivot, it will become the new
    ! root of the sub-tree after the rotation
    if (.not. associated(rot%parent,piv)) &
        error stop 'rotate_left: rotator is not linked to pivot'
    rot%parent => piv%parent
    piv%parent => rot

    ! pivot becomes rotator's new left child
    ! rotator's old left child node transfered as the pivot's right child
    piv%right => rot%left
    if (associated(piv%right)) then
      if (.not. associated(piv%right%parent,rot)) &
          error stop 'rotate_left: rotator''s left child not linked to rotator'
      piv%right%parent => piv
    end if
    rot%left => piv

    call fix_link_at_parent(piv, rot, tree)
  end function rotate_left


  function rotate_right(piv, tree) result(rot)
    !! Rotate right.
    !! Fails if pivot is leaf or if pivot's left child (rotator) is leaf
    type(rbnode_t), intent(in), pointer :: piv
    type(rbbasetree_t), intent(inout) :: tree
    type(rbnode_t), pointer :: rot

    if (.not. associated(piv)) &
        error stop 'rotate_right: pivot is null'
    rot => piv%left
    if (.not. associated(rot)) &
        error stop 'rotate_right: rotator is null'

    ! rotator is the left child of pivot, it will become the new
    ! root of the sub-tree after the rotation
    if (.not. associated(rot%parent,piv)) &
        error stop 'rotate_right: rotator is not linked to pivot'
    rot%parent => piv%parent
    piv%parent => rot

    ! pivot becomes rotator's new right child
    ! rotator's old right child node transfered as the pivot's left child
    piv%left => rot%right
    if (associated(piv%left)) then
      if (.not. associated(piv%left%parent,rot)) &
          error stop 'rotate_right: rotator''s right child not linked to rotator'
      piv%left%parent => piv
    end if
    rot%right => piv

    call fix_link_at_parent(piv, rot, tree)
  end function rotate_right


  function rbnode_leftmost(node) result(leftmost)
    !! Get to the left-most node in current sub-tree
    type(rbnode_t), intent(in), pointer :: node
    type(rbnode_t), pointer :: leftmost

    integer :: i

    if (.not. associated(node)) &
        error stop 'leftmost: null node as input'

    leftmost => node
    do i=1,MAX_SAFE_DEPTH
      if (.not. associated(leftmost%left)) exit
      leftmost => leftmost%left
    end do

    if (i==MAX_SAFE_DEPTH+1) &
      error stop 'leftmost: MAX_SAFE_DEPTH reached, increase it if this is not error'
  end function rbnode_leftmost


  function rbnode_rightmost(node) result(rightmost)
    !! Get to the right-most node in current sub-tree
    type(rbnode_t), intent(in), pointer :: node
    type(rbnode_t), pointer :: rightmost

    integer :: i

    if (.not. associated(node)) &
        error stop 'rightmost: null node as input'

    rightmost => node
    do i=1,MAX_SAFE_DEPTH
      if (.not. associated(rightmost%right)) exit
      rightmost => rightmost%right
    end do

    if (i==MAX_SAFE_DEPTH+1) &
      error stop 'rightmost: MAX_SAFE_DEPTH reached, increase it if this is not error'
  end function rbnode_rightmost


  function rbnode_successor(node) result(succ)
    !! Return next node, or null pointer if the node is the last node
    type(rbnode_t), intent(in), pointer :: node
    type(rbnode_t), pointer :: succ

    type(rbnode_t), pointer :: child
    integer :: i

    if (.not. associated(node)) &
        error stop 'successor: input is a null node'

    ! If node has a right sub-tree, the next node is the leftmost node in this
    ! sub-tree
    if (associated(node%right)) then
      succ => rbnode_leftmost(node%right)
      return
    end if

    ! Otherwise, the next node is the first parent of a left child that is
    ! encountered when traversing up the tree. If no such parent exists, it
    ! means that the node is the last node
    child => node
    do i=1,MAX_SAFE_DEPTH
      succ => child%parent

      ! no more parents, return null
      if (.not. associated(succ)) exit

      select case(rbnode_whichchild(child))
      case(LEFT_CHILD)
        ! parent of a left child is the next node and is returned
        exit
      case(RIGHT_CHILD)
        ! parent of a right child is not the next node, but its
        ! grand-parent could be...
        child => child%parent
      case default
        error stop 'successor: node has no parent and this is not expected'
      end select
    end do

    if (i==MAX_SAFE_DEPTH+1) &
      error stop 'successor: MAX_SAFE_DEPTH reached, increase it if this is not error'
  end function rbnode_successor


  function rbnode_predecessor(node) result(pred)
    !! Return prev node, or null pointer if the node is the first node
    type(rbnode_t), intent(in), pointer :: node
    type(rbnode_t), pointer :: pred

    type(rbnode_t), pointer :: child
    integer :: i

    if (.not. associated(node)) &
        error stop 'prevnode: input is a null node'

    ! If node has a left sub-tree, the next node is the rightmost node in this
    ! sub-tree
    if (associated(node%left)) then
      pred => rbnode_rightmost(node%left)
      return
    end if

    ! Otherwise, the prev node is the first parent of a right child that is
    ! encountered when traversing up the tree. If no such parent exists, it
    ! means that the node is the first node
    child => node
    do i=1,MAX_SAFE_DEPTH
      pred => child%parent

      ! no more parents, return null
      if (.not. associated(pred)) exit

      select case(rbnode_whichchild(child))
      case(RIGHT_CHILD)
        ! parent of a right child is the prev node and is returned
        exit
      case(LEFT_CHILD)
        ! parent of a left child is not the prev node, but its
        ! grand-parent could be...
        child => child%parent
      case default
        error stop 'prevnode: node has no parent and this is not expected'
      end select
    end do

    if (i==MAX_SAFE_DEPTH+1) &
      error stop 'predecessor: MAX_SAFE_DEPTH reached, increase it if this is not error'
  end function rbnode_predecessor


  function rbnode_find(start, value, cfun) result(found)
    !! Find a node with the specified value or return null
    type(rbnode_t), intent(in), pointer :: start
    integer(DATA_KIND), intent(in) :: value(:)
    procedure(compare_fun) :: cfun
    type(rbnode_t), pointer :: found

    type(rbnode_t), pointer :: finger
    integer :: i

    found => null()
    finger => start
    do i=1, MAX_SAFE_DEPTH
      if (.not. associated(finger)) exit
      select case(cfun(value, finger%dat))
      case(-1) ! val < finger
        finger => finger%left
      case(+1) ! val > finger
        finger => finger%right
      case(0)  ! val == finger
        found => finger
        exit
      case default
        error stop 'find: invalid value returned from user function'
      end select
    end do
    if (i==MAX_SAFE_DEPTH+1) &
        error stop 'find: MAX_SAFE_DEPTH reached, incr. it if this is not error'
  end function rbnode_find


  ! =========
  ! Insertion
  ! =========

  subroutine rbnode_insert(tree, new, cfun, ierr, new_output)
    !* Insert a new node to the tree.
    !  Optional pointer `new_output` points to the inserted node
    !  and `ierr=0` if insertion was sucessful.
    !
    !  If a duplicate is in the tree, there are three posibililties the
    !  error is handled:
    !  - Error stop if `ierr` was not provided
    !  - New node is freed here if `output` was not provided, `ierr=2` returned.
    !  - User is responsible for freeing node via `output`, `ierr=1` returned.
    !
    type(rbbasetree_t), intent(inout) :: tree
    type(rbnode_t), intent(in), pointer :: new
    procedure(compare_fun) :: cfun
      !! A < B -> -1, A == B -> 0, A > B -> 1
    integer, intent(out), optional :: ierr
      !! on exit: 0...insertion ok
      !!          1...duplicate in tree, node not inserted
      !!          2...duplicate in tree, node was deallocated automatically
    type(rbnode_t), pointer, optional :: new_output
      !! pointer to the new node so it can be deallocated by user, if inserion
      !! failed
    integer, parameter :: FLAG_OK=0, FLAG_DUPLICATE=1, FLAG_DUPLICATE_FREED=2

    integer :: i, ierr0, which_child
    type(rbnode_t), pointer :: new_local, finger

    ! assert the new node is isolated
    if (.not. associated(new)) &
        error stop 'insert: new node is null node'
    if (associated(new%parent) .or. associated(new%left) .or. associated(new%right))&
        error stop 'insert: new node must be alone'

    new_local => new
    if (present(new_output)) new_output => new
    ierr0 = FLAG_OK

    if (.not. associated(tree%root)) then
      ! insert a first node to an empty tree
      tree%root => new_local
      which_child = NO_PARENT

    else
      ! find place where new node will be inserted
      finger => tree%root
      DOWN_LOOP: do i=1, MAX_SAFE_DEPTH
        select case(cfun(new_local%dat, finger%dat))
        case(-1) ! new < root
          if (associated(finger%left)) then
            finger => finger%left
          else
            which_child = LEFT_CHILD
            exit DOWN_LOOP
          end if
        case(+1) ! new > root
          if (associated(finger%right)) then
            finger => finger%right
          else
            which_child = RIGHT_CHILD
            exit DOWN_LOOP
          end if
        case(0)  ! new == root
          ierr0 = FLAG_DUPLICATE
          exit DOWN_LOOP
        case default
          error stop 'insert: invalid value returned from user function'
        end select
      end do DOWN_LOOP
      if (i==MAX_SAFE_DEPTH+1) &
        error stop 'insert: MAX_SAFE_DEPTH reached, increase it if this is not error'
    end if

    ! Insert node or handle duplicates error
    if (ierr0 == FLAG_OK) then
      if (which_child == LEFT_CHILD) then
        finger%left => new_local
      else if (which_child == RIGHT_CHILD) then
        finger%right => new_local
      end if
      if (which_child /= NO_PARENT) new_local%parent => finger

    else if (ierr0 == FLAG_DUPLICATE) then
      if (.not. present(ierr)) then
        ! Panic if `ierr` is not provided
        error stop 'insert: not sucessfull as the same node is already in tree'
      else if (.not. present(new_output)) then
        ! try to deallocate new node and silently return
        call rbnode_free(new_local)
        ierr0 = FLAG_DUPLICATE_FREED
      end if
    else
      error stop 'insert: unreachable branch'
    end if
    if (present(ierr)) ierr = ierr0

    ! Repair red-black tree
    if (ierr0==FLAG_OK) then
      new_local%isblack = .false. ! inserted node is red
      call insert_repair(new_local, tree)
    end if
  end subroutine rbnode_insert


  recursive subroutine insert_repair(n, tree)
    type(rbnode_t), intent(inout), pointer :: n
    type(rbbasetree_t), intent(inout) :: tree

    type(rbnode_t), pointer :: p, u, g, tmp
    logical :: uncle_exists, uncle_is_red
    integer :: whichchild_p, whichchild_n

    if (.not. associated(n)) &
      error stop 'insert_repair: n is null node'

    p => n%parent
    MAIN: if (.not. associated(p)) then
      ! CASE I - `n` is root, root should be black
      !Allow red roots? TODO
      !n%isblack = .true.
      if (.not. associated(tree%root,n)) &
          error stop 'node n seems to be root, but root points elsewhere'

    else if (p%isblack) then MAIN
      ! CASE II - nothing has to be done
      continue

    else MAIN
      ! parent is red
      u => rbnode_uncle(n, whichchild_p)
      g => rbnode_grandparent(n)
      uncle_exists = associated(u)
      uncle_is_red = .false.
      if (uncle_exists) uncle_is_red = .not. u%isblack

      if (uncle_exists .and. uncle_is_red) then
        ! CASE III - repaint parent and uncle black and repair grandparent
        p%isblack = .true.
        u%isblack = .true.
        g%isblack = .false.
        call insert_repair(g, tree)

      else
        ! CASE IV - parent is red and uncle is black
        whichchild_n = rbnode_whichchild(n)

        ! case IV, step 1 (move n to the outer-side of sub-tree if needed)
        if (whichchild_n==RIGHT_CHILD .and. whichchild_p==LEFT_CHILD) then
           tmp => rotate_left(p, tree)
           n => p
        else if (whichchild_n==LEFT_CHILD .and. whichchild_p==RIGHT_CHILD) then
           tmp => rotate_right(p, tree)
           n => p
        end if

        ! case IV, step 2
        p => n%parent
        g => rbnode_grandparent(n)

        if (whichchild_p==LEFT_CHILD) then
          tmp => rotate_right(g, tree)
        else if (whichchild_p==RIGHT_CHILD) then
          tmp => rotate_left(g, tree)
        else if (whichchild_p==NO_PARENT) then
          ! parent is red-root, it can be always relabeled
          p%isblack = BLACK_COLOUR
          return
        else 
          error stop 'case IV, part 2, unreachable branch'
        end if

        p%isblack = .true.
        g%isblack = .false.
      end if

    end if MAIN
  end subroutine insert_repair


  ! ========
  ! Deletion
  ! ========

  subroutine rbnode_delete(tree, what, ierr, deleted_output)
    !* Remove (and maybe free) a node from the tree.
    !  Optional pointer `deleted_output` points to the deleted node
    !  in the case user wants to take responsibility for freeing the node.
    !  If `deleted_output` is not provided, the node is freed here.
    !  Error handling in case the node is null:
    !    - set error flag and silently return
    !    - error stop if error flag was not provided
    !
    class(rbbasetree_t), intent(inout) :: tree
    type(rbnode_t), pointer, intent(in) :: what
    integer, optional, intent(out) :: ierr
    type(rbnode_t), pointer, intent(out), optional :: deleted_output
      !! If not provided, the node is deleted and freed
    integer, parameter :: FLAG_OK=0, FLAG_NOT_FOUND=1, FLAG_JUSTREMOVED=-1

    type(rbnode_t), pointer :: n, ch
    integer(kind=DATA_KIND), allocatable :: tmp_dat(:)

    ! If the deleted node does not exist, raise error flag or stop
    if (.not. associated(what)) then
      if (present(ierr)) then
        ierr = FLAG_NOT_FOUND
        return
      end if
      error stop 'delete: what is null node'
    end if

    n => what

    ! CASE I: N has two children
    ! * find the successor node, move content of that node to the current
    !   node to be deleted and then delete the successor node
    ! * continue to the CASE II (one or zero children)
    if (associated(n%left) .and. associated(n%right)) then
      ch => rbnode_leftmost(n%right)
      if (present(deleted_output)) then
        ! swap n%dat and ch%dat
        call move_alloc(n%dat, tmp_dat)
        call move_alloc(ch%dat, n%dat)
        call move_alloc(tmp_dat, ch%dat)
      else
        ! just move ch%dat, n%dat can be discarded
        call move_alloc(ch%dat, n%dat)
      end if
      n => ch
    else
      continue
    endif


    ! CASE II: N has one or zero children:
    ! * If N is red, both its children must be leafs.
    !   Node can be removed without violating red-black properties.
    ! * If N is black and its child CH is red, then N can be replaced by
    !   CH, CH is relabeled black and we are done.
    ! * If N is black with no children, removing N will break the
    !   red-black tree properties and it must be rebalanced in
    !   "delete_case1" subroutines.
    if (associated(n%left)) then
      ch => n%left
    elseif (associated(n%right)) then
      ch => n%right
    else
      ch => null()
    endif

    if (.not. associated(ch)) then
      ! N has no children
      if (n%isblack) call delete_case1(tree, n)

      ! N is red
      if (.not. associated(n%parent)) then
        ! N was root
        tree%root => null()
      elseif (rbnode_whichchild(n)==LEFT_CHILD) then
        n%parent%left => null()
      elseif (rbnode_whichchild(n)==RIGHT_CHILD) then
        n%parent%right => null()
      else
        error stop 'Delete: impossible branch'
      endif

    else
      ! N has one child (N must be black and CH must be red)
      ch % parent => n % parent
      if (.not. associated(n%parent)) then
        ! N was root, CH is new root
        tree%root => ch
      elseif (rbnode_whichchild(n)==LEFT_CHILD) then
        n%parent%left => ch
      elseif (rbnode_whichchild(n)==RIGHT_CHILD) then
        n%parent%right => ch
      else
        error stop 'Delete: impossible branch2'
      endif

      ! Assert N is black and CH is red
      !if (.not. n%isblack .and. .not. ch%isblack) then
      if (.not. (n%isblack .and. .not. ch%isblack)) then
        error stop "delete: assertion failed"
      endif

      ch%isblack = .true.
    endif


    ! Now N can be deallocated
    if (present(deleted_output)) then
      deleted_output => n
      n%parent => null()
      if (present(ierr)) ierr = FLAG_JUSTREMOVED
    else
      call rbnode_free(n)
      if (present(ierr)) ierr = FLAG_OK
    end if
  end subroutine rbnode_delete


  recursive subroutine delete_case1(tree, m)
    class(rbbasetree_t), intent(inout) :: tree
    type(rbnode_t), pointer, intent(in) :: m
!
! M is a black node without children.
! If M is the new root, nothing needs to be done.
! Otherwise proceed to case 2.
!
    if (associated(m%parent)) call delete_case2(tree, m)
  end subroutine delete_case1


  recursive subroutine delete_case2(tree, m)
    class(rbbasetree_t), intent(inout) :: tree
    type(rbnode_t), pointer, intent(in) :: m
!
! If S is red:
! * make S black, make P red and
! * rotate left/right around P so S will become grandparent of M
!
    type(rbnode_t), pointer :: s, p, tmp

    s => rbnode_sibling(m)
    p => m%parent

    if (.not. rbnode_isblack(s)) then
      p%isblack = .false. ! red_node
      s%isblack = .true.  ! black_node
      if (rbnode_whichchild(m)==LEFT_CHILD) then
        tmp => rotate_left(p, tree)
      else
        tmp => rotate_right(p, tree)
      endif
    endif
    call delete_case34(tree, m)
  end subroutine delete_case2


  recursive subroutine delete_case34(tree, m)
    class(rbbasetree_t), intent(inout) :: tree
    type(rbnode_t), pointer, intent(in) :: m
!
! If S, Sleft, Sright and P are black then
! * repaint S red: this compensates the deleted black node in S' subtree
!                  but the whole P->M and P->S sub-trees are one black node
!                  less than the remaining branches, therefore ...
! * rebalance up-level: use delete_case1 on P
!
! If S, Sleft and Sright are black but P is red then
! * exchange color of S and P and we are done
!
! Otherwise proceed to delete_case5
!
    type(rbnode_t), pointer :: s, p

    s => rbnode_sibling(m)
    p => m%parent

    ! assert that sibling is not leaf
    if (.not. associated(s)) &
    &   error stop "delete_case34: defensive check, sibling is a leaf:"

    if (p%isblack .and. s%isblack .and. &
    &   rbnode_isblack(s%left) .and. rbnode_isblack(s%right)) then
      s%isblack = .false. ! red_node
      call delete_case1(tree, p)

    elseif ( .not. p%isblack .and. s%isblack .and. &
    &   rbnode_isblack(s%left) .and. rbnode_isblack(s%right)) then
      s%isblack = .false. ! red_node
      p%isblack = .true.  ! black_node

    else
      call delete_case5(tree, m)
    endif
  end subroutine delete_case34


  subroutine delete_case5(tree, m)
    class(rbbasetree_t), intent(inout) :: tree
    type(rbnode_t), pointer, intent(in) :: m
!
! S is black, S left is red, S right is black and M is the left child
! * rotate right at S so S left is new sibling of M
! * exchange colors of S and its ne parent (it was S left)
!
! Mirrored situation
! S is black, S right is red, S left is black and M is the right child
! * rotate left at S so S right is new sibling of M
! * exchange colort of S and its new parent (it was S right)
!
! At the enf M should have black sibling with red children on the
! outside of the tree and this falls into case 6
!
    type(rbnode_t), pointer :: s, tmp

    s => rbnode_sibling(m)
    ! assert that sibling is not leaf
    if (.not. associated(s)) &
    &   error stop "delete_case5: defensive check, sibling is a leaf:"

    ! assert that sibling is black
    if (.not. s%isblack) &
    &   error stop "delete_case5: sibling is red"

    if (rbnode_whichchild(m)==LEFT_CHILD .and. rbnode_isblack(s%right)) then
      ! assert S left is red
      if (rbnode_isblack(s%left)) error stop "delete_case5: assert1"

      s%isblack      = .false. ! red_node
      s%left%isblack = .true.  ! black_node
      tmp => rotate_right(s, tree)

    elseif (rbnode_whichchild(m)==RIGHT_CHILD .and. rbnode_isblack(s%left)) then
      ! assert S right is red
      if (rbnode_isblack(s%right)) error stop "delete_case5: assert2"

      s%isblack      = .false. ! red_node
      s%right%isblack = .true.  ! black_node
      tmp => rotate_left(s, tree)
    endif

    call delete_case6(tree, m)
  end subroutine delete_case5


  subroutine delete_case6(tree, m)
    class(rbbasetree_t), intent(inout) :: tree
    type(rbnode_t), pointer, intent(in) :: m
!
! If S is black, its right child is red and M is the left child then
! * rotate left at P so S becomes the parent of P and S's right child
! * exchange colors P and S and make S's right child black
!
! Mirrored situation
! If S is black, its left child is red and M is the right child then
! * rotate right at P so S becomes the parent of P and S's left child
! * exchange colors P and S and make S's left child black
!
! The properties of red-black tree are now restored
!
    type(rbnode_t), pointer :: s, p, tmp

    s => rbnode_sibling(m)
    p => m%parent

    ! assert that sibling is not leaf
    if (.not. associated(s)) &
    &   error stop "delete_case6: defensive check, sibling is a leaf:"

    if (rbnode_isblack(p)) then
      s%isblack = .true.  ! black_node
    else
      s%isblack = .false. ! red_node
    endif
    p%isblack = .true.    ! black_node

    if (rbnode_whichchild(m)==LEFT_CHILD) then
      s%right%isblack = .true. ! black_node
      tmp => rotate_left(p, tree)
    else
      s%left%isblack = .true.  ! black_node
      tmp => rotate_right(p, tree)
    endif
  end subroutine delete_case6


  ! =============================
  ! Set operations
  ! =============================

  recursive subroutine rbnode_freetree(T)
    type(rbnode_t), pointer, intent(inout) :: T

    type(rbnode_t), pointer :: L, m, R

    if (.not. associated(T)) return
    if (associated(T%parent)) error stop 'freetree: node must be root'

    call unlink_root(L, m, R, T)
    if (associated(m)) call rbnode_free(m)
    call rbnode_freetree(L)
    call rbnode_freetree(R)
    T => null()
  end subroutine rbnode_freetree


  recursive function rbnode_copytree(tsrc) result(T)
    !! Allocate new tree - duplicate of subtree "tsrc"
    type(rbnode_t), pointer, intent(in) :: tsrc
    type(rbnode_t), pointer :: T

    if (.not. associated(tsrc)) then
      T => null()
    else
      T => link_node(rbnode_t(tsrc%left), &
                     rbnode_t(rbnode_read(tsrc)), &
                     rbnode_t(tsrc%right))
      T%isblack = tsrc%isblack
    end if
  end function rbnode_copytree


  function link_node(L, m, R) result(T)
    !! connect left and right subtrees as the children of "m"
    type(rbnode_t), pointer, intent(in) :: L, m, R
    type(rbnode_t), pointer :: T

    if (.not. associated(m)) error stop 'link_node: node is nil'

    ! assert both sub-trees are roots
    if (associated(L)) then
      if (associated(L%parent)) error stop 'link_node: left subtree is not root'
    endif
    if (associated(R)) then
      if (associated(R%parent)) error stop 'link_node: right subtree is not root'
    end if

    ! assert node is isolated
    if (associated(m%parent) .or. associated(m%left) .or. associated(m%right)) &
      error stop 'link_node: node is not isolated'

    ! make the connections
    T => m
    T%left => L
    T%right => R
    if (associated(T%left)) T%left%parent => T
    if (associated(T%right)) T%right%parent => T
  end function link_node


  subroutine unlink_root(L, m, R, T)
    !! Reverse operation to "link_node": disconnect left and right subtrees from
    !! the root, return pointers to both subtrees and to the old root node
    type(rbnode_t), pointer, intent(out) :: L, m, R
    type(rbnode_t), pointer, intent(in) :: T

    ! assert root exists and that it is root
    if (.not. associated(T)) error stop 'unlink_root: root is nil'
    if (associated(T%parent)) error stop 'unlink_root: root is not root'

    L => T%left
    m => T
    R => T%right

    if (associated(L)) then
      if (.not. associated(L%parent, m)) &
          error stop 'unlink_root: left child is not a child'
      L%parent => null()
      m%left => null()
    end if
    if (associated(R)) then 
      if (.not. associated(R%parent, m)) &
          error stop 'unlink_root: right child is not a child'
      R%parent => null()
      m%right => null()
    end if
  end subroutine unlink_root


! TODO (future) - store black-height instead of measuring it repeatedly
! to fasten the set operations

  function join_3(tl, k, tr) result(tjoin)
    !* Join two sub-trees and return the pointer to a root of a new tree.
    !
    !  Important: all nodes in the left subtree must have key less than "key",
    !  and all nodes in the right subtree must have key greater than "key"
    !
    !  Note: Black-height of nodes remains unaffected (except for the new root)
    !
    type(rbnode_t), intent(in), pointer :: tl, tr, k
    type(rbnode_t), pointer :: tjoin

    if (.not. associated(k)) error stop 'join: nil key node'

    ! Assert all nodes are roots
    if (associated(tl)) then
      if (associated(tl%parent)) error stop 'join: tl is not root'
    end if
    if (associated(tr)) then
      if (associated(tr%parent)) error stop 'join: tr is not root'
    end if
    if (associated(k%parent)) error stop 'join - key node must be root'

    ! node k will go to the left sub-tree
    if (rbnode_blackheight(tl) > rbnode_blackheight(tr)) then
      tjoin => join_right(tl, k, tr)
      ! tjoin tree is never empty: it contains a key node at least
      if ((rbnode_isblack(tjoin) .eqv. RED_COLOUR) .and. &
          (rbnode_isblack(tjoin%right) .eqv. RED_COLOUR )) &
          tjoin%isblack = BLACK_COLOUR
      return
! A possible "no double red nodes"-rule violation was fixed above
!         t-red                  t-black
!          /  \           ---->   /  \
!       tl-*   tr-red          tl-*   tr-red
! Because "t" is root, it can be relabeled. This is the only case, when the
! black-node height of a node can change during the join operation.
    end if

    ! node k will go to the right sub-tree
    if (rbnode_blackheight(tl) < rbnode_blackheight(tr)) then
     tjoin => join_left(tl, k, tr)
     if ((rbnode_isblack(tjoin) .eqv. RED_COLOUR) .and. &
         (rbnode_isblack(tjoin%left) .eqv. RED_COLOUR )) &
         tjoin%isblack = BLACK_COLOUR
     return
    end if

    ! Both trees have the same black height, "k" will be the new root.
    ! If both sub-trees have black root, "k" can be red.
    if ((rbnode_isblack(tl) .eqv. BLACK_COLOUR) .and. &
        (rbnode_isblack(tr) .eqv. BLACK_COLOUR)) then
      tjoin => link_node(tl, k, tr)
      tjoin%isblack = RED_COLOUR
    else
      tjoin => link_node(tl, k, tr)
      tjoin%isblack = BLACK_COLOUR
    end if
  end function join_3


  recursive function join_right(tl, k, tr) result(tjoin)
    type(rbnode_t), intent(in), pointer :: tl, tr, k
    type(rbnode_t), pointer :: tjoin

    type(rbnode_t), pointer :: tl_left, tl_right, m
    type(rbbasetree_t) :: tree
    logical(int8) :: tl_isblack

    ! Assert nodes are roots
    if (associated(tl)) then
      if (associated(tl%parent)) error stop 'join_right: tl is not root'
    end if
    if (associated(tr)) then
      if (associated(tr%parent)) error stop 'join_right: tr is not root'
    end if

    tl_isblack = rbnode_isblack(tl)

    if (tl_isblack .and. rbnode_blackheight(tl)==rbnode_blackheight(tr)) then
      ! We recursively moved down along the right spine of "tl", until both
      ! sub-trees are of equal height and can become the children of "k" node:
      !     k-R
      !    /   \
      ! tl-B   tr-R/B
      !
      ! As no new black nodes were introduced, the black-height is unchanged.
      ! A possible no "red-red rule" violation will be fixed during backtracking
      ! or inside the main "join" routine.
      tjoin => link_node(tl, k, tr)
      tjoin%isblack = RED_COLOUR
      return
    end if

    ! the "k" and "tr" will be joined somewhere in the right sub-tree of "tl"
    call unlink_root(tl_left, m, tl_right, tl)
    tree%root => link_node(tl_left, m, join_right(tl_right, k, tr))

    if (tl_isblack .and. &
       ((rbnode_isblack(tree%root%right) .eqv. RED_COLOUR) .and. &
        (rbnode_isblack(tree%root%right%right) .eqv. RED_COLOUR))) then
      tree%root%right%right%isblack = BLACK_COLOUR
      tjoin => rotate_left(tree%root, tree)
    else
      tjoin => tree%root
    end if
! The following possible configuration was fixed in the code above:
!  tl-B          tl-B              p-R
!  /  \          /  \             /   \
! *   p-R  ---> *   p-R   ---> tl-B   q-B
!     / \           / \         / \
!    *  q-R        *  q-B      *   *
! Recolouring "q" added one black node to the path.
! The number of black nodes in all paths was restored by the rotation.
! A configuration where the new parent of "p" is also red, will be resolved
! on the higher level or inside the main join function.
  end function join_right


  recursive function join_left(tl, k, tr) result(tjoin)
    ! A symmetric case to "join_right" subroutine
    type(rbnode_t), intent(in), pointer :: tl, tr, k
    type(rbnode_t), pointer :: tjoin

    type(rbnode_t), pointer :: tr_left, tr_right, m
    type(rbbasetree_t) :: tree
    logical(int8) :: tr_isblack

    ! Assert nodes are roots
    if (associated(tl)) then
      if (associated(tl%parent)) error stop 'join_left: tl is not root'
    end if
    if (associated(tr)) then
      if (associated(tr%parent)) error stop 'join_left: tr is not root'
    end if

    tr_isblack = rbnode_isblack(tr)

    if (tr_isblack .and. rbnode_blackheight(tl)==rbnode_blackheight(tr)) then
      tjoin => link_node(tl, k, tr)
      tjoin%isblack = RED_COLOUR
      return
    end if

    call unlink_root(tr_left, m, tr_right, tr)
    tree%root => link_node(join_left(tl, k, tr_left), m, tr_right)

    if (tr_isblack .and. &
       ((rbnode_isblack(tree%root%left) .eqv. RED_COLOUR) .and. &
        (rbnode_isblack(tree%root%left%left) .eqv. RED_COLOUR))) then
      tree%root%left%left%isblack = BLACK_COLOUR
      tjoin => rotate_right(tree%root, tree)
    else
      tjoin => tree%root
    end if
  end function join_left


  recursive subroutine split(L, k, R, T, key, cfun)
    !* Split tree. Nodes with a value less than "key" go to the left subtree,
    !  and nodes with a value larger than "key" go to the right subtree.
    !  Pointers to the roots of both subtrees are returned.
    !
    !  If tree contains a node with the same value as "key", this node will be
    !  excluded from any of the sub-trees and returned as a "k" pointer.
    !  If no such node exists, a null pointer will be returned.
    !
    type(rbnode_t), pointer, intent(out) :: L, R, k
      !! roots of left and right sub-trees, key node if it was present in "t"
    type(rbnode_t), pointer, intent(in) :: T
    integer(DATA_KIND), intent(in) :: key(:)
    procedure(compare_fun) :: cfun

    type(rbnode_t), pointer :: l1, r1, m

    L => null(); k => null(); R => null()
    if (.not. associated(T)) return

    ! assert T is already a root of a tree
    if (associated(T%parent)) error stop 'split - working node must be root'

    call unlink_root(L, m, R, T)

    select case(cfun(key, rbnode_read(m)))
    case(0) ! key == old_root%key
      k => m

    case(-1) ! key < old_root%key
      call split(l1, k, r1, L, key, cfun)
      L => l1
      R => join_3(r1, m, R)

    case(+1) ! key > old_root%key
      call split(l1, k, r1, R, key, cfun)
      R => r1
      L => join_3(L, m, l1)

    case default
      error stop 'split: user function returned invalid value'
    end select
  end subroutine split


  recursive subroutine splitlast(Ts, m, T)
    !! Split the rightmost node from the tree and return pointers to the node
    !! and the rest of the tree.
    type(rbnode_t), pointer, intent(out) :: Ts, m
    type(rbnode_t), pointer, intent(in) :: T

    type(rbnode_t), pointer :: Ts1, m1, L, R

    call unlink_root(L, m, R, T)
    if (.not. associated(R)) then
      Ts => L
    else
      call splitlast(Ts1, m1, R)
      Ts => join_3(L, m, Ts1)
      m => m1
    end if
  end subroutine splitlast


  function join_2(L, R) result(T)
    !! Same as join, but without the central node
    type(rbnode_t), pointer, intent(in) :: L, R
    type(rbnode_t), pointer :: T

    type(rbnode_t), pointer :: Ls, m

    if (.not. associated(L)) then
      T => R
    else
      call splitlast(Ls, m, L)
      T => join_3(Ls, m, R)
    end if
  end function join_2


  recursive function union(T1, T2, cfun) result(t12)
    type(rbnode_t), pointer, intent(in) :: T1, T2
    procedure(compare_fun) :: cfun
    type(rbnode_t), pointer :: T12

    type(rbnode_t), pointer :: L1, L2, L12, R1, R2, R12, m1, m2

    if (.not. associated(T1)) then
      T12 => T2
      return
    else if (.not. associated(T2)) then
      T12 => T1
      return
    end if

    call unlink_root(L1, m1, R1, T1)
    call split(L2, m2, R2, T2, rbnode_read(m1), cfun)

    ! these two branches can run independently
    L12 => union(L1, L2, cfun)
    R12 => union(R1, R2, cfun)

    T12 => join(L12, m1, R12)
    if (associated(m2)) call rbnode_free(m2)
  end function union


  recursive function intersection(T1, T2, cfun) result(T12)
    type(rbnode_t), pointer, intent(inout) :: T1, T2
    procedure(compare_fun) :: cfun
    type(rbnode_t), pointer :: T12

    type(rbnode_t), pointer :: L1, L2, L12, R1, R2, R12, m1, m2

    T12 => null()
    if (.not. associated(T1) .or. .not. associated(T2)) then
      call rbnode_freetree(T1)
      call rbnode_freetree(T2)
      return
    end if

    call unlink_root(L1, m1, R1, T1)
    call split(L2, m2, R2, T2, rbnode_read(m1), cfun)

    ! these two branches can run independently
    L12 => intersection(L1, L2, cfun)
    R12 => intersection(R1, R2, cfun)

    if (associated(m2)) then
      T12 => join(L12, m1, R12)
      call rbnode_free(m2)
    else
      T12 => join(L12, R12)
      call rbnode_free(m1)
    end if
  end function intersection


  recursive function difference(T1, T2, cfun) result(T12)
    type(rbnode_t), pointer, intent(inout) :: T1, T2
    procedure(compare_fun) :: cfun
    type(rbnode_t), pointer :: T12

    type(rbnode_t), pointer :: L1, L2, L12, R1, R2, R12, m1, m2 

    if (.not. associated(T1)) then
      T12 => null()
      call rbnode_freetree(T2)
      return
    else if (.not. associated(T2)) then
      T12 => T1
      return
    end if

    call unlink_root(L1, m1, R1, T1)
    call split(L2, m2, R2, T2, rbnode_read(m1), cfun)

    ! these two branches can run independently
    L12 => difference(L1, L2, cfun)
    R12 => difference(R1, R2, cfun)

    if (associated(m2)) then
      T12 => join(L12, R12)
      call rbnode_free(m2)
      call rbnode_free(m1)
    else
      T12 => join(L12, m1, R12)
    end if
  end function difference


  function rbnode_export(T, starts) result(values)
    !! Store sub-tree `T` data into an array
    type(rbnode_t), pointer, intent(in) :: T
    integer, allocatable, intent(out), optional :: starts(:)
      !! The pair [starts(i), starts(i+1)-1] is the index range specifying data
      !! blocks in `values` belonging to node `i`.
      !! (Data for the last block ends at the end of `values`.)
    integer(DATA_KIND), allocatable :: values(:)

    integer n, nval, i, j

    n = rbnode_size(T)
    if (present(starts)) allocate(starts(n))
    nval = rbnode_countvalues(T)
    allocate(values(nval))

    i = 0 ! the last defined record in starts
    j = 0 ! the last defined record in values
    call visit_nodes_export(T, i, j, starts, values)

    if (i/=n) error stop 'export: number of visited nodes mismatch'
    if (j/=nval) error stop 'export: number of exported data values mismatch'
  end function rbnode_export


  recursive subroutine visit_nodes_export(node, i, j, starts, values)
    type(rbnode_t), intent(in), pointer :: node
    integer, intent(inout) :: i, j
    integer(DATA_KIND), intent(inout) :: values(:)
    integer, intent(inout), optional :: starts(:)

    if (.not. associated(node)) return

    call visit_nodes_export(node%left, i, j, starts, values)

    associate(curdata=>rbnode_read(node))
      i = i + 1
      j = j + 1
      values(j:j+size(curdata)-1) = curdata
      if (present(starts)) starts(i) = j
      j = j+size(curdata)-1
    end associate

    call visit_nodes_export(node%right, i, j, starts, values)
  end subroutine visit_nodes_export


  recursive function rbnode_importsorted(values, starts) result(T)
    !! Build tree from an array. The order of nodes is determined by the order
    !! of data in input arrays.
    !!
    !! The nodes must be sorted - no verification is done here!
    !!
    integer(DATA_KIND), intent(in) :: values(:)
    integer, intent(in) :: starts(:)
      !! The pair [starts(i), starts(i+1)-1] is the index range specifying data
      !! blocks in `values` belonging to node `i`.
      !! (Data for the last block ends at the end of `values`.)
    type(rbnode_t), pointer :: T

    type(rbnode_t), pointer :: L, R
    integer :: n, i, jbeg, jend

    n = size(starts)
    if (n==0) then
      if (size(values)/=0) &
          error stop 'importsorted: values array of zero size expected here'
      T => null()

    else if (n==1) then
      T => link_node(null(), rbnode_t(values), null())

    else
      i = n/2+1
      jbeg = starts(i)-starts(1)+1
      if (n>2) then
        jend = starts(i+1)-starts(1)+1 - 1
      else
        jend = size(values)
      end if
      L => rbnode_importsorted(values(:jbeg-1), starts(:i-1))
      R => rbnode_importsorted(values(jend+1:), starts(i+1:))
      T => join(L, rbnode_t(values(jbeg:jend)), R)
    end if
  end function rbnode_importsorted


  ! ================================
  ! Validation and debugging helpers
  ! ================================
  function rbbasetree_size(this) result(n)
    class(rbbasetree_t), intent(in) :: this
    integer :: n
    n = rbnode_size(this%root)
  end function rbbasetree_size


  recursive function rbnode_size(node) result(n)
    type(rbnode_t), pointer, intent(in) :: node
    integer :: n
    n = 0
    if (.not. associated(node)) return
    n = 1 + rbnode_size(node%left) + rbnode_size(node%right)
  end function rbnode_size


  recursive function rbnode_countvalues(node) result(nval)
    type(rbnode_t), pointer, intent(in) :: node
    integer :: nval
    nval = 0
    if (.not. associated(node)) return
    if (allocated(node%dat)) nval = size(node%dat,1)
    nval = nval + rbnode_countvalues(node%left) + rbnode_countvalues(node%right)
  end function rbnode_countvalues


  function rbbasetree_isvalid(this, cfun) result(isvalid)
    class(rbbasetree_t), intent(in) :: this
    procedure(compare_fun) :: cfun
    logical :: isvalid

    integer :: nblacks

    call rbnode_validate(this%root, cfun, isvalid, nblacks)
    if (associated(this%root)) isvalid = isvalid .and. &
        .not. associated(this%root%parent)
  end function rbbasetree_isvalid


  recursive subroutine rbnode_validate(root, cfun, isvalid, nblacks)
    type(rbnode_t), intent(in), pointer :: root
    procedure(compare_fun) :: cfun
    logical, intent(out) :: isvalid
    integer, intent(out) :: nblacks

    logical :: isvalid_left, isvalid_right, isvalid_bst
    integer :: nblacks_left, nblacks_right

    ! empty tree is a valid tree
    if (.not. associated(root)) then
      isvalid = .true.
      nblacks = 1 ! leaf node is assumed black
      return
    end if

    ! validate left and right sub-trees
    call rbnode_validate(root%left, cfun, isvalid_left, nblacks_left)
    call rbnode_validate(root%right, cfun, isvalid_right, nblacks_right)
    isvalid = isvalid_left .and. isvalid_right

    ! assert red-node has only black children
    if (.not. root%isblack) isvalid = isvalid .and. &
        rbnode_isblack(root%left) .and. rbnode_isblack(root%right)

    ! assert children point back at parent node
    if (associated(root%left))  isvalid = isvalid .and. associated(root%left%parent, root)
    if (associated(root%right)) isvalid = isvalid .and. associated(root%right%parent, root)

    ! assert that number of black nodes is same in both sub-trees
    isvalid = isvalid .and. (nblacks_left == nblacks_right)
    nblacks = nblacks_left
    if (rbnode_isblack(root)) nblacks = nblacks + 1

    ! assert binary search tree data are in correct order
    isvalid_bst = .true.
    if (associated(root%left)) isvalid_bst = isvalid_bst .and. &
        cfun(rbnode_read(root%left), rbnode_read(root)) == -1
    if (associated(root%right)) isvalid_bst = isvalid_bst .and. &
        cfun(rbnode_read(root), rbnode_read(root%right)) == -1
    isvalid = isvalid .and. isvalid_bst
  end subroutine rbnode_validate


  function rbbasetree_blackheight(this) result(bh)
    class(rbbasetree_t), intent(in) :: this
    integer :: bh
    bh = rbnode_blackheight(this%root)
  end function rbbasetree_blackheight


  function rbnode_blackheight(node) result(bh)
    !* The black height of a red–black tree is the number of black nodes in any
    !  path from the root to the leaves. The black height of a node is the black
    !  height of the subtree rooted by it. The black height of a NIL node shall
    !  be set to 0, because its subtree is empty, and its tree height is also 0.
    !
    type(rbnode_t), pointer, intent(in) :: node
    integer :: bh
    type(rbnode_t), pointer :: current

    bh = 0
    current => node
    do
      if (.not. associated(current)) exit
      if (current%isblack) bh = bh + 1
      current => current%left ! if tree is valid, the choice is arbitrary
    end do
  end function rbnode_blackheight


  subroutine rbbasetree_graphviz(this, basename, get_node_label)
    !! Make a PNG image of the red-black tree using an external application
    class(rbbasetree_t), intent(in) :: this
    character(len=*), intent(in) :: basename
    procedure(get_node_label_fun) :: get_node_label

    character(len=*), parameter :: SUFTXT='gv.txt', SUFPNG='.png'
    integer, parameter :: TREE_SIZE_LIMIT = 1000
    integer :: fid, cmdstat, exitstat, tree_size
    character(len=200) cmdmsg

    ! sanity check to not draw too large trees
    tree_size = this%size()
    if (tree_size> TREE_SIZE_LIMIT) then
      !print '("graphviz: tree size ",i0," is too large to be shown as PNG image")',&
      !    tree_size
      return
    end if

    ! write tree to a temporary file...
    open(newunit=fid, file=basename//SUFTXT, status='replace')
    write(fid,'(a,/,a)') 'digraph {','node [fontname="Arial"];'
    call visit_nodes_graphviz(this%root, fid, .true., get_node_label)
    write(fid,'(a)') '}'
    flush(fid)

    ! ...and then try to call 'dot' application
    call execute_command_line('dot -Tpng < '//basename//SUFTXT//' > '//basename//SUFPNG, &
        exitstat=exitstat, cmdstat=cmdstat, cmdmsg=cmdmsg)
    if (exitstat==0 .and. cmdstat==0) then
      ! if graph was rendered to PNG, the file is of no use anymore
      close(fid, status='delete')
    else
      ! in case of an error, keep the file
      close(fid)
      print '("Call to DOT failed: """,a,"""")', trim(cmdmsg)
    end if
  end subroutine rbbasetree_graphviz


  recursive subroutine visit_nodes_graphviz(current, fid, isrootlevel, get_node_label)
    type(rbnode_t), pointer, intent(in) :: current
    integer, intent(in) :: fid
    logical, intent(in) :: isrootlevel
    procedure(get_node_label_fun) :: get_node_label

    type(rbnode_t), pointer :: par
    character(len=:), allocatable :: label_cur, label_par

    if (.not. associated(current)) then
      if (isrootlevel) write(fid,'(a)') '"empty tree" [color=gray]'
      return
    end if

    label_cur = get_node_label(rbnode_read(current))

    if (.not. current%isblack) then
      write(fid,'(a)') trim(adjustl(label_cur))//' [color=red]'
    else if (isrootlevel .and. .not. associated(current%leftnode()) .and. .not. associated(current%rightnode())) then
      write(fid,'(a)') trim(adjustl(label_cur))
    end if

    par => current%upnode()
    if (associated(par)) then
      label_par = get_node_label(rbnode_read(par))
      write(fid,'(a)') trim(adjustl(label_par))//' -> '//trim(adjustl(label_cur))
    end if

    call visit_nodes_graphviz(current%leftnode(), fid, .false., get_node_label)
    call visit_nodes_graphviz(current%rightnode(), fid, .false., get_node_label)
  end subroutine visit_nodes_graphviz


!TODO a temporary function for debugging
  function print_node(node) result(str)
    type(rbnode_t), pointer :: node
    character(len=:), allocatable :: str
    character(len=21) :: str0 
    if (associated(node)) then
      write(str0,*) transfer(rbnode_read(node),1)
    else 
      str0='nil'
    end if
    str = trim(adjustl(str0))
  end function print_node

end module rbnode_mod
