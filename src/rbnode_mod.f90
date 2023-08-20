! Red-black tree implementation
! -----------------------------------------------------
! Algorithms based on this Wikipedia article:
! https://en.wikipedia.org/wiki/Red%E2%80%93black_tree
!
module rbnode_mod
  !*
  !
  !
  use common_mod, only : DATA_KIND, mold, compare_fun
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
    procedure :: is_node_black
    procedure :: leftnode, rightnode, upnode
    procedure :: nextnode => nextnode_tbp
  end type rbnode_t
  interface rbnode_t
    module procedure rbnode_new, rbnode_newroot
    !module procedure rbnode_import
    !module procedure rbnode_copy
  end interface

  type, public :: rbbasetree_t
    type(rbnode_t), pointer :: root => null()
  contains
    procedure :: isvalid => rbbasetree_isvalid
    procedure :: blackheight => rbbasetree_blackheight
  end type rbbasetree_t

  integer, parameter :: LEFT_CHILD=1, RIGHT_CHILD=2, NO_PARENT=0
  logical(int8), parameter :: RED_COLOUR=.false.,BLACK_COLOUR=.true.


  integer, parameter :: MAX_SAFE_DEPTH=1000000
    !! Defensive test to avoid infinite loops in the code

  public rbnode_update, rbnode_read, rbnode_free
  public rbnode_find
  public rbnode_leftmost, rbnode_nextnode, rbnode_prevnode
  public rbnode_insert, rbnode_delete
  public rbnode_validate, rbnode_blackheight
  public join, split, union

  integer, public, save :: allocation_counter = 0
    !! temporary, just for mem.leakage debuging TODO


contains

  ! ==============
  ! Simple getters
  ! ==============
  logical function is_node_black(this)
    class(rbnode_t), intent(in) :: this
    is_node_black = this%isblack
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

  function nextnode_tbp(this) result(nn)
    class(rbnode_t), intent(in), target :: this
    type(rbnode_t), pointer :: nn
    nn => this
    nn => rbnode_nextnode(nn)
  end function
! TODO prevnode, leftmost?, rightmost?


  ! ================================
  ! Allocate new node (CONSTRUCTOR)
  ! Update node data (???)
  ! Read data from node
  ! Deallocate node
  ! ================================

  function rbnode_new(dat) result(new)
    !! Allocate new node, fill it with data, result pointer
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


  function rbnode_newroot(left, dat, colour, right) result(new)
    !! Allocate new node, fill it with given data and colour, and make
    !! it a root of two given sub-trees
    type(rbnode_t), intent(in), pointer :: left, right
    integer(DATA_KIND), intent(in) :: dat(:)
    logical(int8), intent(in) :: colour
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
    new%isblack = colour
    new%parent => null()
    new%left => left
    if (associated(new%left)) new%left%parent => new
    new%right => right
    if (associated(new%right)) new%right%parent => new
  end function rbnode_newroot


  subroutine rbnode_update(node, newdata)
    !! Update the data content of the node by newdata
    !! TODO may invalidate the red-black tree!!!
    type(rbnode_t), intent(in), pointer :: node
    integer(DATA_KIND), intent(in) :: newdata(:)

    integer :: ierr

    if (.not. associated(node)) &
        error stop 'could not update data: node is null'
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
  ! Jump inside tree (grandparent, sibling, uncle)
  ! Tree rotations
  ! In-order traversal (nextnode, prevnode)
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
    !! Allow to query also null nodes (they are assumed black)
    type(rbnode_t), pointer, intent(in) :: node
    logical :: isblack
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


  subroutine rehang_tree(oldchild, newchild, tree)
    type(rbnode_t), pointer, intent(in) :: oldchild, newchild
    type(rbbasetree_t), intent(inout) :: tree

    type(rbnode_t), pointer :: parent

    ! Old child (pivot) exchanged place with new child (rotator) during the
    ! rotation. Here, we repair the parent->child link.
    ! The other way link (child->parent) should have been updates in
    ! rotate_...()
    parent => newchild%parent

    if (associated(parent)) then
      ! Assert old child was really one of parents children
      if (associated(parent%left, oldchild)) then
        parent%left => newchild
      else if (associated(parent%right, oldchild)) then
        parent%right => newchild
      else
        error stop 'rehang_tree - old child not recognized by parent'
      end if
    else
      ! Pivot was root, and did not have any parent. Root-pointer must
      ! be repaired instead
      if (.not. associated(tree%root, oldchild)) &
          error stop 'rehang_tree - unexpected association of root pointer'
      tree%root => newchild
    end if
  end subroutine rehang_tree


  function rotate_left(piv, tree) result(rot)
    !! Rotate left.
    !! Fails if pivot is leaf or if pivot's right child (rotator) is leaf
    !! Remember: pivot's parent must be relinked to rotator outside!
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

    call rehang_tree(piv, rot, tree)
  end function rotate_left


  function rotate_right(piv, tree) result(rot)
    !! Rotate right.
    !! Fails if pivot is leaf or if pivot's left child (rotator) is leaf
    !! Remember: pivot's parent must be relinked to rotator outside!
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

    call rehang_tree(piv, rot, tree)
  end function rotate_right


  function rbnode_leftmost(node) result(leftmost)
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


  function rbnode_nextnode(node) result(successor)
    !! Return next node, or null pointer if node is the last node
    type(rbnode_t), intent(in), pointer :: node
    type(rbnode_t), pointer :: successor

    type(rbnode_t), pointer :: child
    integer :: i

    if (.not. associated(node)) &
        error stop 'nextnode: input is a null node'

    ! If node has a right sub-tree, the next node is the leftmost node in this
    ! sub-tree
    if (associated(node%right)) then
      successor => rbnode_leftmost(node%right)
      return
    end if

    ! Otherwise, the next node is the first parent of a left child that is
    ! encountered when traversing up the tree. If no such parent exists, it
    ! means that the node is the last node
    child => node
    do i=1,MAX_SAFE_DEPTH
      successor => child%parent

      ! no more parents, return null
      if (.not. associated(successor)) exit

      select case(rbnode_whichchild(child))
      case(LEFT_CHILD)
        ! parent of a left child is the next node and is returned
        exit
      case(RIGHT_CHILD)
        ! parent of a right child is not the next node, but its
        ! grand-parent could be...
        child => child%parent
      case default
        error stop 'nextnode: node has no parent and this is not expected'
      end select
    end do

    if (i==MAX_SAFE_DEPTH+1) &
      error stop 'nextnode: MAX_SAFE_DEPTH reached, increase it if this is not error'
  end function rbnode_nextnode


  function rbnode_prevnode(node) result(predecessor)
    !! Return prev node, or null pointer if node is the first node
    type(rbnode_t), intent(in), pointer :: node
    type(rbnode_t), pointer :: predecessor

    type(rbnode_t), pointer :: child
    integer :: i

    if (.not. associated(node)) &
        error stop 'prevnode: input is a null node'

    ! If node has a left sub-tree, the next node is the rightmost node in this
    ! sub-tree
    if (associated(node%left)) then
      predecessor => rbnode_rightmost(node%left)
      return
    end if

    ! Otherwise, the prev node is the first parent of a right child that is
    ! encountered when traversing up the tree. If no such parent exists, it
    ! means that the node is the first node
    child => node
    do i=1,MAX_SAFE_DEPTH
      predecessor => child%parent

      ! no more parents, return null
      if (.not. associated(predecessor)) exit

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
      error stop 'prevnode: MAX_SAFE_DEPTH reached, increase it if this is not error'
  end function rbnode_prevnode


  function rbnode_find(start, val, cfun) result(found)
    type(rbnode_t), intent(in), pointer :: start
    integer(DATA_KIND), intent(in) :: val(:)
    procedure(compare_fun) :: cfun
    type(rbnode_t), pointer :: found

    type(rbnode_t), pointer :: finger
    integer :: i

    found => null()
    finger => start
    do i=1, MAX_SAFE_DEPTH
      if (.not. associated(finger)) exit
      select case(cfun(val, finger%dat))
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
    !
    class(rbbasetree_t), intent(inout) :: tree
    type(rbnode_t), pointer, intent(in) :: what
    integer, optional, intent(out) :: ierr
    type(rbnode_t), pointer, intent(out), optional :: deleted_output

    integer :: ierr0 ! TODO error processing not finished
    type(rbnode_t), pointer :: n, ch
    integer(kind=DATA_KIND), allocatable :: tmp_dat(:)

    if (.not. associated(what)) &
        error stop 'delete: what is null node'
      ! TODO for now we panic if the deleted node does not exist
      !      later allow to exit quietly?

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
    else
      call rbnode_free(n)
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
  ! Set operations (experimental)
  ! =============================

  recursive function re_root(left,root,right) result(newroot)
    !* Same job as "rbnode_newroot" but here we do not allocate any new memory.
    !  The existing memory of "root" is reused instead.
    !
    type(rbnode_t), intent(in), pointer :: left, root, right
    type(rbnode_t), pointer :: newroot

    newroot => root
    newroot%parent => null()
    newroot%left => left
    if (associated(newroot%left)) newroot%left%parent => newroot
    newroot%right => right
    if (associated(newroot%right)) newroot%right%parent => newroot
  end function re_root


  recursive function join_right(tl, key, tr) result(newroot)
    type(rbnode_t), intent(in), pointer :: tl, tr
    integer(DATA_KIND), intent(in) :: key(:)
    type(rbnode_t), pointer :: newroot

    type(rbbasetree_t) :: t
    logical(int8) :: tl_isblack

    tl_isblack = rbnode_isblack(tl)

    if (tl_isblack .and. rbnode_blackheight(tl)==rbnode_blackheight(tr)) then
      newroot => rbnode_t(tl, key, RED_COLOUR, tr)
      return
    end if

    t%root => re_root(tl%left, tl, join_right(tl%right, key, tr))

    if (tl_isblack .and. &
       ((rbnode_isblack(t%root%right) .eqv. RED_COLOUR) .and. &
        (rbnode_isblack(t%root%right%right) .eqv. RED_COLOUR))) then
      t%root%right%right%isblack = BLACK_COLOUR
      newroot => rotate_left(t%root, t)
    else
      newroot => t%root
    end if
  end function join_right


  recursive function join_left(tl, key, tr) result(newroot)
    type(rbnode_t), intent(in), pointer :: tl, tr
    integer(DATA_KIND), intent(in) :: key(:)
    type(rbnode_t), pointer :: newroot

    type(rbbasetree_t) :: t
    logical(int8) :: tr_isblack

    tr_isblack = rbnode_isblack(tr)

    if (tr_isblack .and. rbnode_blackheight(tl)==rbnode_blackheight(tr)) then
      newroot => rbnode_t(tl, key, RED_COLOUR, tr)
      return
    end if

    t%root => re_root(join_left(tl, key, tr%left), tr, tr%right)

    if (tr_isblack .and. &
       ((rbnode_isblack(t%root%left) .eqv. RED_COLOUR) .and. &
        (rbnode_isblack(t%root%left%left) .eqv. RED_COLOUR))) then
      t%root%left%left%isblack = BLACK_COLOUR
      newroot => rotate_right(t%root, t)
    else
      newroot => t%root
    end if
  end function join_left


  function join(tl, key, tr) result(newroot)
    !* Join two sub-trees and return the pointer to a root of new tree. New
    !  node with "key" data is allocated during the process.
    !
    !  Note: all nodes in the left subtree must have key less than "key", and
    !  all nodes in the right subtree must have key greater than "key" !
    !
    type(rbnode_t), intent(in), pointer :: tl, tr
    integer(DATA_KIND), intent(in) :: key(:)
    type(rbnode_t), pointer :: newroot

    if (rbnode_blackheight(tl) > rbnode_blackheight(tr)) then
      newroot => join_right(tl, key, tr)
      if ((rbnode_isblack(newroot) .eqv. RED_COLOUR) .and. &
          (rbnode_isblack(newroot%right) .eqv. RED_COLOUR )) &
          newroot%isblack = BLACK_COLOUR
      return
    end if

    if (rbnode_blackheight(tl) < rbnode_blackheight(tr)) then
      newroot => join_left(tl, key, tr)
      if ((rbnode_isblack(newroot) .eqv. RED_COLOUR) .and. &
          (rbnode_isblack(newroot%left) .eqv. RED_COLOUR )) &
          newroot%isblack = BLACK_COLOUR
      return
    end if

    ! both trees have the same black height
    if ((rbnode_isblack(tl) .eqv. BLACK_COLOUR) .and. &
        (rbnode_isblack(tr) .eqv. BLACK_COLOUR)) then
      newroot => rbnode_t(tl,key,RED_COLOUR,tr)
    else
      newroot => rbnode_t(tl,key,BLACK_COLOUR,tr)
    end if
  end function join


  recursive subroutine split(l_root, key_node, r_root, old_root, key, cfun)
    !* Split tree. Nodes with a value less than "key" go to the left subtree,
    !  and nodes with a value larger than "key" go to the right subtree.
    !
    !  If tree contains a node with the same value as "key", this node will not
    !  be included in any of the sub-trees. This node can be accessed through the
    !  returned pointer "key_node" and it can be freed or inserted to any of the
    !  subtrees as needed.
    !
    !  Tree rooted by "old_root" no longer exists after this operation !
    !
    type(rbnode_t), pointer, intent(out) :: l_root, r_root
    type(rbnode_t), intent(out), pointer :: key_node
    type(rbnode_t), pointer, intent(in) :: old_root
    integer(DATA_KIND), intent(in) :: key(:)
    procedure(compare_fun) :: cfun

    type(rbnode_t), pointer :: ltmp, rtmp, to_delete

    if (.not. associated(old_root)) then
      l_root => null()
      key_node => null()
      r_root => null()
      return
    end if

    select case(cfun(key, rbnode_read(old_root)))
    case(0) ! key == old_root%key
      l_root => old_root%left
      if (associated(l_root)) l_root%parent=>null()
      r_root => old_root%right
      if (associated(r_root)) r_root%parent=>null()

      ! the key-node will no longer be part of any sub-tree
      key_node => old_root
      key_node%left => null()
      key_node%right => null()
      key_node%parent => null()

    case(-1) ! key < old_root%key
      call split(ltmp, key_node, rtmp, old_root%left, key, cfun)
      to_delete => old_root
      l_root => ltmp
      r_root => join(rtmp, rbnode_read(old_root), old_root%right)

      ! While "join" returns a correct root node, the parent pointer of the root
      ! to the other tree must be nullified here. 
      if (associated(l_root)) l_root%parent=>null()

      ! As a new node has been allocated during "join" process, the actual
      ! root became superfluous and must be freed. 
      call rbnode_free(to_delete)

    case(+1) ! key > old_root%key
      call split(ltmp, key_node, rtmp, old_root%right, key, cfun)
      to_delete => old_root
      l_root => join(old_root%left,rbnode_read(old_root),ltmp)
      r_root => rtmp

      if (associated(r_root)) r_root%parent=>null()
      call rbnode_free(to_delete)

      !Note: there might be an error on wikipedia...
      !r_root => old_root%right

    case default
      error stop 'split: user function returned invalid value'
    end select
  end subroutine split


  recursive function union(t1, t2, cfun) result(t12)
    type(rbnode_t), pointer, intent(inout) :: t1, t2
    procedure(compare_fun) :: cfun
    type(rbnode_t), pointer :: t12

    type(rbnode_t), pointer :: l2, r2, key2, l12, r12

    if (.not. associated(t1)) then
      t12 => t2
      return
    else if (.not. associated(t2)) then
      t12 => t1
      return
    end if

    call split(l2, key2, r2, t2, rbnode_read(t1), cfun)

    ! these two branches can run independently (paralelization?)
    l12 => union(t1%left, l2, cfun)
    r12 => union(t1%right, r2, cfun)

    ! as soon as both branches are complete
    t12 => join(l12, rbnode_read(t1), r12)

    ! as root of "t1" is re-created by join, it must be removed
    ! if T2 contained the same key, it must be removed as well
    call rbnode_free(t1)
    if (associated(key2)) call rbnode_free(key2)
  end function union


  ! ================================
  ! Validation and debugging helpers
  ! ================================

  function rbbasetree_isvalid(this, cfun) result(isvalid)
    class(rbbasetree_t), intent(in) :: this
    procedure(compare_fun) :: cfun
    logical :: isvalid

    integer :: nblacks

    call rbnode_validate(this%root, cfun, isvalid, nblacks)
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


  function rbbasetree_blackheight(this) result(bh)
    class(rbbasetree_t), intent(in) :: this
    integer :: bh
    bh = rbnode_blackheight(this%root)
  end function rbbasetree_blackheight

end module rbnode_mod
