module rbtrnode_mod
  !*
  !
  !
  use common_mod, only : DATA_KIND, mold, compare_fun
  use iso_fortran_env, only : int8
  implicit none
  private

  type, public :: rbtrnode_t
    !! Red-black tree node
    private
    integer(kind=DATA_KIND), allocatable :: dat(:)
    logical(int8) :: isblack = .false.
    type(rbtrnode_t), pointer :: left => null()
    type(rbtrnode_t), pointer :: right => null()
    type(rbtrnode_t), pointer :: parent => null()
  contains
    procedure :: is_node_black
  end type rbtrnode_t
  interface rbtrnode_t 
    module procedure rbtrnode_new
    !module procedure rbtrnode_import
    !module procedure rbtrnode_copy
  end interface

  integer, parameter :: LEFT_CHILD=1, RIGHT_CHILD=2, NO_PARENT=0

  integer, parameter :: MAX_SAFE_DEPTH=1000000
    !! Defensive test to avoid infinite loops in the code

  public rbtrnode_insert
  public rbtrnode_leftmost, rbtrnode_nextnode, rbtrnode_prevnode
  public rbtrnode_read, rbtrnode_free
  public rbtrnode_validate

contains

  ! ===============
  ! Simple GETTER's
  ! ===============
  logical function is_node_black(this)
    class(rbtrnode_t), intent(in) :: this
    is_node_black = this%isblack
  end function


  ! ================================
  ! Allocate new node (CONSTRUCTOR)
  ! Update node data (???)
  ! Read data from node
  ! Deallocate node
  ! ================================

  function rbtrnode_new(dat) result(new)
    !! Allocate new node, fill it with data, result pointer
    integer(DATA_KIND), intent(in) :: dat(:)
    type(rbtrnode_t), pointer :: new

    integer :: ierr

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
  end function rbtrnode_new


  subroutine rbtrnode_update(node, newdata)
    !! Update the data content of the node by newdata
    !! TODO may invalidate the red-black tree!!!
    type(rbtrnode_t), intent(in), pointer :: node
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
  end subroutine rbtrnode_update


  function rbtrnode_read(node) result(dat)
    !! Return node data
    type(rbtrnode_t), intent(in), pointer :: node
    integer(DATA_KIND), allocatable :: dat(:)
    if (.not. associated(node)) &
        error stop 'could not read: node is null'
    if (.not. allocated(node%dat)) &
        error stop 'could not read: node contains no data'
    allocate(dat(size(node%dat)))
    dat = node%dat
  end function rbtrnode_read


  subroutine rbtrnode_free(deleted)
    !! Deallocae the node from memory
    type(rbtrnode_t), pointer, intent(inout) :: deleted

    integer :: ierr

    if (.not. associated(deleted)) &
        error stop 'could not free node: null pointer'
    if (allocated(deleted%dat)) deallocate(deleted%dat, stat=ierr)
    if (ierr /= 0) &
        error stop 'could not free node: data deallocation failed'
    deallocate(deleted, stat=ierr)
    if (ierr /= 0) &
        error stop 'could not free node: node deallocation failed'
  end subroutine rbtrnode_free


  ! =================================================
  ! Get grandparent, sibling and uncle. 
  ! Tree rotations
  ! In-order traversal
  ! =================================================

  function rbtrnode_whichchild(node) result(we)
    type(rbtrnode_t), intent(in), pointer :: node
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
  end function rbtrnode_whichchild


  function rbtrnode_isblack(node) result(isblack)
    !! Allow to query also null nodes (they are assumed black)
    type(rbtrnode_t), pointer, intent(in) :: node
    logical :: isblack
    isblack = .true.
    if (associated(node)) isblack = node%isblack
  end function rbtrnode_isblack


  function rbtrnode_grandparent(node) result(grandparent)
    type(rbtrnode_t), intent(in), pointer :: node
    type(rbtrnode_t), pointer :: grandparent

    if (.not. associated(node)) &
        error stop 'grandparent: null node as input'

    if (associated(node%parent)) then
      grandparent => node%parent%parent
    else
      grandparent => null()
    end if
  end function rbtrnode_grandparent


  function rbtrnode_sibling(node, node_is_which) result(sibling)
    type(rbtrnode_t), intent(in), pointer :: node
    integer, intent(out), optional :: node_is_which
    type(rbtrnode_t), pointer :: sibling

    if (.not. associated(node)) &
        error stop 'sibling: null node as input'

    associate (we => rbtrnode_whichchild(node))
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
  end function rbtrnode_sibling


  function rbtrnode_uncle(node, parent_is_which) result(uncle)
    type(rbtrnode_t), intent(in), pointer :: node
    integer, intent(out), optional :: parent_is_which
    type(rbtrnode_t), pointer :: uncle

    if (.not. associated(node)) &
        error stop 'uncle: null node as input'

    if (associated(node%parent)) then
      uncle => rbtrnode_sibling(node%parent, parent_is_which)
    else
      uncle => null()
      if (present(parent_is_which)) parent_is_which=NO_PARENT
    end if
  end function rbtrnode_uncle


  subroutine rehang_tree(oldchild, newchild, root)
    type(rbtrnode_t), pointer, intent(inout) :: root
    type(rbtrnode_t), pointer, intent(in) :: oldchild, newchild

    type(rbtrnode_t), pointer :: parent

    ! Old child (pivot) changed place with new child (rotator) during rotation,
    ! repair the parent --> child link.
    ! The other way link (child --> parent) should have been repointed during
    ! rotation operation
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
      if (.not. associated(root, oldchild)) &
          error stop 'rehang_tree - unexpected association of root pointer'
      root => newchild
    end if

  end subroutine rehang_tree


  subroutine rotate_left(piv, rot)
    !! Rotate left.
    !! Fails if pivot is leaf or if pivot's right child (rotator) is leaf
    !! Remember: pivot's parent must be relinked to rotator outside!
    type(rbtrnode_t), intent(in), pointer :: piv
    type(rbtrnode_t), intent(out), pointer :: rot

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
  end subroutine rotate_left


  subroutine rotate_right(piv, rot)
    !! Rotate right.
    !! Fails if pivot is leaf or if pivot's left child (rotator) is leaf
    !! Remember: pivot's parent must be relinked to rotator outside!
    type(rbtrnode_t), intent(in), pointer :: piv
    type(rbtrnode_t), intent(out), pointer :: rot

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
  end subroutine rotate_right


  function rbtrnode_leftmost(node) result(leftmost)
    type(rbtrnode_t), intent(in), pointer :: node
    type(rbtrnode_t), pointer :: leftmost

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
  end function rbtrnode_leftmost


  function rbtrnode_rightmost(node) result(rightmost)
    type(rbtrnode_t), intent(in), pointer :: node
    type(rbtrnode_t), pointer :: rightmost

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
  end function rbtrnode_rightmost


  function rbtrnode_nextnode(node) result(successor)
    !! Return next node, or null pointer if node is the last node
    type(rbtrnode_t), intent(in), pointer :: node
    type(rbtrnode_t), pointer :: successor

    type(rbtrnode_t), pointer :: child
    integer :: i

    if (.not. associated(node)) &
        error stop 'nextnode: input is a null node'

    ! If node has a right sub-tree, the next node is the leftmost node in this
    ! sub-tree
    if (associated(node%right)) then
      successor => rbtrnode_leftmost(node%right)
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

      select case(rbtrnode_whichchild(child))
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
  end function rbtrnode_nextnode


  function rbtrnode_prevnode(node) result(predecessor)
    !! Return prev node, or null pointer if node is the first node
    type(rbtrnode_t), intent(in), pointer :: node
    type(rbtrnode_t), pointer :: predecessor

    type(rbtrnode_t), pointer :: child
    integer :: i

    if (.not. associated(node)) &
        error stop 'prevnode: input is a null node'

    ! If node has a left sub-tree, the next node is the rightmost node in this
    ! sub-tree
    if (associated(node%left)) then
      predecessor => rbtrnode_rightmost(node%left)
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

      select case(rbtrnode_whichchild(child))
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
  end function rbtrnode_prevnode


  ! =========
  ! Insertion
  ! =========

  subroutine rbtrnode_insert(root, new, cfun, ierr, new_output)
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
    type(rbtrnode_t), intent(inout), pointer :: root
    type(rbtrnode_t), intent(in), pointer :: new
    procedure(compare_fun) :: cfun
      !! A < B -> -1, A == B -> 0, A > B -> 1
    integer, intent(out), optional :: ierr
      !! on exit: 0...insertion ok
      !!          1...duplicate in tree, node not inserted
      !!          2...duplicate in tree, node was deallocated automatically
    type(rbtrnode_t), pointer, optional :: new_output
      !! pointer to the new node so it can be deallocated by user, if inserion
      !! failed
    integer, parameter :: FLAG_OK=0, FLAG_DUPLICATE=1, FLAG_DUPLICATE_FREED=2

    integer :: i, ierr0, which_child
    type(rbtrnode_t), pointer :: new_local, finger

    ! assert the new node is isolated
    if (associated(new%parent) .or. associated(new%left) .or. associated(new%right))&
        error stop 'insert: new node must be alone'

    new_local => new
    if (present(new_output)) new_output => new
    ierr0 = FLAG_OK

    if (.not. associated(root)) then
      ! insert a first node to an empty tree
      root => new_local
      which_child = NO_PARENT

    else
      ! find place where new node will be inserted
      finger => root
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
        call rbtrnode_free(new_local)
        ierr0 = FLAG_DUPLICATE_FREED
      end if
    else
      error stop 'insert: unreachable branch'
    end if
    if (present(ierr)) ierr = ierr0

    ! Repair red-black tree
    if (ierr0==FLAG_OK) then
      new_local%isblack = .false. ! inserted node is red
      call insert_repair(new_local, root)
    end if
  end subroutine rbtrnode_insert


  recursive subroutine insert_repair(n, root)
    type(rbtrnode_t), intent(inout), pointer :: n, root

    type(rbtrnode_t), pointer :: p, u, g, rot
    logical :: uncle_exists, uncle_is_red

    if (.not. associated(n)) &
      error stop 'insert_repair: n is null node'

    p => n%parent
    MAIN: if (.not. associated(p)) then
      ! CASE I - root node must be black
      n%isblack = .true.
      if (.not. associated(root,n)) &
          error stop 'node n seems to be root, but root points elsewhere'

    else if (p%isblack) then MAIN
      ! CASE II - nothing has to be done
      continue

    else MAIN
      ! parent is red
      u => rbtrnode_uncle(n)
      uncle_exists = associated(u)
      uncle_is_red = .false.
      if (uncle_exists) uncle_is_red = .not. u%isblack

      RED_PARENT: if (uncle_exists .and. uncle_is_red) then
        ! CASE III - repaint parent and uncle black and repair grandparent
        g => rbtrnode_grandparent(n)
        p%isblack = .true.
        u%isblack = .true.
        g%isblack = .false.
        call insert_repair(g, root)
      else RED_PARENT

        ! CASE IV - parent is red and uncle is black
        g => rbtrnode_grandparent(n)

        ! case 4, step 1
        if (rbtrnode_whichchild(n) == RIGHT_CHILD .and. &
            rbtrnode_whichchild(p) == LEFT_CHILD) then

           call rotate_left(p, rot)
if (.not. associated(rot,n)) error stop 'repair: unexpected'
           call rehang_tree(p, rot, root)
           n => p

        else if (rbtrnode_whichchild(n) == LEFT_CHILD .and. &
                 rbtrnode_whichchild(p) == RIGHT_CHILD) then

           call rotate_right(p, rot)
if (.not. associated(rot,n)) error stop 'repair: unexpected'
           call rehang_tree(p, rot, root)
           n => p
        end if

        ! case 4, step 2
        p => n%parent
        g => rbtrnode_grandparent(n)

        if (rbtrnode_whichchild(n)==LEFT_CHILD) then
          call rotate_right(g, rot)
          call rehang_tree(g, rot, root)

        else if (rbtrnode_whichchild(n)==RIGHT_CHILD) then
          call rotate_left(g, rot)
          call rehang_tree(g, rot, root)

        else
          error stop 'case 4, part 2, unreachable branch'
        end if
        p%isblack = .true.
        g%isblack = .false.
      end if RED_PARENT

    end if MAIN

  end subroutine insert_repair


  ! ================================
  ! Validation and debugging helpers
  ! ================================

  recursive subroutine rbtrnode_validate(root, cfun, isvalid, nblacks)
    type(rbtrnode_t), intent(in), pointer :: root
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
    call rbtrnode_validate(root%left, cfun, isvalid_left, nblacks_left)
    call rbtrnode_validate(root%right, cfun, isvalid_right, nblacks_right)
    isvalid = isvalid_left .and. isvalid_right

    ! assert red-node has only black children
    if (.not. root%isblack) isvalid = isvalid .and. &
        rbtrnode_isblack(root%left) .and. rbtrnode_isblack(root%right)

    ! assert children point back at parent node
    if (associated(root%left))  isvalid = isvalid .and. associated(root%left%parent, root)
    if (associated(root%right)) isvalid = isvalid .and. associated(root%right%parent, root)

    ! assert that number of black nodes is same in both sub-trees
    isvalid = isvalid .and. (nblacks_left == nblacks_right)
    nblacks = nblacks_left
    if (rbtrnode_isblack(root)) nblacks = nblacks + 1

    ! assert binary search tree data are in correct order
    isvalid_bst = .true.
    if (associated(root%left)) isvalid_bst = isvalid_bst .and. &
        cfun(rbtrnode_read(root%left), rbtrnode_read(root)) == -1
    if (associated(root%right)) isvalid_bst = isvalid_bst .and. &
        cfun(rbtrnode_read(root), rbtrnode_read(root%right)) == -1
    isvalid = isvalid .and. isvalid_bst

  end subroutine rbtrnode_validate

end module rbtrnode_mod