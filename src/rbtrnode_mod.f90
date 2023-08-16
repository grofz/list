module rbtrnode_mod
  !*
  !
  !
  use common_mod, only : DATA_KIND, mold, compare_fun
  implicit none
  private

  type, public :: rbtrnode_t
    !! Red-black tree node
    private
    integer(kind=DATA_KIND), allocatable :: dat(:)
    type(rbtrnode_t), pointer :: left => null()
    type(rbtrnode_t), pointer :: right => null()
    type(rbtrnode_t), pointer :: parent => null()
  contains
  end type rbtrnode_t
  interface rbtrnode_t 
    module procedure rbtrnode_new
    !module procedure rbtrnode_import
    !module procedure rbtrnode_copy
  end interface

  integer, parameter :: LEFT_CHILD=1, RIGHT_CHILD=2, NO_PARENT=0

  integer, parameter :: MAX_SAFE_DEPTH=1000000
    !! Defensive test to avoid infinite loops in the code

contains

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
      if (.not. associated(node%left)) exit
      leftmost => node%left
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
      if (.not. associated(node%right)) exit
      rightmost => node%right
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

end module rbtrnode_mod