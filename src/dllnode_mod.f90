! Defines "node" type and basic operations with the chain of 
! nodes.
!
module dllnode_mod
  use iso_fortran_env, only : int64
  implicit none
  private

  integer, parameter, public :: &
    DATA_KIND=int64, &
    DATA_SIZE=2

  type, public :: dllnode_t
    private
    integer(kind=DATA_KIND) :: data(DATA_SIZE)
    type(dllnode_t), pointer :: next => null()
    type(dllnode_t), pointer :: prev => null()
  contains
    procedure :: gonext, goprev
  end type dllnode_t
  interface dllnode_t
    module procedure dllnode_new
    module procedure dllnode_import
    module procedure dllnode_copy
  end interface

  type(dllnode_t) :: mold

  public dllnode_update, dllnode_read, dllnode_free
  public dllnode_count, dllnode_export
  public dllnode_insertinfrontof, dllnode_remove, dllnode_freechain
  public dllnode_find, dllnode_head, dllnode_tail

contains

  ! ==========================
  ! Next and previous (TBP's)
  ! ==========================
  function goprev(this)
    class(dllnode_t), intent(in) :: this
    type(dllnode_t), pointer :: goprev
    goprev => this%prev
  end function goprev

  function gonext(this)
    class(dllnode_t), intent(in) :: this
    type(dllnode_t), pointer :: gonext
    gonext => this%next
  end function gonext


  ! ================================
  ! Allocate new node (CONSTRUCTOR)
  ! Update node data
  ! Read data from node
  ! Deallocate the node
  ! ================================

  function dllnode_new(data) result(new)
    integer(DATA_KIND), intent(in) :: data(:)
    type(dllnode_t), pointer :: new

    integer :: ierr

    if (size(data,1)/=size(mold%data,1)) &
        error stop 'dllnode_new ERROR: input array size is wrong'
    allocate(new, stat=ierr)
    if (ierr /= 0) &
        error stop 'dllnode_new ERROR: could not allocate node'
    new%data = data
    new%prev => null()
    new%next => null()
  end function dllnode_new


  subroutine dllnode_update(node,data)
    type(dllnode_t), intent(in), pointer :: node
    integer(DATA_KIND), intent(in) :: data(:)

    if (size(data,1)/=size(mold%data,1)) &
        error stop 'dllnode_update ERROR: input array size is wrong'
    if (.not. associated(node)) &
        error stop 'dllnode_update ERROR: node is null'
    node%data = data
  end subroutine dllnode_update


  function dllnode_read(node) result(data)
    type(dllnode_t), intent(in), pointer :: node
    integer(DATA_KIND) :: data(size(mold%data))
    if (.not. associated(node)) &
        error stop 'dllnode_read ERROR: node is null'
    data = node%data
  end function dllnode_read


  subroutine dllnode_free(deleted)
    type(dllnode_t), pointer, intent(inout) :: deleted

    integer :: ierr

    if (.not. associated(deleted)) &
        error stop 'dllnode_free ERROR: null pointer'
    deallocate(deleted, stat=ierr)
    if (ierr /= 0) &
        error stop 'dllnode_free ERROR: deallocation failed'
  end subroutine dllnode_free


  ! ========================================
  ! Count nodes in the chain
  ! Export nodes to array
  ! Import nodes from array (CONSTRUCTOR)
  ! Make a copy of the chain (CONSTRUCTOR)
  ! Insert node to the chain
  ! Remove node from the chain
  ! Remove and deallocate nodes in the chain
  ! ========================================

  function dllnode_count(head) result(n)
    type(dllnode_t), pointer, intent(in) :: head
    integer :: n

    type(dllnode_t), pointer :: current
    current => head
    n = 0
    do
      if (.not. associated(current)) exit
      n = n + 1
      current => current%next
    end do
  end function dllnode_count


  function dllnode_export(head) result(arr)
    type(dllnode_t), pointer, intent(in) :: head
    integer(DATA_KIND), allocatable :: arr(:,:)

    integer :: i, n
    type(dllnode_t), pointer :: current

    n = dllnode_count(head)
    allocate(arr(size(mold%data,1),n))
    current => head
    do i = 1, n
      if (.not. associated(current)) &
          error stop 'dllnode_export ERROR: unexpected end of chain'
      arr(:,i) = current%data
      current => current%next
    end do
  end function dllnode_export


  function dllnode_import(arr) result(head)
    integer(DATA_KIND), intent(in) :: arr(:,:)
    type(dllnode_t), pointer :: head

    integer :: i, n
    type(dllnode_t), pointer :: head1

    if (size(arr,1)/=size(mold%data,1)) &
        error stop 'dllnode_import ERROR: input array rows count is wrong'
    n = size(arr,2)
    head => null()
    do i=n,1,-1
      call dllnode_insertinfrontof(head, dllnode_new(arr(:,i)), head1)
      head => head1
    end do
  end function dllnode_import


  function dllnode_copy(oldhead) result(newhead)
    type(dllnode_t), pointer, intent(in) :: oldhead
    type(dllnode_t), pointer :: newhead
!
! Make a new list that is the copy of the chain starting with
! item "oldhead"
!
    type(dllnode_t), pointer :: current, head1

    newhead => null()
    current => dllnode_tail(oldhead)
    do
      if (.not. associated(current)) exit
      call dllnode_insertinfrontof( &
          newhead, dllnode_new(dllnode_read(current)), head1)
      newhead => head1
      if (associated(current, oldhead)) exit
      current => current % prev
    end do
    if (associated(oldhead)) then
      if (.not. associated(current, oldhead)) &
          error stop 'dllnode_copy ERROR: old-node was not reached'
    end if
  end function dllnode_copy


  subroutine dllnode_insertinfrontof(where, new, output)
    type(dllnode_t), pointer, intent(in) :: where, new
    type(dllnode_t), pointer, intent(out), optional :: output
!
! Insert "new" in front of "where".
! Optional "output" points to the inserted node
!
    if (present(output)) output => new
    if (associated(new%prev) .or. associated(new%next)) &
        error stop 'dll_insertinfrontof ERROR: inserted node is not a single node'
    if (.not. associated(where)) return

    ! the chain before
    !  PREV -> WHERE 
    !      <-          :- NEW -: 
    ! the chain after
    !  PREV -4> NEW -2> WHERE
    !       <1-     <3-
    new%prev => where%prev    ! (1)
    new%next => where         ! (2)
    where%prev => new         ! (3)
    if (associated(new%prev)) new%prev%next => new ! (4)
  end subroutine dllnode_insertinfrontof


  subroutine dllnode_remove(what, deleted, next_in_chain)
    type(dllnode_t), pointer, intent(in) :: what
    type(dllnode_t), pointer, intent(out) :: deleted, next_in_chain
!
! Remove "what" from chain. On return, "deleted" points to the
! removed node and must be dealocated else-where, 
! "next_in_chain" points preferentialy to the next node
! (if it exists), or to the prev node, or to null.
!
    deleted => what
    next_in_chain => null()
    if (.not. associated(what)) return

    if (associated(what%prev)) what%prev%next => what%next
    if (associated(what%next)) what%next%prev => what%prev
    if (associated(what%next)) then
      next_in_chain => what%next
    else
      next_in_chain => what%prev
    end if
  end subroutine dllnode_remove


  subroutine dllnode_freechain(first)
    type(dllnode_t), intent(inout), pointer :: first
!
! Remove and deallocate whole chain starting with "first"
! The NEXT pointer of an node in front of "first" is also
! modified
!
    type(dllnode_t), pointer :: deleted

    if (.not. associated(first)) return

    if (associated(first%prev)) first%prev%next => null()

    do
      deleted => first
      first => first%next
      call dllnode_free(deleted)
      if (.not. associated(first)) exit
    end do
  end subroutine dllnode_freechain


  ! ===============================
  ! Search for a particular node
  ! Move to the head of the chain
  ! Move to the tail of the chain
  ! ===============================

  function dllnode_find(start, value) result(found)
    type(dllnode_t), pointer, intent(in) :: start
    integer(DATA_KIND), intent(in) :: value(:)
    type(dllnode_t), pointer :: found

    type(dllnode_t), pointer :: current

    if (size(value,1) /= size(mold%data,1)) &
        error stop 'dllnode_find ERROR: wrong array size'
    current => start
    found => null()
    do
      if (.not. associated(current)) exit
      if (all(dllnode_read(current)==value)) then
        found => current
        exit
      end if
      current => current%next
    end do
  end function dllnode_find
  

  function dllnode_head(start) result(head)
    type(dllnode_t), pointer, intent(in) :: start
    type(dllnode_t), pointer :: head

    head => start
    if (.not. associated(head)) return
    do
      if (.not. associated(head%prev)) exit
      head => head%prev
    end do
  end function dllnode_head


  function dllnode_tail(start) result(tail)
    type(dllnode_t), pointer, intent(in) :: start
    type(dllnode_t), pointer :: tail

    tail => start
    if (.not. associated(tail)) return
    do
      if (.not. associated(tail%next)) exit
      tail => tail%next
    end do
  end function dllnode_tail

end module dllnode_mod