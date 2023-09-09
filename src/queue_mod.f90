module queue_mod
  !* Implementation of queue via double-linked list
  !
  use common_mod, only : mold, DATA_KIND
  use dllnode_mod, only : dllnode_t, dllnode_count, dllnode_insertinfrontof,   &
    dllnode_read, dllnode_remove, dllnode_free, dllnode_validate, dllnode_tail,&
    dllnode_freechain
  implicit none
  private

  type, public :: queue_t
    private
    type(dllnode_t), pointer :: back => null()
    type(dllnode_t), pointer :: front => null()
  contains
    procedure :: enqueue => queue_enqueue, dequeue => queue_dequeue
    procedure :: isempty => queue_isempty, size => queue_size
    procedure :: isvalid => queue_validate
    final :: queue_finalize
  end type

contains

  function queue_isempty(this) result(isempty)
    class(queue_t), intent(in) :: this
    logical :: isempty
    isempty = .not. associated(this%back)
  end function queue_isempty


  function queue_size(this) result(n)
    class(queue_t), intent(in) :: this
    integer :: n
    n = dllnode_count(this%back)
  end function queue_size


  subroutine queue_enqueue(this, val)
    class(queue_t), intent(inout) :: this
    integer(DATA_KIND), intent(in) :: val(:)

    type(dllnode_t), pointer :: output

    call dllnode_insertinfrontof(this%back, dllnode_t(val), output)
    this%back => output
    if (.not. associated(output%nextnode())) this%front => output
  end subroutine queue_enqueue


  function queue_dequeue(this) result(val)
    class(queue_t), intent(inout) :: this
    integer(DATA_KIND), allocatable :: val(:)

    type(dllnode_t), pointer :: deleted, next

    if (.not. associated(this%front)) error stop 'dequeue - queue is empty'
    val = dllnode_read(this%front)

    call dllnode_remove(this%front, deleted, next)
    this%front => next
    call dllnode_free(deleted)
    if (.not. associated(next)) this%back => null()
  end function queue_dequeue


  elemental impure subroutine queue_finalize(this)
    type(queue_t), intent(inout) :: this
    call dllnode_freechain(this%back)
    this%front => null()
  end subroutine queue_finalize


  function queue_validate(this) result(isvalid)
    class(queue_t), intent(in) :: this
    logical :: isvalid

    ! both pointers must be either associated or null
    isvalid = associated(this%back) .eqv. associated(this%front)
    if (.not. isvalid) return

    ! linked-list starting at the back of the queue must be valid
    isvalid = dllnode_validate(this%back)
    if (.not. isvalid) return

    ! tail of linked-list is the front of the queue
    if (associated(this%back)) &
        isvalid = associated(this%front, dllnode_tail(this%back))
  end function queue_validate

end module queue_mod