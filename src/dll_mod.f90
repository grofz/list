!TODO
!ISSUES:
! - [ ] When passing pointer to a node using type-bounded-procedures, it is not
!       enforced, that the specified node is part of the specified list.
!       How to enforce this, while avoid traversing the list?
!
!       A possible solution is to add a DEBUG mode that can be enabled/disabled 
!       that would make the checks...
!

module dll_mod
  !* Here we define the double-linked list and provide Python-like methods to operate with the
  !  list
  !
  use dllnode_mod
  implicit none
  private

  type, public :: dll_t
    private
    type(dllnode_t), pointer :: head => null()
    type(dllnode_t), pointer :: tail => null()
    integer :: n = 0
  contains
    procedure :: append => dll_append
    procedure :: clear => dll_clear
    procedure, pass(this) :: copy => dll_copy
    procedure :: count => dll_count
    procedure :: extend => dll_extend
    procedure :: index => dll_find
    procedure :: insert => dll_insert
    procedure :: pop => dll_pop
    procedure :: remove => dll_remove
    procedure :: reverse => dll_reverse
    procedure :: sort => dll_sort
    procedure :: firstnode => dll_firstnode
  end type dll_t
  interface dll_t
    module procedure dll_import
  end interface dll_t

contains

  function dll_import(arr) result(new)
    !! Make list using rank-2 array for node values
    integer(DATA_KIND), intent(in) :: arr(:,:)
    type(dll_t) :: new
error stop 'not-implemented'
  end function dll_import


  subroutine dll_append(this, value)
    !! Append an element to the end of the list
    class(dll_t), intent(inout) :: this
    integer(DATA_KIND), intent(in) :: value(:)

    type(dllnode_t), pointer :: new

    call dllnode_insertbehind(this%tail, dllnode_t(value), new)
    if (.not. associated(this%head)) this%head => new
    this%tail => new
    this%n = this%n+1
  end subroutine dll_append


  subroutine dll_clear(this)
    !! Remove elements from a list
    class(dll_t), intent(inout) :: this

    call dllnode_freechain(this%head)
    this%head => null()
    this%tail => null()
    this%n = 0
  end subroutine dll_clear


  subroutine dll_copy(copy, this)
    !! Make a copy of the list
    type(dll_t), intent(inout) :: copy
    class(dll_t), intent(in) :: this
error stop 'not-implemented'
  end subroutine dll_copy


  function dll_count(this, value) result(n)
    !! Return the number of elements with the specified value
    class(dll_t), intent(in) :: this
    integer(DATA_KIND), intent(in) :: value(:)
    integer :: n

    type(dllnode_t), pointer :: current

    n = 0
    current => this%head
    do
      if (.not. associated(current)) exit
      associate(node_value=>dllnode_read(current))
        if (all(value==node_value(1:size(value)))) n = n+1
      end associate
      current => current%gonext()
    end do
  end function dll_count


! TODO allow another list or number of elements
  subroutine dll_extend(this, items)
    !! At the list elements to the end of current list
    class(dll_t), intent(inout) :: this
    type(dll_t), intent(in) :: items
error stop 'not-implemented'
  end subroutine dll_extend


  function dll_find(this, value) result(found)
    !! Return a pointer to the first occurrence of the specified value
    class(dll_t), intent(in) :: this
    integer(DATA_KIND), intent(in) :: value(:)
    type(dllnode_t), pointer :: found

    found => dllnode_find(this%head, value)
  end function dll_find


  subroutine dll_insert(this, where, value)
    !! Insert the specified value in front of specified node
    class(dll_t), intent(inout) :: this
    type(dllnode_t), intent(in), pointer :: where
    integer(DATA_KIND), intent(in) :: value(:)

    type(dllnode_t), pointer :: output

    if (.not. associated(where)) &
        error stop 'dll_insert ERROR: where is null pointer'

    call dllnode_insertinfrontof(where, dllnode_t(value), output)
    if (associated(where,this%head)) this%head => output
    this%n = this%n + 1
  end subroutine dll_insert


  subroutine dll_pop(this, what)
    !! Remove the specified node
    class(dll_t), intent(inout) :: this
    type(dllnode_t), intent(in), pointer :: what

    type(dllnode_t), pointer :: deleted, next_in_chain

    if (.not. associated(what)) &
        error stop 'dll_pop ERROR: null pointer'
    call dllnode_remove(what, deleted, next_in_chain)
    if (associated(deleted, this%head)) this%head => next_in_chain
    if (associated(deleted, this%tail)) this%tail => next_in_chain
    this%n = this%n - 1
    call dllnode_free(deleted)
  end subroutine dll_pop


  subroutine dll_remove(this, value)
    !! Remove the first occurence of the element with the specified value
    class(dll_t), intent(inout) :: this
    integer(DATA_KIND), intent(in) :: value(:)

    type(dllnode_t), pointer :: found, deleted, next_in_chain

    found => dllnode_find(this%head, value)
    if (.not. associated(found)) return
    call dllnode_remove(found, deleted, next_in_chain)
    if (associated(deleted, this%head)) this%head => next_in_chain
    if (associated(deleted, this%tail)) this%tail => next_in_chain
    this%n = this%n - 1
    call dllnode_free(deleted)
  end subroutine dll_remove


  subroutine dll_reverse(this)
    !! Reverse the sorting order of the elements
    class(dll_t), intent(inout) :: this

    this%tail => this%head
    this%head => dllnode_reverse(this%head)
  end subroutine dll_reverse


  subroutine dll_sort(this, cfun)
    !! Sort the list using the provided comparison function `cfun`
    class(dll_t), intent(inout) :: this
    procedure(compare_fun) :: cfun

    this%head => dllnode_mergesort(this%head, cfun)
    this%tail => dllnode_tail(this%head)
  end subroutine dll_sort


  function dll_firstnode(this) result(head)
    class(dll_t), intent(in) :: this
    type(dllnode_t), pointer :: head
    head => this%head
  end function dll_firstnode

  
end module dll_mod
