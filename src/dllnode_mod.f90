module dllnode_mod
  !* Here we define the **double-linked list node** and provide basic
  !  operations with the node or a chain of nodes.
  !
  !  The data in the list are stored as a fixed size array of integers, but
  !  any type can be stored if user wraps the data (i.e. via using
  !  `transfer`function).
  !
  use iso_fortran_env, only : int64
  implicit none
  private

  integer, parameter, public :: DATA_KIND=int64
  !!  Kind of integer array to store node data
  integer, parameter, public :: DATA_MAXSIZE=4
  !!  Size of integer array to store node data (modify if necessary)

  integer(DATA_KIND), public :: mold(DATA_MAXSIZE)
  !* This variable can be used as _mold_ argument in `transfer` function
  !  to cast the user type variable to the type accepted in argument of
  !  `dllnode_*` subroutines and functions

  type, public :: dllnode_t
    !! Double-linked list node
    private
    integer(kind=DATA_KIND) :: data(DATA_MAXSIZE) = 0
    type(dllnode_t), pointer :: next => null()
    type(dllnode_t), pointer :: prev => null()
  contains
    procedure :: gonext, goprev
  end type dllnode_t
  interface dllnode_t
    !* Use the following argument in the constructor to:
    !
    !   * rank-1 array to return a single node
    !
    !   * rank-2 array to return a chain of nodes
    !
    !   * pointer to the head-node makes to return a copy of the chain
    module procedure dllnode_new
    module procedure dllnode_import
    module procedure dllnode_copy
  end interface

  abstract interface
    function compare_fun(adat, bdat) result(ires)
      !* An user function to compare value of two nodes and return:
      !
      ! * -1 if A is less than B;
      !
      ! *  0 if A equals B;
      !
      ! * +1 if A is greater than B
      import :: DATA_KIND, mold
      implicit none
      integer(DATA_KIND), dimension(size(mold)), intent(in) :: adat, bdat
      integer :: ires
    end function
  end interface
  public compare_fun

  public dllnode_update, dllnode_read, dllnode_free
  public dllnode_count, dllnode_export
  public dllnode_insertinfrontof, dllnode_insertbehind
  public dllnode_remove, dllnode_freechain, dllnode_reverse
  public dllnode_find, dllnode_head, dllnode_tail, dllnode_validate
  public dllnode_mergesort

contains

  ! ==========================
  ! Next and previous (TBP's)
  ! ==========================
  function goprev(this)
    !! Return the pointer to the next node in the list
    class(dllnode_t), intent(in) :: this
    type(dllnode_t), pointer :: goprev
    goprev => this%prev
  end function goprev

  function gonext(this)
    !! Return the pointer to the previous node in the list
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
    !! Allocate new node, fill it with data, return the pointer to the new node
    integer(DATA_KIND), intent(in) :: data(:)
    type(dllnode_t), pointer :: new

    integer :: ierr

    if (size(data,1) > size(mold,1)) &
        error stop 'dllnode_new ERROR: input array size is too big to fit'
    allocate(new, stat=ierr)
    if (ierr /= 0) &
        error stop 'dllnode_new ERROR: could not allocate node'
    new%data(1:size(data)) = data
    new%prev => null()
    new%next => null()
  end function dllnode_new


  subroutine dllnode_update(node, data)
    !! Update the data content of the node by data
    type(dllnode_t), intent(in), pointer :: node
    integer(DATA_KIND), intent(in) :: data(:)

    if (size(data,1) > size(mold,1)) &
        error stop 'dllnode_update ERROR: input array size is too big to fit'
    if (.not. associated(node)) &
        error stop 'dllnode_update ERROR: node is null'
    node%data(1:size(data)) = data
  end subroutine dllnode_update


  function dllnode_read(node) result(data)
    !! Return the node data
    type(dllnode_t), intent(in), pointer :: node
    integer(DATA_KIND) :: data(size(mold))
    if (.not. associated(node)) &
        error stop 'dllnode_read ERROR: node is null'
    data = node%data
  end function dllnode_read


  subroutine dllnode_free(deleted)
    !! Dealocate the node from memory
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
  ! Reverse the chain
  ! ========================================

  function dllnode_count(head) result(n)
    !! Return the number of nodes starting with **head** node and traversing
    !! the chain forward
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
    !! Return rank-2 array with the data from all nodes starting with **head**
    !! and traversing the chain forward
    type(dllnode_t), pointer, intent(in) :: head
    integer(DATA_KIND), allocatable :: arr(:,:)

    integer :: i, n
    type(dllnode_t), pointer :: current

    n = dllnode_count(head)
    allocate(arr(size(mold,1),n))
    current => head
    do i = 1, n
      if (.not. associated(current)) &
          error stop 'dllnode_export ERROR: unexpected end of chain'
      !!!arr(:,i) = current%data
      arr(:,i) = dllnode_read(current)
      current => current%next
    end do
  end function dllnode_export


  function dllnode_import(arr) result(head)
    !! Make a new chain of nodes with data from rank-2 array, return the
    !! pointer to the head of the chain
    integer(DATA_KIND), intent(in) :: arr(:,:)
    type(dllnode_t), pointer :: head

    integer :: i, n
    type(dllnode_t), pointer :: head1

    if (size(arr,1) /= size(mold,1)) &
        error stop 'dllnode_import ERROR: input array rows are wrong number'
    n = size(arr,2)
    head => null()
    do i=n,1,-1
      call dllnode_insertinfrontof(head, dllnode_new(arr(:,i)), head1)
      head => head1
    end do
  end function dllnode_import


  function dllnode_copy(oldhead) result(newhead)
    !! Make a new list that is a copy of the chain starting with **oldhead**
    !! and traversing the chain forwards
    type(dllnode_t), pointer, intent(in) :: oldhead
    type(dllnode_t), pointer :: newhead
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
    !! Insert node **new** in front of node **where**.
    !! Optional **output** points to the inserted node in the chain
    type(dllnode_t), pointer, intent(in) :: where, new
    type(dllnode_t), pointer, intent(out), optional :: output

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


  subroutine dllnode_insertbehind(where, new, output)
    !! Insert node **new** behind node **where**.
    !! Optional **output** points to the inserted node in the chain
    type(dllnode_t), pointer, intent(in) :: where, new
    type(dllnode_t), pointer, intent(out), optional :: output

    if (present(output)) output => new
    if (associated(new%prev) .or. associated(new%next)) &
        error stop 'dll_insertbehind ERROR: inserted node is not a single node'

    if (.not. associated(where)) return

    new%next => where%next
    new%prev => where
    where%next => new
    if (associated(new%next)) new%next%prev => new
  end subroutine dllnode_insertbehind


  subroutine dllnode_remove(what, deleted, next_in_chain)
    !! Remove **what** from chain. On return, **deleted** points to the
    !! removed node, the node must be dealocated else-where. 
    !! Pointer **next_in_chain** points preferentialy to the next node
    !! (if it exists), or to the prev node, or to null.
    type(dllnode_t), pointer, intent(in) :: what
    type(dllnode_t), pointer, intent(out) :: deleted, next_in_chain
 
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
    !! Remove and deallocate the whole chain starting with **first**
    !! The NEXT pointer of a node in front of **first** is also modified
    type(dllnode_t), intent(inout), pointer :: first
 
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


  function dllnode_reverse(head) result(newhead)
    !! Reverse the double-linked list, return pointer to the new head
    type(dllnode_t), intent(in), pointer :: head
    type(dllnode_t), pointer :: newhead

    type(dllnode_t), pointer :: current, temp

    ! Ref: https://www.geeksforgeeks.org/reverse-a-doubly-linked-list/
    temp => null()
    current => head
    do
      if (.not. associated(current)) exit
      temp => current%prev
      current%prev => current%next
      current%next => temp
      current => current%prev
    end do

    ! Do not change head if the list is empty or contains just one node
    if (.not. associated(temp)) then
      newhead => head
    else
      newhead => temp%prev
    end if
  end function dllnode_reverse


  ! ===============================
  ! Search for a particular node
  ! Move to the head of the chain
  ! Move to the tail of the chain
  ! Validate the chain
  ! ===============================

  function dllnode_find(start, value) result(found)
    !! Traverse the chain forward from **start** node. Return pointer to the
    !! node that matches the **value** or null if the search failed.
    type(dllnode_t), pointer, intent(in) :: start
    integer(DATA_KIND), intent(in) :: value(:)
    type(dllnode_t), pointer :: found

    type(dllnode_t), pointer :: current

    if (size(value,1) > size(mold,1)) &
        error stop 'dllnode_find ERROR: array size too big'
    current => start
    found => null()
    do
      if (.not. associated(current)) exit
      associate(node_data=>dllnode_read(current))
        !if (all(dllnode_read(current)==value)) then
        if (all(node_data(1:size(value))==value)) then
          found => current
          exit
        end if
      end associate
      current => current%next
    end do
  end function dllnode_find
  

  function dllnode_head(start) result(head)
    !! Return the pointer to the node at the head of the chain
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
    !! Return the pointer to the node at the end of the chain
    type(dllnode_t), pointer, intent(in) :: start
    type(dllnode_t), pointer :: tail

    tail => start
    if (.not. associated(tail)) return
    do
      if (.not. associated(tail%next)) exit
      tail => tail%next
    end do
  end function dllnode_tail


  function dllnode_validate(head) result(isvalid)
    !! Verify that the double-linked list at **head** is valid
    type(dllnode_t), pointer, intent(in) :: head
    logical :: isvalid

    type(dllnode_t), pointer :: current

    ! Empty list is valid
    isvalid = .true.
    if (.not. associated(head)) return

    ! Head node must not have a previous node
    if (associated(head%prev)) isvalid = .false.
    if (.not. isvalid) return

    ! Next node must have a back-link to current node
    current => head
    do
      if (.not. associated(current%next)) exit
      if (.not. associated(current%next%prev, current)) then
        isvalid = .false.
        return
      end if
      current => current%next
    end do
  end function dllnode_validate


  ! =====================================================================
  ! Sort the list
  ! Ref: https://www.geeksforgeeks.org/merge-sort-for-doubly-linked-list
  ! =====================================================================

  recursive function dllnode_mergesort(head, cfun) result(sortedhead)
    !! Sort the list starting at **head**, return a new head pointer
    !! **cfun** is a function that returns -1|0|1 based on the comparison
    !! of two nodes.
    type(dllnode_t), intent(in), pointer :: head
    procedure(compare_fun) :: cfun

    type(dllnode_t), pointer :: sortedhead

    type(dllnode_t), pointer :: headone, headtwo

    ! zero- or one-sized list is sorted
    sortedhead => head
    if (.not. associated(head)) return
    if (.not. associated(head%next)) return

    ! split into two and sort left and right halves recursively
    headtwo => split(head)
    headone => dllnode_mergesort(head, cfun)
    headtwo => dllnode_mergesort(headtwo, cfun)

    ! merge sorted halves
    sortedhead => merge0(headone, headtwo, cfun)
  end function dllnode_mergesort


  function split(head) result(two)
    !! Split the chain in the middle, eg. 1|1, 2|1, 2|2, 3|2, etc.,
    !! and return pointer at the second half
    type(dllnode_t), intent(in), pointer :: head
    type(dllnode_t), pointer :: two

    type(dllnode_t), pointer :: fast, slow

    fast => head
    slow => head
    do
      ! it is assummed the chain has two or more nodes,
      ! therefore the loop will run at least once
      if (.not. associated(fast%next)) exit
      if (.not. associated(fast%next%next)) exit
      fast => fast%next%next
      slow => slow%next
    end do
    ! "slow" now points at the middle-node (odd number of nodes) or
    ! at the node before middle of the chain (even number of nodes)
    ! "slow" is therefore the last node of the first half
    two => slow%next
    slow%next => null()
  end function split


  recursive function merge0(headone, headtwo, cfun) result(mergedhead)
    type(dllnode_t), intent(in), pointer :: headone, headtwo
    procedure(compare_fun) :: cfun
    type(dllnode_t), pointer :: mergedhead

    if (.not. associated(headone)) then
      mergedhead => headtwo
      return
    else if (.not. associated(headtwo)) then
      mergedhead => headone
      return
    end if

    ! Select a smaller value
    if (cfun(headone%data, headtwo%data) < 0) then
      headone%next => merge0(headone%next, headtwo, cfun)
      headone%next%prev => headone
      headone%prev => null()
      mergedhead => headone
    else
      headtwo%next => merge0(headone, headtwo%next, cfun)
      headtwo%next%prev => headtwo
      headtwo%prev => null()
      mergedhead => headtwo
    end if
  end function merge0


end module dllnode_mod
