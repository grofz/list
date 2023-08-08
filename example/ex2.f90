program example_dllnode
  use dllnode_mod
  use user_mod
  implicit none

  type(dllnode_t), pointer :: head, newhead, copied_head, imported_head, &
        found_node, deleted, next_in_chain, current
  integer :: i
  integer(DATA_KIND), allocatable :: values(:)
  integer, allocatable :: starts(:)

  ! Initialize empty list
  head => null()

  ! Add integers to the list
  do i=1,10
    call dllnode_insertinfrontof(head, dllnode_t(transfer(i,mold)), newhead)
    head => newhead
  end do

  ! Export list
  values = dllnode_export(head, starts)
  print '("Exported values = ",*(i0,1x))', values
  print '("Exported starts = ",*(i0,1x))', starts

  ! Make a copy of the list
  copied_head => dllnode_t(head)

  ! Import list
  imported_head => dllnode_t(values, starts)

  ! Find and replace a value in the list
  found_node => dllnode_find(head, transfer(4,mold))
  if (associated(found_node)) then
    call dllnode_update(found_node, transfer(42,mold))
  else
    print '("Fail: node not found")'
  end if
  print '("One node modified = ",*(i0,1x))', dllnode_export(head)

  ! Sort the list
  head => dllnode_mergesort(head, my_compare_fun)
  print '("List sorted = ",*(i0,1x))', dllnode_export(head)

  ! Delete node if it is in list
  call dllnode_remove(dllnode_find(head, transfer(1,mold)), deleted, next_in_chain)
  if (associated(deleted, head)) head => next_in_chain
  if (associated(deleted)) then
    call dllnode_free(deleted)
  else
    print '("Fail: could not find node to delete")'
  end if
  print '("One node removed = ",*(i0,1x))', dllnode_export(head)

  ! Iterate over the list
  current => head
  do
    if (.not. associated(current)) exit
    print *, transfer(dllnode_read(current),i)
    current => current%nextnode()
  end do

  ! Free the lists
  call dllnode_freechain(head)
  call dllnode_freechain(imported_head)
  call dllnode_freechain(copied_head)

contains

  integer function my_compare_fun(adat, bdat) result(ires)
    integer(DATA_KIND), dimension(:), intent(in) :: adat, bdat
    integer :: a, b
    a = transfer(adat,a)
    b = transfer(bdat,b)
    if (a<b) then
      ires = -1
    else if (a==b) then
      ires = 0
    else
      ires = 1
    end if
  end function

end program example_dllnode
