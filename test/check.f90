program test_dll
  implicit none
  interface
    subroutine test1()
    end subroutine test1
  end interface
  call test1()
end program test_dll

subroutine test1()
  use dllnode_mod
  implicit none

  type(dllnode_t), pointer :: head, head1, deleted, next_in_chain, found
  integer :: i
  integer, parameter :: MAXN = 10

  ! Working with an empty list
  head => null()
  print '("Head export = ",*(i0,1x))', dllnode_export(head)
  print '("Tail node is = ",L)', associated(dllnode_tail(head))
  print '("Head node is = ",L)', associated(dllnode_head(head))
  print '("3 node is = ",L)', &
      associated(dllnode_find(head,int([3,33],DATA_KIND)))

  ! Add nodes to the list
  do i=1, MAXN
    call dllnode_insertinfrontof(       &
        head,                           &
        dllnode_t(int([i,i],DATA_KIND)),  &
        head1)
    head => head1
    print '("Head export = ",*(i0,1x))', dllnode_export(head)
  end do

  found=>dllnode_find(head,int([7,7],DATA_KIND))
  if (associated(found)) then
    call dllnode_update(found, int([42,42],DATA_KIND))
    print '("[7] updated = ",*(i0,1x))', dllnode_export(head)
  else
    print '("Could not find item to update")'
  end if

  ! Test find, tail, head
  print '("Tail node is = ",*(i0,1x))', dllnode_read(dllnode_tail(head))
  print '("Head node is = ",*(i0,1x))', dllnode_read(dllnode_head(head))
  print '("42 node is = ",*(i0,1x))', &
      dllnode_read(dllnode_find(head,int([42,42],DATA_KIND)))

  ! Delete nodes from the list
  do i=MAXN,1,-3
    call dllnode_remove( &
        dllnode_find(head,int([i,i],DATA_KIND)),&
        deleted,next_in_chain)
    if (associated(deleted, head)) head => next_in_chain
    if (associated(deleted)) deallocate(deleted)
    print '("After removing ",i0," remains = ",*(i0,1x))', i,dllnode_export(head)
  end do

  found => dllnode_find(head,int([42,42],DATA_KIND))
  if (associated(found)) found => found%gonext()
  if (associated(found)) print '("After 42 is ",*(i0,1x))', dllnode_read(found)
  found => dllnode_find(head,int([9,9],DATA_KIND))
  if (associated(found)) found => found%goprev()
  print '("Before [9] is ",L)', associated(found)
end subroutine test1
