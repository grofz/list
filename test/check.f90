program test_dll
  use tree_test_mod
  implicit none
  interface
    subroutine test1()
    end subroutine test1
    subroutine test2()
    end subroutine test2
  end interface

  !call tree_test_basic()
  !call tree_test_joinsplit() ! NICED
  call tree_test_union()  ! NICED
  !call tree_test_playground
  stop

  call test1()
  print *
  print *
  print '("===================== TEST 2 =====================")'
  call test2()
end program test_dll


subroutine test1()
  use dllnode_mod
  use common_mod
  implicit none

  type(dllnode_t), pointer :: head, head1, deleted, next_in_chain, &
      found, head_b, head_c
  integer :: i, num_to_insert
  integer(DATA_KIND), allocatable :: data(:)
  integer, allocatable :: starts(:)
  integer, parameter :: MAXN = 10

  ! Working with an empty list
  head => null()
  print '("Head export = ",*(i0,1x))', dllnode_export(head)
  print '("Tail node is = ",L1)', associated(dllnode_tail(head))
  print '("Head node is = ",L1)', associated(dllnode_head(head))
  print '("3 node is = ",L1)', associated(dllnode_find(head, win(3)))
  print '("Is valid an empty node ? ",L1)', dllnode_validate(head)

  ! Add nodes to the list
  do i=1, MAXN+1
    num_to_insert = i
    if (i==MAXN+1) num_to_insert = 42
    call dllnode_insertinfrontof(       &
        head,                           &
        dllnode_t(win(num_to_insert)),  &
        head1)
    head => head1
    print '("Head export = ",*(i0,1x))', dllnode_export(head)
    print '("Is list valid? ",L1)', dllnode_validate(head)
  end do

  print '("Sorting...")'
  head => dllnode_mergesort(head, cfun_my)
  print '("... sorted")'
  print '("After sort = ",*(i0,1x))', dllnode_export(head)
  print '("Is list valid? ",L1)', dllnode_validate(head)

  found=>dllnode_find(head,win(7))
  if (associated(found)) then
    call dllnode_update(found, win(42))
    print '("[7] updated = ",*(i0,1x))', dllnode_export(head)
  else
    print '("Could not find item to update")'
  end if

  ! Test find, tail, head
  print '("Tail node is = ",*(i0,1x))', dllnode_read(dllnode_tail(head))
  print '("Head node is = ",*(i0,1x))', dllnode_read(dllnode_head(head))
  print '("42 node is = ",*(i0,1x))', &
      wout(dllnode_read(dllnode_find(head,win(42))))
  print '("Is list valid? Should be F ",L1)', dllnode_validate(dllnode_find(head,win(42)))


  ! Delete nodes from the list
  do i=MAXN,1,-3
    call dllnode_remove( &
        dllnode_find(head,win(i)),&
        deleted,next_in_chain)
    if (associated(deleted, head)) head => next_in_chain
    if (associated(deleted)) call dllnode_free(deleted)
    print '("After removing ",i0," remains = ",*(i0,1x))', i,dllnode_export(head)
  end do

  found => dllnode_find(head,win(42))
  if (associated(found)) found => found%nextnode()
  if (associated(found)) print '("After 42 is ",*(i0,1x))', wout(dllnode_read(found))
  found => dllnode_find(head,win(9))
  if (associated(found)) found => found%prevnode()
  print '("Before [9] is ",L1)', associated(found)

  ! Test import and copy
  print *
  data = dllnode_export(head, starts)
  head_b => dllnode_t(data, starts) ! dllnode_import
  print '("HeadB export = ",*(i0,1x))', dllnode_export(head_b)

  print *
  found => dllnode_find(head, win(5))
  !head_c =>  dllnode_t(found) ! dllnode_copy
  head_c =>  dllnode_t(head) ! dllnode_copy
  print '("HeadC export = ",*(i0,1x))', dllnode_export(head_c)
  head_c => dllnode_mergesort(head_c, cfun_my)
  print '("HeadC sorted = ",*(i0,1x))', dllnode_export(head_c)
  print '("Is list valid? ",L1)', dllnode_validate(head)

  ! Deallocation tests
  print *
  found => dllnode_find(head, win(6))
  call dllnode_freechain(found)
  print '("Head after removal = ",*(i0,1x))', dllnode_export(head)
  print '("HeadB export = ",*(i0,1x))', dllnode_export(head_b)
  print '("HeadC export = ",*(i0,1x))', dllnode_export(head_c)

  call dllnode_freechain(head)
  print '("Head after removal = ",*(i0,1x))', dllnode_export(head)
  call dllnode_freechain(head_b)
  print '("HeadB export = ",*(i0,1x))', dllnode_export(head_b)
  call dllnode_freechain(head_c)
  print '("HeadB export = ",*(i0,1x))', dllnode_export(head_c)

contains
  integer function cfun_my(adat, bdat) result(ierr)
    integer(DATA_KIND), dimension(:), intent(in) :: adat, bdat
    integer :: a, b
    a = wout(adat)
    b = wout(bdat)
    if (a < b) then
      ierr = -1
    else if (a==b) then
      ierr = 0
    else
      ierr = 1
    end if
  end function cfun_my

  pure function win(a) result(data0)
    integer(DATA_KIND), allocatable :: data0(:)
    integer, intent(in) :: a
    data0 = transfer(a,mold)
  end function win

  pure function wout(data0) result(a)
    integer(DATA_KIND), intent(in) :: data0(size(mold))
    integer :: a
    a = transfer(data0,a)
  end function wout

end subroutine test1


subroutine test2()
  use common_mod
  use dllnode_mod
  implicit none

  type(dllnode_t), pointer :: head, head1
  integer :: i
  integer, parameter :: MAXN = 10
 !integer(DATA_KIND), allocatable :: data(:,:)

  ! Working with an empty list
  head => null()
  head => dllnode_reverse(head)
  print '("Is valid an empty node ? ",L2)', dllnode_validate(head)


  ! Add nodes to the list
  do i=1, MAXN
    call dllnode_insertinfrontof(       &
        head,                           &
        dllnode_t(int([i,i],DATA_KIND)),  &
        head1)
    head => head1
    print '("Head export = ",*(i0,1x))', dllnode_export(head)
    print '("Is list valid? ",L2)', dllnode_validate(head)
    if (i==1) then
      head => dllnode_reverse(head)
      print '("Is list valid? ",L2)', dllnode_validate(head)
      print '("After reverse = ",*(i0,1x))', dllnode_export(head)
    else if (i==2) then
      head => dllnode_reverse(head)
      print '("Is list valid? ",L2)', dllnode_validate(head)
      print '("After reverse = ",*(i0,1x))', dllnode_export(head)
    else if (i==MAXN) then
      head => dllnode_reverse(head)
      print '("Is list valid? ",L2)', dllnode_validate(head)
      print '("After reverse = ",*(i0,1x))', dllnode_export(head)
    end if
  end do
  call dllnode_freechain(head)
end subroutine test2
