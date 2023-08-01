program test_dll
  implicit none
  interface
    subroutine test1()
    end subroutine test1
    subroutine test2()
    end subroutine test2
    subroutine test3()
    end subroutine test3
  end interface
  !call test1()
  !call test2()
  call test3()
end program test_dll


subroutine test1()
  use dllnode_mod
  implicit none

  type(dllnode_t), pointer :: head, head1, deleted, next_in_chain, &
      found, head_b, head_c
  integer :: i
  integer, parameter :: MAXN = 10
  integer(DATA_KIND), allocatable :: data(:,:)

  ! Working with an empty list
  head => null()
  print '("Head export = ",*(i0,1x))', dllnode_export(head)
  print '("Tail node is = ",L)', associated(dllnode_tail(head))
  print '("Head node is = ",L)', associated(dllnode_head(head))
  print '("3 node is = ",L)', &
      associated(dllnode_find(head,int([3,33],DATA_KIND)))
  print '("Is valid an empty node ? ",L)', dllnode_validate(head)

  ! Add nodes to the list
  do i=1, MAXN
    call dllnode_insertinfrontof(       &
        head,                           &
        dllnode_t(int([i,i],DATA_KIND)),  &
        head1)
    head => head1
    print '("Head export = ",*(i0,1x))', dllnode_export(head)
    print '("Is list valid? ",L)', dllnode_validate(head)
  end do

  print '("Sorting...")'
  head => dllnode_mergesort(head, cfun_my)
  print '("... sorted")'
  print '("After sort = ",*(i0,1x))', dllnode_export(head)
  print '("Is list valid? ",L)', dllnode_validate(head)

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
  print '("Is list valid? Should be F ",L)', dllnode_validate(dllnode_find(head,int([42,42],DATA_KIND)))


  ! Delete nodes from the list
  do i=MAXN,1,-3
    call dllnode_remove( &
        dllnode_find(head,int([i,i],DATA_KIND)),&
        deleted,next_in_chain)
    if (associated(deleted, head)) head => next_in_chain
    if (associated(deleted)) call dllnode_free(deleted)
    print '("After removing ",i0," remains = ",*(i0,1x))', i,dllnode_export(head)
  end do

  found => dllnode_find(head,int([42,42],DATA_KIND))
  if (associated(found)) found => found%gonext()
  if (associated(found)) print '("After 42 is ",*(i0,1x))', dllnode_read(found)
  found => dllnode_find(head,int([9,9],DATA_KIND))
  if (associated(found)) found => found%goprev()
  print '("Before [9] is ",L)', associated(found)

  ! Test import and copy
  print *
  data = dllnode_export(head)
  head_b => dllnode_t(data) ! dllnode_import
  print '("HeadB export = ",*(i0,1x))', dllnode_export(head_b)
  
  print *
  found => dllnode_find(head, int([5,5],DATA_KIND))
  !head_c =>  dllnode_t(found) ! dllnode_copy
  head_c =>  dllnode_t(head) ! dllnode_copy
  print '("HeadC export = ",*(i0,1x))', dllnode_export(head_c)
  head_c => dllnode_mergesort(head_c, cfun_my)
  print '("HeadC sorted = ",*(i0,1x))', dllnode_export(head_c)
  print '("Is list valid? ",L)', dllnode_validate(head)

  ! Deallocation tests
  print *
  found => dllnode_find(head, int([6,6],DATA_KIND))
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
  integer function cfun_my(a, b) result(ierr)
    integer(DATA_KIND), dimension(size(mold)), intent(in) :: a, b
    if (a(1) < b(1)) then
      ierr = -1
    else if (a(1)==b(1)) then
      ierr = 0
    else
      ierr = 1
    end if
  end function cfun_my

end subroutine test1


subroutine test2()
  use dllnode_mod
  implicit none

  type(dllnode_t), pointer :: head, head1
  integer :: i
  integer, parameter :: MAXN = 10
  integer(DATA_KIND), allocatable :: data(:,:)

  ! Working with an empty list
  head => null()
  head => dllnode_reverse(head)
  print '("Is valid an empty node ? ",L)', dllnode_validate(head)


  ! Add nodes to the list
  do i=1, MAXN
    call dllnode_insertinfrontof(       &
        head,                           &
        dllnode_t(int([i,i],DATA_KIND)),  &
        head1)
    head => head1
    print '("Head export = ",*(i0,1x))', dllnode_export(head)
    print '("Is list valid? ",L)', dllnode_validate(head)
    if (i==1) then
      head => dllnode_reverse(head)
      print '("Is list valid? ",L)', dllnode_validate(head)
      print '("After reverse = ",*(i0,1x))', dllnode_export(head)
    else if (i==2) then
      head => dllnode_reverse(head)
      print '("Is list valid? ",L)', dllnode_validate(head)
      print '("After reverse = ",*(i0,1x))', dllnode_export(head)
    else if (i==MAXN) then
      head => dllnode_reverse(head)
      print '("Is list valid? ",L)', dllnode_validate(head)
      print '("After reverse = ",*(i0,1x))', dllnode_export(head)
    end if
  end do
  call dllnode_freechain(head)
end subroutine test2


module user_mod
  !! Used for test3
  use iso_fortran_env, only : int64
  use dll_mod
  use dllnode_mod, only : dllnode_t, mold, DATA_KIND, dllnode_read
  implicit none

  integer, parameter :: MAXDIM=10
  type :: testone_t
    integer :: a(MAXDIM)
  end type testone_t

  type :: testtwo_t
    type(testone_t), pointer :: ptr
  end type testtwo_t

  interface win
    module procedure one2dat
    module procedure two2dat
    module procedure int2dat
    module procedure real2dat
  end interface

contains

  pure function one2dat(one) result(dat)
    type(testone_t), intent(in) :: one
    integer(DATA_KIND) :: dat(size(mold))
    dat = transfer(one,mold,size(mold))
  end function
  pure function two2dat(one) result(dat)
    type(testtwo_t), intent(in) :: one
    integer(DATA_KIND) :: dat(size(mold))
    dat = transfer(one,mold,size(mold))
  end function
  pure function int2dat(one) result(dat)
    integer, intent(in) :: one
    integer(DATA_KIND) :: dat(size(mold))
    dat = transfer(one,mold,size(mold))
  end function
  pure function real2dat(one) result(dat)
    real, intent(in) :: one
    integer(DATA_KIND) :: dat(size(mold))
    dat = transfer(one,mold,size(mold))
  end function


  pure function dat2one(dat) result(one)
    integer(DATA_KIND), intent(in) :: dat(size(mold))
    type(testone_t) :: one
    one = transfer(dat,one)
  end function
  pure function dat2two(dat) result(one)
    integer(DATA_KIND), intent(in) :: dat(size(mold))
    type(testtwo_t) :: one
    one = transfer(dat,one)
  end function
  pure function dat2int(dat) result(one)
    integer(DATA_KIND), intent(in) :: dat(size(mold))
    integer :: one
    one = transfer(dat,one)
  end function
  pure function dat2real(dat) result(one)
    integer(DATA_KIND), intent(in) :: dat(size(mold))
    real :: one
    one = transfer(dat,one)
  end function

end module user_mod


subroutine test3()
  !! Test of using list with different types
  use user_mod
  implicit none

  type(dll_t) :: root
  type(dllnode_t), pointer :: node
  type(testone_t), target :: x, y
  type(testtwo_t) :: px, py
  integer :: i

  ! Initialize test values
  x = testone_t(42)
  y = testone_t([(i,i=1,MAXDIM)])
  px%ptr => x
  py%ptr => null()

  write(*,'("Empty list",t25)',advance='no')
  call printlist

  ! TEST ONE: Add values to the list and print the list
  call root%append(win(x))
  call root%append(win(y))
  call root%append(win(py))
  call root%append(win(py))
  call root%append(win(7))
  call root%append(win(3.14))
  write(*,'("Added some notes",t25)',advance='no')
  call printlist

! call root%reverse()
! call printlist

  ! TEST TWO: Search for node and print it
  node => root%index(win(y))
  if (associated(node)) then
    associate(xx=>dat2one(dllnode_read(node)))
      print *, '24? ', xx%a
    end associate
  else
    print '("Searched node is not in the list")'
  end if
  node => root%index(win(py))
  if (associated(node)) then
    associate(p=>dat2two(dllnode_read(node)))
      if (associated(p%ptr)) then
        print *, 'Pointing to ',p%ptr%a
      else
        print *, 'Pointer not associated'
      end if
    end associate
  else
    print '("Searched node is not in the list")'
  end if
  node => root%index(win(7))
  if (associated(node)) then
    print *, dat2int(dllnode_read(node))
  else
    print '("Searched node is not in the list")'
  end if
  node => root%index(win(3.14))
  if (associated(node)) then
    print *, dat2real(dllnode_read(node))
  else
    print '("Searched node is not in the list")'
  end if

! print *, 'count method: 2', root%count(win(y))
! print *, 'associated = T',associated(root%index(win(y)))
! call root%remove(win(y))

! call root%reverse()
!call printlist
!  call root%clear()
!call printlist

! print *, 'size of node ', sizeof(x), storage_size(x), storage_size(mold)
! print *, 'size of mold', sizeof(mold)
! print *, 'transfer ', transfer(x, mold, DATA_SIZE)
! print *, 'transfer ', transfer(x, mold)

contains
  subroutine printlist
    type(dllnode_t), pointer :: head
    character(len=2) :: chw

    head => root%firstnode()
    write(chw,'(i0)') size(mold)
    print '("The list is:")'
    do
      if (.not. associated(head)) exit
      print '('//chw//'(i0,:,1x))', dllnode_read(head)
      head => head%gonext()
    end do
    print *
  end subroutine


end subroutine test3
