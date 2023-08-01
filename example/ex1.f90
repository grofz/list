
program ex1
  !! Example of using list with different types
  !!
  !! In this example we make an heterogeneous list, however
  !! it is expected users will put different data types in different
  !! lists.
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

  ! The list is initially empty (no need to initialize)
  write(*,'("Empty list",t25)',advance='no')
  call printlist

  ! TEST ONE: Add values to the list and print the list
  call root%append(win(x))
  call root%append(win(y))
  call root%append(win(px))
  call root%append(win(py))
  call root%append(win(7))
  call root%append(win(3.14))
  write(*,'("Added some notes",t25)',advance='no')
  call printlist

  ! Reverse the order in the list
  call root%reverse()
  call printlist

  ! TEST TWO: Search for node and retrieve its value

  ! type with an array
  node => root%index(win(y))
  if (associated(node)) then
    associate(xx=>dat2one(dllnode_read(node)))
      print *, 'y=? ', xx%a
    end associate
  else
    print '("Searched node is not in the list")'
  end if

  ! type with a pointer
  node => root%index(win(px))
  if (associated(node)) then
    associate(p=>dat2two(dllnode_read(node)))
      x % a = 43 ! just change the value of the original variable
      if (associated(p%ptr)) then
        print *, 'Pointing to ',p%ptr%a
      else
        print *, 'Pointer not associated (WRONG)'
      end if
    end associate
  else
    print '("Searched node is not in the list")'
  end if

  ! the integer
  node => root%index(win(7))
  if (associated(node)) then
    print *, '7? ',dat2int(dllnode_read(node))
  else
    print '("Searched node is not in the list")'
  end if

  ! real
  node => root%index(win(3.14))
  if (associated(node)) then
    print *, '3.14?', dat2real(dllnode_read(node))
  else
    print '("Searched node is not in the list")'
  end if

  ! TEST 3
  ! Remove nodes
  print '("Removing nodes")'
  print *, 'count before removing: ', root%count(win(y))
  call root%remove(win(y))
  print *, 'removed? ', root%count(win(y))

  ! Search and remove current node
  print *, 'count before removing: ', root%count(win(7))
  node => root%index(win(7))
  if (associated(node)) then
    call root%pop(node)
  else
    print *, 'could not find node to delete'
  end if
  print *, 'removed? ', root%count(win(7))

  write(*,'("Removed some notes",t25)',advance='no')
  call printlist

  ! End
  ! Finalization is not automatic yet
  call root%clear()
  write(*,'("Removed remaining nodes",t25)',advance='no')
  call printlist

contains

  subroutine printlist
    !! Iterate through nodes and print the data
    !! Because this is an heterogeneous-list we have no means of
    !! knowing the type of the actual data
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
  end subroutine printlist

end program ex1
