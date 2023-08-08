
program ex1
  !! Example of using list with different types
  !!
  !! For demonstration reasons, we add several different types to a
  !! single list (but will remember their order)
  !! Actually, it is not expected mixed data type within a single list
  !!
  use user_mod
  implicit none

  type(dll_t) :: root
  type(dllnode_t), pointer :: node
  type(testone_t), target :: x, y
    !! type 1 contains an integer array
  type(testtwo_t) :: px, py
    !! type 2 contains a pointer component to type 1 variable
  integer :: i

  ! Initialize test values
  x = testone_t(42)
  y = testone_t([(i,i=1,MAXDIM)])
  px%ptr => x
  py%ptr => null()

  ! The list is initially empty (no need to initialize)
  write(*,'("Here is empty list",t25)',advance='no')
  call printlist

  ! TEST ONE: Add values to the list and print the list
  call root%append(win(x))
  call root%append(win(y))
  call root%append(win(px))
  call root%append(win(py))
  call root%append(win(7))     ! integer
  call root%append(win(3.14))  ! real
  write(*,'("Added some notes",t25)',advance='no')
  call printlist

  ! Reverse the order in the list
  call root%reverse()
  write(*,'("List was reversed",t25)',advance='no')
  call printlist

  ! TEST TWO: Search for node and retrieve its value

  ! type with an array
  node => root%index(win(y))
  if (associated(node)) then
    print '("Success: The node has been found")'
    associate(xx=>dat2one(dllnode_read(node)))
      print *, 'y=? ', xx%a
    end associate
  else
    print '("Fail: Searched node is not in the list")'
  end if

  ! type with a pointer
  node => root%index(win(px))
  if (associated(node)) then
    associate(p=>dat2two(dllnode_read(node)))
      x % a = 43 ! just change the value of the original variable
      if (associated(p%ptr)) then
        print *, 'Pointing to ',p%ptr%a, 'Ok ?',p%ptr%a==x%a
      else
        print *, 'Pointer not associated (Fail)'
      end if
    end associate
  else
    print '("Fail: Searched node is not in the list")'
  end if

  ! the integer
  node => root%index(win(7))
  if (associated(node)) then
    print *, 'Found ',dat2int(dllnode_read(node)), ' should be',7
  else
    print '("Fail: Searched node is not in the list")'
  end if

  ! real
  node => root%index(win(3.14))
  if (associated(node)) then
    print *, '3.14?', dat2real(dllnode_read(node)), 'should be',3.14
  else
    print '("Fail: Searched node is not in the list")'
  end if

  ! TEST 3
  ! Remove nodes
  print '("Removing nodes")'
  print *, 'count before removing: ', root%count(win(y))
  call root%remove(win(y))
  print *, 'count after removing:  ', root%count(win(y))

  ! Search and remove current node using `pop`
  print *, 'count before pop:      ', root%count(win(7))
  node => root%index(win(7))
  if (associated(node)) then
    call root%pop(node)
  else
    print *, 'Fail: could not find node to delete'
  end if
  print *, 'count after pop:       ', root%count(win(7))

  write(*,'("Removed some notes",t25)',advance='no')
  call printlist

  ! End
  ! Finalization is not automatic yet
  call root%clear()
  write(*,'("Removed all nodes",t25)',advance='no')
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
      head => head%nextnode()
    end do
    print *
  end subroutine printlist

end program ex1
