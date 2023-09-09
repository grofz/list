program test_queue
  implicit none
  interface
    subroutine test_queue_sub()
    end subroutine
  end interface

  call test_queue_sub
end program test_queue


subroutine test_queue_sub()
  use common_mod, only : DATA_KIND, mold
  use queue_mod, only : queue_t
  implicit none

  type(queue_t) :: q

  print 900,"Empty queue", q%isempty(), q%isvalid(), q%size()

  call q%enqueue(transfer(10,mold))
  print 900,"One element added", q%isempty(), q%isvalid(), q%size()
  print '("Dequeued =",i0)', q%dequeue()
  print 900,"One element removed", q%isempty(), q%isvalid(), q%size()

  call q%enqueue(transfer(10,mold))
  call q%enqueue(transfer(20,mold))
  print 900,"Two elements added", q%isempty(), q%isvalid(), q%size()
  print '("Dequeued =",i0)', q%dequeue()
  print 900,"One element removed", q%isempty(), q%isvalid(), q%size()
  print '("Dequeued =",i0)', q%dequeue()
  print 900,"One element removed", q%isempty(), q%isvalid(), q%size()

  print *
  call q%enqueue(transfer(10,mold))
  call q%enqueue(transfer(20,mold))
  print 900,"Two elements added", q%isempty(), q%isvalid(), q%size()
  print '("Dequeued =",i0)', q%dequeue()
  call q%enqueue(transfer(30,mold))
  print 900,"Two add, one remove, one add", q%isempty(), q%isvalid(), q%size()
  print '("Dequeued =",i0)', q%dequeue()
  print 900,"removed", q%isempty(), q%isvalid(), q%size()
  print '("Dequeued =",i0)', q%dequeue()
  print 900,"removed", q%isempty(), q%isvalid(), q%size()

  900 format (a,": empty=",L1," valid=",L1," size=",I0)
end subroutine test_queue_sub