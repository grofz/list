! Example of an use of a recursion in sorting algorithm
! Advanced Programming Course
! grofz@vscht.cz (2020)
!
! ---------------------------------------------------------------------------
  module mergesort_module
! ---------------------------------------------------------------------------
  use iso_fortran_env, only : I4B => int32
  implicit none

  private
  public mergesort
  public merge ! merge was needed by one of tests

  contains



! ---------------------------------------------------------------------------
  subroutine mergesort(A)
! ---------------------------------------------------------------------------
! Merge-sort algorithm implentation for an array of integers
!
  integer(I4B), intent(inout) :: A(:)
  integer(I4B) :: n
  integer(I4B) :: Atmp(size(A,dim=1))      ! temporary working array

  n = size(A, dim=1)
  call msort(A,Atmp,1,n) 
! ---------------------------------------------------------------------------
  end subroutine mergesort
! ---------------------------------------------------------------------------



! ---------------------------------------------------------------------------
  recursive subroutine msort(A, Atmp, ifirst, ilast)
! ---------------------------------------------------------------------------
  integer(I4B), intent(inout) :: A(:), Atmp(:)
  integer(I4B), intent(in)    :: ifirst, ilast
  integer(I4B) :: imid

  ! -

  if (ilast == ifirst) return ! one element array is already sorted

  ! divide assigned section of an array into two and recursively sort them
  imid = (ilast+ifirst)/2
  call msort(A, Atmp, ifirst, imid)
  call msort(A, Atmp, imid+1, ilast)

  ! merge the two sections 
  call merge(A(ifirst:imid), A(imid+1:ilast), Atmp(ifirst:ilast))

  ! "Atmp" was used to store output from merge operation: copy it to "A"
  A(ifirst:ilast) = Atmp(ifirst:ilast)
! ---------------------------------------------------------------------------
  end subroutine msort
! ---------------------------------------------------------------------------



! ---------------------------------------------------------------------------
  subroutine merge(A, B, C)
! ---------------------------------------------------------------------------
! Two-way merge of sorted arrays A and B into array C
!
  integer(I4B), intent(in)  :: A(:), B(:)
  integer(I4B), intent(out) :: C(:)

  integer(I4B) :: i, j, k, n, m

  ! -

  m = size(A, dim=1)
  n = size(B, dim=1)
  i = 1; j = 1; k = 1 ! indices pointing to not yet used
                      ! positions in arrays A, B and C

  ! Go through arrays A and B and copy smaller element into C
  do 
    if (A(i) < B(j)) then 
      C(k) = A(i)
      k = k+1
      i = i+1
    else                  
      C(k) = B(j)
      k = k+1
      j = j+1
    endif
  
    if (i>m .or. j>n) exit ! reached the end of A or B array
  enddo

  ! Go through the remaining elements in arrays A or B
  ! Note that only one cycle below would actually run because i>m OR j>n
  do i=i,m
    C(k) = A(i)
    k = k+1
  enddo
  do j=j,n
    C(k) = B(j)
    k = k+1
  enddo
! ---------------------------------------------------------------------------
  end subroutine merge
! ---------------------------------------------------------------------------



! ---------------------------------------------------------------------------
  end module mergesort_module
! ---------------------------------------------------------------------------
