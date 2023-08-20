! Example of an use of a recursion in sorting algorithm
! Advanced Programming Course
! grofz@vscht.cz (2020)
!
! ---------------------------------------------------------------------------
  module quicksort_module
! ---------------------------------------------------------------------------
  use iso_fortran_env, only : I4B => int32
  implicit none
  private
  public quicksort

  contains


! ---------------------------------------------------------------------------
  subroutine quicksort(A,mode)
! ---------------------------------------------------------------------------
! Quick-sort algorithm implementation.
! mode = 1 - Hoare partition scheme (default)
!        2 - Lomuto partition scheme
!
  integer(I4B), intent(inout) :: A(:)
  integer(I4B), intent(in), optional :: mode ! (1=Hoare, 2=Lomuto) 
  integer(I4B), parameter :: MODE_DEF = 1
  integer(I4B) :: mode_use

  mode_use = MODE_DEF
  if (present(mode)) mode_use = mode

  select case (mode_use)
  case(1)
    call quicksort_hoare(A, 1, size(A,dim=1))
  case(2)
    call quicksort_lomuto(A, 1, size(A,dim=1))
  case default ! do nothing if wrong mode value is entered
    print *, 'quicksort ERROR: wrong mode, nothing will be done'
    return
  end select
! ---------------------------------------------------------------------------
  end subroutine quicksort
! ---------------------------------------------------------------------------



! ---------------------------------------------------------------------------
  recursive subroutine quicksort_hoare(A, lo, hi)
! ---------------------------------------------------------------------------
  integer(I4B), intent(inout) :: A(:)
  integer(I4B), intent(in)    :: lo, hi
  integer(I4B)  :: piv

  ! -

  if (lo < hi) then
    piv = hoare_partition(A, lo, hi)
    call quicksort_hoare(A, lo, piv)
    call quicksort_hoare(A, piv+1, hi)
  endif
! ---------------------------------------------------------------------------
  end subroutine quicksort_hoare
! ---------------------------------------------------------------------------



! ---------------------------------------------------------------------------
  function hoare_partition(A, lo, hi)
! ---------------------------------------------------------------------------
  integer(I4B), intent(inout) :: A(:)
  integer(I4B), intent(in)    :: lo, hi
  integer(I4B)                :: hoare_partition

  integer(I4B) :: itmp, Apiv, i, j

  ! -

  Apiv = A(lo+((hi-lo)/2)) ! this is safer than "(hi+lo)/2" which can overflow
                           ! for very large arrays
  i = lo - 1
  j = hi + 1

  do 
    ! Move "i" and "j" indices and skip correctly placed positions
    do 
      i = i + 1
      if (A(i) < Apiv) cycle 
      exit 
    enddo

    do 
      j = j - 1
      if (A(j) > Apiv) cycle
      exit
    enddo


    if (i >= j) then ! If indices meet, exit the loop
      hoare_partition = j
      exit
    end if

    ! Now A(i) >= Apiv and A(j) <= Apiv
    ! Swap A(i) and A(j) to make them correctly placed
    itmp = A(i)
    A(i) = A(j)
    A(j) = itmp
  enddo
! ---------------------------------------------------------------------------
  end function hoare_partition
! ---------------------------------------------------------------------------



! ---------------------------------------------------------------------------
  recursive subroutine quicksort_lomuto(A, lo, hi)
! ---------------------------------------------------------------------------
  integer(I4B), intent(inout) :: A(:)
  integer(I4B), intent(in)    :: lo, hi
  integer(I4B)  :: piv

  ! -

  if (lo < hi) then
    piv = lomuto_partition(A, lo, hi)
    call quicksort_lomuto(A, lo, piv-1)
    call quicksort_lomuto(A, piv+1, hi)
  endif
! ---------------------------------------------------------------------------
  end subroutine quicksort_lomuto
! ---------------------------------------------------------------------------



! ---------------------------------------------------------------------------
  function lomuto_partition(A, lo, hi)
! ---------------------------------------------------------------------------
  integer(I4B), intent(inout) :: A(:)
  integer(I4B), intent(in)    :: lo, hi
  integer(I4B)                :: lomuto_partition

  integer(I4B) :: i, j, Apiv, mi

  ! -

  ! Median from A(low), A(middle) and A(high) selected as pivot and moved
  ! to the rightmost position
  mi = lo + ((hi-lo)/2)
  if (A(mi) < A(lo)) call swap(A(mi),A(lo))
  if (A(hi) < A(lo)) call swap(A(hi),A(lo))
  if (A(mi) < A(hi)) call swap(A(mi),A(hi))

  Apiv = A(hi) ! select last element as a pivot
  i = lo
  do j = lo, hi
    if (A(j) < Apiv) then
      if (i /= j) call swap(A(i),A(j))
      i = i + 1
    endif
  enddo
  if (i /= hi) call swap(A(i),A(hi))
  lomuto_partition = i

  contains

  subroutine swap(c,d)
    integer(I4B), intent(inout) :: c,d
    integer(I4B) :: t 
    t = c; c = d; d = t
  end subroutine swap
! ---------------------------------------------------------------------------
  end function lomuto_partition
! ---------------------------------------------------------------------------



! ---------------------------------------------------------------------------
  end module quicksort_module
! ---------------------------------------------------------------------------
