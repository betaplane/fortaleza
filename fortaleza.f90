 ! https://sysbio.ioc.ee/projects/f2py2e/usersguide/index.html

module mod
  use mpi
  implicit none
  logical :: initialized = .false.
  real, allocatable :: x(:)
  integer :: info(4)
contains
  subroutine spawn(n, file_name, var_name)
    character(len = *), intent(in) :: file_name
    character(len = *), intent(in), optional :: var_name
    integer, intent(in) :: n
    integer :: i, j, ierr, children_comm, cerr(n), requests(n)
    integer :: ndims
    integer, allocatable :: count(:)

    if (.not. initialized) then
       call MPI_INIT(ierr)
       initialized = .true.
    endif

    call MPI_Comm_spawn("./children", MPI_ARGV_NULL, &
         n, MPI_INFO_NULL, 0, MPI_COMM_SELF, children_comm, cerr, ierr)

    call MPI_Bcast(len(file_name), 1, MPI_INT, MPI_ROOT, children_comm, ierr)
    call MPI_Bcast(file_name, len(file_name), MPI_Char, MPI_ROOT, children_comm, ierr)

    ! get file info
    call MPI_IRecv(info, 4, MPI_INT, 0, 1, children_comm, requests(1), ierr)

    if (len(trim(var_name)) == 0) then
       call MPI_Bcast(0, 1, MPI_INT, MPI_ROOT, children_comm, ierr)
    else
       call MPI_Bcast(1, 1, MPI_INT, MPI_ROOT, children_comm, ierr)
       call MPI_Bcast(len(var_name), 1, MPI_INT, MPI_ROOT, children_comm, ierr)
       call MPI_Bcast(var_name, len(var_name), MPI_Char, MPI_ROOT, children_comm, ierr)
       call MPI_Recv(ndims, 1, MPI_INT, 0, 2, children_comm, MPI_STATUS_IGNORE, ierr)
       allocate( count(ndims) )
       call MPI_Recv(count, ndims, MPI_INT, 0, 3, children_comm, MPI_STATUS_IGNORE, ierr)

       call MPI_Wait(requests(1), MPI_STATUS_IGNORE, ierr)

       j = product(count)
       allocate( x(j * n), stat=ierr)
       if (ierr /= 0) stop 3

       do i=1, n
          call MPI_IRecv(x((i-1) * j + 1: i * j), j, MPI_REAL, &
               i-1, 10, children_comm, requests(i), ierr)
       enddo

       call MPI_Waitall(n, requests, MPI_STATUSES_IGNORE, ierr)
       deallocate( count )
    endif
  end subroutine spawn
end module
