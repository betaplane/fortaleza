 ! https://sysbio.ioc.ee/projects/f2py2e/usersguide/index.html

module mod
  use mpi
  implicit none
  logical :: initialized = .false.
  integer :: info(4)
  integer, allocatable :: shape(:)
  real, allocatable :: x(:)

contains

  subroutine spawn(n, file_name, var_name)
    character(len = *), intent(in) :: file_name
    character(len = *), intent(in), optional :: var_name
    integer, intent(in) :: n
    integer :: i, ierr, children_comm, cerr(n), requests(n)
    integer :: ndims, command, tot_count, m

    if (.not. initialized) then
       call MPI_INIT(ierr)
       initialized = .true.
    endif

    call MPI_Comm_spawn("./child_mean", MPI_ARGV_NULL, &
         n, MPI_INFO_NULL, 0, MPI_COMM_SELF, children_comm, cerr, ierr)

    command = merge(1, 0, len(trim(var_name)) > 0) ! fortran's ternary operator (f95 standard)
    call MPI_Bcast(command, 1, MPI_INT, MPI_ROOT, children_comm, ierr)
    call MPI_Bcast(len(file_name), 1, MPI_INT, MPI_ROOT, children_comm, ierr)
    call MPI_Bcast(file_name, len(file_name), MPI_Char, MPI_ROOT, children_comm, ierr)
    if (command == 1) then
       call MPI_Bcast(len(var_name), 1, MPI_INT, MPI_ROOT, children_comm, ierr)
       call MPI_Bcast(var_name, len(var_name), MPI_Char, MPI_ROOT, children_comm, ierr)
    endif

    ! get file info
    call MPI_Recv(info, 4, MPI_INT, 0, 1, children_comm, MPI_STATUS_IGNORE, ierr)

    if (command == 1) then
       call MPI_Recv(ndims, 1, MPI_INT, 0, 10, children_comm, MPI_STATUS_IGNORE, ierr)
       call MPI_Recv(tot_count, 1, MPI_INT, 0, 11, children_comm, MPI_STATUS_IGNORE, ierr)
       if (allocated(shape)) deallocate( shape )
       allocate( shape(ndims) )
       call MPI_Recv(shape, ndims, MPI_INT, 0, 12, children_comm, MPI_STATUS_IGNORE, ierr)
       print *, "received shape ", shape, tot_count

       allocate( x(tot_count), stat=ierr)
       if (ierr /= 0) stop 3

       m = tot_count / n

       do i=1, n
          call MPI_IRecv(x((i-1) * m + 1: i * m), m, MPI_REAL, &
               i-1, 20, children_comm, requests(i), ierr)
          print *, "received from ", i, ierr
       enddo

       call MPI_Waitall(n, requests, MPI_STATUSES_IGNORE, ierr)
       print *, "all done ", ierr
    endif
  end subroutine spawn

end module
