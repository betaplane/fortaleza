 ! https://sysbio.ioc.ee/projects/f2py2e/usersguide/index.html

module mod
  use mpi
  use netcdf
  implicit none

  logical :: initialized = .false.
  integer :: info(4)
  integer, allocatable :: var_shape(:)
  real, allocatable :: x(:)
  double precision, allocatable :: dims(:)
  integer, allocatable, dimension(:) :: var_n_dims, var_xtypes

contains

  subroutine spawn(n, file_name, var_name)
    character(len = *), intent(in) :: file_name
    character(len = *), intent(in), optional :: var_name
    integer, intent(in) :: n ! number of processors
    integer :: i, j, ierr, children_comm, cerr(n), requests(n)
    integer :: ndims, command, tot_count, m, length

    call MPI_initialized(initialized, ierr)
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
    call MPI_Recv(info, 4, MPI_INT, 0, 0, children_comm, MPI_STATUS_IGNORE, ierr)
    if (allocated(var_n_dims)) deallocate(var_n_dims)
    if (allocated(var_xtypes)) deallocate(var_xtypes)
    allocate( var_n_dims(info(2)), var_xtypes(info(2)) )
    do i=1, info(2)
       call MPI_Recv(var_n_dims(i), 1, MPI_INT, 0, i, children_comm, MPI_STATUS_IGNORE, ierr)
       call MPI_Recv(var_xtypes(i), 1, MPI_INT, 0, i + info(2), children_comm, MPI_STATUS_IGNORE, ierr)
    enddo

    if (command == 1) then
       call MPI_Recv(ndims, 1, MPI_INT, 0, 100, children_comm, MPI_STATUS_IGNORE, ierr)
       call MPI_Recv(tot_count, 1, MPI_INT, 0, 101, children_comm, MPI_STATUS_IGNORE, ierr)
       if (allocated(var_shape)) deallocate( var_shape )
       allocate( var_shape(ndims) )
       call MPI_Recv(var_shape, ndims, MPI_INT, 0, 102, children_comm, MPI_STATUS_IGNORE, ierr)
       print *, "received shape ", var_shape, tot_count

       if (allocated(dims)) deallocate(dims)
       allocate( dims(sum(var_shape)) )
       j = 1
       do i=1, ndims
          call MPI_Recv(dims(j: j+var_shape(i)), var_shape(i), MPI_DOUBLE, 0, 102+i, &
               children_comm, MPI_STATUS_IGNORE, ierr)
          j = j + var_shape(i)
       enddo

       if (allocated(x)) deallocate(x)
       allocate( x(tot_count), stat=ierr)
       if (ierr /= 0) stop 3

       m = tot_count / n

       do i=1, n
          call MPI_IRecv(x((i-1) * m + 1: i * m), m, MPI_REAL, &
               i-1, 200, children_comm, requests(i), ierr)
       enddo

       call MPI_Waitall(n, requests, MPI_STATUSES_IGNORE, ierr)
       print *, "all done ", ierr
    endif
  end subroutine spawn

end module
