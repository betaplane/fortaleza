program test
  use netcdf
  use mpi
  implicit none

  integer :: narg, p, rank, ierr, i
  integer :: ncid, ndims, nvars, natts, unltdid, varid, xtype
  character (len = nf90_max_name) :: file_name, name, var_name
  integer, dimension(:), allocatable :: dimids, count, start
  real, dimension(:, :, :, :), allocatable :: x

  narg = command_argument_count()
  call get_command_argument(1, file_name)
  if (narg == 2) then
     call get_command_argument(2, var_name)
  endif

  call MPI_Init(ierr)
  call MPI_Comm_rank(MPI_COMM_WORLD, rank, ierr)
  call MPI_Comm_size(MPI_COMM_WORLD, p, ierr)
  ! call MPI_Info_create( info, ierr )

  if (narg == 0) then
     print *,p,rank
  else
     call check( nf90_open(trim(file_name), ior(NF90_NOWRITE, NF90_MPIIO), ncid, &
          comm = MPI_COMM_WORLD, info = MPI_INFO_NULL) )
     call check( nf90_inquire(ncid, ndims, nvars, natts, unltdid) )
  endif

  if (rank == 0 .and. narg == 1) then
     do varid = 1, nvars
        call check( nf90_inquire_variable(ncid, varid, name, xtype, ndims) )
        print *, trim(name), ndims, trim(data_type(xtype))
     end do
  endif

  if (narg == 2) then
     call check( nf90_inq_varid(ncid, var_name, varid) )
     call check( nf90_inquire_variable(ncid, varid, ndims = ndims) )
     allocate(dimids(ndims), count(ndims), start(ndims), stat=ierr)
     if (ierr /= 0) stop 3

     call check( nf90_inquire_variable(ncid, varid, xtype = xtype, dimids = dimids) )
     do i=1, ndims
        call check( nf90_inquire_dimension(ncid, i, len=count(i)) )
     enddo
     print *, count
     count(1) = count(1) / p
     print *, count
     allocate(x(count(1), count(2), count(3), count(4)), stat=ierr)
     if (ierr /= 0) stop 3
     start = (/rank * count(1) + 1, 1, 1, 1/)
     call check( nf90_get_var(ncid, varid, x, start = start, count = count) )
     deallocate(dimids, count, start, x)
  endif

  if (narg /= 0) call check( nf90_close(ncid) )

  call MPI_finalize(ierr)

contains
  subroutine check(status)
    integer, intent(in) :: status
    if(status /= nf90_noerr) then 
       print *, trim(nf90_strerror(status))
       stop 2
    end if
  end subroutine check

  function data_type(type) result(str)
    integer, intent(in) :: type
    character(len=6) :: str
    select case (type)
       case (NF90_BYTE)
          str = "byte"
       case (NF90_CHAR)
          str = "char"
       case (NF90_SHORT)
          str = "short"
       case (NF90_INT)
          str = "int"
       case (NF90_FLOAT)
          str = "float"
       case (NF90_DOUBLE)
          str = "double"
    end select
  end function
end program
