program mpi_child
  use netcdf
  use mpi
  use fortaleza_common
  implicit none

  integer :: p, rank, ierr, i, j, k, parent_comm, length, command, request
  integer :: ncid, nd, varid, xtype, info(4)
  character (len=:), allocatable :: name
  character (len=:), allocatable :: var_names(:)
  integer, dimension(:), allocatable :: dimids, count, start, ndims, xtypes
  real, dimension(:, :, :, :), allocatable :: x

  call init(parent_comm, p, rank)

  call MPI_Bcast(length, 1, MPI_INT, 0, parent_comm, ierr)
  allocate(character(length) :: name)
  call MPI_Bcast(name, length, MPI_Character, 0, parent_comm, ierr)
  if (rank == 0) then
     print *, "opening ", name
  endif

  ! info = (/glob_dims, n_vars, n_atts, ulimited_id /)
  call check( nf90_open(name, ior(NF90_NOWRITE, NF90_MPIIO), ncid, &
       comm = MPI_COMM_WORLD, info = MPI_INFO_NULL) )
  call check( nf90_inquire(ncid, info(1), info(2), info(3), info(4)) )
  deallocate(name)

  ! send file info
  if (rank == 0) then
     call MPI_Send(info, 4, MPI_INT, 0, 1, parent_comm, ierr)
     allocate(character(nf90_max_name) :: var_names(info(2)))
     allocate( ndims(info(2)), xtypes(info(2)) )
     do i = 1, info(2)
        call check( nf90_inquire_variable(ncid, i, var_names(i), xtypes(i), ndims(i)) )
     end do
  endif
  call MPI_Bcast(command, 1, MPI_INT, 0, parent_comm, ierr)

  if (command == 0) then ! just get some info on the file
     if (rank == 0) then
        do i = 1, info(2)
           print *, trim(var_names(i)), ndims(i), trim(data_type(xtypes(i)))
        enddo
     endif
  else ! if a variable name is sent, do something
     call MPI_Bcast(length, 1, MPI_INT, 0, parent_comm, ierr)
     allocate(character(length) :: name)
     call MPI_Bcast(name, length, MPI_Character, 0, parent_comm, ierr)
     call check( nf90_inq_varid(ncid, name, varid) )
     deallocate(name)

     call check( nf90_inquire_variable(ncid, varid, ndims = nd) )
     allocate(dimids(nd), count(nd), start(nd), stat=ierr)
     if (ierr /= 0) stop 3

     j = -1
     call check( nf90_inquire_variable(ncid, varid, xtype = xtype, dimids = dimids) )
     do i=1, nd
        if (dimids(i) == info(4)) j = i
        call check( nf90_inquire_dimension(ncid, dimids(i), len = count(i)) )
     enddo

     if (j /= -1) then
        count = (/count(1) / p, count(2), count(3), 10/)
        k = product(count(1:3))

        if (rank == 0) then
           call MPI_Send(nd - 1, 1, MPI_INT, 0, 2, parent_comm, ierr)
           call MPI_Send(count(1: nd - 1), nd - 1, MPI_INT, 0, 3, parent_comm, ierr)
        endif

        allocate(x(count(1), count(2), count(3), count(4)), stat=ierr)
        if (ierr /= 0) stop 3

        start = (/rank * count(1) + 1, 1, 1, 1/)
        call check( nf90_get_var(ncid, varid, x, start = start, count = count) )
        call MPI_Send(sum(x, dim=j), k, MPI_REAL, 0, 10, parent_comm, ierr)
     endif

     deallocate(dimids, count, start, x)
  endif

  call check( nf90_close(ncid) )
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
