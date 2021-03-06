module fortaleza_common
  use mpi
  use netcdf
  implicit none

  type :: ncvar
     character(len=nf90_max_name) :: name
     integer :: n_dims, xtype, id, ncid
     integer :: ultd_id = -1, count_wo_ultd = 1
     integer, dimension(:), allocatable :: shape, dim_ids
     real, allocatable :: real1D(:), real2D(:, :), real3D(:, :, :), real4D(:, :, :, :)
     double precision, allocatable :: double1D(:), double2D(:, :), double3D(:, :, :), double4D(:, :, :, :)
     type(ncvar), pointer :: dims(:) => null()
   contains
     procedure :: init => init_var
     procedure :: dealloc => deallocate_var
     procedure :: get => get_data
  end type ncvar

  type :: parallel_file
     character(len=:), allocatable :: name
     integer :: rank, np, parent, ncid, n_dims, n_vars, n_atts, ultd_id
     type(ncvar), pointer :: vars(:) => null()
     type(ncvar), pointer :: var => null()
   contains
     procedure :: init => init_pfile
     procedure :: dealloc => deallocate_pfile
     procedure :: get_var => get_var
  end type parallel_file

contains

  subroutine init_pfile(this)
    class(parallel_file) :: this
    integer :: i, ierr, length, command, varid, ndims
    character (len=:), allocatable :: name
    type(ncvar), pointer :: v

    call MPI_Init(ierr)
    call MPI_Comm_rank(MPI_COMM_WORLD, this%rank, ierr)
    call MPI_Comm_size(MPI_COMM_WORLD, this%np, ierr)
    call MPI_Comm_get_parent(this%parent, ierr)
    ! call MPI_Info_create( info, ierr )

    ! get "command" from parent
    call MPI_Bcast(command, 1, MPI_INT, 0, this%parent, ierr)

    call MPI_Bcast(length, 1, MPI_INT, 0, this%parent, ierr)
    allocate(character(length) :: this%name)
    call MPI_Bcast(this%name, length, MPI_Character, 0, this%parent, ierr)
    if (this%rank == 0) then
       print *, "opening ", this%name
    endif

    ! info = (/glob_dims, n_vars, n_atts, ulimited_id /)
    call check( nf90_open(this%name, ior(NF90_NOWRITE, NF90_MPIIO), this%ncid, &
         comm = MPI_COMM_WORLD, info = MPI_INFO_NULL) )
    call check( nf90_inquire(this%ncid, this%n_dims, this%n_vars, this%n_atts, this%ultd_id) )


    if (command == 1) then
       call MPI_Bcast(length, 1, MPI_INT, 0, this%parent, ierr)
       allocate(character(length) :: name)
       call MPI_Bcast(name, length, MPI_Character, 0, this%parent, ierr)
       call check( nf90_inq_varid(this%ncid, name, varid) )
       deallocate(name)
    endif

    ! send file info
    if (this%rank == 0) then
       call MPI_Send((/this%n_dims, this%n_vars, this%n_atts, this%ultd_id/), &
            4, MPI_INT, 0, 0, this%parent, ierr)

       allocate( this%vars(this%n_vars) )

       do i = 1, this%n_vars
          call this%vars(i)%init(this, i)
          v => this%vars(i)
          call MPI_Send(v%n_dims, 1, MPI_INT, 0, i, this%parent, ierr)
          call MPI_Send(v%xtype, 1, MPI_INT, 0, i + this%n_vars, this%parent, ierr)

          if (command == 0) &
             print *, trim(v%name), v%n_dims, trim(data_type(v%xtype))
       enddo
    endif
    if (command == 1) then
       if (this%rank /= 0) then
          allocate( this%var )
          call this%var%init(this, varid)
       else
          this%var => this%vars(varid)
       endif
    endif
  end subroutine init_pfile

  subroutine get_var(this)
    class(parallel_file) :: this
    integer :: ierr, i, length
    type(ncvar), pointer :: v
    if (.not. associated(this%var) ) stop 2

    if (this%rank == 0) then
       call MPI_Send(this%var%n_dims, 1, MPI_INT, 0, 100, this%parent, ierr)
       call MPI_Send(this%var%count_wo_ultd, 1, MPI_INT, 0, 101, this%parent, ierr)
       call MPI_Send(this%var%shape, this%var%n_dims, MPI_INT, 0, 102, this%parent, ierr)

       do i=1, this%var%n_dims
          v => this%vars(this%var%dim_ids(i))
          call v%get((0), (1)) ! the brackets are necessary to pass literal values as 'expressions'
          call MPI_Send(v%double1D, v%shape(1), MPI_DOUBLE, 0, 102+i, this%parent, ierr)
       enddo
    endif

    call this%var%get(this%rank, this%np)
  end subroutine get_var

  subroutine get_data(this, rank, np)
    class(ncvar) :: this
    integer, intent(in) :: rank, np
    integer :: ierr
    integer, dimension(:), allocatable :: count, start

    allocate( count, source=this%shape, stat=ierr )
    allocate( start(this%n_dims), source=1, stat=ierr )

    select case (this%xtype)
    case(NF90_REAL)
       select case (this%n_dims)
       case (4)
          count(1) = count(1) / np
          start(1) = rank * count(1) + 1
          if ( allocated( this%real4D) ) deallocate( this%real4D )
          allocate(this%real4D(count(1), count(2), count(3), count(4)), stat=ierr )
          call check( nf90_get_var(this%ncid, this%id, this%real4D, start = start, count = count) )
       end select
    case (NF90_DOUBLE)
       select case (this%n_dims)
       case (1)
          allocate(this%double1D(count(1)), stat=ierr )
          call check( nf90_get_var(this%ncid, this%id, this%double1D, start = start, count = count) )
       end select
    end select
    print *, rank, "got ", trim(this%name)
    deallocate( count, start )
  end subroutine get_data

  subroutine deallocate_pfile(this)
    class(parallel_file) :: this
    integer :: i, ierr
    if (allocated(this%name)) deallocate(this%name)
    if (associated(this%vars)) then
       do i=1, this%n_vars
          call this%vars(i)%dealloc()
       enddo
       deallocate( this%vars )
       nullify( this%var )
    elseif (associated(this%var)) then
       call this%var%dealloc()
       deallocate(this%var)
    endif
    call check( nf90_close(this%ncid) )
    call MPI_finalize(ierr)
  end subroutine deallocate_pfile

  subroutine init_var(this, pfile, varid)
    class(ncvar) :: this
    class(parallel_file) :: pfile
    integer, intent(in) :: varid
    integer :: i
    this%id = varid
    this%ncid = pfile%ncid
    call check( nf90_inquire_variable(pfile%ncid, varid, this%name, this%xtype, this%n_dims) )
    allocate( this%shape(this%n_dims), this%dim_ids(this%n_dims) )
    call check( nf90_inquire_variable(pfile%ncid, varid, dimids = this%dim_ids) )
    do i=1, this%n_dims
       call check( nf90_inquire_dimension(pfile%ncid, this%dim_ids(i), len = this%shape(i)) )
       if (this%dim_ids(i) == pfile%ultd_id) then
          this%ultd_id = i
       else
          this%count_wo_ultd = this%count_wo_ultd * this%shape(i)
       endif
    enddo
  end subroutine init_var

  subroutine deallocate_var(this)
    class(ncvar) :: this
    if (allocated(this%shape)) deallocate( this%shape )
    if (allocated(this%dim_ids)) deallocate( this%dim_ids )
    if (allocated(this%real1D)) deallocate( this%real1D )
    if (allocated(this%real2D)) deallocate( this%real2D )
    if (allocated(this%real3D)) deallocate( this%real3D )
    if (allocated(this%real4D)) deallocate( this%real4D )
    if (allocated(this%double1D)) deallocate( this%double1D )
    if (allocated(this%double2D)) deallocate( this%double2D )
    if (allocated(this%double3D)) deallocate( this%double3D )
    if (allocated(this%double4D)) deallocate( this%double4D )
  end subroutine deallocate_var

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
  end function data_type

end module fortaleza_common
