module fortaleza_common
  use mpi
  implicit none

contains

  subroutine init(parent_comm, p, rank)
    integer, intent(out) :: rank, p, parent_comm
    integer :: ierr
    call MPI_Init(ierr)
    call MPI_Comm_rank(MPI_COMM_WORLD, rank, ierr)
    call MPI_Comm_size(MPI_COMM_WORLD, p, ierr)
    call MPI_Comm_get_parent(parent_comm, ierr)
    ! call MPI_Info_create( info, ierr )
  end subroutine init

end module fortaleza_common
