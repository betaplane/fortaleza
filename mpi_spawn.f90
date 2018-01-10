module mpi_spawn
  use mpi
  implicit none

contains
  subroutine spawn()
  integer :: ierr, rank, p, intercom, cerr(4)
  call MPI_INIT(ierr)
  call MPI_Comm_spawn("./test", MPI_ARGV_NULL, &
       4, MPI_INFO_NULL, 0, MPI_COMM_WORLD, intercom, MPI_ERRCODES_IGNORE, ierr)
  call MPI_Comm_rank(MPI_COMM_WORLD, rank, ierr)
  call MPI_Comm_size(MPI_COMM_WORLD, p, ierr)
  print *,rank,p

  call MPI_finalize(ierr)
  end subroutine
end module
