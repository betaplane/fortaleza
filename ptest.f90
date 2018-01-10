module mod
  use mpi
  implicit none
contains
  function test(a) result(b)
    integer a, b
    b = a * 2
  end function test
  subroutine spawn()
    integer :: ierr, rank, p, intercom, cerr(4)
    call MPI_INIT(ierr)
    call MPI_Comm_spawn("./test", MPI_ARGV_NULL, &
         4, MPI_INFO_NULL, 0, MPI_COMM_WORLD, intercom, MPI_ERRCODES_IGNORE, ierr)
    call MPI_Comm_rank(MPI_COMM_WORLD, rank, ierr)
    call MPI_Comm_size(MPI_COMM_WORLD, p, ierr)
  end subroutine spawn
end module
