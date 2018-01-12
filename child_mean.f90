program mean
  use netcdf
  use mpi
  use fortaleza_common
  implicit none

  integer :: ierr, tot_count, request
  real, dimension(:, :, :, :), allocatable :: x
  type(parallel_file) :: pfile
  type(ncvar) :: var

  call pfile%init()

  if (associated(pfile%var)) then
     if (pfile%var%ultd_id /= -1) then
        if (pfile%rank == 0) then
           call pfile%send_shape()
        endif
        tot_count = pfile%var%count_wo_ultd / pfile%np
        call pfile%get_var(x)
        call MPI_Isend(sum(x, dim=pfile%var%ultd_id), tot_count, MPI_REAL, 0, 20, pfile%parent, request, ierr)
        call MPI_Wait(request, MPI_STATUS_IGNORE, ierr)
        print *, "before dealloc ", pfile%rank
        deallocate(x)
     endif
  endif

  ! call pfile%dealloc()
end program
