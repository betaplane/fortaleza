program mean
  use netcdf
  use mpi
  use fortaleza_common
  implicit none

  integer :: ierr, tot_count, request
  real, dimension(:, :, :, :), allocatable :: x
  real :: rec_count
  type(parallel_file) :: pfile
  type(ncvar) :: var

  call pfile%init()

  if (associated(pfile%var)) then
     if (pfile%var%ultd_id /= -1) then
        tot_count = pfile%var%count_wo_ultd / pfile%np
        rec_count = pfile%var%shape(pfile%var%ultd_id)
        call pfile%get_var(x)
        call MPI_Isend(sum(x, dim=pfile%var%ultd_id) / rec_count, &
             tot_count, MPI_REAL, 0, 20, pfile%parent, request, ierr)
        call MPI_Wait(request, MPI_STATUS_IGNORE, ierr)
        deallocate(x)
     endif
  endif

  call pfile%dealloc()
end program
