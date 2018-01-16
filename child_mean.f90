program mean
  use netcdf
  use mpi
  use fortaleza_common
  implicit none

  integer :: ierr, tot_count, request
  real :: rec_count
  type(parallel_file) :: pfile
  type(ncvar) :: var

  ! intializes both MPI and opens the netCDF file
  call pfile%init()

  if (associated(pfile%var)) then
     if (pfile%var%ultd_id /= -1) then
        tot_count = pfile%var%count_wo_ultd / pfile%np
        rec_count = pfile%var%shape(pfile%var%ultd_id)
        call pfile%get_var()
        call MPI_Isend(sum(pfile%var%real4D, dim=pfile%var%ultd_id) / rec_count, &
             tot_count, MPI_REAL, 0, 200, pfile%parent, request, ierr)
        call MPI_Wait(request, MPI_STATUS_IGNORE, ierr)
     endif
  endif

  ! finalizes MPI and closes the netCDF file
  call pfile%dealloc()
end program
