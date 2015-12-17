program main
  use object_space_mod
  use verlet_integrator_mod
  use inputReader_mod
  implicit none

  character(100) :: filename, outputName
  integer :: numberofObjects, steps, everyNth, error
  real :: timescale
  type(object_space), allocatable :: list(:)

  filename = "input.dat"

  error = readStats(filename, outputName, numberofObjects, timescale, steps, everyNth)

  if (error == 0) then
    stop
  end if

  error = readObjects(filename, list, numberofObjects)

  if (error == 0) then
    stop
  end if

  call run_verlet_integration(list, numberofObjects, timescale, steps, everyNth, outputName)

  print *, "Run succesful."
end program
