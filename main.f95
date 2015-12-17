program main
  use object_space_mod
  use verlet_integrator_mod
  use runge_kutta_mod
  use inputReader_mod
  implicit none

  character(100) :: filename, outputName
  integer :: numberofObjects, steps, everyNth, error, sim_type
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

  if (sim_type == 1) then 
    call run_verlet_integration(list, numberofObjects, timescale, steps, everyNth, outputName)
  else if (sim_type == 2) then
    call run_runge_kutta(list, timescale, numberofObjects, steps, outputName, everyNth)  
  end if
  print *, "Run succesful."
end program
