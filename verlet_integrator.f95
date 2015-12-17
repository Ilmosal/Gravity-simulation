module verlet_integrator_mod
  use object_space_mod
  implicit none

contains

  subroutine run_verlet_integration(list, siz, timescale, lenght, everyNth, output_file)
    implicit none
    integer, intent(in) :: siz, lenght, everyNth
    real, intent(in) :: timescale
    type(object_space), intent(inout) :: list(siz)
    character(LEN=100), intent(in) :: output_file
    integer :: i, step, ios

!Opening the file for output
    open(unit=1, file=output_file, status='replace')

!write the amount of objects in simulation and the amount of frames
    write(1,*) siz
    write(1,*) (lenght / everyNth)

!calculate all initial accelerations for the objects
    do i = 1, siz
      call total_acceleration(i, siz, list)
    end do

    do step = 1, lenght
      !do the first step for all objects
      do i = 1, siz
        call verlet_integration_first(i, siz, list, timescale)
      end do

      !Second step for all objects
      do i = 1, siz
        call verlet_integration_second(i, siz, list, timescale)
      end do

      !Third step for all objects. Identical to function total acceleration
      do i = 1, siz
        call total_acceleration(i, siz, list)
      end do

      !Fourth step for all objects
      do i = 1, siz
        call verlet_integration_fourth(i, siz, list, timescale)
      end do

      if (MOD(step, everyNth) == 1) then
        do i = 1, siz
          write (1,*) list(i)%pos(1)
          write (1,*) list(i)%pos(2)
          write (1,*) list(i)%pos(3)
        end do
      end if
    end do

!Closing the file
    close(unit=1, status='keep')

  end subroutine


!Subroutine for one iteration of verlet_integration for one object the first phase. siz is the amount of objects in list
  subroutine verlet_integration_first(object, siz, list, timescale)
    implicit none
    integer, intent(in) :: siz, object
    real, intent(in) :: timescale
    type(object_space), intent(inout) :: list(siz)
    integer :: i

!Do the first step v(t + 0.5*dt) = v(y) + .5a(t)*dt for one object
    do i = 1, 3
      list(object)%vel(i) = list(object)%vel(i)+0.5*list(object)%acc(i)*timescale
    end do
  end subroutine verlet_integration_first

!Subroutine for one iteration of verlet integration for one object for the second phase of the integrator.
  subroutine verlet_integration_second(object, siz, list, timescale)
    implicit none
    integer, intent(in) :: object, siz
    type(object_space), intent(inout) :: list(siz)
    real, intent(in) :: timescale
    integer :: i

!Do the second step x(t + dt) = x(t) + v(t + 0.5*dt)*dt
    do i = 1, 3
      list(object)%pos(i) = list(object)%pos(i)+list(object)%vel(i)*timescale
    end do

  end subroutine

!the fourth phase of verlet integration. Calculating the final velocity for the next phase
  subroutine verlet_integration_fourth(object, siz, list, timescale)
    implicit none
    integer, intent(in) :: object, siz
    type(object_space), intent(inout) :: list(siz)
    real, intent(in) :: timescale
    integer :: i
    real :: acc(3)

!Calculate the v(t + dt) = v(t + 0.5*dt) + 0.5*a(t + dt)*timescale
    do i = 1, 3
      list(object)%vel(i)=list(object)%vel(i)+0.5*list(object)%acc(i)*timescale
    end do

  end subroutine

end module verlet_integrator_mod
