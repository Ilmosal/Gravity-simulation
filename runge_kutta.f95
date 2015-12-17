module runge_kutta_mod
  use object_space_mod

contains

  subroutine run_runge_kutta(list, timescale, siz, steps, output_file, everyNth)
    implicit none
    integer, intent(in) :: siz, steps, everyNth
    real, intent(in) :: timescale
    character(LEN=100), intent(in) :: output_file
    integer :: i, j, x
    type(object_space), intent(inout) :: list(siz)

    open(unit=1, file=output_file, status='replace')

    write(1,*) siz
    write(1,*) (steps / everyNth)

    do j=1, steps

!Calculate acceleration
      do x=1, siz
        call total_acceleration(x, siz, list)
      end do

!Calculate runge_kutta
      do i=1, siz

        call calculate_runge_kutta(i, list, timescale, siz)

!Write position
        if (MOD(j, everyNth) == 1) then
          do x=1, siz
            write(1,*) list(x)%pos(1)
            write(1,*) list(x)%pos(2)
            write(1,*) list(x)%pos(3)
          end do
        end if
      end do

    end do

    close(unit=1, status='keep')

  end subroutine

!Calculate the the position
  subroutine calculate_runge_kutta(object, list, timescale, siz)
    implicit none
    integer, intent(in) :: object, siz
    real, intent(in) :: timescale
    real :: ax, ay, bx, by, cx, cy, dx, dy, velx, vely, posx, posy
    type(object_space), intent(inout) :: list(siz)

    list(object)%vel(1) = list(object)%vel(1) + list(object)%acc(1)*timescale
    list(object)%vel(2) = list(object)%vel(2) + list(object)%acc(2)*timescale

    velx = list(object)%vel(1)
    vely = list(object)%vel(2)
    posx = list(object)%pos(1)
    posy = list(object)%pos(2)

    ax = velx
    ay = vely

    list(object)%pos(1) = list(object)%pos(1) + 0.5 *timescale * ax
    list(object)%pos(2) = list(object)%pos(2) + 0.5 *timescale * ay

    call total_acceleration(object, siz, list)

    bx = list(object)%vel(1) + list(object)%acc(1) * timescale
    by = list(object)%vel(2) + list(object)%acc(2) * timescale

    list(object)%pos(1) = list(object)%pos(1) + 0.5 *timescale * bx
    list(object)%pos(2) = list(object)%pos(2) + 0.5 *timescale * by

    call total_acceleration(object, siz, list)

    cx = list(object)%vel(1) + list(object)%acc(1) * timescale
    cy = list(object)%vel(2) + list(object)%acc(2) * timescale

    list(object)%pos(1) = list(object)%pos(1) + timescale * cx
    list(object)%pos(2) = list(object)%pos(2) + timescale * cy

    dx = list(object)%vel(1) + cx * timescale
    dy = list(object)%vel(2) + cy * timescale

    list(object)%pos(1) = posx + timescale * (ax + 2 * bx + 2 * cx + dx) / 6
    list(object)%pos(2) = posy + timescale * (ay + 2 * by + 2 * cy + dy) / 6

  end subroutine calculate_runge_kutta

end module runge_kutta_mod
