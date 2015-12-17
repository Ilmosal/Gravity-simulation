module object_space_mod
  implicit none
  integer, parameter :: MAXBUF=20
!Defining the object
  type :: object_space
    character(MAXBUF) :: obj_name
    real :: pos(3), vel(3), acc(3), mass
  end type object_space

contains

!Function for creating a new Object
  type(object_space) function newObject(n, p, v, m)
    implicit none
    integer, parameter :: MAXBUFF=20
    character(MAXBUFF), intent(in) :: n
    real, intent(in) :: p(3), v(3), m
    real :: a(3)=0

    newObject=object_space(n, p, v, a, m)
  end function newObject

!Subroutine for calculating the total acceleration of an object.
  subroutine total_acceleration(object, siz, list)
    implicit none
    integer, intent(in) :: object, siz
    type(object_space), intent(inout) :: list(siz)
    integer :: i, j

!zero the existing acceleration
    list(object)%acc=0


!Loop through the list, skip the object that we are currently moving and calculate total acceleration a(t) 
    do i=1, siz
      if (list(object)%obj_name /= list(i)%obj_name) then
        !Calculate acceleration for all components
        do j=1, 3
          list(object)%acc(j) = list(object)%acc(j) + calc_acceleration(list(object), list(i), j)
        end do
      end if
    end do
  end subroutine total_acceleration

!Calculate acceleration for one component
  real function calc_acceleration(object1, object2, comp)
     implicit none
     type(object_space), intent(in) :: object1, object2
     real :: acc, grav_acc
     integer :: comp

     grav_acc = 4*(3.1415**2)

     if (dist_between(object1, object2) /= 0) then
       acc = grav_acc*(object2%pos(comp) - object1%pos(comp)) * object2%mass/(dist_between(object1, object2)**3)
     else
       acc = 0
     end if

     calc_acceleration = acc
  end function calc_acceleration

!Distance between objects o1 and o2
  real function dist_between(o1, o2)
    implicit none
    type(object_space), intent(in) :: o1, o2
    real :: distance

    distance = ((o2%pos(1)-o1%pos(1))**2 + (o2%pos(2)-o1%pos(2))**2 + (o2%pos(3)-o1%pos(3))**2)**0.5

    dist_between = distance
  end function dist_between

!print out info of the object
  subroutine print_object(object)
    implicit none
    type(object_space) :: object
    print *, object%obj_name, object%pos(1), object%pos(2), object%pos(3)
  end subroutine

end module
