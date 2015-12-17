module inputReader_mod
  use object_space_mod
  implicit none

contains

!Function for reading the file
  integer function readStats(filename, outputName, numberofObjects, timescale, steps, everyNth)
    implicit none
    character(LEN=100), intent(in) :: filename
    character(LEN=100), intent(out) :: outputName
    integer, intent(out) :: numberofObjects, steps, everyNth
    integer :: i, ios
    real, intent(out) :: timescale

    open(unit=1, file=filename, iostat=ios, status='old')

    !Error handling    
    if (ios/=0) then
      print *, "error opening file: ", trim(filename), ". No such file"
      readStats = 0
      return
    end if

    read(1,*)
    read(1,*,iostat=ios) outputName
    read(1,*)
    read(1,*)
    read(1,*,iostat=ios) numberofObjects
    read(1,*)
    read(1,*)
    read(1,*,iostat=ios) timescale
    read(1,*)
    read(1,*)
    read(1,*,iostat=ios) steps
    read(1,*)
    read(1,*)
    read(1,*,iostat=ios) everyNth

    close(unit=1, status='keep')
    readStats=1
  end function readStats

  integer function readObjects(filename, list, numberof)
    implicit none
    integer, intent(in) :: numberof
    character(len=100), intent(in) :: filename
    type(object_space), allocatable, intent(out) :: list(:)
    type(object_space) :: temp
    integer :: ios, i
    character(len=100) :: planetName
    real :: temp1, temp2, temp3, pos(3), vel(3)

    allocate(list(numberof))

    open(unit=1, file=filename, iostat=ios, status='old')

!Skip the first 14 lines
    do i=1, 14
      read(1,*)
    end do

!Read the planet information
    do i=1, numberof
      read(1,*)
      read(1,*) planetName
      read(1,*) temp1, temp2, temp3
      pos = (/temp1, temp2, temp3/)
      read(1,*) temp1, temp2, temp3
      vel = (/temp1, temp2, temp3/)
      read(1,*) temp1

      list(i)=newObject(planetName, pos, vel, temp1)
    end do
    close(unit=1, status='keep')
    readObjects=1
  end function readObjects

end module
