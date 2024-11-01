! attempting quadratures over the GPU

program main
  use cudafor
  use quadrature_module
  implicit none

  integer, parameter :: num_time_steps = 1000000
  integer, parameter :: num_elements = 1024
  integer :: i
  real :: t_start, t_end
  real, allocatable :: surface_elements(:,:)
  real :: observer_position(3)
  real, allocatable :: results(:)
  real :: total_time

  ! timing variables
  real :: time_generate, time_compute, time_total
  real :: t1, t2

  ! allocate arrays for the input of points
  allocate(surface_elements(num_elements,3))
  allocate(results(num_time_steps))

  !print *, 'Allocations complete, beginning program............................'

  ! setting observer position - for now just 0's
  observer_position = [0.0, 0.0, 0.0]

  ! start time tracking to see execution length of steps
  call cpu_time(t_start)

  ! generate surface elements
  do i = 1, num_elements
    surface_elements(i,1) = real(i) * 0.01      ! x-coordinate
    surface_elements(i,2) = real(i) * 0.01      ! y-coordinate
    surface_elements(i,3) = 0                   ! z-coordinate
  end do 

  ! time of surface generation
  call cpu_time(t1)
  time_generate = t1 - t_start
  print *, 'Time to generate surface elements:', time_generate, 'seconds.........................'

  ! loop over time steps
  do i = 1, num_time_steps
    call quadrature_gpu(surface_elements, observer_position, i, results(i))
  end do

  ! record quadrature time, output first few values
  call cpu_time(t2)
  time_compute = t2 - t1
  print *, 'Time to cpmute quadratures:", time_compute, "seconds.........................'
  time_total = t2 - t_start
  print *, 'Total execution time:', time_total, 'seconds.........................'

  ! output first few results to make sure it looks as expected
  print *, 'First few results:'
  do i = 1, 10
    print *, 'Time step', i, ':', results(i)
  end do

  deallocate(surface_elements)
  deallocate(results)
end program main
