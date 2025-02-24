

program main
  use cudafor
  use quadrature_module
  use quadrature_module_cpu
  implicit none

  ! nt = 2^20 takes 3.05 seconds, nt = 2^30 takes 672.89 seconds (at 128x128 gridpoints)
  ! nt = 2**5, nx = 2^16, ny = 2**16 takes 78.47796 seconds
  integer, parameter :: dp = kind(1.0d0)

  integer, parameter :: nt = 2**8
  integer, parameter :: nx = 2**12
  integer, parameter :: ny = 2**12
  integer, parameter :: n = 1
  integer, parameter :: m = 1
  real, parameter :: square_length = 1.0
  real, parameter :: square_height = 1.0
  integer :: i, j, k
  real(kind=dp) :: hx, hy
  real :: pi
  real :: error
  real :: accumulator = 0.0
  real :: minimum_quadrature = 2**10
  real :: maximum_quadrature = 0.0

  ! x, y, and time position of grid point
  real, allocatable :: x_array(:), y_array(:), t_array(:)

  ! results are per time step
  real(kind=dp), allocatable :: result_array(:)
  real(kind=dp), allocatable :: result_array_cpu(:)

  ! timing variables
  real :: time_generate, time_compute, time_total, time_total_cpu
  real :: t1, t2, t_start, t_end

  ! allocate arrays for the input of points x = nx+1 and y = ny + 1 for correct corner/edge points
  allocate(x_array(nx))
  allocate(y_array(ny))
  allocate(t_array(nt))
  allocate(result_array(nt))
  allocate(result_array_cpu(nt))


  ! distance between grid points
  hx = square_height / (nx)           !nx-1 is the number of segments, nx is number of points
  hy = square_length / (ny)           !ny-1 is the number of segments, ny is number of points


  !print *, 'Allocations complete, beginning program............................'

  ! start time tracking to see execution length of steps
  call cpu_time(t_start)

  ! x, y, and time points calculation - also initialize results array to 0.0 
  do i = 1, nx
    x_array(i) = i * hx
  end do
  do j = 1, ny
    y_array(j) = j * hy
  end do
  do k = 1, nt
    t_array(k) = k
    result_array(k) = 0.0
    result_array_cpu(k) = 0.0
  end do

  
  print *, "Points in x direction: ", size(x_array)
  print *, "Points in y direction: ", size(y_array)
  print *, "Points in time direction: ", size(t_array)
  ! flush(6)


  ! time of surface generation
  call cpu_time(t1)
  time_generate = t1 - t_start
  print *, 'Time to generate surface elements:', time_generate, 'seconds............'

  call quadrature_gpu(x_array, y_array, t_array, result_array, nx, ny, nt, hx, hy, n, m)

  ! record quadrature time, output first few values
  call cpu_time(t2)
  time_compute = t2 - t1
  print *, 'Time to compute quadratures:', time_compute, 'seconds..................'
  time_total = t2 - t_start
  print *, 'Total execution time:', time_total, 'seconds.........................'
  ! flush(6)

  ! output first few results to make sure it looks as expected
  ! print *, 'First few results:'
  ! do i = 1, 9
  !   print '(A, I4, A, D20.14)', 'Time step', i, ':', result_array(i)
  ! end do

  ! print *, 'Last few results:'
  ! do i = nt-9, size(result_array)
  !   print '(A, I4, A, D20.14)', 'Time step', i, ':', result_array(i)
  ! end do
  
  do i = 1, size(result_array)
    accumulator = accumulator + result_array(i)
    if (result_array(i) == 0.0) then
      print *, "Quadrature is 0 on step",i
    end if
    if (result_array(i) < minimum_quadrature .and. result_array(i) /= 0) then
      minimum_quadrature = result_array(i)
    end if
    if (result_array(i) > maximum_quadrature) then
      maximum_quadrature = result_array(i)
    end if
  end do
  print *, 'Total of all time steps:', accumulator
  print *, 'Maximum of all time steps:', maximum_quadrature
  print *, 'Minimum of all time steps:', minimum_quadrature
  ! print *, 'max - min:', (maximum_quadrature - minimum_quadrature)
  write(*,'(A,g14.7)') 'max - min:', (maximum_quadrature - minimum_quadrature)

  
  print *, 'Running Quadrature on CPU.................................................'
  call cpu_time(t_start)

  call quadrature_cpu(x_array, y_array, t_array, result_array_cpu, hx, hy, n, m)

  call cpu_time(t1)
  time_total_cpu = t1 - t_start
  print *, 'Total execution time:', time_total_cpu, 'seconds.........................'
  print *, 'Checking result values from CPU quadrature ............................'

  accumulator = 0
  do i = 1, size(result_array_cpu)
    accumulator = accumulator + result_array_cpu(i)
    if (result_array_cpu(i) == 0.0) then
      print *, "Quadrature is 0 on step",i
    end if
    if (result_array_cpu(i) < minimum_quadrature .and. result_array_cpu(i) /= 0) then
      minimum_quadrature = result_array_cpu(i)
    end if
    if (result_array_cpu(i) > maximum_quadrature) then
      maximum_quadrature = result_array_cpu(i)
    end if
  end do
  print *, 'Total of all time steps:', accumulator
  print *, 'Maximum of all time steps:', maximum_quadrature
  print *, 'Minimum of all time steps:', minimum_quadrature
  write(*,'(A,g14.7)') 'max - min:', (maximum_quadrature - minimum_quadrature)

  print *, 'Calculations on GPU are approximately ', (time_total_cpu/time_total), ' times faster than on CPU'

  !print *, 'Deallocating...............................'
  
  !deallocate(x_array)
  !deallocate(y_array)
  !deallocate(t_array)
  !deallocate(result_array)
end program main
