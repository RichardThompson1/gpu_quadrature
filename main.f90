

program main
  use cudafor
  use quadrature_module
  use quadrature_module_cpu
  use helper_module
  implicit none

  ! nt = 2^20 takes 3.05 seconds, nt = 2^30 takes 672.89 seconds (at 128x128 gridpoints)
  ! nt = 2**5, nx = 2^16, ny = 2**16 takes 78.47796 seconds
  integer, parameter :: dp = kind(1.0d0)

  real, parameter :: square_length = 1.0
  real, parameter :: square_height = 1.0
  integer :: i, j, k
  integer :: nt, nx, ny, n, m 
  real(kind=dp) :: hx, hy
  real :: pi
  real :: error
  real :: integral_value
  real :: result_value
  real :: accumulator = 0.0
  real :: minimum_quadrature = 2**10
  real :: maximum_quadrature = 0.0

  ! x, y, and time position of grid point
  real, allocatable :: x_array(:), y_array(:), t_array(:)

  ! results are per time step
  real(kind=dp), allocatable :: result_array(:)
  real(kind=dp), allocatable :: result_array_cpu_parallel(:)
  real(kind=dp), allocatable :: result_array_cpu_serial(:)

  ! timing variables
  real :: time_generate, time_compute, time_total, time_total_cpu
  real :: time_gpu, time_cpu_parallel, time_cpu_serial
  real :: error_gpu, error_cpu_parallel, error_cpu_serial
  real :: t1, t2, t_start, t_end

  logical :: do_serial

  ! set parameters, check for command line input
  call set_parameters(nx, ny, nt, n, m, do_serial)

  !get analytical solution for comparison
  call analytical_solution(n, m, integral_value)

  ! allocate arrays for the input of points x = nx+1 and y = ny + 1 for correct corner/edge points
  allocate(x_array(nx))
  allocate(y_array(ny))
  allocate(t_array(nt))
  allocate(result_array(nt))
  allocate(result_array_cpu_parallel(nt))
  allocate(result_array_cpu_serial(nt))


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
    result_array_cpu_parallel(k) = 0.0
    result_array_cpu_serial(k) = 0.0
  end do

  
  print *, "Points in x direction: ", size(x_array)
  print *, "Points in y direction: ", size(y_array)
  print *, "Points in time direction: ", size(t_array)
  ! flush(6)


  ! time of surface generation
  call cpu_time(t1)
  time_generate = t1 - t_start
  print *, 'Time to generate surface elements:', time_generate, 'seconds..................'

  print *, '\nRunning Quadrature on GPU ..................................................'
  call quadrature_gpu(x_array, y_array, t_array, result_array, nx, ny, nt, hx, hy, n, m)

  ! record quadrature time, output first few values
  call cpu_time(t2)
  time_gpu = t2 - t1
  print *, 'Time to compute quadratures:', time_gpu, 'seconds........................'
  time_total = t2 - t_start
  print *, 'Total execution time:', time_total, 'seconds...............................'
  ! flush(6)

  ! Compute maximum error for GPU quadrature
  error_gpu = 0.0_dp
  do i = 1, size(result_array)
    if ( abs(result_array(i) - integral_value) > error_gpu ) then
      error_gpu = abs(result_array(i) - integral_value)
    end if
  end do

  !CODE FOR VALIDATING RESULTS - USE FOR TROUBLESHOOTING
  ! do i = 1, size(result_array)
  !   accumulator = accumulator + result_array(i)
  !   if (result_array(i) == 0.0) then
  !     print *, "Quadrature is 0 on step",i
  !   end if
  !   if (result_array(i) < minimum_quadrature .and. result_array(i) /= 0) then
  !     minimum_quadrature = result_array(i)
  !   end if
  !   if (result_array(i) > maximum_quadrature) then
  !     maximum_quadrature = result_array(i)
  !   end if
  ! end do
  ! print *, 'Total of all time steps:', accumulator
  ! print *, 'Maximum of all time steps:', maximum_quadrature
  ! print *, 'Minimum of all time steps:', minimum_quadrature
  ! ! print *, 'max - min:', (maximum_quadrature - minimum_quadrature)
  ! write(*,'(A,g14.7)') 'max - min:', (maximum_quadrature - minimum_quadrature)

  !concurrent CPU execution of quadrature with OpenMP
  print *, '\nRunning Quadrature on CPU (parallel) .......................................'
  call cpu_time(t_start)

  call quadrature_cpu_parallel(x_array, y_array, t_array, result_array_cpu_parallel, hx, hy, n, m)

  call cpu_time(t1)
  time_cpu_parallel = t1 - t_start
  print *, 'Total execution time:', time_cpu_parallel, 'seconds...............................'
  print *, 'Checking result values from CPU quadrature (parallel) ......................'

  ! Compute maximum error for CPU parallel quadrature
  error_cpu_parallel = 0.0_dp
  do i = 1, size(result_array_cpu_parallel)
    if ( abs(result_array_cpu_parallel(i) - integral_value) > error_cpu_parallel ) then
      error_cpu_parallel = abs(result_array_cpu_parallel(i) - integral_value)
    end if
  end do

  !CODE FOR VALIDATING RESULTS - USE FOR TROUBLESHOOTING
  ! accumulator = 0
  ! do i = 1, size(result_array_cpu_parallel)
  !   accumulator = accumulator + result_array_cpu_parallel(i)
  !   if (result_array_cpu_parallel(i) == 0.0) then
  !     print *, "Quadrature is 0 on step",i
  !   end if
  !   if (result_array_cpu_parallel(i) < minimum_quadrature .and. result_array_cpu_parallel(i) /= 0) then
  !     minimum_quadrature = result_array_cpu_parallel(i)
  !   end if
  !   if (result_array_cpu_parallel(i) > maximum_quadrature) then
  !     maximum_quadrature = result_array_cpu_parallel(i)
  !   end if
  ! end do
  ! print *, 'Total of all time steps:', accumulator
  ! print *, 'Maximum of all time steps:', maximum_quadrature
  ! print *, 'Minimum of all time steps:', minimum_quadrature
  ! write(*,'(A,g14.7)') 'max - min:', (maximum_quadrature - minimum_quadrature)

  print *, 'Calculations on GPU are approximately ', (time_cpu_parallel/time_total), ' times faster than concurrent on CPU'

  if (do_serial) then
    !serial CPU execution of quadrature 
    print *, '\nRunning Quadrature on CPU (serial)..........................................'
    call cpu_time(t_start)

    call quadrature_cpu_serial(x_array, y_array, t_array, result_array_cpu_serial, hx, hy, n, m)

    call cpu_time(t1)
    time_cpu_serial = t1 - t_start
    print *, 'Total execution time:', time_cpu_serial, 'seconds...............................'
    print *, 'Checking result values from CPU quadrature (serial) ........................'

    ! Compute maximum error for CPU serial quadrature
    error_cpu_serial = 0.0_dp
    do i = 1, size(result_array_cpu_serial)
      if ( abs(result_array_cpu_serial(i) - integral_value) > error_cpu_serial ) then
        error_cpu_serial = abs(result_array_cpu_serial(i) - integral_value)
        !print *, "CPU serial error ", error_cpu_serial, "at step ", i
      end if
    end do

    !CODE FOR VALIDATING RESULTS - USE FOR TROUBLESHOOTING
    ! accumulator = 0
    ! do i = 1, size(result_array_cpu_serial)
    !   accumulator = accumulator + result_array_cpu_serial(i)
    !   if (result_array_cpu_serial(i) == 0.0) then
    !     print *, "Quadrature is 0 on step",i
    !   end if
    !   if (result_array_cpu_serial(i) < minimum_quadrature .and. result_array_cpu_serial(i) /= 0) then
    !     minimum_quadrature = result_array_cpu_serial(i)
    !   end if
    !   if (result_array_cpu_serial(i) > maximum_quadrature) then
    !     maximum_quadrature = result_array_cpu_serial(i)
    !   end if
    ! end do
    ! print *, 'Total of all time steps:', accumulator
    ! print *, 'Maximum of all time steps:', maximum_quadrature
    ! print *, 'Minimum of all time steps:', minimum_quadrature
    ! write(*,'(A,g14.7)') 'max - min:', (maximum_quadrature - minimum_quadrature)

    print *, 'Calculations on GPU are approximately ', (time_cpu_serial/time_gpu), ' times faster than serial on CPU'
  else if (.not.(do_serial)) then
      time_cpu_serial = 0.0
      error_cpu_serial = 0.0
  endif

  call save_run_info("result_run_no_serial.csv", nx, ny, nt, n, m, time_gpu, time_cpu_serial, time_cpu_parallel, error_gpu, error_cpu_serial, error_cpu_parallel)

end program main
