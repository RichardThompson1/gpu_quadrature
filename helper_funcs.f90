module helper_module
    implicit none
    
    !AoS version:
    ! (x,y) points
    type :: Point
      real :: x
      real :: y
    end type
    ! timestep array of points
    type :: TimeStep_AoS
      type(Point), allocatable :: points(:)
    end type

    !SoA version:
    ! sub arrays are of length nx or ny
    type :: SubArray
      real, allocatable :: data(:)
    end type
    ! each timestep has nx*ny x,y points
    type :: TimeStep_SoA
      type(SubArray), allocatable :: points(:)
    end type

    contains

    subroutine generate_patch(nt, nx, ny, hx, hy, n, m, time_array, observer, direct_solution)
      implicit none
      
      integer, parameter :: dp = kind(1.0d0)

      !type(TimeStep_AoS), allocatable, intent(out) :: time_array(:)
      real, allocatable, intent(out) :: time_array(:,:,:)
      real(kind=dp), allocatable :: origin(:)
      integer, intent(in) :: nt, nx, ny, n, m
      real(kind=dp), intent(in) :: hx, hy
      real, intent(in) :: observer(:)
      real(kind=dp), allocatable, intent(out) :: direct_solution(:)
      integer :: t, i, j, idx, nPoints

      real :: theta, cos_theta, sin_theta
      real :: x_obs, y_obs
      real(kind=dp) :: x_local, y_local
      real(kind=dp) :: r, f, w, quad_sum
      
      real(kind=dp), parameter :: pi = 4*atan(1.)

      x_obs = observer(1)
      y_obs = observer(2)
  
      ! Total number of points in the flattened grid
      nPoints = nx * ny

      !increase to 3 if we move to 3D
      allocate(origin(2))
      allocate(time_array(nt,(nx*ny),2))
      allocate(direct_solution(nt))

      ! point the patch rotates around -  hard coded to (-3.2,-2.3)
      origin(1) = -1.0
      origin(2) = 0.5

      !$OMP PARALLEL DO SCHEDULE(DYNAMIC) PRIVATE(t,i,j,theta,cos_theta,sin_theta,x_local, y_local, idx, quad_sum, r, f, w)
      do t = 1, nt
        
        ! rotation angle for this timestep 
        theta = 2.0 * 4*atan(1.) * t / nt
        cos_theta = cos(theta)
        sin_theta = sin(theta)

        quad_sum = 0.0

        ! Loop over the 2D grid and compute the rotated coordinates.
        do j = 1, ny
          do i = 1, nx
            ! this line maps (i,j) to a 1D index - this is the flattening.
            idx = (j - 1) * nx + i  

            ! Compute the original coordinates relative to the origin.
            x_local = (i * hx) - origin(1)
            y_local = (j * hy) - origin(2)

            ! Apply the rotation transformation.
            time_array(t, idx, 1) = x_local*cos_theta - y_local*sin_theta + origin(1)
            time_array(t, idx, 2) = x_local*sin_theta + y_local*cos_theta + origin(2)

            ! uncomment this to remove rotation.
            ! time_array(t, idx, 1) = (i * hx)
            ! time_array(t, idx, 2) = (j * hy)
            
            ! observer dist
            r = sqrt((time_array(t, idx, 1) - x_obs)**2 + (time_array(t, idx, 2) - y_obs)**2)

            ! integrand
            f = ( sin(n*pi*time_array(t, idx, 1)) * sin(m*pi*time_array(t, idx, 2)) ) / r

            if ((i == 1 .or. i == nx) .and. (j == 1 .or. j == ny)) then
              w = 1.0
            else if ((i == 1 .or. i == nx) .or. (j == 1 .or. j == ny)) then
              w = 2.0
            else
              w = 4.0
            end if

            quad_sum = quad_sum + w * f

          end do
        end do
        
        direct_solution(t) = quad_sum * (hx * hy / 4.0)

      end do
      !$OMP END PARALLEL DO

      do i = 1, nt
        print *, "Direct solution", i, ":", direct_solution(i)
      end do

      ! to verify correct panel rotations.
      ! call save_time_array(time_array, observer)
    end subroutine


    subroutine save_time_array(time_array, observer)
      implicit none
      real, intent(in), allocatable :: time_array(:,:,:)
      integer :: i, j
      integer :: n_timesteps, n_points
      integer :: unit
      real, intent(in) :: observer(:)
    
      n_timesteps = size(time_array, 1)
      n_points    = size(time_array, 2)
    
      open(newunit=unit, file="panel.txt", status='replace', action='write', form='formatted')
    
      do i = 1, n_timesteps
         write(unit, '(A,I0)') "# Timestep ", i
         do j = 1, n_points
            write(unit, '(F10.6, A, F10.6)') time_array(i, j, 1), ",", time_array(i, j, 2)
         end do
         write(unit,*)
      end do
      
      !observer pos
      write(unit, '(F10.6, A, F10.6)') observer(1), ",", observer(2)
    
      close(unit)
    end subroutine save_time_array    

  
    subroutine analytical_solution(n, m, integral_value)

      implicit none
      
      integer, parameter :: dp = kind(1.0d0)
      integer, intent(in) :: n, m
      real, intent(out) :: integral_value
  
      real :: pi
  
      pi = 4*atan(1.)
  
      ! Compute the integral using the analytical formula
      integral_value = ((1.0 - cos(n * acos(-1.0))) / (n * acos(-1.0))) * &
                       ((1.0 - cos(m * acos(-1.0))) / (m * acos(-1.0)))

      print *, "Integral value: ", integral_value
    end subroutine

    subroutine set_parameters(nx, ny, nt, n, m, do_serial, observer)
        implicit none
      
        integer :: num_args, i, ios
        character(len=100) :: arg, next_arg
        integer, intent(out) :: nx, ny, nt, n, m
        logical :: nx_set, ny_set, nt_set, n_set, m_set, s_set, o_set
        logical, intent(out) :: do_serial
        real, allocatable, intent(out) :: observer(:)
      
        ! manually set whether or not to do serial execution this run - allows programmer to skip serial if it is too slow.
        do_serial = .false.

        allocate(observer(2))
        observer(1) = -1.0
        observer(2) = 0.5
      
        ! Initialize default values
        nt = 2**4
        nx = 2**10
        ny = 2**10
        n  = 1
        m  = 1
        nx_set = .false.
        ny_set = .false.
        nt_set = .false.
        n_set = .false.
        m_set = .false.
        s_set = .false.
        o_set = .false.
      
        num_args = COMMAND_ARGUMENT_COUNT()
      
        ! Parse args
        i = 1
        do while (i <= num_args)
          call GET_COMMAND_ARGUMENT(i, arg)
      
          if (trim(arg) == '-x') then
            if (i + 1 <= num_args) then
              call GET_COMMAND_ARGUMENT(i + 1, next_arg)
              read(next_arg, '(I10)', IOSTAT=ios) nx
              if (ios == 0) nx_set = .true.
            endif
            i = i + 1
          else if (trim(arg) == '-y') then
            if (i + 1 <= num_args) then
              call GET_COMMAND_ARGUMENT(i + 1, next_arg)
              read(next_arg, '(I10)', IOSTAT=ios) ny
              if (ios == 0) ny_set = .true.
            endif
            i = i + 1
          else if (trim(arg) == '-t') then
            if (i + 1 <= num_args) then
              call GET_COMMAND_ARGUMENT(i + 1, next_arg)
              read(next_arg, '(I10)', IOSTAT=ios) nt
              if (ios == 0) nt_set = .true.
            endif
            i = i + 1
          else if (trim(arg) == '-n') then
            if (i + 1 <= num_args) then
              call GET_COMMAND_ARGUMENT(i + 1, next_arg)
              read(next_arg, '(I10)', IOSTAT=ios) n
              if (ios == 0) n_set = .true.
            endif
            i = i + 1
          else if (trim(arg) == '-m') then
            if (i + 1 <= num_args) then
              call GET_COMMAND_ARGUMENT(i + 1, next_arg)
              read(next_arg, '(I10)', IOSTAT=ios) m
              if (ios == 0) m_set = .true.
            endif
            i = i + 1
          else if (trim(arg) == '-s') then
            if (i + 1 <= num_args) then
              call GET_COMMAND_ARGUMENT(i + 1, next_arg)
              read(next_arg, '(I10)', IOSTAT=ios) do_serial
              if (ios == 0) s_set = .true.
            endif
            i = i + 1
          ! else if (trim(arg) == '-m') then
          !   if (i + 1 <= num_args) then
          !     call GET_COMMAND_ARGUMENT(i + 1, next_arg)
          !     read(next_arg, '(I10)', IOSTAT=ios) observer
          !     if (ios == 0) o_set = .true.
          !   endif
          !   i = i + 1
          endif
      
          i = i + 1
        end do
      
        ! Print if something was set by command line
        ! if (nx_set) print *, "nx =", nx
        ! if (ny_set) print *, "ny =", ny
        ! if (nt_set) print *, "nt =", nt
        ! if (n_set)  print *, "n =", n
        ! if (m_set)  print *, "m =", m
      end subroutine
      
      subroutine save_run_info(filename, nx, ny, nt, n, m, &
        gpu_time, cpu_serial_time, cpu_parallel_time, &
        gpu_error, cpu_serial_error, cpu_parallel_error)
        implicit none
        character(len=*), intent(in) :: filename
        integer, intent(in) :: nx, ny, nt, n, m
        
        real, optional, intent(in) :: gpu_time, cpu_serial_time, cpu_parallel_time
        real, optional, intent(in) :: gpu_error, cpu_serial_error, cpu_parallel_error
        integer :: unit
        character(len=32) :: gpu_time_str, cpu_serial_time_str, cpu_parallel_time_str
        character(len=32) :: gpu_error_str, cpu_serial_error_str, cpu_parallel_error_str
      
        ! Convert optional values to strings or "N/A"
        if (present(gpu_time)) then
          write(gpu_time_str, '(F10.6)') gpu_time
        else
          gpu_time_str = "N/A"
        end if
      
        if (present(cpu_serial_time)) then
          write(cpu_serial_time_str, '(F10.6)') cpu_serial_time
        else
          cpu_serial_time_str = "N/A"
        end if
      
        if (present(cpu_parallel_time)) then
          write(cpu_parallel_time_str, '(F10.6)') cpu_parallel_time
        else
          cpu_parallel_time_str = "N/A"
        end if
      
        if (present(gpu_error)) then
          write(gpu_error_str, '(F10.6)') gpu_error
        else
          gpu_error_str = "N/A"
        end if
      
        if (present(cpu_serial_error)) then
          write(cpu_serial_error_str, '(F10.6)') cpu_serial_error
        else
          cpu_serial_error_str = "N/A"
        end if
      
        if (present(cpu_parallel_error)) then
          write(cpu_parallel_error_str, '(F10.6)') cpu_parallel_error
        else
          cpu_parallel_error_str = "N/A"
        end if
      
        ! Open the file in append mode (or create it if it doesn't exist).
        open(newunit=unit, file=filename, status="unknown", action="write", position="append")
      
        ! Write out a CSV line in the order:
        ! nx, ny, nt, n, m, gpu_time, cpu_serial_time, cpu_parallel_time,
        ! gpu_error, cpu_serial_error, cpu_parallel_error
        write(unit, '(I0, ",", I0, ",", I0, ",", I0, ",", I0, ",", &
          A, ",", A, ",", A, ",", A, ",", A, ",", A)') &
          nx, ny, nt, n, m, trim(gpu_time_str), trim(cpu_serial_time_str), &
          trim(cpu_parallel_time_str), trim(gpu_error_str), trim(cpu_serial_error_str), &
          trim(cpu_parallel_error_str)
      
        close(unit)
      end subroutine save_run_info

end module helper_module