module helper_module
    implicit none
    
    contains
  
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

    subroutine set_parameters(nx, ny, nt, n, m, do_serial)
        implicit none
      
        integer :: num_args, i, ios
        character(len=100) :: arg, next_arg
        integer, intent(out) :: nx, ny, nt, n, m
        logical :: nx_set, ny_set, nt_set, n_set, m_set
        logical, intent(out) :: do_serial
      
        ! manually set whether or not to do serial execution this run - allows programmer to skip serial if it is too slow.
        do_serial = .false.
      
        ! Initialize default values
        nt = 2**8
        nx = 2**10
        ny = 2**10
        n  = 1
        m  = 1
        nx_set = .false.
        ny_set = .false.
        nt_set = .false.
        n_set = .false.
        m_set = .false.
      
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