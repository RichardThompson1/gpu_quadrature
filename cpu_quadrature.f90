module quadrature_module_cpu
  implicit none
  
  contains

  subroutine quadrature_cpu(x_array, y_array, t_array, result_array, hx, hy, n, m)
    implicit none
    
    integer, parameter :: dp = kind(1.0d0)
    real, intent(in) :: x_array(:)
    real, intent(in) :: y_array(:)
    real, intent(in) :: t_array(:)
    integer, intent(in) :: n, m
    real(kind=dp), intent(in) :: hx, hy
    real(kind=dp), intent(out) :: result_array(:)

    integer :: i, j, k
    real :: f
    real :: pi                       
    real :: w_ij

    pi = 4*atan(1.)

    do k = 1, size(t_array)
      do i = 1, size(x_array)
        do j = 1, size(y_array)
          if ((i == 1 .or. i == size(x_array)) .and. (j == 1 .or. j == size(y_array))) then
            w_ij = 1.0
          else if ((i == 1 .or. i == size(x_array)) .or. (j == 1 .or. j == size(y_array))) then
            w_ij = 2.0
          else
            w_ij = 4.0
          end if
          
          f = sin(n * pi * x_array(i)) * sin(m * pi * y_array(j))
          if (f == 0) then
            print *, 'No contribution at: (',i,j,k,') (x,y,t)'
          end if
          result_array(k) = result_array(k) + (w_ij * f) * (hx * hy / 4.0)
        end do
      end do
    end do

  end subroutine

end module quadrature_module_cpu