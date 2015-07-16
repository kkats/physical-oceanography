subroutine size_r14 (nx, ny)
  integer,parameter :: r14 = selected_real_kind(14,30)
  real(kind=r14) :: x
  double precision :: y
  integer nx, ny
          
  nx = sizeof(x)
  ny = sizeof(y)
  !print *, precision(x), range(x)
  !print *, precision(y), range(y)
  !print *, sizeof(x), sizeof(y)
end subroutine size_r14
