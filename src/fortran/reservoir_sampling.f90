! Transcribed from https://gist.github.com/Pseudomanifold/1ed2b3b5b0a1389bdad97b2fdf5af47e
! Not Knuth's Algorithm S
program ReservoirSampling
  use iso_fortran_env
  implicit none

  integer, parameter :: N = 1000, m = 100
  integer, dimension(m) :: selected
  integer :: i,j, i_element, cnt, seed
  real(kind=real64) :: rand_num

  ! Reservoir sampling algorithm
  cnt = 0
  do i = 1, N
     call random_number(rand_num)

    if (real(N - i + 1) * rand_num < m - cnt) then
      cnt = cnt + 1
      selected(cnt) = i
    end if

    if (cnt == m) exit
  end do

  ! check for duplicates
  do i = 1, m
     i_element = selected(i)
     do j = i + 1, m
        if (i_element == selected(j)) then
           write(*, *) 'Elements', i, 'and', j, 'are the same:', i_element, selected(j)
        endif
     enddo
  enddo

end program ReservoirSampling
