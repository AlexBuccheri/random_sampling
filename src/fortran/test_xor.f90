!> Note, this tests the mapping from [0, period) to [a, b), rather than
!! XOR specifically.
!!
!! gfortran src/xorshifts.f90 src/test.f90 -o test_random
program test_rand
  use iso_fortran_env
  use xorshifts
  implicit none

  integer(int32) :: n_random, i, seed
  integer(int32) :: i_min, i_max, i_rand
  real(real64)   :: r_min, r_max, r_rand
  character(len=6) :: test

  seed = 123_int32

  ! Read settings
  open(unit=101, file='settings')
  read(101, *) test
  read(101, *) n_random

  if (trim(test) == 'real64') then
      read(101, *) r_min, r_max
      close(101)

      do i = 1, n_random
         call random_real64(r_min, r_max, seed, r_rand)
         write(*, *) i, r_rand
      enddo

   elseif (trim(test)=='int32') then
      read(101, *) i_min, i_max
      close(101)

      do i = 1, n_random
         call random_int32(i_min, i_max, seed, i_rand)
         write(*, *) i, i_rand
      enddo

   else
      error stop 101
  end if

end program test_rand
