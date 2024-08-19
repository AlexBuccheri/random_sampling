program test_integer_mapping
  use iso_fortran_env
  use integer_mapping_m
  implicit none

  integer(int32) :: n_random, i, seed
  integer(int32) :: i_min, i_max, i_rand
  real(real64)   :: r_min, r_max, r_rand
  character(len=6) :: test

  ! Note, might not be the best way to set the seed
  CALL SYSTEM_CLOCK(COUNT=seed)

  ! Read settings
  open(unit=101, file='settings')
  read(101, *) test
  read(101, *) n_random

  if (trim(test) == 'real64') then
      read(101, *) r_min, r_max
      close(101)

      do i = 1, n_random
         !r_rand = random_number_real64(r_min, r_max, seed)
         write(*, *) i, r_rand
      enddo

   elseif (trim(test)=='int32') then
      read(101, *) i_min, i_max
      close(101)

      do i = 1, n_random
         i_rand = random_integer_int32(i_min, i_max, seed)
         write(*, *) i, i_rand
      enddo

   else
      error stop 101
  end if

end program test_integer_mapping
