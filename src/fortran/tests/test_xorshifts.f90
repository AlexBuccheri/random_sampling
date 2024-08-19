program test_xorshifts
   use iso_fortran_env
   use xorshifts
   implicit none

   integer(int32) :: x_min, x_max, seed, i

   CALL SYSTEM_CLOCK(COUNT=seed)
   x_min = 100_int32
   x_max = 1000_int32

   do i = 1, 100
      call xorshift_rnd(seed)
      write(*, *) seed  !, mod(seed, 2)
   enddo

end program test_xorshifts
