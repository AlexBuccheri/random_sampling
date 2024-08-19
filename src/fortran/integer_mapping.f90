module integer_mapping_m
   use iso_fortran_env, only: int32, int64, real64, real128
   use xorshifts
   implicit none

contains

   ! TODO(Alex)
   ! Passing seed as an arg then mutating it as the random value is based on Octopus,
   ! and is a shit design - need to change this
   ! TODO(Alex)
   ! Consider if this is biased. Pretty sure it is

   !> @brief Generate a random int32 in the range \f$ [x_{min}, x_{max}] \f$.
   !!
   !! Random numbers follow a uniform distribution, reducing the likelihood
   !! of generating duplicates, however as target range \f$ x_{max} - x_{min}\f$
   !! becomes much smaller than the random number period \f$N\f$
   !! the chances of obtaining duplicates greatly increases.
   !!
   !! Note, this was exactly the same as the `random_int32` routine in an older
   !! commit, except it's reformulated, and I replaced int with nint
   function random_integer_int32(x_min, x_max, seed) result(rand)
      integer(int32), intent(in)    :: x_min, x_max
      integer(int32), intent(inout) :: seed
      integer(int32)                :: rand

      real(real128) :: product
      integer       :: rand_0k
      real(real64) :: k, scale

      ! [0, max_value]
      call xorshift_rnd_32(seed)

      ! [x_min, x_max] - > [0, k]
      k = real(x_max - x_min + 1, real64)
      scale = real(seed, real64) / max_value_xorshift32_real
      ! Check - this step may not be needed
      product = scale * k
      rand_0k = int(product, int32)

      ! Map back to [x_min, x_max]
      rand = rand_0k + x_min

   end function random_integer_int32


   !> @brief Generate a random real64 in the range \f$ [x_{min}, x_{max}] \f$.
   function random_number_real64(x_min, x_max, seed) result(rand)
      real(real64),   intent(in   )  :: x_min, x_max
      integer(int32), intent(inout)  :: seed
      real(real64)    :: rand

      real(real64) :: scale

      call xorshift_rnd_32(seed)
      scale = real(seed, real64) / max_value_xorshift32_real
      rand = scale * (x_max - x_min + 1._real64) + x_min

   end function random_number_real64


   ! Simple Lemire mapping
   ! Return [0, range -1]
   ! Coded specifically for use with the implementation of `xorshift_rnd_32`
   ! Not properly tested
   integer(int32) function bounded_rand_simple(range, x)
      integer(int32), intent(in) :: range
      integer(int32), intent(inout) :: x

      integer(int32) :: l, t
      integer(int64) :: m

      t = mod(max_value_xorshift32_int, range)

      call xorshift_rnd_32(x)
      m = int(x, int64)*int(range, int64)
      l = int(m, int32)

      do while (l < t)
         call xorshift_rnd_32(x)
         m = int(x, int64)*int(range, int64)
         l = int(m, int32)
      end do

      ! Shift by 2^31 to return the upper 31 bits of m
      bounded_rand_simple = ishft(m, -31)

   end function bounded_rand_simple


   ! Efficient Lemire mapping
   ! Return [0, range -1]
   ! Coded specifically for use with the implementation of `xorshift_rnd_32`
   ! Not properly tested
   integer(int32) function bounded_rand(range, x)
      integer(int32), intent(in) :: range
      integer(int32), intent(inout) :: x

      integer(int32) :: l, t
      integer(int64) :: m

      call xorshift_rnd_32(x)
      m = int(x, int64) * int(range, int64)
      l = int(m, int32)

      ! If l < range, there's a risk of bias
      if (l < range) then
         t = -range
         if (t >= range) then
            t = t - range
            if (t >= range) then
               t = mod(t, range)
            end if
         end if

         ! Retry until l >= t to avoid bias
         do while (l < t)
            call xorshift_rnd_32(x)
            m = int(x, int64) * int(range, int64)
            l = int(m, int32)
         end do
      end if

      ! Return the upper 31 bits of m
      bounded_rand = ishft(m, -31)
   end function bounded_rand


end module integer_mapping_m
