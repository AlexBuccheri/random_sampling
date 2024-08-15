module integer_mapping_m
   use iso_fortran_env
   use xorshifts
   implicit none
contains

   ! Efficient Lemire mapping
   ! Return [0, range -1]
   ! Not tested
   integer(int32) function bounded_rand(range, x)
      integer(int32), intent(in) :: range
      integer(int32), intent(inout) :: x

      integer(int32) :: l, t
      integer(int64) :: m

      call xorshift_rnd_32(x)
      m = int(x, int64)*int(range, int64)
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
            m = int(x, int64)*int(range, int64)
            l = int(m, int32)
         end do
      end if

      ! Return the upper 32 bits of m
      bounded_rand = ishft(m, -32)
   end function bounded_rand

   ! Simple Lemire mapping
   ! Return [0, range -1]
   ! Not tested
   integer(int32) function bounded_rand_simple(range, x)
      integer(int32), intent(in) :: range
      integer(int32), intent(inout) :: x
      integer(int64), parameter :: max = 4294967296_int64 !2^32

      integer(int32) :: l, t
      integer(int64) :: m

      t = mod(max, range)
      call xorshift_rnd_32(x)
      m = int(x, int64)*int(range, int64)
      l = int(m, int32)

      do while (l < t)
         call xorshift_rnd_32(x)
         m = int(x, int64)*int(range, int64)
         l = int(m, int32)
      end do

      bounded_rand_simple = ishft(m, -32)

   end function bounded_rand_simple

   !> @brief Generate a random real64 in the range \f$ [x_{min}, x_{max}] \f$.
   subroutine random_real64(x_min, x_max, seed, rand)
      real(real64), intent(in)    :: x_min, x_max
      integer(int32), intent(inout) :: seed
      real(real64), intent(out)   :: rand

      real(real64), parameter :: s_max = 4294967295_real64 !< 2^32 - 1
      real(real64) :: u

      call xorshift_rnd_32(seed)

      u = real(seed, real64)/s_max
      ! Should this be +1?
      rand = u*(x_max - x_min + 1) + x_min

   end subroutine random_real64

   !> @brief Generate a random int32 in the range \f$ [x_{min}, x_{max}] \f$.
  !!
  !! Random numbers follow a uniform distribution, reducing the likelihood
  !! of generating duplicates, however as target range \f$ x_{max} - x_{min}\f$
  !! becomes much smaller than the random number period \f$N\f$
  !! the chances of obtaining duplicates greatly increases.
   subroutine random_int32(x_min, x_max, seed, rand)
      integer(int32), intent(in)    :: x_min, x_max
      integer(int32), intent(inout) :: seed
      integer(int32), intent(out)   :: rand

      real(real64), parameter :: s_max = 4294967295_real64 !< 2^32 - 1
      real(real64) :: u

      call xorshift_rnd_32(seed)

      ! Transform range to [0, 1], from the choice of smax
      u = real(seed, real64)/s_max
      ! Should this be +1?
      rand = int(u*real(x_max - x_min + 1, real64), int32) + x_min

   end subroutine random_int32

end module integer_mapping_m
