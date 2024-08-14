module xorshifts
  use iso_fortran_env
  implicit none
  !private

  !public :: xorshift_rnd, random_value

  interface xorshift_rnd
    module procedure :: xorshift_rnd_32, xorshift_rnd_64
  end interface

!  interface random_value
!    module procedure :: random_real64, random_int32
!  end interface

contains

  !> @brief Random integer generation in the range [0, 2^32 - 1], using xorshift.
  !!
  !! Algorithm "xor" from p. 4 of Marsaglia, "Xorshift RNGs"
  subroutine xorshift_rnd_32(x)
    integer(int32), intent(inout) :: x

    x = ieor(x, ishft(x,  13_int32))  ! x ^= x << 13
    x = ieor(x, ishft(x, -17_int32))  ! x ^= x >> 17
    x = ieor(x, ishft(x,   5_int32))  ! x ^= x << 5

    ! Ensure the highest bit is zero.
    ! This keeps the result in the range [0, 2^32 - 1] (positive)
    x = iand(x, Z'7FFFFFFF')
    
  end subroutine xorshift_rnd_32


  !> @brief Random 64-bit integer generation in the range [0, 2^64 - 1], using xorshift.
  subroutine xorshift_rnd_64(x)
    integer(int64), intent(inout) :: x

    x = ieor(x, ishft(x,  13_int64))  ! x ^= x << 13
    x = ieor(x, ishft(x, -17_int64))  ! x ^= x >> 17
    x = ieor(x, ishft(x,   5_int64))  ! x ^= x << 5

    ! Ensure the highest bit is zero.
    ! This keeps the result in the range [0, 2^64 - 1] (positive)
    x = iand(x, Z'7FFFFFFFFFFFFFFF')

  end subroutine xorshift_rnd_64


  !> @brief Generate a random real64 in the range \f$ [x_{min}, x_{max}] \f$.
  subroutine random_real64(x_min, x_max, seed, rand)
    real(real64),   intent(in)    :: x_min, x_max
    integer(int32), intent(inout) :: seed
    real(real64),   intent(out)   :: rand

    real(real64), parameter :: s_max = 4294967295_real64 !< 2^32 - 1
    real(real64) :: u

    call xorshift_rnd_32(seed)

    u = real(seed, real64) / s_max
    rand = u * (x_max - x_min) + x_min
    
  end subroutine random_real64


  !> @brief Generate a random int32 in the range \f$ (x_{min}, x_{max}] \f$.
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

    ! Transform range to (0, 1]
    u = 1._real64 - (real(seed, real64) / s_max)
    rand = int(u * real(x_max - x_min, real64), int32) + x_min

  end subroutine random_int32


end module xorshifts
