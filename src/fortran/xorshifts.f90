module xorshifts
  use iso_fortran_env, only: int32, int64, real64
  implicit none

  interface xorshift_rnd
    module procedure :: xorshift_rnd_32, xorshift_rnd_64
  end interface

  !> 2^31 - 1 specific to xorshift_rnd_32 function definition
  integer, parameter :: max_value_xorshift32_int = 2147483647
  !> 2^31 - 1 specific to xorshift_rnd_32 function definition
  real(real64), parameter :: max_value_xorshift32_real = 2147483647_real64

contains

  !> @brief Random integer generation in the range [0, 2^31 - 1], using xorshift.
  !!
  !! Algorithm "xor" from p. 4 of Marsaglia, "Xorshift RNGs"
  !! Note that an unsigned int in C can hold numbers [0, 2^32 - 1], however fortran
  !! does not support this data type. Instead, signed int32 has the range
  !! [-2^31, 2^31 - 1]. Negative values are mapped onto their unsigned equivalents,
  !! reducing the period of the algorithm, but simplifying the implementation.
  subroutine xorshift_rnd_32(x)
    integer(int32), intent(inout) :: x

    x = ieor(x, ishft(x,  13_int32))  ! x ^= x << 13
    x = ieor(x, ishft(x, -17_int32))  ! x ^= x >> 17
    x = ieor(x, ishft(x,   5_int32))  ! x ^= x << 5

    ! Ensure the highest bit is zero.
    ! This keeps the result in the range [0, 2^31 - 1] (positive)
    x = iand(x, Z'7FFFFFFF')

  end subroutine xorshift_rnd_32

  !> @brief Random 64-bit integer generation in the range [0, 2^63 - 1], using xorshift.
  subroutine xorshift_rnd_64(x)
    integer(int64), intent(inout) :: x

    x = ieor(x, ishft(x,  13_int64))  ! x ^= x << 13
    x = ieor(x, ishft(x, -17_int64))  ! x ^= x >> 17
    x = ieor(x, ishft(x,   5_int64))  ! x ^= x << 5

    ! Ensure the highest bit is zero.
    ! This keeps the result in the range [0, 2^63 - 1] (positive)
    x = iand(x, Z'7FFFFFFFFFFFFFFF')

  end subroutine xorshift_rnd_64

end module xorshifts
