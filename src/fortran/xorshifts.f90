module xorshifts
  use iso_fortran_env
  implicit none

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

end module xorshifts
