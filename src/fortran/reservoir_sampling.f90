module reservoir_sampling_m
   use iso_fortran_env
   use xorshifts
   use integer_mapping_m
   implicit none
   private
   public ::  reservoir_sampling, &
              reservoir_sampling_algorithml

contains

   !> @brief Simple reservoir sampling using algorithm R.
   !!
   !! Return a sample of m numbers randomly selectly from [1, n], without replacement.
   !! Transcribed from https://gist.github.com/Pseudomanifold/1ed2b3b5b0a1389bdad97b2fdf5af47e
   subroutine reservoir_sampling(m, n, selected, seed)
      integer, intent(in ) :: m               !< Number of samples
      integer, intent(in ) :: n               !< Range of random numbers
      integer, intent(out) :: selected(:)     !< Sampled random numbers
      integer, intent(in ), optional :: seed(:)  !< RNG seed

      integer :: i, j, cnt
      real(kind=real64) :: rand_num

      if (present(seed)) then
         call random_seed(put=seed)
      end if

      cnt = 0
      do i = 1, N
         call random_number(rand_num)

         if (real(N - i + 1)*rand_num < m - cnt) then
            cnt = cnt + 1
            selected(cnt) = i
         end if

         if (cnt == m) return
      end do

   end subroutine reservoir_sampling


   !> @brief Wrapper around Fortran''s intrinsic PRNG, to avoid
   !! zero, such that it returns (0, 1)
   subroutine random_number_avoid_zero(r)
      real(real64), intent(out) :: r

      call random_number(r)
      do while (r < 1.e-10_real64)
          call random_number(r)
      end do

   end subroutine random_number_avoid_zero


   !>  Reservoir sampling using algorithm L
   !!
   !! Return a sample of m numbers randomly selectly from [1, n], without replacement.
   !! Based on the implementation shown on [wikipedia](https://en.wikipedia.org/wiki/Reservoir_sampling#Optimal:_Algorithm_L)
   subroutine reservoir_sampling_algorithml(m, n, seed, reservoir)
      integer, intent(in   ) :: m                !< Number of samples
      integer, intent(in   ) :: n                !< Range of random numbers
      integer, intent(inout) :: seed(:)  !< RNG seed, which propagates as the random number
      integer, intent(out  ) :: reservoir(:)     !< Sampled random numbers

      integer :: i, j
      real(real64) :: w, r

      call random_seed(put=seed)

      do i = 1, m
          reservoir(i) = i
      end do

      call random_number_avoid_zero(r)
      w = exp(log(r) / real(m, real64))

      call random_number_avoid_zero(r)
      i = m + floor(log(r) / log(1._real64 - w), int32) + 1

      do while (i <= n)
        j = random_integer_int32(1, m, seed(1))
        reservoir(j) = i
        call random_number_avoid_zero(r)
        w = w * exp(log(r) / real(m, real64))
        call random_number_avoid_zero(r)
        i = i + floor(log(r) / log(1._real64 - w), int32) + 1
      end do

   end subroutine reservoir_sampling_algorithml


    !> Get m, N, and optionally seed
    subroutine parse_cli_args(m, N, algo, seed)

        integer, intent(out) :: N, m
        integer, allocatable, intent(out) :: seed(:)

        integer :: nargs, seed_size, seed_value
        character(len=32) :: m_str, N_str, seed_str
        character(len=32), intent(out) :: algo

        nargs = command_argument_count()

        if (nargs >= 2) then
          call get_command_argument(1, m_str)
          call get_command_argument(2, N_str)
          call get_command_argument(3, algo)
          read(m_str, *) m
          read(N_str, *) N
        end if

        if (nargs == 4) then
             call get_command_argument(4, seed_str)
             call random_seed(size=seed_size)
             allocate(seed(seed_size))
             read(seed_str, *) seed_value
             seed = seed_value
        end if

    end subroutine parse_cli_args

end module reservoir_sampling_m
