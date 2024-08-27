module reservoir_sampling_m
   use iso_fortran_env
   use xorshifts
   use integer_mapping_m
   implicit none
   private
   public ::  reservoir_sampling, &
              reservoir_sampling_algorithml, &
              reservoir_sampling_aexpj 
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


   !> Based on algorithm A-ExpJ, described in [Weighted random sampling with a reservoir](10.1016/j.ipl.2005.11.003)
   !! and the [wiki section](https://en.wikipedia.org/wiki/Reservoir_sampling#Algorithm_A-Res)
   subroutine reservoir_sampling_aexpj(weight, reservoir, seed_value)
      real(real64),   intent(in )           :: weight(:)    !< Weights for the population
      real(real64),   intent(out)           :: reservoir(:) !< m randomly-sampled values from the population
      integer(int32), intent(in ), optional :: seed_value   !< Initial seed value for PRNG

      integer(int32) :: m                  !< Sample (reservoir) size
      integer(int32) :: n                  !< Population size
      real(real64)   :: u_i, u_i2          !< Uniformly-distributed random numbers
      real(real64)   :: Xw                 !< Random variable
      real(real64), allocatable :: key(:)  !< Key for each reservoir value

      integer(int32)              :: min_key_index, i, seed_size
      integer(int32), allocatable :: seed(:)
      real(real64)                :: thres_w

      ! Random number range must be (0, 1), as log(0) would result in a key of -inf, and log(1) = 0
      ! Alternatively one could sample with intrinsic random_number and retry if it returns 0, however
      ! this will definitely bias the distribution.
      real(real64), parameter :: a = 1.e-10_real64
      real(real64), parameter :: b = 1._real64 - 1.e-10_real64

      m = size(reservoir)
      n = size(weight)
      ! assert(m >= n)
      ! assert(all(weight>1.))

      ! Need a random integer seed to initialise the custom RNG
      if (present(seed_value)) then
          allocate(seed(1), source=seed_value)
      else
          call random_seed(size=seed_size)
          allocate(seed(seed_size))
          call random_seed(get=seed)
      end if

      allocate(key(m))

      ! Initialise the reservoir with the first m items of the population
      ! In this case, V(i) is replaced with i
      min_key_index = 1
      do i = 1, m
         reservoir(i) = i
         u_i = random_number_real64(a, b, seed(1))
         key(i) = u_i ** (1._real64 / weight(i))
         if (key(i) < key(min_key_index)) min_key_index = i
      enddo

      u_i = random_number_real64(a, b, seed(1))
      Xw = log(u_i) / log(key(min_key_index))

      ! Perform swaps at jump-intervals, until the population is exhausted
      i = m + 1
      do while(i <= n)
         Xw = Xw - weight(i)
         if (Xw <= 0._real64) then
            reservoir(min_key_index) = i
            thres_w = key(min_key_index)**weight(i)
            ! U__{i2} \in (t_w, 1)
            u_i2 = random_number_real64(thres_w + a, b, seed(1))
            key(min_key_index) = u_i2 ** (1._real64 / weight(i))
            min_key_index = find_min_key_index(key)
            u_i = random_number_real64(a, b, seed(1))
            Xw = log(u_i) / log(key(min_key_index))
         endif
         i = i + 1
      enddo

   end subroutine reservoir_sampling_aexpj


   function find_min_key_index(key) result(key_index)
      real(real64),   intent(in) :: key(:)  !< Keys for each reservoir sample
      integer(int32)             :: key_index

      integer(int32)  :: i

      key_index = 1
      do i = 2, size(key)
         if (key(i) < key(key_index)) key_index = i
      enddo

   end function find_min_key_index


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
