! Transcribed from https://gist.github.com/Pseudomanifold/1ed2b3b5b0a1389bdad97b2fdf5af47e
! Not Knuth's Algorithm S

module reservoir_sampling_m
   use iso_fortran_env
   implicit none

contains

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


    !> Get m, N, and optionally seed
    subroutine parse_cli_args(m, N, seed)

        integer, intent(out) :: N, m
        integer, allocatable, intent(out) :: seed(:)

        integer :: nargs, seed_size, seed_value
        character(len=32) :: m_str, N_str, seed_str

        nargs = command_argument_count()

        if (nargs >= 2) then
          call get_command_argument(1, m_str)
          call get_command_argument(2, N_str)
          read(m_str, *) m
          read(N_str, *) N
        end if

        if (nargs == 3) then
             call get_command_argument(3, seed_str)
             call random_seed(size=seed_size)
             allocate(seed(seed_size))
             read(seed_str, *) seed_value
             seed = seed_value
        end if

    end subroutine parse_cli_args

end module reservoir_sampling_m


! Example: ./cmake-build/simple_reservoir 100 1000 1234
program test_reservoir_sampling
   use iso_fortran_env
   use reservoir_sampling_m
   implicit none

   integer, allocatable :: random_numbers(:), seed(:)
   integer, parameter :: N_default = 1000, m_default = 100
   integer :: i, j, i_element, N, m, nargs
   real(real64) :: start, finish

   ! Set parameters
   nargs = command_argument_count()
   if (nargs == 0) then
       m = m_default
       N = N_default
   else
       call parse_cli_args(m, N, seed)
   end if

   allocate(random_numbers(m))
   if (allocated(seed)) then
        call reservoir_sampling(m, n, random_numbers, seed)
   else
        call cpu_time(start)
        call reservoir_sampling(m, n, random_numbers)
        call cpu_time(finish)
   end if

   open(unit=101, file='rs_timing.dat')
   write(101, *)  finish - start
   close(101)

   ! Write out selected random numbers
   do i = 1, m
       write(*, *) random_numbers(i)
   end do

   ! Check for duplicates - do externally
!   do i = 1, m
!      i_element = random_numbers(i)
!      do j = i + 1, m
!         if (i_element == random_numbers(j)) then
!            write (*, *) 'Elements', i, 'and', j, 'are the same:', i_element, random_numbers(j)
!         end if
!      end do
!   end do

end program test_reservoir_sampling
