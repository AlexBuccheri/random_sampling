! Example: ./cmake-build/simple_reservoir 100 1000 1234
program test_reservoir_sampling
   use iso_fortran_env
   use reservoir_sampling_m
   implicit none

   integer, allocatable :: random_numbers(:), seed(:)
   integer, parameter :: N_default = 1000, m_default = 100
   integer :: i, j, i_element, N, m, nargs
   real(real64) :: start, finish
   character(len=32) :: algo

   ! Set parameters
   nargs = command_argument_count()
   if (nargs == 0) then
       m = m_default
       N = N_default
       algo = 'simple'
   else
       call parse_cli_args(m, N, algo, seed)
   end if

   allocate(random_numbers(m))

   if (trim(algo) == 'simple') then
       ! This implementation uses the fortran instrinic
       if (allocated(seed)) then
            call cpu_time(start)
            call reservoir_sampling(m, n, random_numbers, seed)
            call cpu_time(finish)
       else
            call cpu_time(start)
            call reservoir_sampling(m, n, random_numbers)
            call cpu_time(finish)
       end if

   else
       if (allocated(seed)) then
           call cpu_time(start)
           call reservoir_sampling_algorithml(m, n, seed(1), random_numbers)
           call cpu_time(finish)
       else
           call SYSTEM_CLOCK(COUNT=seed(1))
           call cpu_time(start)
           call reservoir_sampling_algorithml(m, n, seed(1), random_numbers)
           call cpu_time(finish)
       end if

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