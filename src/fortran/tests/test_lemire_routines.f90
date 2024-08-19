program test_lemire_routines
    use iso_fortran_env
    use integer_mapping_m
    implicit none

    integer(int32)    :: n, m, seed, i, j
    character(len=32) :: algo

    if (command_argument_count() > 0) then
        call parse_cli_args(m, n, algo, seed)
    else
        m = 100
        n = 1000
        algo = 'simple'
        seed = 0
    end if

    if (seed == 0) then
        CALL SYSTEM_CLOCK(COUNT=seed)
    end if

    if (trim(algo) == 'simple') then

        write(*, *) 'Using the simple implementation'
        do i = 1, m
            j = bounded_rand_simple(n, seed)
            write(*, *) j
        end do

   else
        write(*, *) 'Using the efficient implementation'
        do i = 1, m
            j = bounded_rand(n, seed)
            write(*, *) j
        end do
   end if

contains

    !> Get m, N, and optionally seed
    subroutine parse_cli_args(m, N, algo, seed)

        integer, intent(out) :: N, m, seed
        character(len=32), intent(out) :: algo

        integer :: nargs
        character(len=32) :: m_str, N_str, seed_str

        nargs = command_argument_count()
        seed = 0

        if (nargs >= 2) then
          call get_command_argument(1, m_str)
          call get_command_argument(2, N_str)
          call get_command_argument(3, algo)
          read(m_str, *) m
          read(N_str, *) N
        end if

        if (nargs == 4) then
             call get_command_argument(4, seed_str)
             read(seed_str, *) seed
        end if

    end subroutine parse_cli_args


end program test_lemire_routines
