!> Based on the paper
!! https://wrap.warwick.ac.uk/id/eprint/150064/1/WRAP-sequential-random-sampling-revisited-hidden-shuffle-method-2021.pdf
!! TODO:
!! * Make some notes on the theory, such that the basic premise is understood
!! * Plot the number distribution and confirm it is normal

module hidden_shuffle_m
    use iso_fortran_env
    implicit none
    private
    public :: hidden_shuffle

contains

    subroutine hidden_shuffle(m_in, N, result, seed)
        integer, intent(in) :: m_in !< Number of samples to take
        integer, intent(in) :: N !< Range to sample from [0,N)
        integer, intent(out) :: result(:)
        integer, intent(in), optional :: seed(:)

        integer :: H, L, i, s_old, s, cnt, m
        real(real64) :: N_minus_m, q, p_i, random_num, a, F

        if (present(seed)) then
            call random_seed(put=seed)
        else
            call random_seed()
        endif

        m = m_in
        cnt = 0
        H = 0
        i = 0
        N_minus_m = real(N - m, real64)

        ! Step 1 (note what this does)
        if (N > m) then
            H = m
            do while (i < m)
                ! Generate random number between 0 and 1
                call random_number(random_num)

                q = 1._real64 - N_minus_m / real(N - i, real64)
                i = i + int(log_b(random_num, real(1 - q, real64)))
                p_i = 1._real64 - N_minus_m / real(N - i, real64)

                call random_number(random_num)
                if ((i < m) .and. (random_num < p_i / q)) then
                    H = H - 1
                end if
                i = i + 1
            end do
        end if

        ! Step 2. Draw high items. Basically looks like reservoir sampling
        L = m - H
        a = 1._real64
        do while(H > 0)
            s_old = m + int(a * N_minus_m)
            call random_number(random_num)
            a = a * random_num**(1._real64 / real(H, real64))
            s = m + int(a * N_minus_m)
            if (s < s_old) then
                cnt = cnt + 1
                result(cnt) = N - 1 - S
                if (cnt == m) return
            else
                L = L + 1
            end if
            H = H - 1
        end do

        ! Step 3. Draw low items
        do while(L > 0)
            call random_number(random_num)
            s = 0
            f = real(L, real64) / real(m, real64)
            do while(f < random_num .and. s < (m - L - 1))
                f = 1._real64 - (1._real64 - real(L, real64) / real(m - s, real64)) * (1._real64 - f)
                s = s + 1
            end do
            L = L - 1
            m = m - s -1
            cnt = cnt + 1
            result(cnt) = N - 1 - m
            if (cnt == m) return
        end do

    end subroutine hidden_shuffle

    !> Log of a specified base
    function log_b(x, b) result(l)
        real(real64) :: x, b
        real(real64) :: l

        l = log(x) / log(b)

    end function log_b

end module hidden_shuffle_m

program test
    use hidden_shuffle_m
    integer :: i, m
    integer, allocatable :: result(:)
    ! To fix the seed
    !integer :: min_seed_size = 10
    !call random_seed(put=min_seed_size)
    !allocate(seed(min_seed_size), source=[(i, i=1, min_seed_size)])

    m = 100
    allocate(result(m))
    call hidden_shuffle(m, 10000, result)

    ! Transform to limit [i, j) by setting N = j - i
    ! For [1, np], this means passing N = np
    ! hen adding i = 1 to the return values
    do i = 1, m
        write(*, *) result(i) + 1
    end do

end program test
