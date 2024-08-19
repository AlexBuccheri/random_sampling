module fisher_yates_m
    use iso_fortran_env
    use integer_mapping_m
    implicit none
    private
    public :: fisher_yates_shuffle

contains

    ! https://en.wikipedia.org/wiki/Fisher–Yates_shuffle
    ! Note, this is O(n) in memory, although a period of 10^6 integers
    ! will only take 4 MB.
    subroutine fisher_yates_shuffle(m, n, seed, values)
        integer, intent(in)  :: m   !< Subset of random numbers to sample
        integer, intent(in)  :: n   !< Period of the random numbers
        integer, intent(inout) :: seed !< Initial seed
        integer, intent(out) :: values(:)

        integer, allocatable :: indices(:)
        integer :: i, j, temp

        allocate(indices(n))
        do i = 1, n
            indices(i) = i
        end do

        do i = 1, m
            j = random_integer_int32(i, n, seed)
            ! Perform the swap
            temp = indices(i)
            indices(i) = indices(j)
            indices(j) = temp
            values(i) = indices(i)
        end do

        values(m) = indices(m)
        deallocate(indices)

    end subroutine fisher_yates_shuffle

end module fisher_yates_m
