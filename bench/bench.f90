program benchmark
    use, intrinsic :: iso_fortran_env, only: int64, real64
    use formerr_result, only: result_type, ok, err
    use formerr_option, only: option, some, none
    implicit none

    integer, parameter :: ITERATIONS = 10000000
    integer(int64) :: t_start, t_end, t_rate
    real(real64) :: t_native, t_formerr

    print *, "Running benchmarks with", ITERATIONS, "iterations..."
    call system_clock(count_rate=t_rate)

    ! --- SCENARIO 1: Success Path (Ok vs Integer Status) ---
    print *, "------------------------------------------------"
    print *, "SCENARIO: Simple Arithmetic Success"

    ! 1. Baseline: Native Integer Status
    call system_clock(t_start)
    call bench_native_success(ITERATIONS)
    call system_clock(t_end)
    t_native = real(t_end - t_start, real64)/real(t_rate, real64)
    print '(A, F10.9, A)', "Native Fortran (Status): ", t_native, " s"

    ! 2. Formerr: Result Type
    call system_clock(t_start)
    call bench_result_success(ITERATIONS)
    call system_clock(t_end)
    t_formerr = real(t_end - t_start, real64)/real(t_rate, real64)
    print '(A, F10.9, A)', "Formerr (Result Type):   ", t_formerr, " s"

    print '(A, F12.2, A)', "Overhead factor:         ", t_formerr/t_native, "x"

contains

    ! --- Native implementation ---
    subroutine bench_native_success(n)
        integer, intent(in) :: n
        integer :: i, stat, val
        do i = 1, n
            call native_add(i, 1, val, stat)
            if (stat /= 0) error stop "Fail"
        end do
    end subroutine

    subroutine native_add(a, b, res, stat)
        integer, intent(in) :: a, b
        integer, intent(out) :: res, stat
        res = a + b
        stat = 0 ! Success
    end subroutine

    ! --- Formerr implementation ---
    subroutine bench_result_success(n)
        integer, intent(in) :: n
        integer :: i
        type(result_type) :: res
        class(*), pointer :: val

        do i = 1, n
            res = formerr_add(i, 1)
            if (.not. res%is_ok()) error stop "Fail"

            ! Access value (simulation of unwrap cost)
            val => res%unwrap()
            select type (val)
            type is (integer)
                ! use val to prevent optimization removal
                if (val < 0) stop
            end select
        end do
    end subroutine

    function formerr_add(a, b) result(res)
        integer, intent(in) :: a, b
        type(result_type) :: res
        res = ok(a + b)
    end function

end program benchmark
