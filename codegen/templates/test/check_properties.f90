module property_tests
    use testdrive, only: new_unittest, check, error_type
    use formerr_option, only: option, some, none
    use formerr_result, only: result_type, ok, err
    use formerr_either, only: either, new_left, new_right
    use, intrinsic :: iso_fortran_env, only: {{ supported_types.iso_uses() }}
    implicit none
    private

    {% include "shared/selected_kinds.f90" %}

    public :: test_option_invariants, test_result_invariants, test_either_invariants

    integer, parameter :: NUM_ITERATIONS = 1000

contains

    !> Fuzz test for Option type invariants
    !> Properties checked:
    !> 1. is_some() is always true for some(x)
    !> 2. is_none() is always false for some(x)
    !> 3. unwrap(some(x)) == x
    !> 4. unwrap_or(some(x), y) == x
    !> 5. unwrap_or(none, y) == y
    subroutine test_option_invariants(error)
        type(error_type), allocatable, intent(out) :: error
        type(option) :: opt
        integer :: i, rand_int, default_int
        real :: rand_val
        class(*), pointer :: ptr
        class(*), allocatable :: res_alloc

        do i = 1, NUM_ITERATIONS
            call random_number(rand_val)
            rand_int = int(rand_val*10000)
            default_int = -1

            ! --- Test Some(x) properties ---
            opt = some(rand_int)

            call check(error, opt%is_some(), "Property failed: some(x) must be is_some")
            if (allocated(error)) return

            call check(error,.not. opt%is_none(), "Property failed: some(x) must not be is_none")
            if (allocated(error)) return

            ! Check identity: unwrap(some(x)) == x
            ptr => opt%unwrap()
            select type (ptr)
            type is (integer)
                if (ptr /= rand_int) then
                    call check(error, .false., "Property failed: unwrap(some(x)) != x")
                    return
                end if
            end select

            ! Check unwrap_or priority: unwrap_or(some(x), y) == x
            res_alloc = opt%unwrap_or(default_int)
            select type (res_alloc)
            type is (integer)
                if (res_alloc /= rand_int) then
                    call check(error, .false., "Property failed: unwrap_or(some(x), y) != x")
                    return
                end if
            end select

            ! --- Test None properties ---
            opt = none()

            call check(error, opt%is_none(), "Property failed: none() must be is_none")
            if (allocated(error)) return

            ! Check default fallback: unwrap_or(none(), y) == y
            res_alloc = opt%unwrap_or(default_int)
            select type (res_alloc)
            type is (integer)
                if (res_alloc /= default_int) then
                    call check(error, .false., "Property failed: unwrap_or(none, y) != y")
                    return
                end if
            end select
        end do

    end subroutine test_option_invariants

    !> Fuzz test for Result type invariants
    !> Properties checked:
    !> 1. is_ok() XOR is_err()
    !> 2. unwrap(ok(x)) == x
    !> 3. unwrap_err(err(x)) == x
    subroutine test_result_invariants(error)
        type(error_type), allocatable, intent(out) :: error
        type(result_type) :: res
        integer :: i, val
        real :: r
        class(*), pointer :: ptr

        do i = 1, NUM_ITERATIONS
            call random_number(r)
            val = int(r*10000)

            ! --- Test OK path ---
            res = ok(val)

            call check(error, res%is_ok() .and. .not. res%is_err(), &
                       "Property failed: ok(x) must be is_ok and not is_err")
            if (allocated(error)) return

            ptr => res%unwrap()
            select type (ptr)
            type is (integer)
                if (ptr /= val) then
                    call check(error, .false., "Property failed: unwrap(ok(x)) != x")
                    return
                end if
            end select

            ! --- Test ERR path ---
            res = err(val)

            call check(error, res%is_err() .and. .not. res%is_ok(), &
                       "Property failed: err(x) must be is_err and not is_ok")
            if (allocated(error)) return

            ptr => res%unwrap_err()
            select type (ptr)
            type is (integer)
                if (ptr /= val) then
                    call check(error, .false., "Property failed: unwrap_err(err(x)) != x")
                    return
                end if
            end select
        end do
    end subroutine

    !> Fuzz test for Either type invariants
    !> Properties checked:
    !> 1. Strict exclusivity: Left cannot be Right, Right cannot be Left
    !> 2. Data persistence: Data put in is data got out
    subroutine test_either_invariants(error)
        type(error_type), allocatable, intent(out) :: error
        type(either) :: e
        integer :: i
        real :: val
        class(*), pointer :: ptr

        do i = 1, NUM_ITERATIONS
            call random_number(val)

            ! --- Test Left ---
            e = new_left(val)

            call check(error, e%is_left(), "Property failed: left(x) is not is_left")
            if (allocated(error)) return

            call check(error,.not. e%is_right(), "Property failed: left(x) is is_right")
            if (allocated(error)) return

            ptr => e%get_left()
            select type (ptr)
            type is (real)
                if (abs(ptr - val) > 1e-6) then
                    call check(error, .false., "Property failed: get_left() value mismatch")
                    return
                end if
            end select

            ! --- Test Right ---
            e = new_right(val)

            call check(error, e%is_right(), "Property failed: right(x) is not is_right")
            if (allocated(error)) return

            call check(error,.not. e%is_left(), "Property failed: right(x) is is_left")
            if (allocated(error)) return

            ptr => e%get_right()
            select type (ptr)
            type is (real)
                if (abs(ptr - val) > 1e-6) then
                    call check(error, .false., "Property failed: get_right() value mismatch")
                    return
                end if
            end select
        end do
    end subroutine test_either_invariants

end module property_tests

module property_suite
    use testdrive, only: new_unittest, unittest_type, error_type, check
    use property_tests
    implicit none
    private

    public :: collect_property_suite

contains

    subroutine collect_property_suite(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
                    new_unittest("option_invariants", test_option_invariants), &
                    new_unittest("result_invariants", test_result_invariants), &
                    new_unittest("either_invariants", test_either_invariants) &
                    ]

    end subroutine collect_property_suite

end module property_suite
