module result_tests
    use testdrive, only: new_unittest, check, error_type
    use formerr_result
    implicit none
    private

    public :: test_ok_integer, test_err_string, &
              test_unwrap_logic, test_unwrap_err_logic, &
              test_unwrap_or_logic, test_nested_containers

contains

    subroutine test_ok_integer(error)
        type(error_type), allocatable, intent(out) :: error
        type(result_type) :: res
        class(*), pointer :: p

        res = ok(42)

        call check(error, res%is_ok(), "ok(42) should be is_ok()")
        if (allocated(error)) return

        call check(error,.not. res%is_err(), "ok(42) should not be is_err()")
        if (allocated(error)) return

        p => res%unwrap()
        call check(error, associated(p), "unwrap() on Ok should return associated pointer")
        if (allocated(error)) return

        select type (p)
        type is (integer)
            call check(error, p == 42, "Unwrapped value should be 42")
        class default
            call check(error, .false., "Unwrapped value has wrong type (expected Integer)")
        end select
    end subroutine test_ok_integer

    subroutine test_err_string(error)
        type(error_type), allocatable, intent(out) :: error
        type(result_type) :: res
        class(*), pointer :: p

        res = err("Something went wrong")

        call check(error, res%is_err(), "err(...) should be is_err()")
        if (allocated(error)) return

        call check(error,.not. res%is_ok(), "err(...) should not be is_ok()")
        if (allocated(error)) return

        p => res%unwrap_err()
        call check(error, associated(p), "unwrap_err() on Err should return associated pointer")
        if (allocated(error)) return

        select type (p)
        type is (character(*))
            call check(error, p == "Something went wrong", "Unwrapped error message mismatch")
        class default
            call check(error, .false., "Unwrapped error has wrong type (expected Character)")
        end select
    end subroutine test_err_string

    subroutine test_unwrap_logic(error)
        type(error_type), allocatable, intent(out) :: error
        type(result_type) :: res
        class(*), pointer :: p

        res = ok(3.14)
        p => res%unwrap()

        select type (p)
        type is (real)
            call check(error, abs(p - 3.14) < 1.0e-5, "unwrap() real value mismatch")
        class default
            call check(error, .false., "Expected real type from unwrap")
        end select
    end subroutine test_unwrap_logic

    subroutine test_unwrap_err_logic(error)
        type(error_type), allocatable, intent(out) :: error
        type(result_type) :: res
        class(*), pointer :: p

        res = err(999)
        p => res%unwrap_err()

        select type (p)
        type is (integer)
            call check(error, p == 999, "unwrap_err() integer value mismatch")
        class default
            call check(error, .false., "Expected integer type from unwrap_err")
        end select
    end subroutine test_unwrap_err_logic

    subroutine test_unwrap_or_logic(error)
        type(error_type), allocatable, intent(out) :: error
        type(result_type) :: res
        class(*), allocatable :: val

        res = ok(10)
        val = res%unwrap_or(20)

        select type (val)
        type is (integer)
            call check(error, val == 10, "unwrap_or on Ok(10) should return 10")
        class default
            call check(error, .false., "unwrap_or returned wrong type (Ok case)")
        end select
        if (allocated(error)) return

        res = err("Error")
        val = res%unwrap_or(20)

        select type (val)
        type is (integer)
            call check(error, val == 20, "unwrap_or on Err should return default (20)")
        class default
            call check(error, .false., "unwrap_or returned wrong type (Err case)")
        end select
    end subroutine test_unwrap_or_logic

    subroutine test_nested_containers(error)
        use formerr_option, only: some, option
        type(error_type), allocatable, intent(out) :: error
        type(result_type) :: res
        type(option) :: opt_in
        class(*), pointer :: outer_ptr, inner_ptr

        opt_in = some(100)
        res = ok(opt_in)

        outer_ptr => res%unwrap()

        select type (outer_ptr)
        type is (option)
            call check(error, outer_ptr%is_some(), "Inner option should be Some")

            inner_ptr => outer_ptr%unwrap()
            select type (inner_ptr)
            type is (integer)
                call check(error, inner_ptr == 100, "Nested value mismatch")
            class default
                call check(error, .false., "Failed to unwrap nested Integer")
            end select

        class default
            call check(error, .false., "Failed to unwrap nested Option")
        end select
    end subroutine test_nested_containers

end module result_tests

module result_suite
    use testdrive, only: new_unittest, unittest_type, error_type, check
    use result_tests
    implicit none
    private

    public :: collect_result_suite

contains

    subroutine collect_result_suite(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
                    new_unittest("ok_integer", test_ok_integer), &
                    new_unittest("err_string", test_err_string), &
                    new_unittest("unwrap_logic", test_unwrap_logic), &
                    new_unittest("unwrap_err_logic", test_unwrap_err_logic), &
                    new_unittest("unwrap_or_logic", test_unwrap_or_logic), &
                    new_unittest("nested_containers", test_nested_containers) &
                    ]

    end subroutine collect_result_suite

end module result_suite
