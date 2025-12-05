module result_tests
    use testdrive, only: new_unittest, check, error_type
    use formerr_result
    use, intrinsic :: iso_fortran_env, only: real32, real64, real128, int8, int16, int32, int64
    implicit none
    private

    integer, parameter :: real8 = selected_real_kind(2, 2)
    integer, parameter :: real16 = selected_real_kind(4, 4)
    integer, parameter :: int128 = selected_int_kind(38)

    public :: test_ok_integer, test_err_string, &
              test_unwrap_logic, test_unwrap_err_logic, &
              test_unwrap_or_logic, test_nested_containers, &
              test_types_int, &
              test_types_real, &
              test_types_log, &
              test_types_cpx, &
              test_types_r8, &
              test_types_r16, &
              test_types_r32, &
              test_types_r64, &
              test_types_r128, &
              test_types_i8, &
              test_types_i16, &
              test_types_i32, &
              test_types_i64, &
              test_types_i128, &
              test_types_c8, &
              test_types_c16, &
              test_types_c32, &
              test_types_c64, &
              test_types_c128

contains

    ! --- Standard Logic Tests (Static) ---
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

    ! --- Specialized Type Tests (Generated) ---

    subroutine test_types_int(error)
        type(error_type), allocatable, intent(out) :: error
        type(result_type) :: res
        integer :: val, val_err, expected_ok, expected_err

        expected_ok = 42
        expected_err = -1

        ! Ok path
        res = ok_int(expected_ok)
        call check(error, res%is_ok(), "ok_int should be Ok")
        if (allocated(error)) return

        val = res%unwrap_int()

        call check(error, val == expected_ok, "unwrap_int value mismatch")

        if (allocated(error)) return

        ! Err path
        res = err_int(expected_err)
        call check(error, res%is_err(), "err_int should be Err")
        if (allocated(error)) return

        val_err = res%unwrap_err_int()

        call check(error, val_err == expected_err, "unwrap_err_int value mismatch")

    end subroutine test_types_int

    subroutine test_types_real(error)
        type(error_type), allocatable, intent(out) :: error
        type(result_type) :: res
        real :: val, val_err, expected_ok, expected_err

        expected_ok = 1.23
        expected_err = -9.99

        ! Ok path
        res = ok_real(expected_ok)
        call check(error, res%is_ok(), "ok_real should be Ok")
        if (allocated(error)) return

        val = res%unwrap_real()

        call check(error, abs(val - expected_ok) < 1e-5, "unwrap_real value mismatch")

        if (allocated(error)) return

        ! Err path
        res = err_real(expected_err)
        call check(error, res%is_err(), "err_real should be Err")
        if (allocated(error)) return

        val_err = res%unwrap_err_real()

        call check(error, abs(val_err - expected_err) < 1e-5, "unwrap_err_real value mismatch")

    end subroutine test_types_real

    subroutine test_types_log(error)
        type(error_type), allocatable, intent(out) :: error
        type(result_type) :: res
        logical :: val, val_err, expected_ok, expected_err

        expected_ok = .true.
        expected_err = .false.

        ! Ok path
        res = ok_log(expected_ok)
        call check(error, res%is_ok(), "ok_log should be Ok")
        if (allocated(error)) return

        val = res%unwrap_log()

        call check(error, val .eqv. expected_ok, "unwrap_log value mismatch")

        if (allocated(error)) return

        ! Err path
        res = err_log(expected_err)
        call check(error, res%is_err(), "err_log should be Err")
        if (allocated(error)) return

        val_err = res%unwrap_err_log()

        call check(error, val_err .eqv. expected_err, "unwrap_err_log value mismatch")

    end subroutine test_types_log

    subroutine test_types_cpx(error)
        type(error_type), allocatable, intent(out) :: error
        type(result_type) :: res
        complex :: val, val_err, expected_ok, expected_err

        expected_ok = (1.0, 2.0)
        expected_err = (-1.0, -2.0)

        ! Ok path
        res = ok_cpx(expected_ok)
        call check(error, res%is_ok(), "ok_cpx should be Ok")
        if (allocated(error)) return

        val = res%unwrap_cpx()

        call check(error, abs(val - expected_ok) < 1e-5, "unwrap_cpx value mismatch")

        if (allocated(error)) return

        ! Err path
        res = err_cpx(expected_err)
        call check(error, res%is_err(), "err_cpx should be Err")
        if (allocated(error)) return

        val_err = res%unwrap_err_cpx()

        call check(error, abs(val_err - expected_err) < 1e-5, "unwrap_err_cpx value mismatch")

    end subroutine test_types_cpx

    subroutine test_types_r8(error)
        type(error_type), allocatable, intent(out) :: error
        type(result_type) :: res
        real(8) :: val, val_err, expected_ok, expected_err

        expected_ok = 1.23_8
        expected_err = -9.99_8

        ! Ok path
        res = ok_r8(expected_ok)
        call check(error, res%is_ok(), "ok_r8 should be Ok")
        if (allocated(error)) return

        val = res%unwrap_r8()

        call check(error, abs(val - expected_ok) < 1e-5, "unwrap_r8 value mismatch")

        if (allocated(error)) return

        ! Err path
        res = err_r8(expected_err)
        call check(error, res%is_err(), "err_r8 should be Err")
        if (allocated(error)) return

        val_err = res%unwrap_err_r8()

        call check(error, abs(val_err - expected_err) < 1e-5, "unwrap_err_r8 value mismatch")

    end subroutine test_types_r8

    subroutine test_types_r16(error)
        type(error_type), allocatable, intent(out) :: error
        type(result_type) :: res
        real(16) :: val, val_err, expected_ok, expected_err

        expected_ok = 1.23_16
        expected_err = -9.99_16

        ! Ok path
        res = ok_r16(expected_ok)
        call check(error, res%is_ok(), "ok_r16 should be Ok")
        if (allocated(error)) return

        val = res%unwrap_r16()

        call check(error, abs(val - expected_ok) < 1e-5, "unwrap_r16 value mismatch")

        if (allocated(error)) return

        ! Err path
        res = err_r16(expected_err)
        call check(error, res%is_err(), "err_r16 should be Err")
        if (allocated(error)) return

        val_err = res%unwrap_err_r16()

        call check(error, abs(val_err - expected_err) < 1e-5, "unwrap_err_r16 value mismatch")

    end subroutine test_types_r16

    subroutine test_types_r32(error)
        type(error_type), allocatable, intent(out) :: error
        type(result_type) :: res
        real(real32) :: val, val_err, expected_ok, expected_err

        expected_ok = 1.23_real32
        expected_err = -9.99_real32

        ! Ok path
        res = ok_r32(expected_ok)
        call check(error, res%is_ok(), "ok_r32 should be Ok")
        if (allocated(error)) return

        val = res%unwrap_r32()

        call check(error, abs(val - expected_ok) < 1e-5, "unwrap_r32 value mismatch")

        if (allocated(error)) return

        ! Err path
        res = err_r32(expected_err)
        call check(error, res%is_err(), "err_r32 should be Err")
        if (allocated(error)) return

        val_err = res%unwrap_err_r32()

        call check(error, abs(val_err - expected_err) < 1e-5, "unwrap_err_r32 value mismatch")

    end subroutine test_types_r32

    subroutine test_types_r64(error)
        type(error_type), allocatable, intent(out) :: error
        type(result_type) :: res
        real(real64) :: val, val_err, expected_ok, expected_err

        expected_ok = 1.23_real64
        expected_err = -9.99_real64

        ! Ok path
        res = ok_r64(expected_ok)
        call check(error, res%is_ok(), "ok_r64 should be Ok")
        if (allocated(error)) return

        val = res%unwrap_r64()

        call check(error, abs(val - expected_ok) < 1e-5, "unwrap_r64 value mismatch")

        if (allocated(error)) return

        ! Err path
        res = err_r64(expected_err)
        call check(error, res%is_err(), "err_r64 should be Err")
        if (allocated(error)) return

        val_err = res%unwrap_err_r64()

        call check(error, abs(val_err - expected_err) < 1e-5, "unwrap_err_r64 value mismatch")

    end subroutine test_types_r64

    subroutine test_types_r128(error)
        type(error_type), allocatable, intent(out) :: error
        type(result_type) :: res
        real(real128) :: val, val_err, expected_ok, expected_err

        expected_ok = 1.23_real128
        expected_err = -9.99_real128

        ! Ok path
        res = ok_r128(expected_ok)
        call check(error, res%is_ok(), "ok_r128 should be Ok")
        if (allocated(error)) return

        val = res%unwrap_r128()

        call check(error, abs(val - expected_ok) < 1e-5, "unwrap_r128 value mismatch")

        if (allocated(error)) return

        ! Err path
        res = err_r128(expected_err)
        call check(error, res%is_err(), "err_r128 should be Err")
        if (allocated(error)) return

        val_err = res%unwrap_err_r128()

        call check(error, abs(val_err - expected_err) < 1e-5, "unwrap_err_r128 value mismatch")

    end subroutine test_types_r128

    subroutine test_types_i8(error)
        type(error_type), allocatable, intent(out) :: error
        type(result_type) :: res
        integer(int8) :: val, val_err, expected_ok, expected_err

        expected_ok = 42_int8
        expected_err = -1_int8

        ! Ok path
        res = ok_i8(expected_ok)
        call check(error, res%is_ok(), "ok_i8 should be Ok")
        if (allocated(error)) return

        val = res%unwrap_i8()

        call check(error, val == expected_ok, "unwrap_i8 value mismatch")

        if (allocated(error)) return

        ! Err path
        res = err_i8(expected_err)
        call check(error, res%is_err(), "err_i8 should be Err")
        if (allocated(error)) return

        val_err = res%unwrap_err_i8()

        call check(error, val_err == expected_err, "unwrap_err_i8 value mismatch")

    end subroutine test_types_i8

    subroutine test_types_i16(error)
        type(error_type), allocatable, intent(out) :: error
        type(result_type) :: res
        integer(int16) :: val, val_err, expected_ok, expected_err

        expected_ok = 42_int16
        expected_err = -1_int16

        ! Ok path
        res = ok_i16(expected_ok)
        call check(error, res%is_ok(), "ok_i16 should be Ok")
        if (allocated(error)) return

        val = res%unwrap_i16()

        call check(error, val == expected_ok, "unwrap_i16 value mismatch")

        if (allocated(error)) return

        ! Err path
        res = err_i16(expected_err)
        call check(error, res%is_err(), "err_i16 should be Err")
        if (allocated(error)) return

        val_err = res%unwrap_err_i16()

        call check(error, val_err == expected_err, "unwrap_err_i16 value mismatch")

    end subroutine test_types_i16

    subroutine test_types_i32(error)
        type(error_type), allocatable, intent(out) :: error
        type(result_type) :: res
        integer(int32) :: val, val_err, expected_ok, expected_err

        expected_ok = 42_int32
        expected_err = -1_int32

        ! Ok path
        res = ok_i32(expected_ok)
        call check(error, res%is_ok(), "ok_i32 should be Ok")
        if (allocated(error)) return

        val = res%unwrap_i32()

        call check(error, val == expected_ok, "unwrap_i32 value mismatch")

        if (allocated(error)) return

        ! Err path
        res = err_i32(expected_err)
        call check(error, res%is_err(), "err_i32 should be Err")
        if (allocated(error)) return

        val_err = res%unwrap_err_i32()

        call check(error, val_err == expected_err, "unwrap_err_i32 value mismatch")

    end subroutine test_types_i32

    subroutine test_types_i64(error)
        type(error_type), allocatable, intent(out) :: error
        type(result_type) :: res
        integer(int64) :: val, val_err, expected_ok, expected_err

        expected_ok = 42_int64
        expected_err = -1_int64

        ! Ok path
        res = ok_i64(expected_ok)
        call check(error, res%is_ok(), "ok_i64 should be Ok")
        if (allocated(error)) return

        val = res%unwrap_i64()

        call check(error, val == expected_ok, "unwrap_i64 value mismatch")

        if (allocated(error)) return

        ! Err path
        res = err_i64(expected_err)
        call check(error, res%is_err(), "err_i64 should be Err")
        if (allocated(error)) return

        val_err = res%unwrap_err_i64()

        call check(error, val_err == expected_err, "unwrap_err_i64 value mismatch")

    end subroutine test_types_i64

    subroutine test_types_i128(error)
        type(error_type), allocatable, intent(out) :: error
        type(result_type) :: res
        integer(int128) :: val, val_err, expected_ok, expected_err

        expected_ok = 42_int128
        expected_err = -1_int128

        ! Ok path
        res = ok_i128(expected_ok)
        call check(error, res%is_ok(), "ok_i128 should be Ok")
        if (allocated(error)) return

        val = res%unwrap_i128()

        call check(error, val == expected_ok, "unwrap_i128 value mismatch")

        if (allocated(error)) return

        ! Err path
        res = err_i128(expected_err)
        call check(error, res%is_err(), "err_i128 should be Err")
        if (allocated(error)) return

        val_err = res%unwrap_err_i128()

        call check(error, val_err == expected_err, "unwrap_err_i128 value mismatch")

    end subroutine test_types_i128

    subroutine test_types_c8(error)
        type(error_type), allocatable, intent(out) :: error
        type(result_type) :: res
        complex(real8) :: val, val_err, expected_ok, expected_err

        expected_ok = (1.0_real8, 2.0_real8)
        expected_err = (-1.0_real8, -2.0_real8)

        ! Ok path
        res = ok_c8(expected_ok)
        call check(error, res%is_ok(), "ok_c8 should be Ok")
        if (allocated(error)) return

        val = res%unwrap_c8()

        call check(error, abs(val - expected_ok) < 1e-5, "unwrap_c8 value mismatch")

        if (allocated(error)) return

        ! Err path
        res = err_c8(expected_err)
        call check(error, res%is_err(), "err_c8 should be Err")
        if (allocated(error)) return

        val_err = res%unwrap_err_c8()

        call check(error, abs(val_err - expected_err) < 1e-5, "unwrap_err_c8 value mismatch")

    end subroutine test_types_c8

    subroutine test_types_c16(error)
        type(error_type), allocatable, intent(out) :: error
        type(result_type) :: res
        complex(real16) :: val, val_err, expected_ok, expected_err

        expected_ok = (1.0_real16, 2.0_real16)
        expected_err = (-1.0_real16, -2.0_real16)

        ! Ok path
        res = ok_c16(expected_ok)
        call check(error, res%is_ok(), "ok_c16 should be Ok")
        if (allocated(error)) return

        val = res%unwrap_c16()

        call check(error, abs(val - expected_ok) < 1e-5, "unwrap_c16 value mismatch")

        if (allocated(error)) return

        ! Err path
        res = err_c16(expected_err)
        call check(error, res%is_err(), "err_c16 should be Err")
        if (allocated(error)) return

        val_err = res%unwrap_err_c16()

        call check(error, abs(val_err - expected_err) < 1e-5, "unwrap_err_c16 value mismatch")

    end subroutine test_types_c16

    subroutine test_types_c32(error)
        type(error_type), allocatable, intent(out) :: error
        type(result_type) :: res
        complex(real32) :: val, val_err, expected_ok, expected_err

        expected_ok = (1.0_real32, 2.0_real32)
        expected_err = (-1.0_real32, -2.0_real32)

        ! Ok path
        res = ok_c32(expected_ok)
        call check(error, res%is_ok(), "ok_c32 should be Ok")
        if (allocated(error)) return

        val = res%unwrap_c32()

        call check(error, abs(val - expected_ok) < 1e-5, "unwrap_c32 value mismatch")

        if (allocated(error)) return

        ! Err path
        res = err_c32(expected_err)
        call check(error, res%is_err(), "err_c32 should be Err")
        if (allocated(error)) return

        val_err = res%unwrap_err_c32()

        call check(error, abs(val_err - expected_err) < 1e-5, "unwrap_err_c32 value mismatch")

    end subroutine test_types_c32

    subroutine test_types_c64(error)
        type(error_type), allocatable, intent(out) :: error
        type(result_type) :: res
        complex(real64) :: val, val_err, expected_ok, expected_err

        expected_ok = (1.0_real64, 2.0_real64)
        expected_err = (-1.0_real64, -2.0_real64)

        ! Ok path
        res = ok_c64(expected_ok)
        call check(error, res%is_ok(), "ok_c64 should be Ok")
        if (allocated(error)) return

        val = res%unwrap_c64()

        call check(error, abs(val - expected_ok) < 1e-5, "unwrap_c64 value mismatch")

        if (allocated(error)) return

        ! Err path
        res = err_c64(expected_err)
        call check(error, res%is_err(), "err_c64 should be Err")
        if (allocated(error)) return

        val_err = res%unwrap_err_c64()

        call check(error, abs(val_err - expected_err) < 1e-5, "unwrap_err_c64 value mismatch")

    end subroutine test_types_c64

    subroutine test_types_c128(error)
        type(error_type), allocatable, intent(out) :: error
        type(result_type) :: res
        complex(real128) :: val, val_err, expected_ok, expected_err

        expected_ok = (1.0_real128, 2.0_real128)
        expected_err = (-1.0_real128, -2.0_real128)

        ! Ok path
        res = ok_c128(expected_ok)
        call check(error, res%is_ok(), "ok_c128 should be Ok")
        if (allocated(error)) return

        val = res%unwrap_c128()

        call check(error, abs(val - expected_ok) < 1e-5, "unwrap_c128 value mismatch")

        if (allocated(error)) return

        ! Err path
        res = err_c128(expected_err)
        call check(error, res%is_err(), "err_c128 should be Err")
        if (allocated(error)) return

        val_err = res%unwrap_err_c128()

        call check(error, abs(val_err - expected_err) < 1e-5, "unwrap_err_c128 value mismatch")

    end subroutine test_types_c128

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
                    new_unittest("nested_containers", test_nested_containers), &
                    new_unittest("types_int", test_types_int), &
                    new_unittest("types_real", test_types_real), &
                    new_unittest("types_log", test_types_log), &
                    new_unittest("types_cpx", test_types_cpx), &
                    new_unittest("types_r8", test_types_r8), &
                    new_unittest("types_r16", test_types_r16), &
                    new_unittest("types_r32", test_types_r32), &
                    new_unittest("types_r64", test_types_r64), &
                    new_unittest("types_r128", test_types_r128), &
                    new_unittest("types_i8", test_types_i8), &
                    new_unittest("types_i16", test_types_i16), &
                    new_unittest("types_i32", test_types_i32), &
                    new_unittest("types_i64", test_types_i64), &
                    new_unittest("types_i128", test_types_i128), &
                    new_unittest("types_c8", test_types_c8), &
                    new_unittest("types_c16", test_types_c16), &
                    new_unittest("types_c32", test_types_c32), &
                    new_unittest("types_c64", test_types_c64), &
                    new_unittest("types_c128", test_types_c128) &
                    ]

    end subroutine collect_result_suite

end module result_suite
