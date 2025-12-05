module option_tests
    use testdrive, only: new_unittest, check, error_type
    use formerr_option
    use, intrinsic :: iso_fortran_env, only: real32, real64, real128, int8, int16, int32, int64
    implicit none
    private

    integer, parameter :: real8 = selected_real_kind(2, 2)
    integer, parameter :: real16 = selected_real_kind(4, 4)
    integer, parameter :: int128 = selected_int_kind(38)

    type :: custom_type
        integer :: id
        real :: value
    end type custom_type

    public :: test_some_integer, test_none_behavior, &
              test_unwrap_or_logic, test_unwrap_or_copy_semantics, &
              test_string_option, test_custom_type, &
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

    subroutine test_some_integer(error)
        type(error_type), allocatable, intent(out) :: error
        type(option) :: opt
        class(*), pointer :: p

        opt = some(42)

        call check(error, opt%is_some(), "some(42) should be is_some()")
        if (allocated(error)) return

        call check(error,.not. opt%is_none(), "some(42) should not be is_none()")
        if (allocated(error)) return

        p => opt%unwrap()
        call check(error, associated(p), "unwrap() on Some should return associated pointer")
        if (allocated(error)) return

        select type (p)
        type is (integer)
            call check(error, p == 42, "Unwrapped value should be 42")
        class default
            call check(error, .false., "Unwrapped value has wrong type (expected Integer)")
        end select
    end subroutine test_some_integer

    subroutine test_none_behavior(error)
        type(error_type), allocatable, intent(out) :: error
        type(option) :: opt
        class(*), pointer :: p

        opt = none()

        call check(error, opt%is_none(), "none() should be is_none()")
        if (allocated(error)) return

        call check(error,.not. opt%is_some(), "none() should not be is_some()")
        if (allocated(error)) return

    end subroutine test_none_behavior

    subroutine test_unwrap_or_logic(error)
        type(error_type), allocatable, intent(out) :: error
        type(option) :: opt
        class(*), allocatable :: res

        opt = some(100)
        res = opt%unwrap_or(999)

        select type (res)
        type is (integer)
            call check(error, res == 100, "unwrap_or on Some(100) should return 100")
        class default
            call check(error, .false., "unwrap_or returned wrong type for integer test")
        end select
        if (allocated(error)) return

        opt = none()
        res = opt%unwrap_or(999)

        select type (res)
        type is (integer)
            call check(error, res == 999, "unwrap_or on None should return default (999)")
        class default
            call check(error, .false., "unwrap_or returned wrong type for default test")
        end select
    end subroutine test_unwrap_or_logic

    subroutine test_unwrap_or_copy_semantics(error)
        type(error_type), allocatable, intent(out) :: error
        type(option) :: opt
        class(*), allocatable :: res

        opt = some(10)
        res = opt%unwrap_or(0)

        select type (res)
        type is (integer)
            ! Modify the result
            res = 50
        end select

        ! Verify original is untouched
        select type (orig => opt%unwrap())
        type is (integer)
            call check(error, orig == 10, "Original value was modified by unwrap_or result!")
        end select
    end subroutine test_unwrap_or_copy_semantics

    subroutine test_string_option(error)
        type(error_type), allocatable, intent(out) :: error
        type(option) :: opt
        class(*), pointer :: p

        opt = some("Fortran Option")
        p => opt%unwrap()

        select type (p)
        type is (character(*))
            call check(error, p == "Fortran Option", "String content mismatch")
        class default
            call check(error, .false., "Expected character type")
        end select
    end subroutine test_string_option

    subroutine test_custom_type(error)
        type(error_type), allocatable, intent(out) :: error
        type(option) :: opt
        class(*), pointer :: p
        type(custom_type) :: input_val

        input_val = custom_type(99, 1.23)
        opt = some(input_val)

        p => opt%unwrap()

        select type (p)
        type is (custom_type)
            call check(error, p%id == 99, "Custom type ID mismatch")
            call check(error, abs(p%value - 1.23) < 1e-5, "Custom type Value mismatch")
        class default
            call check(error, .false., "Failed to recover custom derived type")
        end select
    end subroutine test_custom_type

    ! --- Specialized Type Tests (Generated) ---

    subroutine test_types_int(error)
        type(error_type), allocatable, intent(out) :: error
        type(option) :: opt
        integer :: val, expected

        expected = 42

        ! Test Some
        opt = some_int(expected)
        call check(error, opt%is_some(), "some_int should be Some")
        if (allocated(error)) return

        val = opt%unwrap_int()

        call check(error, val == expected, "unwrap_int value mismatch")

        if (allocated(error)) return

        ! Test None behavior with specialized unwrap_or
        opt = none()
        val = opt%unwrap_or_int(expected)

        call check(error, val == expected, "unwrap_or_int default value mismatch")

    end subroutine test_types_int

    subroutine test_types_real(error)
        type(error_type), allocatable, intent(out) :: error
        type(option) :: opt
        real :: val, expected

        expected = 1.23

        ! Test Some
        opt = some_real(expected)
        call check(error, opt%is_some(), "some_real should be Some")
        if (allocated(error)) return

        val = opt%unwrap_real()

        call check(error, abs(val - expected) < 1e-5, "unwrap_real value mismatch")

        if (allocated(error)) return

        ! Test None behavior with specialized unwrap_or
        opt = none()
        val = opt%unwrap_or_real(expected)

        call check(error, abs(val - expected) < 1e-5, "unwrap_or_real default value mismatch")

    end subroutine test_types_real

    subroutine test_types_log(error)
        type(error_type), allocatable, intent(out) :: error
        type(option) :: opt
        logical :: val, expected

        expected = .true.

        ! Test Some
        opt = some_log(expected)
        call check(error, opt%is_some(), "some_log should be Some")
        if (allocated(error)) return

        val = opt%unwrap_log()

        call check(error, val .eqv. expected, "unwrap_log value mismatch")

        if (allocated(error)) return

        ! Test None behavior with specialized unwrap_or
        opt = none()
        val = opt%unwrap_or_log(expected)

        call check(error, val .eqv. expected, "unwrap_or_log default value mismatch")

    end subroutine test_types_log

    subroutine test_types_cpx(error)
        type(error_type), allocatable, intent(out) :: error
        type(option) :: opt
        complex :: val, expected

        expected = (1.0, 2.0)

        ! Test Some
        opt = some_cpx(expected)
        call check(error, opt%is_some(), "some_cpx should be Some")
        if (allocated(error)) return

        val = opt%unwrap_cpx()

        call check(error, abs(val - expected) < 1e-5, "unwrap_cpx value mismatch")

        if (allocated(error)) return

        ! Test None behavior with specialized unwrap_or
        opt = none()
        val = opt%unwrap_or_cpx(expected)

        call check(error, abs(val - expected) < 1e-5, "unwrap_or_cpx default value mismatch")

    end subroutine test_types_cpx

    subroutine test_types_r8(error)
        type(error_type), allocatable, intent(out) :: error
        type(option) :: opt
        real(8) :: val, expected

        expected = 1.23_8

        ! Test Some
        opt = some_r8(expected)
        call check(error, opt%is_some(), "some_r8 should be Some")
        if (allocated(error)) return

        val = opt%unwrap_r8()

        call check(error, abs(val - expected) < 1e-5, "unwrap_r8 value mismatch")

        if (allocated(error)) return

        ! Test None behavior with specialized unwrap_or
        opt = none()
        val = opt%unwrap_or_r8(expected)

        call check(error, abs(val - expected) < 1e-5, "unwrap_or_r8 default value mismatch")

    end subroutine test_types_r8

    subroutine test_types_r16(error)
        type(error_type), allocatable, intent(out) :: error
        type(option) :: opt
        real(16) :: val, expected

        expected = 1.23_16

        ! Test Some
        opt = some_r16(expected)
        call check(error, opt%is_some(), "some_r16 should be Some")
        if (allocated(error)) return

        val = opt%unwrap_r16()

        call check(error, abs(val - expected) < 1e-5, "unwrap_r16 value mismatch")

        if (allocated(error)) return

        ! Test None behavior with specialized unwrap_or
        opt = none()
        val = opt%unwrap_or_r16(expected)

        call check(error, abs(val - expected) < 1e-5, "unwrap_or_r16 default value mismatch")

    end subroutine test_types_r16

    subroutine test_types_r32(error)
        type(error_type), allocatable, intent(out) :: error
        type(option) :: opt
        real(real32) :: val, expected

        expected = 1.23_real32

        ! Test Some
        opt = some_r32(expected)
        call check(error, opt%is_some(), "some_r32 should be Some")
        if (allocated(error)) return

        val = opt%unwrap_r32()

        call check(error, abs(val - expected) < 1e-5, "unwrap_r32 value mismatch")

        if (allocated(error)) return

        ! Test None behavior with specialized unwrap_or
        opt = none()
        val = opt%unwrap_or_r32(expected)

        call check(error, abs(val - expected) < 1e-5, "unwrap_or_r32 default value mismatch")

    end subroutine test_types_r32

    subroutine test_types_r64(error)
        type(error_type), allocatable, intent(out) :: error
        type(option) :: opt
        real(real64) :: val, expected

        expected = 1.23_real64

        ! Test Some
        opt = some_r64(expected)
        call check(error, opt%is_some(), "some_r64 should be Some")
        if (allocated(error)) return

        val = opt%unwrap_r64()

        call check(error, abs(val - expected) < 1e-5, "unwrap_r64 value mismatch")

        if (allocated(error)) return

        ! Test None behavior with specialized unwrap_or
        opt = none()
        val = opt%unwrap_or_r64(expected)

        call check(error, abs(val - expected) < 1e-5, "unwrap_or_r64 default value mismatch")

    end subroutine test_types_r64

    subroutine test_types_r128(error)
        type(error_type), allocatable, intent(out) :: error
        type(option) :: opt
        real(real128) :: val, expected

        expected = 1.23_real128

        ! Test Some
        opt = some_r128(expected)
        call check(error, opt%is_some(), "some_r128 should be Some")
        if (allocated(error)) return

        val = opt%unwrap_r128()

        call check(error, abs(val - expected) < 1e-5, "unwrap_r128 value mismatch")

        if (allocated(error)) return

        ! Test None behavior with specialized unwrap_or
        opt = none()
        val = opt%unwrap_or_r128(expected)

        call check(error, abs(val - expected) < 1e-5, "unwrap_or_r128 default value mismatch")

    end subroutine test_types_r128

    subroutine test_types_i8(error)
        type(error_type), allocatable, intent(out) :: error
        type(option) :: opt
        integer(int8) :: val, expected

        expected = 42_int8

        ! Test Some
        opt = some_i8(expected)
        call check(error, opt%is_some(), "some_i8 should be Some")
        if (allocated(error)) return

        val = opt%unwrap_i8()

        call check(error, val == expected, "unwrap_i8 value mismatch")

        if (allocated(error)) return

        ! Test None behavior with specialized unwrap_or
        opt = none()
        val = opt%unwrap_or_i8(expected)

        call check(error, val == expected, "unwrap_or_i8 default value mismatch")

    end subroutine test_types_i8

    subroutine test_types_i16(error)
        type(error_type), allocatable, intent(out) :: error
        type(option) :: opt
        integer(int16) :: val, expected

        expected = 42_int16

        ! Test Some
        opt = some_i16(expected)
        call check(error, opt%is_some(), "some_i16 should be Some")
        if (allocated(error)) return

        val = opt%unwrap_i16()

        call check(error, val == expected, "unwrap_i16 value mismatch")

        if (allocated(error)) return

        ! Test None behavior with specialized unwrap_or
        opt = none()
        val = opt%unwrap_or_i16(expected)

        call check(error, val == expected, "unwrap_or_i16 default value mismatch")

    end subroutine test_types_i16

    subroutine test_types_i32(error)
        type(error_type), allocatable, intent(out) :: error
        type(option) :: opt
        integer(int32) :: val, expected

        expected = 42_int32

        ! Test Some
        opt = some_i32(expected)
        call check(error, opt%is_some(), "some_i32 should be Some")
        if (allocated(error)) return

        val = opt%unwrap_i32()

        call check(error, val == expected, "unwrap_i32 value mismatch")

        if (allocated(error)) return

        ! Test None behavior with specialized unwrap_or
        opt = none()
        val = opt%unwrap_or_i32(expected)

        call check(error, val == expected, "unwrap_or_i32 default value mismatch")

    end subroutine test_types_i32

    subroutine test_types_i64(error)
        type(error_type), allocatable, intent(out) :: error
        type(option) :: opt
        integer(int64) :: val, expected

        expected = 42_int64

        ! Test Some
        opt = some_i64(expected)
        call check(error, opt%is_some(), "some_i64 should be Some")
        if (allocated(error)) return

        val = opt%unwrap_i64()

        call check(error, val == expected, "unwrap_i64 value mismatch")

        if (allocated(error)) return

        ! Test None behavior with specialized unwrap_or
        opt = none()
        val = opt%unwrap_or_i64(expected)

        call check(error, val == expected, "unwrap_or_i64 default value mismatch")

    end subroutine test_types_i64

    subroutine test_types_i128(error)
        type(error_type), allocatable, intent(out) :: error
        type(option) :: opt
        integer(int128) :: val, expected

        expected = 42_int128

        ! Test Some
        opt = some_i128(expected)
        call check(error, opt%is_some(), "some_i128 should be Some")
        if (allocated(error)) return

        val = opt%unwrap_i128()

        call check(error, val == expected, "unwrap_i128 value mismatch")

        if (allocated(error)) return

        ! Test None behavior with specialized unwrap_or
        opt = none()
        val = opt%unwrap_or_i128(expected)

        call check(error, val == expected, "unwrap_or_i128 default value mismatch")

    end subroutine test_types_i128

    subroutine test_types_c8(error)
        type(error_type), allocatable, intent(out) :: error
        type(option) :: opt
        complex(real8) :: val, expected

        expected = (1.0_real8, 2.0_real8)

        ! Test Some
        opt = some_c8(expected)
        call check(error, opt%is_some(), "some_c8 should be Some")
        if (allocated(error)) return

        val = opt%unwrap_c8()

        call check(error, abs(val - expected) < 1e-5, "unwrap_c8 value mismatch")

        if (allocated(error)) return

        ! Test None behavior with specialized unwrap_or
        opt = none()
        val = opt%unwrap_or_c8(expected)

        call check(error, abs(val - expected) < 1e-5, "unwrap_or_c8 default value mismatch")

    end subroutine test_types_c8

    subroutine test_types_c16(error)
        type(error_type), allocatable, intent(out) :: error
        type(option) :: opt
        complex(real16) :: val, expected

        expected = (1.0_real16, 2.0_real16)

        ! Test Some
        opt = some_c16(expected)
        call check(error, opt%is_some(), "some_c16 should be Some")
        if (allocated(error)) return

        val = opt%unwrap_c16()

        call check(error, abs(val - expected) < 1e-5, "unwrap_c16 value mismatch")

        if (allocated(error)) return

        ! Test None behavior with specialized unwrap_or
        opt = none()
        val = opt%unwrap_or_c16(expected)

        call check(error, abs(val - expected) < 1e-5, "unwrap_or_c16 default value mismatch")

    end subroutine test_types_c16

    subroutine test_types_c32(error)
        type(error_type), allocatable, intent(out) :: error
        type(option) :: opt
        complex(real32) :: val, expected

        expected = (1.0_real32, 2.0_real32)

        ! Test Some
        opt = some_c32(expected)
        call check(error, opt%is_some(), "some_c32 should be Some")
        if (allocated(error)) return

        val = opt%unwrap_c32()

        call check(error, abs(val - expected) < 1e-5, "unwrap_c32 value mismatch")

        if (allocated(error)) return

        ! Test None behavior with specialized unwrap_or
        opt = none()
        val = opt%unwrap_or_c32(expected)

        call check(error, abs(val - expected) < 1e-5, "unwrap_or_c32 default value mismatch")

    end subroutine test_types_c32

    subroutine test_types_c64(error)
        type(error_type), allocatable, intent(out) :: error
        type(option) :: opt
        complex(real64) :: val, expected

        expected = (1.0_real64, 2.0_real64)

        ! Test Some
        opt = some_c64(expected)
        call check(error, opt%is_some(), "some_c64 should be Some")
        if (allocated(error)) return

        val = opt%unwrap_c64()

        call check(error, abs(val - expected) < 1e-5, "unwrap_c64 value mismatch")

        if (allocated(error)) return

        ! Test None behavior with specialized unwrap_or
        opt = none()
        val = opt%unwrap_or_c64(expected)

        call check(error, abs(val - expected) < 1e-5, "unwrap_or_c64 default value mismatch")

    end subroutine test_types_c64

    subroutine test_types_c128(error)
        type(error_type), allocatable, intent(out) :: error
        type(option) :: opt
        complex(real128) :: val, expected

        expected = (1.0_real128, 2.0_real128)

        ! Test Some
        opt = some_c128(expected)
        call check(error, opt%is_some(), "some_c128 should be Some")
        if (allocated(error)) return

        val = opt%unwrap_c128()

        call check(error, abs(val - expected) < 1e-5, "unwrap_c128 value mismatch")

        if (allocated(error)) return

        ! Test None behavior with specialized unwrap_or
        opt = none()
        val = opt%unwrap_or_c128(expected)

        call check(error, abs(val - expected) < 1e-5, "unwrap_or_c128 default value mismatch")

    end subroutine test_types_c128

end module option_tests

module option_suite
    use testdrive, only: new_unittest, unittest_type, error_type, check
    use option_tests
    implicit none
    private

    public :: collect_option_suite

contains

    subroutine collect_option_suite(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
                    new_unittest("some_integer", test_some_integer), &
                    new_unittest("none_behavior", test_none_behavior), &
                    new_unittest("unwrap_or_logic", test_unwrap_or_logic), &
                    new_unittest("unwrap_or_copy_semantics", test_unwrap_or_copy_semantics), &
                    new_unittest("string_option", test_string_option), &
                    new_unittest("custom_type", test_custom_type), &
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

    end subroutine collect_option_suite

end module option_suite
