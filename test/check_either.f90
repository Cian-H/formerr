module either_tests
    use testdrive, only: new_unittest, check, error_type
    use formerr_either
    use, intrinsic :: iso_fortran_env, only: real32, real64, real128, int8, int16, int32, int64
    implicit none

    integer, parameter :: real8 = selected_real_kind(2, 2)
    integer, parameter :: real16 = selected_real_kind(4, 4)
    integer, parameter :: int128 = selected_int_kind(38)

    public :: test_integer_left, test_real_right, test_string_logic, &
              test_generic_constructor, test_specialized_either, &
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

    subroutine test_integer_left(error)
        type(error_type), allocatable, intent(out) :: error
        type(either) :: e
        class(*), pointer :: p

        e = new_left(42)

        call check(error, e%is_left(), "is_left() should be true")
        if (allocated(error)) return

        call check(error,.not. e%is_right(), "is_right() should be false")
        if (allocated(error)) return

        p => e%get_left()
        call check(error, associated(p), "get_left() pointer should be associated")
        if (allocated(error)) return

        select type (p)
        type is (integer)
            call check(error, p == 42, "Value inside left should be 42")
        class default
            call check(error, .false., "Wrong type stored in Left (expected Integer)")
        end select
    end subroutine test_integer_left

    subroutine test_real_right(error)
        type(error_type), allocatable, intent(out) :: error
        type(either) :: e
        class(*), pointer :: p

        e = new_right(3.14)

        call check(error, e%is_right(), "is_right() should be true")
        if (allocated(error)) return

        p => e%get_left()
        call check(error,.not. associated(p), "get_left() on a Right value should return null")
        if (allocated(error)) return

        p => e%get_right()
        select type (p)
        type is (real)
            ! Using a small epsilon for float comparison
            call check(error, abs(p - 3.14) < 0.0001, "Value inside right should be approx 3.14")
        class default
            call check(error, .false., "Wrong type stored in Right (expected Real)")
        end select
    end subroutine test_real_right

    subroutine test_string_logic(error)
        type(error_type), allocatable, intent(out) :: error
        type(either) :: e
        class(*), pointer :: p

        e = new_left("Fortran")
        p => e%get_left()

        select type (p)
        type is (character(*))
            call check(error, p == "Fortran", "String content should match 'Fortran'")
        class default
            call check(error, .false., "Wrong type stored (expected Character)")
        end select
    end subroutine test_string_logic

    subroutine test_generic_constructor(error)
        type(error_type), allocatable, intent(out) :: error
        type(either) :: e

        e = either(left_val=100)
        call check(error, e%is_left(), "either(left_val=...) creates Left")
        if (allocated(error)) return

        e = either(right_val=200)
        call check(error, e%is_right(), "either(right_val=...) creates Right")
    end subroutine test_generic_constructor

    subroutine test_specialized_either(error)
        type(error_type), allocatable, intent(out) :: error
        type(either) :: e
        integer :: i_val
        real :: r_val

        ! Test specialized setters/getters directly on Either

        ! Integer Left
        call e%set_left_int(999)
        call check(error, e%is_left(), "set_left_int should make is_left true")
        i_val = e%get_left_int()
        call check(error, i_val == 999, "get_left_int value mismatch")

        ! Real Right
        call e%set_right_real(5.55)
        call check(error, e%is_right(), "set_right_real should make is_right true")
        call check(error,.not. e%is_left(), "set_right_real should clear left")
        r_val = e%get_right_real()
        call check(error, abs(r_val - 5.55) < 1e-5, "get_right_real value mismatch")

    end subroutine test_specialized_either

    ! --- Specialized Type Tests (Generated) ---

    subroutine test_types_int(error)
        type(error_type), allocatable, intent(out) :: error
        type(either) :: e
        integer :: val, expected

        expected = 42

        ! Test Left
        call e%set_left_int(expected)
        call check(error, e%is_left(), "set_left_int should make is_left true")
        if (allocated(error)) return

        val = e%get_left_int()

        call check(error, val == expected, "get_left_int value mismatch")

        if (allocated(error)) return

        ! Test Right
        call e%set_right_int(expected)
        call check(error, e%is_right(), "set_right_int should make is_right true")
        if (allocated(error)) return

        val = e%get_right_int()

        call check(error, val == expected, "get_right_int value mismatch")

    end subroutine test_types_int

    subroutine test_types_real(error)
        type(error_type), allocatable, intent(out) :: error
        type(either) :: e
        real :: val, expected

        expected = 1.23

        ! Test Left
        call e%set_left_real(expected)
        call check(error, e%is_left(), "set_left_real should make is_left true")
        if (allocated(error)) return

        val = e%get_left_real()

        call check(error, abs(val - expected) < 1e-5, "get_left_real value mismatch")

        if (allocated(error)) return

        ! Test Right
        call e%set_right_real(expected)
        call check(error, e%is_right(), "set_right_real should make is_right true")
        if (allocated(error)) return

        val = e%get_right_real()

        call check(error, abs(val - expected) < 1e-5, "get_right_real value mismatch")

    end subroutine test_types_real

    subroutine test_types_log(error)
        type(error_type), allocatable, intent(out) :: error
        type(either) :: e
        logical :: val, expected

        expected = .true.

        ! Test Left
        call e%set_left_log(expected)
        call check(error, e%is_left(), "set_left_log should make is_left true")
        if (allocated(error)) return

        val = e%get_left_log()

        call check(error, val .eqv. expected, "get_left_log value mismatch")

        if (allocated(error)) return

        ! Test Right
        call e%set_right_log(expected)
        call check(error, e%is_right(), "set_right_log should make is_right true")
        if (allocated(error)) return

        val = e%get_right_log()

        call check(error, val .eqv. expected, "get_right_log value mismatch")

    end subroutine test_types_log

    subroutine test_types_cpx(error)
        type(error_type), allocatable, intent(out) :: error
        type(either) :: e
        complex :: val, expected

        expected = (1.0, 2.0)

        ! Test Left
        call e%set_left_cpx(expected)
        call check(error, e%is_left(), "set_left_cpx should make is_left true")
        if (allocated(error)) return

        val = e%get_left_cpx()

        call check(error, abs(val - expected) < 1e-5, "get_left_cpx value mismatch")

        if (allocated(error)) return

        ! Test Right
        call e%set_right_cpx(expected)
        call check(error, e%is_right(), "set_right_cpx should make is_right true")
        if (allocated(error)) return

        val = e%get_right_cpx()

        call check(error, abs(val - expected) < 1e-5, "get_right_cpx value mismatch")

    end subroutine test_types_cpx

    subroutine test_types_r8(error)
        type(error_type), allocatable, intent(out) :: error
        type(either) :: e
        real(8) :: val, expected

        expected = 1.23_8

        ! Test Left
        call e%set_left_r8(expected)
        call check(error, e%is_left(), "set_left_r8 should make is_left true")
        if (allocated(error)) return

        val = e%get_left_r8()

        call check(error, abs(val - expected) < 1e-5, "get_left_r8 value mismatch")

        if (allocated(error)) return

        ! Test Right
        call e%set_right_r8(expected)
        call check(error, e%is_right(), "set_right_r8 should make is_right true")
        if (allocated(error)) return

        val = e%get_right_r8()

        call check(error, abs(val - expected) < 1e-5, "get_right_r8 value mismatch")

    end subroutine test_types_r8

    subroutine test_types_r16(error)
        type(error_type), allocatable, intent(out) :: error
        type(either) :: e
        real(16) :: val, expected

        expected = 1.23_16

        ! Test Left
        call e%set_left_r16(expected)
        call check(error, e%is_left(), "set_left_r16 should make is_left true")
        if (allocated(error)) return

        val = e%get_left_r16()

        call check(error, abs(val - expected) < 1e-5, "get_left_r16 value mismatch")

        if (allocated(error)) return

        ! Test Right
        call e%set_right_r16(expected)
        call check(error, e%is_right(), "set_right_r16 should make is_right true")
        if (allocated(error)) return

        val = e%get_right_r16()

        call check(error, abs(val - expected) < 1e-5, "get_right_r16 value mismatch")

    end subroutine test_types_r16

    subroutine test_types_r32(error)
        type(error_type), allocatable, intent(out) :: error
        type(either) :: e
        real(real32) :: val, expected

        expected = 1.23_real32

        ! Test Left
        call e%set_left_r32(expected)
        call check(error, e%is_left(), "set_left_r32 should make is_left true")
        if (allocated(error)) return

        val = e%get_left_r32()

        call check(error, abs(val - expected) < 1e-5, "get_left_r32 value mismatch")

        if (allocated(error)) return

        ! Test Right
        call e%set_right_r32(expected)
        call check(error, e%is_right(), "set_right_r32 should make is_right true")
        if (allocated(error)) return

        val = e%get_right_r32()

        call check(error, abs(val - expected) < 1e-5, "get_right_r32 value mismatch")

    end subroutine test_types_r32

    subroutine test_types_r64(error)
        type(error_type), allocatable, intent(out) :: error
        type(either) :: e
        real(real64) :: val, expected

        expected = 1.23_real64

        ! Test Left
        call e%set_left_r64(expected)
        call check(error, e%is_left(), "set_left_r64 should make is_left true")
        if (allocated(error)) return

        val = e%get_left_r64()

        call check(error, abs(val - expected) < 1e-5, "get_left_r64 value mismatch")

        if (allocated(error)) return

        ! Test Right
        call e%set_right_r64(expected)
        call check(error, e%is_right(), "set_right_r64 should make is_right true")
        if (allocated(error)) return

        val = e%get_right_r64()

        call check(error, abs(val - expected) < 1e-5, "get_right_r64 value mismatch")

    end subroutine test_types_r64

    subroutine test_types_r128(error)
        type(error_type), allocatable, intent(out) :: error
        type(either) :: e
        real(real128) :: val, expected

        expected = 1.23_real128

        ! Test Left
        call e%set_left_r128(expected)
        call check(error, e%is_left(), "set_left_r128 should make is_left true")
        if (allocated(error)) return

        val = e%get_left_r128()

        call check(error, abs(val - expected) < 1e-5, "get_left_r128 value mismatch")

        if (allocated(error)) return

        ! Test Right
        call e%set_right_r128(expected)
        call check(error, e%is_right(), "set_right_r128 should make is_right true")
        if (allocated(error)) return

        val = e%get_right_r128()

        call check(error, abs(val - expected) < 1e-5, "get_right_r128 value mismatch")

    end subroutine test_types_r128

    subroutine test_types_i8(error)
        type(error_type), allocatable, intent(out) :: error
        type(either) :: e
        integer(int8) :: val, expected

        expected = 42_int8

        ! Test Left
        call e%set_left_i8(expected)
        call check(error, e%is_left(), "set_left_i8 should make is_left true")
        if (allocated(error)) return

        val = e%get_left_i8()

        call check(error, val == expected, "get_left_i8 value mismatch")

        if (allocated(error)) return

        ! Test Right
        call e%set_right_i8(expected)
        call check(error, e%is_right(), "set_right_i8 should make is_right true")
        if (allocated(error)) return

        val = e%get_right_i8()

        call check(error, val == expected, "get_right_i8 value mismatch")

    end subroutine test_types_i8

    subroutine test_types_i16(error)
        type(error_type), allocatable, intent(out) :: error
        type(either) :: e
        integer(int16) :: val, expected

        expected = 42_int16

        ! Test Left
        call e%set_left_i16(expected)
        call check(error, e%is_left(), "set_left_i16 should make is_left true")
        if (allocated(error)) return

        val = e%get_left_i16()

        call check(error, val == expected, "get_left_i16 value mismatch")

        if (allocated(error)) return

        ! Test Right
        call e%set_right_i16(expected)
        call check(error, e%is_right(), "set_right_i16 should make is_right true")
        if (allocated(error)) return

        val = e%get_right_i16()

        call check(error, val == expected, "get_right_i16 value mismatch")

    end subroutine test_types_i16

    subroutine test_types_i32(error)
        type(error_type), allocatable, intent(out) :: error
        type(either) :: e
        integer(int32) :: val, expected

        expected = 42_int32

        ! Test Left
        call e%set_left_i32(expected)
        call check(error, e%is_left(), "set_left_i32 should make is_left true")
        if (allocated(error)) return

        val = e%get_left_i32()

        call check(error, val == expected, "get_left_i32 value mismatch")

        if (allocated(error)) return

        ! Test Right
        call e%set_right_i32(expected)
        call check(error, e%is_right(), "set_right_i32 should make is_right true")
        if (allocated(error)) return

        val = e%get_right_i32()

        call check(error, val == expected, "get_right_i32 value mismatch")

    end subroutine test_types_i32

    subroutine test_types_i64(error)
        type(error_type), allocatable, intent(out) :: error
        type(either) :: e
        integer(int64) :: val, expected

        expected = 42_int64

        ! Test Left
        call e%set_left_i64(expected)
        call check(error, e%is_left(), "set_left_i64 should make is_left true")
        if (allocated(error)) return

        val = e%get_left_i64()

        call check(error, val == expected, "get_left_i64 value mismatch")

        if (allocated(error)) return

        ! Test Right
        call e%set_right_i64(expected)
        call check(error, e%is_right(), "set_right_i64 should make is_right true")
        if (allocated(error)) return

        val = e%get_right_i64()

        call check(error, val == expected, "get_right_i64 value mismatch")

    end subroutine test_types_i64

    subroutine test_types_i128(error)
        type(error_type), allocatable, intent(out) :: error
        type(either) :: e
        integer(int128) :: val, expected

        expected = 42_int128

        ! Test Left
        call e%set_left_i128(expected)
        call check(error, e%is_left(), "set_left_i128 should make is_left true")
        if (allocated(error)) return

        val = e%get_left_i128()

        call check(error, val == expected, "get_left_i128 value mismatch")

        if (allocated(error)) return

        ! Test Right
        call e%set_right_i128(expected)
        call check(error, e%is_right(), "set_right_i128 should make is_right true")
        if (allocated(error)) return

        val = e%get_right_i128()

        call check(error, val == expected, "get_right_i128 value mismatch")

    end subroutine test_types_i128

    subroutine test_types_c8(error)
        type(error_type), allocatable, intent(out) :: error
        type(either) :: e
        complex(real8) :: val, expected

        expected = (1.0_real8, 2.0_real8)

        ! Test Left
        call e%set_left_c8(expected)
        call check(error, e%is_left(), "set_left_c8 should make is_left true")
        if (allocated(error)) return

        val = e%get_left_c8()

        call check(error, abs(val - expected) < 1e-5, "get_left_c8 value mismatch")

        if (allocated(error)) return

        ! Test Right
        call e%set_right_c8(expected)
        call check(error, e%is_right(), "set_right_c8 should make is_right true")
        if (allocated(error)) return

        val = e%get_right_c8()

        call check(error, abs(val - expected) < 1e-5, "get_right_c8 value mismatch")

    end subroutine test_types_c8

    subroutine test_types_c16(error)
        type(error_type), allocatable, intent(out) :: error
        type(either) :: e
        complex(real16) :: val, expected

        expected = (1.0_real16, 2.0_real16)

        ! Test Left
        call e%set_left_c16(expected)
        call check(error, e%is_left(), "set_left_c16 should make is_left true")
        if (allocated(error)) return

        val = e%get_left_c16()

        call check(error, abs(val - expected) < 1e-5, "get_left_c16 value mismatch")

        if (allocated(error)) return

        ! Test Right
        call e%set_right_c16(expected)
        call check(error, e%is_right(), "set_right_c16 should make is_right true")
        if (allocated(error)) return

        val = e%get_right_c16()

        call check(error, abs(val - expected) < 1e-5, "get_right_c16 value mismatch")

    end subroutine test_types_c16

    subroutine test_types_c32(error)
        type(error_type), allocatable, intent(out) :: error
        type(either) :: e
        complex(real32) :: val, expected

        expected = (1.0_real32, 2.0_real32)

        ! Test Left
        call e%set_left_c32(expected)
        call check(error, e%is_left(), "set_left_c32 should make is_left true")
        if (allocated(error)) return

        val = e%get_left_c32()

        call check(error, abs(val - expected) < 1e-5, "get_left_c32 value mismatch")

        if (allocated(error)) return

        ! Test Right
        call e%set_right_c32(expected)
        call check(error, e%is_right(), "set_right_c32 should make is_right true")
        if (allocated(error)) return

        val = e%get_right_c32()

        call check(error, abs(val - expected) < 1e-5, "get_right_c32 value mismatch")

    end subroutine test_types_c32

    subroutine test_types_c64(error)
        type(error_type), allocatable, intent(out) :: error
        type(either) :: e
        complex(real64) :: val, expected

        expected = (1.0_real64, 2.0_real64)

        ! Test Left
        call e%set_left_c64(expected)
        call check(error, e%is_left(), "set_left_c64 should make is_left true")
        if (allocated(error)) return

        val = e%get_left_c64()

        call check(error, abs(val - expected) < 1e-5, "get_left_c64 value mismatch")

        if (allocated(error)) return

        ! Test Right
        call e%set_right_c64(expected)
        call check(error, e%is_right(), "set_right_c64 should make is_right true")
        if (allocated(error)) return

        val = e%get_right_c64()

        call check(error, abs(val - expected) < 1e-5, "get_right_c64 value mismatch")

    end subroutine test_types_c64

    subroutine test_types_c128(error)
        type(error_type), allocatable, intent(out) :: error
        type(either) :: e
        complex(real128) :: val, expected

        expected = (1.0_real128, 2.0_real128)

        ! Test Left
        call e%set_left_c128(expected)
        call check(error, e%is_left(), "set_left_c128 should make is_left true")
        if (allocated(error)) return

        val = e%get_left_c128()

        call check(error, abs(val - expected) < 1e-5, "get_left_c128 value mismatch")

        if (allocated(error)) return

        ! Test Right
        call e%set_right_c128(expected)
        call check(error, e%is_right(), "set_right_c128 should make is_right true")
        if (allocated(error)) return

        val = e%get_right_c128()

        call check(error, abs(val - expected) < 1e-5, "get_right_c128 value mismatch")

    end subroutine test_types_c128

end module either_tests

module either_suite
    use testdrive, only: new_unittest, unittest_type, error_type, check
    use either_tests
    implicit none
    private

    public :: collect_either_suite

contains

    subroutine collect_either_suite(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
                    new_unittest("integer_left", test_integer_left), &
                    new_unittest("real_right", test_real_right), &
                    new_unittest("string_logic", test_string_logic), &
                    new_unittest("generic_constructor", test_generic_constructor), &
                    new_unittest("specialized_either", test_specialized_either), &
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

    end subroutine collect_either_suite

end module either_suite
