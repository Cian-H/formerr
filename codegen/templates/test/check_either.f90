module either_tests
    use testdrive, only: new_unittest, check, error_type
    use formerr_either
    use, intrinsic :: iso_fortran_env, only: {{ supported_types.iso_uses() }}
    implicit none

    {% include "shared/selected_kinds.f90" %}

    public :: test_integer_left, test_real_right, test_string_logic, &
              test_generic_constructor, test_specialized_either, &
              {% for t in supported_types.SUPPORTED_TYPES %}
              test_types_{{ t.suffix }}{% if not loop.last %}, &{% endif %}
              {% endfor %}

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
        call check(error, .not. e%is_left(), "set_right_real should clear left")
        r_val = e%get_right_real()
        call check(error, abs(r_val - 5.55) < 1e-5, "get_right_real value mismatch")

    end subroutine test_specialized_either

    ! --- Specialized Type Tests (Generated) ---
    {% for t in supported_types.SUPPORTED_TYPES %}
    subroutine test_types_{{ t.suffix }}(error)
        type(error_type), allocatable, intent(out) :: error
        type(either) :: e
        {{ t.type_def }} :: val, expected

        expected = {{ supported_types.get_test_value(t) }}

        ! Test Left
        call e%set_left_{{ t.suffix }}(expected)
        call check(error, e%is_left(), "set_left_{{ t.suffix }} should make is_left true")
        if (allocated(error)) return
        
        val = e%get_left_{{ t.suffix }}()
        {% if "real" in t.name or "complex" in t.name %}
            call check(error, abs(val - expected) < 1e-5, "get_left_{{ t.suffix }} value mismatch")
        {% elif "logical" in t.name %}
             call check(error, val .eqv. expected, "get_left_{{ t.suffix }} value mismatch")
        {% else %}
             call check(error, val == expected, "get_left_{{ t.suffix }} value mismatch")
        {% endif %}
        if (allocated(error)) return

        ! Test Right
        call e%set_right_{{ t.suffix }}(expected)
        call check(error, e%is_right(), "set_right_{{ t.suffix }} should make is_right true")
        if (allocated(error)) return
        
        val = e%get_right_{{ t.suffix }}()
        {% if "real" in t.name or "complex" in t.name %}
            call check(error, abs(val - expected) < 1e-5, "get_right_{{ t.suffix }} value mismatch")
        {% elif "logical" in t.name %}
             call check(error, val .eqv. expected, "get_right_{{ t.suffix }} value mismatch")
        {% else %}
             call check(error, val == expected, "get_right_{{ t.suffix }} value mismatch")
        {% endif %}
    end subroutine test_types_{{ t.suffix }}
    {% endfor %}

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
                    {% for t in supported_types.SUPPORTED_TYPES %}
                    new_unittest("types_{{ t.suffix }}", test_types_{{ t.suffix }}){% if not loop.last %}, &{% else %} &{% endif %}
                    {% endfor %}
                    ]

    end subroutine collect_either_suite

end module either_suite
