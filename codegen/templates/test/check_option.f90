module option_tests
    use testdrive, only: new_unittest, check, error_type
    use formerr_option
    use, intrinsic :: iso_fortran_env, only: {{ supported_types.iso_uses() }}
    implicit none
    private

    {% include "shared/selected_kinds.f90" %}

    type :: custom_type
        integer :: id
        real :: value
    end type custom_type

    public :: test_some_integer, test_none_behavior, &
              test_unwrap_or_logic, test_unwrap_or_copy_semantics, &
              test_string_option, test_custom_type, &
              {% for t in supported_types.SUPPORTED_TYPES %}
              test_types_{{ t.suffix }}{% if not loop.last %}, &{% endif %}
              {% endfor %}

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
    {% for t in supported_types.SUPPORTED_TYPES %}
    subroutine test_types_{{ t.suffix }}(error)
        type(error_type), allocatable, intent(out) :: error
        type(option) :: opt
        {{ t.type_def }} :: val, expected

        expected = {{ supported_types.get_test_value(t) }}

        ! Test Some
        opt = some_{{ t.suffix }}(expected)
        call check(error, opt%is_some(), "some_{{ t.suffix }} should be Some")
        if (allocated(error)) return
        
        val = opt%unwrap_{{ t.suffix }}()
        {% if "real" in t.name or "complex" in t.name %}
            call check(error, abs(val - expected) < 1e-5, "unwrap_{{ t.suffix }} value mismatch")
        {% elif "logical" in t.name %}
             call check(error, val .eqv. expected, "unwrap_{{ t.suffix }} value mismatch")
        {% else %}
             call check(error, val == expected, "unwrap_{{ t.suffix }} value mismatch")
        {% endif %}
        if (allocated(error)) return

        ! Test None behavior with specialized unwrap_or
        opt = none()
        val = opt%unwrap_or_{{ t.suffix }}(expected)
        {% if "real" in t.name or "complex" in t.name %}
            call check(error, abs(val - expected) < 1e-5, "unwrap_or_{{ t.suffix }} default value mismatch")
        {% elif "logical" in t.name %}
             call check(error, val .eqv. expected, "unwrap_or_{{ t.suffix }} default value mismatch")
        {% else %}
             call check(error, val == expected, "unwrap_or_{{ t.suffix }} default value mismatch")
        {% endif %}
    end subroutine test_types_{{ t.suffix }}
    {% endfor %}

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
                    {% for t in supported_types.SUPPORTED_TYPES %}
                    new_unittest("types_{{ t.suffix }}", test_types_{{ t.suffix }}){% if not loop.last %}, &{% else %} &{% endif %}
                    {% endfor %}
                    ]

    end subroutine collect_option_suite

end module option_suite
