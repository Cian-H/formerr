program benchmark
    use, intrinsic :: iso_fortran_env, only: {{ supported_types.iso_uses() }}, int64, real64
    use formerr_result
    use formerr_option, only: option, some, none
    implicit none

    {% include "shared/selected_kinds.f90" %}

    integer, parameter :: ITERATIONS = 10000000
    integer(int64) :: t_start, t_end, t_rate
    real(real64) :: t_sso, t_dyn

    ! Timing variables for numeric types
    {% for t in supported_types.SUPPORTED_TYPES if supported_types.is_numeric(t) %}
    real(real64) :: t_{{ t.suffix }}_native, t_{{ t.suffix }}_generic, t_{{ t.suffix }}_spec_unwrap, t_{{ t.suffix }}_fully_spec
    {% endfor %}

    print *, "Running benchmarks with", ITERATIONS, "iterations..."
    call system_clock(count_rate=t_rate)

    {% for t in supported_types.SUPPORTED_TYPES if supported_types.is_numeric(t) %}
    ! 1. Baseline: Native
    call system_clock(t_start)
    call bench_native_{{ t.suffix }}(ITERATIONS)
    call system_clock(t_end)
    t_{{ t.suffix }}_native = real(t_end - t_start, real64)/real(t_rate, real64)

    ! 2. Formerr: Generic
    call system_clock(t_start)
    call bench_{{ t.suffix }}_generic(ITERATIONS)
    call system_clock(t_end)
    t_{{ t.suffix }}_generic = real(t_end - t_start, real64)/real(t_rate, real64)

    ! 3. Formerr: Specialized Unwrap
    call system_clock(t_start)
    call bench_{{ t.suffix }}_specialized(ITERATIONS)
    call system_clock(t_end)
    t_{{ t.suffix }}_spec_unwrap = real(t_end - t_start, real64)/real(t_rate, real64)

    ! 4. Formerr: Fully Specialized
    call system_clock(t_start)
    call bench_{{ t.suffix }}_fully_specialized(ITERATIONS)
    call system_clock(t_end)
    t_{{ t.suffix }}_fully_spec = real(t_end - t_start, real64)/real(t_rate, real64)
    {% endfor %}

    ! Print Numeric Table
    print '(A)', repeat("-", 114)
    print '(A20, 1X, A10, 1X, A10, 1X, A8, 1X, A10, 1X, A8, 1X, A10, 1X, A8)', &
          "Type", "Native(s)", "Generic(s)", "Ovhd", "Spec(s)", "Ovhd", "Full(s)", "Ovhd"
    print '(A)', repeat("-", 114)

    {% for t in supported_types.SUPPORTED_TYPES if supported_types.is_numeric(t) %}
    print '(A20, 1X, F10.6, 1X, F10.6, 1X, F7.2, "x", 1X, F10.6, 1X, F7.2, "x", 1X, F10.6, 1X, F7.2, "x")', &
          "{{ t.name }}", &
          t_{{ t.suffix }}_native, &
          t_{{ t.suffix }}_generic, t_{{ t.suffix }}_generic/t_{{ t.suffix }}_native, &
          t_{{ t.suffix }}_spec_unwrap, t_{{ t.suffix }}_spec_unwrap/t_{{ t.suffix }}_native, &
          t_{{ t.suffix }}_fully_spec, t_{{ t.suffix }}_fully_spec/t_{{ t.suffix }}_native
    {% endfor %}
    print '(A)', repeat("-", 114)
    print *

    ! --- String Benchmarks ---
    ! 1. SSO Path (< 32 chars)
    call system_clock(t_start)
    call bench_sso_string(ITERATIONS)
    call system_clock(t_end)
    t_sso = real(t_end - t_start, real64)/real(t_rate, real64)

    ! 2. Dynamic Path (>= 32 chars)
    call system_clock(t_start)
    call bench_dyn_string(ITERATIONS)
    call system_clock(t_end)
    t_dyn = real(t_end - t_start, real64)/real(t_rate, real64)

    print '(A)', repeat("-", 50)
    print '(A30, 1X, A10)', "String Scenario", "Time(s)"
    print '(A)', repeat("-", 50)
    print '(A30, 1X, F10.6)', "SSO String (<32 chars)", t_sso
    print '(A30, 1X, F10.6)', "Dynamic String (>=32)", t_dyn
    print '(A30, 1X, F9.2, "x")', "Speedup (SSO vs Dyn)", t_dyn/t_sso
    print '(A)', repeat("-", 50)

contains

    {% for t in supported_types.SUPPORTED_TYPES if supported_types.is_numeric(t) %}
    ! --- {{ t.name }} Benchmarks ---
    subroutine bench_native_{{ t.suffix }}(n)
        integer, intent(in) :: n
        integer :: i, stat
        {{ t.type_def }} :: val, a, b
        {{ t.type_def }}, volatile :: sink

        {% if "complex" in t.name %}
        a = {{ supported_types.get_test_value(t) }}
        b = {{ supported_types.get_test_value(t) }}
        {% else %}
        a = 1
        b = 2
        {% endif %}

        do i = 1, n
            call native_add_{{ t.suffix }}(a, b, val, stat)
            if (stat /= 0) error stop "Fail"
            sink = val
        end do
    end subroutine

    subroutine native_add_{{ t.suffix }}(a, b, res, stat)
        {{ t.type_def }}, intent(in) :: a, b
        {{ t.type_def }}, intent(out) :: res
        integer, intent(out) :: stat
        res = a + b
        stat = 0 ! Success
    end subroutine

    subroutine bench_{{ t.suffix }}_generic(n)
        integer, intent(in) :: n
        integer :: i
        type(result_type) :: res
        class(*), pointer :: ptr
        {{ t.type_def }} :: val, a, b
        {{ t.type_def }}, volatile :: sink

        {% if "complex" in t.name %}
        a = {{ supported_types.get_test_value(t) }}
        b = {{ supported_types.get_test_value(t) }}
        {% else %}
        a = 1
        b = 2
        {% endif %}

        do i = 1, n
            res = formerr_add_{{ t.suffix }}_generic(a, b)
            if (.not. res%is_ok()) error stop "Fail"
            ptr => res%unwrap()
            select type (ptr)
            type is ({{ t.type_def }})
                val = ptr
                ! Simple check to prevent DCE
                {% if "complex" in t.name %}
                if (real(val) < 0) stop
                {% else %}
                if (val < 0) stop
                {% endif %}
                sink = val
            end select
        end do
    end subroutine

    subroutine bench_{{ t.suffix }}_specialized(n)
        integer, intent(in) :: n
        integer :: i
        type(result_type) :: res
        {{ t.type_def }} :: val, a, b
        {{ t.type_def }}, volatile :: sink

        {% if "complex" in t.name %}
        a = {{ supported_types.get_test_value(t) }}
        b = {{ supported_types.get_test_value(t) }}
        {% else %}
        a = 1
        b = 2
        {% endif %}

        do i = 1, n
            res = formerr_add_{{ t.suffix }}_generic(a, b)
            if (.not. res%is_ok()) error stop "Fail"
            val = res%unwrap_{{ t.suffix }}()
            {% if "complex" in t.name %}
            if (real(val) < 0) stop
            {% else %}
            if (val < 0) stop
            {% endif %}
            sink = val
        end do
    end subroutine

    subroutine bench_{{ t.suffix }}_fully_specialized(n)
        integer, intent(in) :: n
        integer :: i
        {{ t.type_def }} :: val, a, b
        type(result_type) :: res
        {{ t.type_def }}, volatile :: sink

        {% if "complex" in t.name %}
        a = {{ supported_types.get_test_value(t) }}
        b = {{ supported_types.get_test_value(t) }}
        {% else %}
        a = 1
        b = 2
        {% endif %}

        do i = 1, n
            res = ok_{{ t.suffix }}(a + b)
            if (.not. res%is_ok()) error stop "Fail"
            val = res%unwrap_{{ t.suffix }}()
            {% if "complex" in t.name %}
            if (real(val) < 0) stop
            {% else %}
            if (val < 0) stop
            {% endif %}
            sink = val
        end do
    end subroutine

    function formerr_add_{{ t.suffix }}_generic(a, b) result(res)
        {{ t.type_def }}, intent(in) :: a, b
        type(result_type) :: res
        res = ok(a + b)
    end function
    {% endfor %}

    ! --- String Benchmarks ---
    subroutine bench_sso_string(n)
        integer, intent(in) :: n
        integer :: i
        type(result_type) :: res
        character(:), allocatable :: val
        ! Short string (fits in 32 bytes)
        character(len=*), parameter :: s = "Short string"

        do i = 1, n
            res = ok_string(s)
            val = res%unwrap_string()
            if (len(val) /= len(s)) stop "Length mismatch"
        end do
    end subroutine

    subroutine bench_dyn_string(n)
        integer, intent(in) :: n
        integer :: i
        type(result_type) :: res
        character(:), allocatable :: val
        ! Long string (> 32 bytes)
        character(len=*), parameter :: s = "This string is definitely longer than thirty-two bytes so it allocates"

        do i = 1, n
            res = ok_string(s)
            val = res%unwrap_string()
            if (len(val) /= len(s)) stop "Length mismatch"
        end do
    end subroutine

end program benchmark