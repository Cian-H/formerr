program benchmark
    use, intrinsic :: iso_fortran_env, only: real32, real64, real128, int8, int16, int32, int64, int64, real64
    use formerr_result
    use formerr_option, only: option, some, none
    implicit none

    integer, parameter :: real8 = selected_real_kind(2, 2)
    integer, parameter :: real16 = selected_real_kind(4, 4)
    integer, parameter :: int128 = selected_int_kind(38)

    integer, parameter :: ITERATIONS = 10000000
    integer(int64) :: t_start, t_end, t_rate
    real(real64) :: t_sso, t_dyn

    ! Timing variables for numeric types

    real(real64) :: t_int_native, t_int_generic, t_int_spec_unwrap, t_int_fully_spec

    real(real64) :: t_real_native, t_real_generic, t_real_spec_unwrap, t_real_fully_spec

    real(real64) :: t_cpx_native, t_cpx_generic, t_cpx_spec_unwrap, t_cpx_fully_spec

    real(real64) :: t_r8_native, t_r8_generic, t_r8_spec_unwrap, t_r8_fully_spec

    real(real64) :: t_r16_native, t_r16_generic, t_r16_spec_unwrap, t_r16_fully_spec

    real(real64) :: t_r32_native, t_r32_generic, t_r32_spec_unwrap, t_r32_fully_spec

    real(real64) :: t_r64_native, t_r64_generic, t_r64_spec_unwrap, t_r64_fully_spec

    real(real64) :: t_r128_native, t_r128_generic, t_r128_spec_unwrap, t_r128_fully_spec

    real(real64) :: t_i8_native, t_i8_generic, t_i8_spec_unwrap, t_i8_fully_spec

    real(real64) :: t_i16_native, t_i16_generic, t_i16_spec_unwrap, t_i16_fully_spec

    real(real64) :: t_i32_native, t_i32_generic, t_i32_spec_unwrap, t_i32_fully_spec

    real(real64) :: t_i64_native, t_i64_generic, t_i64_spec_unwrap, t_i64_fully_spec

    real(real64) :: t_i128_native, t_i128_generic, t_i128_spec_unwrap, t_i128_fully_spec

    real(real64) :: t_c8_native, t_c8_generic, t_c8_spec_unwrap, t_c8_fully_spec

    real(real64) :: t_c16_native, t_c16_generic, t_c16_spec_unwrap, t_c16_fully_spec

    real(real64) :: t_c32_native, t_c32_generic, t_c32_spec_unwrap, t_c32_fully_spec

    real(real64) :: t_c64_native, t_c64_generic, t_c64_spec_unwrap, t_c64_fully_spec

    real(real64) :: t_c128_native, t_c128_generic, t_c128_spec_unwrap, t_c128_fully_spec

    print *, "Running benchmarks with", ITERATIONS, "iterations..."
    call system_clock(count_rate=t_rate)

    ! 1. Baseline: Native
    call system_clock(t_start)
    call bench_native_int(ITERATIONS)
    call system_clock(t_end)
    t_int_native = real(t_end - t_start, real64)/real(t_rate, real64)

    ! 2. Formerr: Generic
    call system_clock(t_start)
    call bench_int_generic(ITERATIONS)
    call system_clock(t_end)
    t_int_generic = real(t_end - t_start, real64)/real(t_rate, real64)

    ! 3. Formerr: Specialized Unwrap
    call system_clock(t_start)
    call bench_int_specialized(ITERATIONS)
    call system_clock(t_end)
    t_int_spec_unwrap = real(t_end - t_start, real64)/real(t_rate, real64)

    ! 4. Formerr: Fully Specialized
    call system_clock(t_start)
    call bench_int_fully_specialized(ITERATIONS)
    call system_clock(t_end)
    t_int_fully_spec = real(t_end - t_start, real64)/real(t_rate, real64)

    ! 1. Baseline: Native
    call system_clock(t_start)
    call bench_native_real(ITERATIONS)
    call system_clock(t_end)
    t_real_native = real(t_end - t_start, real64)/real(t_rate, real64)

    ! 2. Formerr: Generic
    call system_clock(t_start)
    call bench_real_generic(ITERATIONS)
    call system_clock(t_end)
    t_real_generic = real(t_end - t_start, real64)/real(t_rate, real64)

    ! 3. Formerr: Specialized Unwrap
    call system_clock(t_start)
    call bench_real_specialized(ITERATIONS)
    call system_clock(t_end)
    t_real_spec_unwrap = real(t_end - t_start, real64)/real(t_rate, real64)

    ! 4. Formerr: Fully Specialized
    call system_clock(t_start)
    call bench_real_fully_specialized(ITERATIONS)
    call system_clock(t_end)
    t_real_fully_spec = real(t_end - t_start, real64)/real(t_rate, real64)

    ! 1. Baseline: Native
    call system_clock(t_start)
    call bench_native_cpx(ITERATIONS)
    call system_clock(t_end)
    t_cpx_native = real(t_end - t_start, real64)/real(t_rate, real64)

    ! 2. Formerr: Generic
    call system_clock(t_start)
    call bench_cpx_generic(ITERATIONS)
    call system_clock(t_end)
    t_cpx_generic = real(t_end - t_start, real64)/real(t_rate, real64)

    ! 3. Formerr: Specialized Unwrap
    call system_clock(t_start)
    call bench_cpx_specialized(ITERATIONS)
    call system_clock(t_end)
    t_cpx_spec_unwrap = real(t_end - t_start, real64)/real(t_rate, real64)

    ! 4. Formerr: Fully Specialized
    call system_clock(t_start)
    call bench_cpx_fully_specialized(ITERATIONS)
    call system_clock(t_end)
    t_cpx_fully_spec = real(t_end - t_start, real64)/real(t_rate, real64)

    ! 1. Baseline: Native
    call system_clock(t_start)
    call bench_native_r8(ITERATIONS)
    call system_clock(t_end)
    t_r8_native = real(t_end - t_start, real64)/real(t_rate, real64)

    ! 2. Formerr: Generic
    call system_clock(t_start)
    call bench_r8_generic(ITERATIONS)
    call system_clock(t_end)
    t_r8_generic = real(t_end - t_start, real64)/real(t_rate, real64)

    ! 3. Formerr: Specialized Unwrap
    call system_clock(t_start)
    call bench_r8_specialized(ITERATIONS)
    call system_clock(t_end)
    t_r8_spec_unwrap = real(t_end - t_start, real64)/real(t_rate, real64)

    ! 4. Formerr: Fully Specialized
    call system_clock(t_start)
    call bench_r8_fully_specialized(ITERATIONS)
    call system_clock(t_end)
    t_r8_fully_spec = real(t_end - t_start, real64)/real(t_rate, real64)

    ! 1. Baseline: Native
    call system_clock(t_start)
    call bench_native_r16(ITERATIONS)
    call system_clock(t_end)
    t_r16_native = real(t_end - t_start, real64)/real(t_rate, real64)

    ! 2. Formerr: Generic
    call system_clock(t_start)
    call bench_r16_generic(ITERATIONS)
    call system_clock(t_end)
    t_r16_generic = real(t_end - t_start, real64)/real(t_rate, real64)

    ! 3. Formerr: Specialized Unwrap
    call system_clock(t_start)
    call bench_r16_specialized(ITERATIONS)
    call system_clock(t_end)
    t_r16_spec_unwrap = real(t_end - t_start, real64)/real(t_rate, real64)

    ! 4. Formerr: Fully Specialized
    call system_clock(t_start)
    call bench_r16_fully_specialized(ITERATIONS)
    call system_clock(t_end)
    t_r16_fully_spec = real(t_end - t_start, real64)/real(t_rate, real64)

    ! 1. Baseline: Native
    call system_clock(t_start)
    call bench_native_r32(ITERATIONS)
    call system_clock(t_end)
    t_r32_native = real(t_end - t_start, real64)/real(t_rate, real64)

    ! 2. Formerr: Generic
    call system_clock(t_start)
    call bench_r32_generic(ITERATIONS)
    call system_clock(t_end)
    t_r32_generic = real(t_end - t_start, real64)/real(t_rate, real64)

    ! 3. Formerr: Specialized Unwrap
    call system_clock(t_start)
    call bench_r32_specialized(ITERATIONS)
    call system_clock(t_end)
    t_r32_spec_unwrap = real(t_end - t_start, real64)/real(t_rate, real64)

    ! 4. Formerr: Fully Specialized
    call system_clock(t_start)
    call bench_r32_fully_specialized(ITERATIONS)
    call system_clock(t_end)
    t_r32_fully_spec = real(t_end - t_start, real64)/real(t_rate, real64)

    ! 1. Baseline: Native
    call system_clock(t_start)
    call bench_native_r64(ITERATIONS)
    call system_clock(t_end)
    t_r64_native = real(t_end - t_start, real64)/real(t_rate, real64)

    ! 2. Formerr: Generic
    call system_clock(t_start)
    call bench_r64_generic(ITERATIONS)
    call system_clock(t_end)
    t_r64_generic = real(t_end - t_start, real64)/real(t_rate, real64)

    ! 3. Formerr: Specialized Unwrap
    call system_clock(t_start)
    call bench_r64_specialized(ITERATIONS)
    call system_clock(t_end)
    t_r64_spec_unwrap = real(t_end - t_start, real64)/real(t_rate, real64)

    ! 4. Formerr: Fully Specialized
    call system_clock(t_start)
    call bench_r64_fully_specialized(ITERATIONS)
    call system_clock(t_end)
    t_r64_fully_spec = real(t_end - t_start, real64)/real(t_rate, real64)

    ! 1. Baseline: Native
    call system_clock(t_start)
    call bench_native_r128(ITERATIONS)
    call system_clock(t_end)
    t_r128_native = real(t_end - t_start, real64)/real(t_rate, real64)

    ! 2. Formerr: Generic
    call system_clock(t_start)
    call bench_r128_generic(ITERATIONS)
    call system_clock(t_end)
    t_r128_generic = real(t_end - t_start, real64)/real(t_rate, real64)

    ! 3. Formerr: Specialized Unwrap
    call system_clock(t_start)
    call bench_r128_specialized(ITERATIONS)
    call system_clock(t_end)
    t_r128_spec_unwrap = real(t_end - t_start, real64)/real(t_rate, real64)

    ! 4. Formerr: Fully Specialized
    call system_clock(t_start)
    call bench_r128_fully_specialized(ITERATIONS)
    call system_clock(t_end)
    t_r128_fully_spec = real(t_end - t_start, real64)/real(t_rate, real64)

    ! 1. Baseline: Native
    call system_clock(t_start)
    call bench_native_i8(ITERATIONS)
    call system_clock(t_end)
    t_i8_native = real(t_end - t_start, real64)/real(t_rate, real64)

    ! 2. Formerr: Generic
    call system_clock(t_start)
    call bench_i8_generic(ITERATIONS)
    call system_clock(t_end)
    t_i8_generic = real(t_end - t_start, real64)/real(t_rate, real64)

    ! 3. Formerr: Specialized Unwrap
    call system_clock(t_start)
    call bench_i8_specialized(ITERATIONS)
    call system_clock(t_end)
    t_i8_spec_unwrap = real(t_end - t_start, real64)/real(t_rate, real64)

    ! 4. Formerr: Fully Specialized
    call system_clock(t_start)
    call bench_i8_fully_specialized(ITERATIONS)
    call system_clock(t_end)
    t_i8_fully_spec = real(t_end - t_start, real64)/real(t_rate, real64)

    ! 1. Baseline: Native
    call system_clock(t_start)
    call bench_native_i16(ITERATIONS)
    call system_clock(t_end)
    t_i16_native = real(t_end - t_start, real64)/real(t_rate, real64)

    ! 2. Formerr: Generic
    call system_clock(t_start)
    call bench_i16_generic(ITERATIONS)
    call system_clock(t_end)
    t_i16_generic = real(t_end - t_start, real64)/real(t_rate, real64)

    ! 3. Formerr: Specialized Unwrap
    call system_clock(t_start)
    call bench_i16_specialized(ITERATIONS)
    call system_clock(t_end)
    t_i16_spec_unwrap = real(t_end - t_start, real64)/real(t_rate, real64)

    ! 4. Formerr: Fully Specialized
    call system_clock(t_start)
    call bench_i16_fully_specialized(ITERATIONS)
    call system_clock(t_end)
    t_i16_fully_spec = real(t_end - t_start, real64)/real(t_rate, real64)

    ! 1. Baseline: Native
    call system_clock(t_start)
    call bench_native_i32(ITERATIONS)
    call system_clock(t_end)
    t_i32_native = real(t_end - t_start, real64)/real(t_rate, real64)

    ! 2. Formerr: Generic
    call system_clock(t_start)
    call bench_i32_generic(ITERATIONS)
    call system_clock(t_end)
    t_i32_generic = real(t_end - t_start, real64)/real(t_rate, real64)

    ! 3. Formerr: Specialized Unwrap
    call system_clock(t_start)
    call bench_i32_specialized(ITERATIONS)
    call system_clock(t_end)
    t_i32_spec_unwrap = real(t_end - t_start, real64)/real(t_rate, real64)

    ! 4. Formerr: Fully Specialized
    call system_clock(t_start)
    call bench_i32_fully_specialized(ITERATIONS)
    call system_clock(t_end)
    t_i32_fully_spec = real(t_end - t_start, real64)/real(t_rate, real64)

    ! 1. Baseline: Native
    call system_clock(t_start)
    call bench_native_i64(ITERATIONS)
    call system_clock(t_end)
    t_i64_native = real(t_end - t_start, real64)/real(t_rate, real64)

    ! 2. Formerr: Generic
    call system_clock(t_start)
    call bench_i64_generic(ITERATIONS)
    call system_clock(t_end)
    t_i64_generic = real(t_end - t_start, real64)/real(t_rate, real64)

    ! 3. Formerr: Specialized Unwrap
    call system_clock(t_start)
    call bench_i64_specialized(ITERATIONS)
    call system_clock(t_end)
    t_i64_spec_unwrap = real(t_end - t_start, real64)/real(t_rate, real64)

    ! 4. Formerr: Fully Specialized
    call system_clock(t_start)
    call bench_i64_fully_specialized(ITERATIONS)
    call system_clock(t_end)
    t_i64_fully_spec = real(t_end - t_start, real64)/real(t_rate, real64)

    ! 1. Baseline: Native
    call system_clock(t_start)
    call bench_native_i128(ITERATIONS)
    call system_clock(t_end)
    t_i128_native = real(t_end - t_start, real64)/real(t_rate, real64)

    ! 2. Formerr: Generic
    call system_clock(t_start)
    call bench_i128_generic(ITERATIONS)
    call system_clock(t_end)
    t_i128_generic = real(t_end - t_start, real64)/real(t_rate, real64)

    ! 3. Formerr: Specialized Unwrap
    call system_clock(t_start)
    call bench_i128_specialized(ITERATIONS)
    call system_clock(t_end)
    t_i128_spec_unwrap = real(t_end - t_start, real64)/real(t_rate, real64)

    ! 4. Formerr: Fully Specialized
    call system_clock(t_start)
    call bench_i128_fully_specialized(ITERATIONS)
    call system_clock(t_end)
    t_i128_fully_spec = real(t_end - t_start, real64)/real(t_rate, real64)

    ! 1. Baseline: Native
    call system_clock(t_start)
    call bench_native_c8(ITERATIONS)
    call system_clock(t_end)
    t_c8_native = real(t_end - t_start, real64)/real(t_rate, real64)

    ! 2. Formerr: Generic
    call system_clock(t_start)
    call bench_c8_generic(ITERATIONS)
    call system_clock(t_end)
    t_c8_generic = real(t_end - t_start, real64)/real(t_rate, real64)

    ! 3. Formerr: Specialized Unwrap
    call system_clock(t_start)
    call bench_c8_specialized(ITERATIONS)
    call system_clock(t_end)
    t_c8_spec_unwrap = real(t_end - t_start, real64)/real(t_rate, real64)

    ! 4. Formerr: Fully Specialized
    call system_clock(t_start)
    call bench_c8_fully_specialized(ITERATIONS)
    call system_clock(t_end)
    t_c8_fully_spec = real(t_end - t_start, real64)/real(t_rate, real64)

    ! 1. Baseline: Native
    call system_clock(t_start)
    call bench_native_c16(ITERATIONS)
    call system_clock(t_end)
    t_c16_native = real(t_end - t_start, real64)/real(t_rate, real64)

    ! 2. Formerr: Generic
    call system_clock(t_start)
    call bench_c16_generic(ITERATIONS)
    call system_clock(t_end)
    t_c16_generic = real(t_end - t_start, real64)/real(t_rate, real64)

    ! 3. Formerr: Specialized Unwrap
    call system_clock(t_start)
    call bench_c16_specialized(ITERATIONS)
    call system_clock(t_end)
    t_c16_spec_unwrap = real(t_end - t_start, real64)/real(t_rate, real64)

    ! 4. Formerr: Fully Specialized
    call system_clock(t_start)
    call bench_c16_fully_specialized(ITERATIONS)
    call system_clock(t_end)
    t_c16_fully_spec = real(t_end - t_start, real64)/real(t_rate, real64)

    ! 1. Baseline: Native
    call system_clock(t_start)
    call bench_native_c32(ITERATIONS)
    call system_clock(t_end)
    t_c32_native = real(t_end - t_start, real64)/real(t_rate, real64)

    ! 2. Formerr: Generic
    call system_clock(t_start)
    call bench_c32_generic(ITERATIONS)
    call system_clock(t_end)
    t_c32_generic = real(t_end - t_start, real64)/real(t_rate, real64)

    ! 3. Formerr: Specialized Unwrap
    call system_clock(t_start)
    call bench_c32_specialized(ITERATIONS)
    call system_clock(t_end)
    t_c32_spec_unwrap = real(t_end - t_start, real64)/real(t_rate, real64)

    ! 4. Formerr: Fully Specialized
    call system_clock(t_start)
    call bench_c32_fully_specialized(ITERATIONS)
    call system_clock(t_end)
    t_c32_fully_spec = real(t_end - t_start, real64)/real(t_rate, real64)

    ! 1. Baseline: Native
    call system_clock(t_start)
    call bench_native_c64(ITERATIONS)
    call system_clock(t_end)
    t_c64_native = real(t_end - t_start, real64)/real(t_rate, real64)

    ! 2. Formerr: Generic
    call system_clock(t_start)
    call bench_c64_generic(ITERATIONS)
    call system_clock(t_end)
    t_c64_generic = real(t_end - t_start, real64)/real(t_rate, real64)

    ! 3. Formerr: Specialized Unwrap
    call system_clock(t_start)
    call bench_c64_specialized(ITERATIONS)
    call system_clock(t_end)
    t_c64_spec_unwrap = real(t_end - t_start, real64)/real(t_rate, real64)

    ! 4. Formerr: Fully Specialized
    call system_clock(t_start)
    call bench_c64_fully_specialized(ITERATIONS)
    call system_clock(t_end)
    t_c64_fully_spec = real(t_end - t_start, real64)/real(t_rate, real64)

    ! 1. Baseline: Native
    call system_clock(t_start)
    call bench_native_c128(ITERATIONS)
    call system_clock(t_end)
    t_c128_native = real(t_end - t_start, real64)/real(t_rate, real64)

    ! 2. Formerr: Generic
    call system_clock(t_start)
    call bench_c128_generic(ITERATIONS)
    call system_clock(t_end)
    t_c128_generic = real(t_end - t_start, real64)/real(t_rate, real64)

    ! 3. Formerr: Specialized Unwrap
    call system_clock(t_start)
    call bench_c128_specialized(ITERATIONS)
    call system_clock(t_end)
    t_c128_spec_unwrap = real(t_end - t_start, real64)/real(t_rate, real64)

    ! 4. Formerr: Fully Specialized
    call system_clock(t_start)
    call bench_c128_fully_specialized(ITERATIONS)
    call system_clock(t_end)
    t_c128_fully_spec = real(t_end - t_start, real64)/real(t_rate, real64)

    ! Print Numeric Table
    print '(A)', repeat("-", 114)
    print '(A20, 1X, A10, 1X, A10, 1X, A8, 1X, A10, 1X, A8, 1X, A10, 1X, A8)', &
        "Type", "Native(s)", "Generic(s)", "Ovhd", "Spec(s)", "Ovhd", "Full(s)", "Ovhd"
    print '(A)', repeat("-", 114)

    print '(A20, 1X, F10.6, 1X, F10.6, 1X, F7.2, "x", 1X, F10.6, 1X, F7.2, "x", 1X, F10.6, 1X, F7.2, "x")', &
        "integer", &
        t_int_native, &
        t_int_generic, t_int_generic/t_int_native, &
        t_int_spec_unwrap, t_int_spec_unwrap/t_int_native, &
        t_int_fully_spec, t_int_fully_spec/t_int_native

    print '(A20, 1X, F10.6, 1X, F10.6, 1X, F7.2, "x", 1X, F10.6, 1X, F7.2, "x", 1X, F10.6, 1X, F7.2, "x")', &
        "real", &
        t_real_native, &
        t_real_generic, t_real_generic/t_real_native, &
        t_real_spec_unwrap, t_real_spec_unwrap/t_real_native, &
        t_real_fully_spec, t_real_fully_spec/t_real_native

    print '(A20, 1X, F10.6, 1X, F10.6, 1X, F7.2, "x", 1X, F10.6, 1X, F7.2, "x", 1X, F10.6, 1X, F7.2, "x")', &
        "complex", &
        t_cpx_native, &
        t_cpx_generic, t_cpx_generic/t_cpx_native, &
        t_cpx_spec_unwrap, t_cpx_spec_unwrap/t_cpx_native, &
        t_cpx_fully_spec, t_cpx_fully_spec/t_cpx_native

    print '(A20, 1X, F10.6, 1X, F10.6, 1X, F7.2, "x", 1X, F10.6, 1X, F7.2, "x", 1X, F10.6, 1X, F7.2, "x")', &
        "real(8)", &
        t_r8_native, &
        t_r8_generic, t_r8_generic/t_r8_native, &
        t_r8_spec_unwrap, t_r8_spec_unwrap/t_r8_native, &
        t_r8_fully_spec, t_r8_fully_spec/t_r8_native

    print '(A20, 1X, F10.6, 1X, F10.6, 1X, F7.2, "x", 1X, F10.6, 1X, F7.2, "x", 1X, F10.6, 1X, F7.2, "x")', &
        "real(16)", &
        t_r16_native, &
        t_r16_generic, t_r16_generic/t_r16_native, &
        t_r16_spec_unwrap, t_r16_spec_unwrap/t_r16_native, &
        t_r16_fully_spec, t_r16_fully_spec/t_r16_native

    print '(A20, 1X, F10.6, 1X, F10.6, 1X, F7.2, "x", 1X, F10.6, 1X, F7.2, "x", 1X, F10.6, 1X, F7.2, "x")', &
        "real(real32)", &
        t_r32_native, &
        t_r32_generic, t_r32_generic/t_r32_native, &
        t_r32_spec_unwrap, t_r32_spec_unwrap/t_r32_native, &
        t_r32_fully_spec, t_r32_fully_spec/t_r32_native

    print '(A20, 1X, F10.6, 1X, F10.6, 1X, F7.2, "x", 1X, F10.6, 1X, F7.2, "x", 1X, F10.6, 1X, F7.2, "x")', &
        "real(real64)", &
        t_r64_native, &
        t_r64_generic, t_r64_generic/t_r64_native, &
        t_r64_spec_unwrap, t_r64_spec_unwrap/t_r64_native, &
        t_r64_fully_spec, t_r64_fully_spec/t_r64_native

    print '(A20, 1X, F10.6, 1X, F10.6, 1X, F7.2, "x", 1X, F10.6, 1X, F7.2, "x", 1X, F10.6, 1X, F7.2, "x")', &
        "real(real128)", &
        t_r128_native, &
        t_r128_generic, t_r128_generic/t_r128_native, &
        t_r128_spec_unwrap, t_r128_spec_unwrap/t_r128_native, &
        t_r128_fully_spec, t_r128_fully_spec/t_r128_native

    print '(A20, 1X, F10.6, 1X, F10.6, 1X, F7.2, "x", 1X, F10.6, 1X, F7.2, "x", 1X, F10.6, 1X, F7.2, "x")', &
        "integer(int8)", &
        t_i8_native, &
        t_i8_generic, t_i8_generic/t_i8_native, &
        t_i8_spec_unwrap, t_i8_spec_unwrap/t_i8_native, &
        t_i8_fully_spec, t_i8_fully_spec/t_i8_native

    print '(A20, 1X, F10.6, 1X, F10.6, 1X, F7.2, "x", 1X, F10.6, 1X, F7.2, "x", 1X, F10.6, 1X, F7.2, "x")', &
        "integer(int16)", &
        t_i16_native, &
        t_i16_generic, t_i16_generic/t_i16_native, &
        t_i16_spec_unwrap, t_i16_spec_unwrap/t_i16_native, &
        t_i16_fully_spec, t_i16_fully_spec/t_i16_native

    print '(A20, 1X, F10.6, 1X, F10.6, 1X, F7.2, "x", 1X, F10.6, 1X, F7.2, "x", 1X, F10.6, 1X, F7.2, "x")', &
        "integer(int32)", &
        t_i32_native, &
        t_i32_generic, t_i32_generic/t_i32_native, &
        t_i32_spec_unwrap, t_i32_spec_unwrap/t_i32_native, &
        t_i32_fully_spec, t_i32_fully_spec/t_i32_native

    print '(A20, 1X, F10.6, 1X, F10.6, 1X, F7.2, "x", 1X, F10.6, 1X, F7.2, "x", 1X, F10.6, 1X, F7.2, "x")', &
        "integer(int64)", &
        t_i64_native, &
        t_i64_generic, t_i64_generic/t_i64_native, &
        t_i64_spec_unwrap, t_i64_spec_unwrap/t_i64_native, &
        t_i64_fully_spec, t_i64_fully_spec/t_i64_native

    print '(A20, 1X, F10.6, 1X, F10.6, 1X, F7.2, "x", 1X, F10.6, 1X, F7.2, "x", 1X, F10.6, 1X, F7.2, "x")', &
        "integer(int128)", &
        t_i128_native, &
        t_i128_generic, t_i128_generic/t_i128_native, &
        t_i128_spec_unwrap, t_i128_spec_unwrap/t_i128_native, &
        t_i128_fully_spec, t_i128_fully_spec/t_i128_native

    print '(A20, 1X, F10.6, 1X, F10.6, 1X, F7.2, "x", 1X, F10.6, 1X, F7.2, "x", 1X, F10.6, 1X, F7.2, "x")', &
        "complex(real8)", &
        t_c8_native, &
        t_c8_generic, t_c8_generic/t_c8_native, &
        t_c8_spec_unwrap, t_c8_spec_unwrap/t_c8_native, &
        t_c8_fully_spec, t_c8_fully_spec/t_c8_native

    print '(A20, 1X, F10.6, 1X, F10.6, 1X, F7.2, "x", 1X, F10.6, 1X, F7.2, "x", 1X, F10.6, 1X, F7.2, "x")', &
        "complex(real16)", &
        t_c16_native, &
        t_c16_generic, t_c16_generic/t_c16_native, &
        t_c16_spec_unwrap, t_c16_spec_unwrap/t_c16_native, &
        t_c16_fully_spec, t_c16_fully_spec/t_c16_native

    print '(A20, 1X, F10.6, 1X, F10.6, 1X, F7.2, "x", 1X, F10.6, 1X, F7.2, "x", 1X, F10.6, 1X, F7.2, "x")', &
        "complex(real32)", &
        t_c32_native, &
        t_c32_generic, t_c32_generic/t_c32_native, &
        t_c32_spec_unwrap, t_c32_spec_unwrap/t_c32_native, &
        t_c32_fully_spec, t_c32_fully_spec/t_c32_native

    print '(A20, 1X, F10.6, 1X, F10.6, 1X, F7.2, "x", 1X, F10.6, 1X, F7.2, "x", 1X, F10.6, 1X, F7.2, "x")', &
        "complex(real64)", &
        t_c64_native, &
        t_c64_generic, t_c64_generic/t_c64_native, &
        t_c64_spec_unwrap, t_c64_spec_unwrap/t_c64_native, &
        t_c64_fully_spec, t_c64_fully_spec/t_c64_native

    print '(A20, 1X, F10.6, 1X, F10.6, 1X, F7.2, "x", 1X, F10.6, 1X, F7.2, "x", 1X, F10.6, 1X, F7.2, "x")', &
        "complex(real128)", &
        t_c128_native, &
        t_c128_generic, t_c128_generic/t_c128_native, &
        t_c128_spec_unwrap, t_c128_spec_unwrap/t_c128_native, &
        t_c128_fully_spec, t_c128_fully_spec/t_c128_native

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

    ! --- integer Benchmarks ---
    subroutine bench_native_int(n)
        integer, intent(in) :: n
        integer :: i, stat
        integer :: val, a, b
        integer, volatile :: sink

        a = 1
        b = 2

        do i = 1, n
            call native_add_int(a, b, val, stat)
            if (stat /= 0) error stop "Fail"
            sink = val
        end do
    end subroutine

    subroutine native_add_int(a, b, res, stat)
        integer, intent(in) :: a, b
        integer, intent(out) :: res
        integer, intent(out) :: stat
        res = a + b
        stat = 0 ! Success
    end subroutine

    subroutine bench_int_generic(n)
        integer, intent(in) :: n
        integer :: i
        type(result_type) :: res
        class(*), pointer :: ptr
        integer :: val, a, b
        integer, volatile :: sink

        a = 1
        b = 2

        do i = 1, n
            res = formerr_add_int_generic(a, b)
            if (.not. res%is_ok()) error stop "Fail"
            ptr => res%unwrap()
            select type (ptr)
            type is (integer)
                val = ptr
                ! Simple check to prevent DCE

                if (val < 0) stop

                sink = val
            end select
        end do
    end subroutine

    subroutine bench_int_specialized(n)
        integer, intent(in) :: n
        integer :: i
        type(result_type) :: res
        integer :: val, a, b
        integer, volatile :: sink

        a = 1
        b = 2

        do i = 1, n
            res = formerr_add_int_generic(a, b)
            if (.not. res%is_ok()) error stop "Fail"
            val = res%unwrap_int()

            if (val < 0) stop

            sink = val
        end do
    end subroutine

    subroutine bench_int_fully_specialized(n)
        integer, intent(in) :: n
        integer :: i
        integer :: val, a, b
        type(result_type) :: res
        integer, volatile :: sink

        a = 1
        b = 2

        do i = 1, n
            res = ok_int(a + b)
            if (.not. res%is_ok()) error stop "Fail"
            val = res%unwrap_int()

            if (val < 0) stop

            sink = val
        end do
    end subroutine

    function formerr_add_int_generic(a, b) result(res)
        integer, intent(in) :: a, b
        type(result_type) :: res
        res = ok(a + b)
    end function

    ! --- real Benchmarks ---
    subroutine bench_native_real(n)
        integer, intent(in) :: n
        integer :: i, stat
        real :: val, a, b
        real, volatile :: sink

        a = 1
        b = 2

        do i = 1, n
            call native_add_real(a, b, val, stat)
            if (stat /= 0) error stop "Fail"
            sink = val
        end do
    end subroutine

    subroutine native_add_real(a, b, res, stat)
        real, intent(in) :: a, b
        real, intent(out) :: res
        integer, intent(out) :: stat
        res = a + b
        stat = 0 ! Success
    end subroutine

    subroutine bench_real_generic(n)
        integer, intent(in) :: n
        integer :: i
        type(result_type) :: res
        class(*), pointer :: ptr
        real :: val, a, b
        real, volatile :: sink

        a = 1
        b = 2

        do i = 1, n
            res = formerr_add_real_generic(a, b)
            if (.not. res%is_ok()) error stop "Fail"
            ptr => res%unwrap()
            select type (ptr)
            type is (real)
                val = ptr
                ! Simple check to prevent DCE

                if (val < 0) stop

                sink = val
            end select
        end do
    end subroutine

    subroutine bench_real_specialized(n)
        integer, intent(in) :: n
        integer :: i
        type(result_type) :: res
        real :: val, a, b
        real, volatile :: sink

        a = 1
        b = 2

        do i = 1, n
            res = formerr_add_real_generic(a, b)
            if (.not. res%is_ok()) error stop "Fail"
            val = res%unwrap_real()

            if (val < 0) stop

            sink = val
        end do
    end subroutine

    subroutine bench_real_fully_specialized(n)
        integer, intent(in) :: n
        integer :: i
        real :: val, a, b
        type(result_type) :: res
        real, volatile :: sink

        a = 1
        b = 2

        do i = 1, n
            res = ok_real(a + b)
            if (.not. res%is_ok()) error stop "Fail"
            val = res%unwrap_real()

            if (val < 0) stop

            sink = val
        end do
    end subroutine

    function formerr_add_real_generic(a, b) result(res)
        real, intent(in) :: a, b
        type(result_type) :: res
        res = ok(a + b)
    end function

    ! --- complex Benchmarks ---
    subroutine bench_native_cpx(n)
        integer, intent(in) :: n
        integer :: i, stat
        complex :: val, a, b
        complex, volatile :: sink

        a = (1.0, 2.0)
        b = (1.0, 2.0)

        do i = 1, n
            call native_add_cpx(a, b, val, stat)
            if (stat /= 0) error stop "Fail"
            sink = val
        end do
    end subroutine

    subroutine native_add_cpx(a, b, res, stat)
        complex, intent(in) :: a, b
        complex, intent(out) :: res
        integer, intent(out) :: stat
        res = a + b
        stat = 0 ! Success
    end subroutine

    subroutine bench_cpx_generic(n)
        integer, intent(in) :: n
        integer :: i
        type(result_type) :: res
        class(*), pointer :: ptr
        complex :: val, a, b
        complex, volatile :: sink

        a = (1.0, 2.0)
        b = (1.0, 2.0)

        do i = 1, n
            res = formerr_add_cpx_generic(a, b)
            if (.not. res%is_ok()) error stop "Fail"
            ptr => res%unwrap()
            select type (ptr)
            type is (complex)
                val = ptr
                ! Simple check to prevent DCE

                if (real(val) < 0) stop

                sink = val
            end select
        end do
    end subroutine

    subroutine bench_cpx_specialized(n)
        integer, intent(in) :: n
        integer :: i
        type(result_type) :: res
        complex :: val, a, b
        complex, volatile :: sink

        a = (1.0, 2.0)
        b = (1.0, 2.0)

        do i = 1, n
            res = formerr_add_cpx_generic(a, b)
            if (.not. res%is_ok()) error stop "Fail"
            val = res%unwrap_cpx()

            if (real(val) < 0) stop

            sink = val
        end do
    end subroutine

    subroutine bench_cpx_fully_specialized(n)
        integer, intent(in) :: n
        integer :: i
        complex :: val, a, b
        type(result_type) :: res
        complex, volatile :: sink

        a = (1.0, 2.0)
        b = (1.0, 2.0)

        do i = 1, n
            res = ok_cpx(a + b)
            if (.not. res%is_ok()) error stop "Fail"
            val = res%unwrap_cpx()

            if (real(val) < 0) stop

            sink = val
        end do
    end subroutine

    function formerr_add_cpx_generic(a, b) result(res)
        complex, intent(in) :: a, b
        type(result_type) :: res
        res = ok(a + b)
    end function

    ! --- real(8) Benchmarks ---
    subroutine bench_native_r8(n)
        integer, intent(in) :: n
        integer :: i, stat
        real(8) :: val, a, b
        real(8), volatile :: sink

        a = 1
        b = 2

        do i = 1, n
            call native_add_r8(a, b, val, stat)
            if (stat /= 0) error stop "Fail"
            sink = val
        end do
    end subroutine

    subroutine native_add_r8(a, b, res, stat)
        real(8), intent(in) :: a, b
        real(8), intent(out) :: res
        integer, intent(out) :: stat
        res = a + b
        stat = 0 ! Success
    end subroutine

    subroutine bench_r8_generic(n)
        integer, intent(in) :: n
        integer :: i
        type(result_type) :: res
        class(*), pointer :: ptr
        real(8) :: val, a, b
        real(8), volatile :: sink

        a = 1
        b = 2

        do i = 1, n
            res = formerr_add_r8_generic(a, b)
            if (.not. res%is_ok()) error stop "Fail"
            ptr => res%unwrap()
            select type (ptr)
            type is (real(8))
                val = ptr
                ! Simple check to prevent DCE

                if (val < 0) stop

                sink = val
            end select
        end do
    end subroutine

    subroutine bench_r8_specialized(n)
        integer, intent(in) :: n
        integer :: i
        type(result_type) :: res
        real(8) :: val, a, b
        real(8), volatile :: sink

        a = 1
        b = 2

        do i = 1, n
            res = formerr_add_r8_generic(a, b)
            if (.not. res%is_ok()) error stop "Fail"
            val = res%unwrap_r8()

            if (val < 0) stop

            sink = val
        end do
    end subroutine

    subroutine bench_r8_fully_specialized(n)
        integer, intent(in) :: n
        integer :: i
        real(8) :: val, a, b
        type(result_type) :: res
        real(8), volatile :: sink

        a = 1
        b = 2

        do i = 1, n
            res = ok_r8(a + b)
            if (.not. res%is_ok()) error stop "Fail"
            val = res%unwrap_r8()

            if (val < 0) stop

            sink = val
        end do
    end subroutine

    function formerr_add_r8_generic(a, b) result(res)
        real(8), intent(in) :: a, b
        type(result_type) :: res
        res = ok(a + b)
    end function

    ! --- real(16) Benchmarks ---
    subroutine bench_native_r16(n)
        integer, intent(in) :: n
        integer :: i, stat
        real(16) :: val, a, b
        real(16), volatile :: sink

        a = 1
        b = 2

        do i = 1, n
            call native_add_r16(a, b, val, stat)
            if (stat /= 0) error stop "Fail"
            sink = val
        end do
    end subroutine

    subroutine native_add_r16(a, b, res, stat)
        real(16), intent(in) :: a, b
        real(16), intent(out) :: res
        integer, intent(out) :: stat
        res = a + b
        stat = 0 ! Success
    end subroutine

    subroutine bench_r16_generic(n)
        integer, intent(in) :: n
        integer :: i
        type(result_type) :: res
        class(*), pointer :: ptr
        real(16) :: val, a, b
        real(16), volatile :: sink

        a = 1
        b = 2

        do i = 1, n
            res = formerr_add_r16_generic(a, b)
            if (.not. res%is_ok()) error stop "Fail"
            ptr => res%unwrap()
            select type (ptr)
            type is (real(16))
                val = ptr
                ! Simple check to prevent DCE

                if (val < 0) stop

                sink = val
            end select
        end do
    end subroutine

    subroutine bench_r16_specialized(n)
        integer, intent(in) :: n
        integer :: i
        type(result_type) :: res
        real(16) :: val, a, b
        real(16), volatile :: sink

        a = 1
        b = 2

        do i = 1, n
            res = formerr_add_r16_generic(a, b)
            if (.not. res%is_ok()) error stop "Fail"
            val = res%unwrap_r16()

            if (val < 0) stop

            sink = val
        end do
    end subroutine

    subroutine bench_r16_fully_specialized(n)
        integer, intent(in) :: n
        integer :: i
        real(16) :: val, a, b
        type(result_type) :: res
        real(16), volatile :: sink

        a = 1
        b = 2

        do i = 1, n
            res = ok_r16(a + b)
            if (.not. res%is_ok()) error stop "Fail"
            val = res%unwrap_r16()

            if (val < 0) stop

            sink = val
        end do
    end subroutine

    function formerr_add_r16_generic(a, b) result(res)
        real(16), intent(in) :: a, b
        type(result_type) :: res
        res = ok(a + b)
    end function

    ! --- real(real32) Benchmarks ---
    subroutine bench_native_r32(n)
        integer, intent(in) :: n
        integer :: i, stat
        real(real32) :: val, a, b
        real(real32), volatile :: sink

        a = 1
        b = 2

        do i = 1, n
            call native_add_r32(a, b, val, stat)
            if (stat /= 0) error stop "Fail"
            sink = val
        end do
    end subroutine

    subroutine native_add_r32(a, b, res, stat)
        real(real32), intent(in) :: a, b
        real(real32), intent(out) :: res
        integer, intent(out) :: stat
        res = a + b
        stat = 0 ! Success
    end subroutine

    subroutine bench_r32_generic(n)
        integer, intent(in) :: n
        integer :: i
        type(result_type) :: res
        class(*), pointer :: ptr
        real(real32) :: val, a, b
        real(real32), volatile :: sink

        a = 1
        b = 2

        do i = 1, n
            res = formerr_add_r32_generic(a, b)
            if (.not. res%is_ok()) error stop "Fail"
            ptr => res%unwrap()
            select type (ptr)
            type is (real(real32))
                val = ptr
                ! Simple check to prevent DCE

                if (val < 0) stop

                sink = val
            end select
        end do
    end subroutine

    subroutine bench_r32_specialized(n)
        integer, intent(in) :: n
        integer :: i
        type(result_type) :: res
        real(real32) :: val, a, b
        real(real32), volatile :: sink

        a = 1
        b = 2

        do i = 1, n
            res = formerr_add_r32_generic(a, b)
            if (.not. res%is_ok()) error stop "Fail"
            val = res%unwrap_r32()

            if (val < 0) stop

            sink = val
        end do
    end subroutine

    subroutine bench_r32_fully_specialized(n)
        integer, intent(in) :: n
        integer :: i
        real(real32) :: val, a, b
        type(result_type) :: res
        real(real32), volatile :: sink

        a = 1
        b = 2

        do i = 1, n
            res = ok_r32(a + b)
            if (.not. res%is_ok()) error stop "Fail"
            val = res%unwrap_r32()

            if (val < 0) stop

            sink = val
        end do
    end subroutine

    function formerr_add_r32_generic(a, b) result(res)
        real(real32), intent(in) :: a, b
        type(result_type) :: res
        res = ok(a + b)
    end function

    ! --- real(real64) Benchmarks ---
    subroutine bench_native_r64(n)
        integer, intent(in) :: n
        integer :: i, stat
        real(real64) :: val, a, b
        real(real64), volatile :: sink

        a = 1
        b = 2

        do i = 1, n
            call native_add_r64(a, b, val, stat)
            if (stat /= 0) error stop "Fail"
            sink = val
        end do
    end subroutine

    subroutine native_add_r64(a, b, res, stat)
        real(real64), intent(in) :: a, b
        real(real64), intent(out) :: res
        integer, intent(out) :: stat
        res = a + b
        stat = 0 ! Success
    end subroutine

    subroutine bench_r64_generic(n)
        integer, intent(in) :: n
        integer :: i
        type(result_type) :: res
        class(*), pointer :: ptr
        real(real64) :: val, a, b
        real(real64), volatile :: sink

        a = 1
        b = 2

        do i = 1, n
            res = formerr_add_r64_generic(a, b)
            if (.not. res%is_ok()) error stop "Fail"
            ptr => res%unwrap()
            select type (ptr)
            type is (real(real64))
                val = ptr
                ! Simple check to prevent DCE

                if (val < 0) stop

                sink = val
            end select
        end do
    end subroutine

    subroutine bench_r64_specialized(n)
        integer, intent(in) :: n
        integer :: i
        type(result_type) :: res
        real(real64) :: val, a, b
        real(real64), volatile :: sink

        a = 1
        b = 2

        do i = 1, n
            res = formerr_add_r64_generic(a, b)
            if (.not. res%is_ok()) error stop "Fail"
            val = res%unwrap_r64()

            if (val < 0) stop

            sink = val
        end do
    end subroutine

    subroutine bench_r64_fully_specialized(n)
        integer, intent(in) :: n
        integer :: i
        real(real64) :: val, a, b
        type(result_type) :: res
        real(real64), volatile :: sink

        a = 1
        b = 2

        do i = 1, n
            res = ok_r64(a + b)
            if (.not. res%is_ok()) error stop "Fail"
            val = res%unwrap_r64()

            if (val < 0) stop

            sink = val
        end do
    end subroutine

    function formerr_add_r64_generic(a, b) result(res)
        real(real64), intent(in) :: a, b
        type(result_type) :: res
        res = ok(a + b)
    end function

    ! --- real(real128) Benchmarks ---
    subroutine bench_native_r128(n)
        integer, intent(in) :: n
        integer :: i, stat
        real(real128) :: val, a, b
        real(real128), volatile :: sink

        a = 1
        b = 2

        do i = 1, n
            call native_add_r128(a, b, val, stat)
            if (stat /= 0) error stop "Fail"
            sink = val
        end do
    end subroutine

    subroutine native_add_r128(a, b, res, stat)
        real(real128), intent(in) :: a, b
        real(real128), intent(out) :: res
        integer, intent(out) :: stat
        res = a + b
        stat = 0 ! Success
    end subroutine

    subroutine bench_r128_generic(n)
        integer, intent(in) :: n
        integer :: i
        type(result_type) :: res
        class(*), pointer :: ptr
        real(real128) :: val, a, b
        real(real128), volatile :: sink

        a = 1
        b = 2

        do i = 1, n
            res = formerr_add_r128_generic(a, b)
            if (.not. res%is_ok()) error stop "Fail"
            ptr => res%unwrap()
            select type (ptr)
            type is (real(real128))
                val = ptr
                ! Simple check to prevent DCE

                if (val < 0) stop

                sink = val
            end select
        end do
    end subroutine

    subroutine bench_r128_specialized(n)
        integer, intent(in) :: n
        integer :: i
        type(result_type) :: res
        real(real128) :: val, a, b
        real(real128), volatile :: sink

        a = 1
        b = 2

        do i = 1, n
            res = formerr_add_r128_generic(a, b)
            if (.not. res%is_ok()) error stop "Fail"
            val = res%unwrap_r128()

            if (val < 0) stop

            sink = val
        end do
    end subroutine

    subroutine bench_r128_fully_specialized(n)
        integer, intent(in) :: n
        integer :: i
        real(real128) :: val, a, b
        type(result_type) :: res
        real(real128), volatile :: sink

        a = 1
        b = 2

        do i = 1, n
            res = ok_r128(a + b)
            if (.not. res%is_ok()) error stop "Fail"
            val = res%unwrap_r128()

            if (val < 0) stop

            sink = val
        end do
    end subroutine

    function formerr_add_r128_generic(a, b) result(res)
        real(real128), intent(in) :: a, b
        type(result_type) :: res
        res = ok(a + b)
    end function

    ! --- integer(int8) Benchmarks ---
    subroutine bench_native_i8(n)
        integer, intent(in) :: n
        integer :: i, stat
        integer(int8) :: val, a, b
        integer(int8), volatile :: sink

        a = 1
        b = 2

        do i = 1, n
            call native_add_i8(a, b, val, stat)
            if (stat /= 0) error stop "Fail"
            sink = val
        end do
    end subroutine

    subroutine native_add_i8(a, b, res, stat)
        integer(int8), intent(in) :: a, b
        integer(int8), intent(out) :: res
        integer, intent(out) :: stat
        res = a + b
        stat = 0 ! Success
    end subroutine

    subroutine bench_i8_generic(n)
        integer, intent(in) :: n
        integer :: i
        type(result_type) :: res
        class(*), pointer :: ptr
        integer(int8) :: val, a, b
        integer(int8), volatile :: sink

        a = 1
        b = 2

        do i = 1, n
            res = formerr_add_i8_generic(a, b)
            if (.not. res%is_ok()) error stop "Fail"
            ptr => res%unwrap()
            select type (ptr)
            type is (integer(int8))
                val = ptr
                ! Simple check to prevent DCE

                if (val < 0) stop

                sink = val
            end select
        end do
    end subroutine

    subroutine bench_i8_specialized(n)
        integer, intent(in) :: n
        integer :: i
        type(result_type) :: res
        integer(int8) :: val, a, b
        integer(int8), volatile :: sink

        a = 1
        b = 2

        do i = 1, n
            res = formerr_add_i8_generic(a, b)
            if (.not. res%is_ok()) error stop "Fail"
            val = res%unwrap_i8()

            if (val < 0) stop

            sink = val
        end do
    end subroutine

    subroutine bench_i8_fully_specialized(n)
        integer, intent(in) :: n
        integer :: i
        integer(int8) :: val, a, b
        type(result_type) :: res
        integer(int8), volatile :: sink

        a = 1
        b = 2

        do i = 1, n
            res = ok_i8(a + b)
            if (.not. res%is_ok()) error stop "Fail"
            val = res%unwrap_i8()

            if (val < 0) stop

            sink = val
        end do
    end subroutine

    function formerr_add_i8_generic(a, b) result(res)
        integer(int8), intent(in) :: a, b
        type(result_type) :: res
        res = ok(a + b)
    end function

    ! --- integer(int16) Benchmarks ---
    subroutine bench_native_i16(n)
        integer, intent(in) :: n
        integer :: i, stat
        integer(int16) :: val, a, b
        integer(int16), volatile :: sink

        a = 1
        b = 2

        do i = 1, n
            call native_add_i16(a, b, val, stat)
            if (stat /= 0) error stop "Fail"
            sink = val
        end do
    end subroutine

    subroutine native_add_i16(a, b, res, stat)
        integer(int16), intent(in) :: a, b
        integer(int16), intent(out) :: res
        integer, intent(out) :: stat
        res = a + b
        stat = 0 ! Success
    end subroutine

    subroutine bench_i16_generic(n)
        integer, intent(in) :: n
        integer :: i
        type(result_type) :: res
        class(*), pointer :: ptr
        integer(int16) :: val, a, b
        integer(int16), volatile :: sink

        a = 1
        b = 2

        do i = 1, n
            res = formerr_add_i16_generic(a, b)
            if (.not. res%is_ok()) error stop "Fail"
            ptr => res%unwrap()
            select type (ptr)
            type is (integer(int16))
                val = ptr
                ! Simple check to prevent DCE

                if (val < 0) stop

                sink = val
            end select
        end do
    end subroutine

    subroutine bench_i16_specialized(n)
        integer, intent(in) :: n
        integer :: i
        type(result_type) :: res
        integer(int16) :: val, a, b
        integer(int16), volatile :: sink

        a = 1
        b = 2

        do i = 1, n
            res = formerr_add_i16_generic(a, b)
            if (.not. res%is_ok()) error stop "Fail"
            val = res%unwrap_i16()

            if (val < 0) stop

            sink = val
        end do
    end subroutine

    subroutine bench_i16_fully_specialized(n)
        integer, intent(in) :: n
        integer :: i
        integer(int16) :: val, a, b
        type(result_type) :: res
        integer(int16), volatile :: sink

        a = 1
        b = 2

        do i = 1, n
            res = ok_i16(a + b)
            if (.not. res%is_ok()) error stop "Fail"
            val = res%unwrap_i16()

            if (val < 0) stop

            sink = val
        end do
    end subroutine

    function formerr_add_i16_generic(a, b) result(res)
        integer(int16), intent(in) :: a, b
        type(result_type) :: res
        res = ok(a + b)
    end function

    ! --- integer(int32) Benchmarks ---
    subroutine bench_native_i32(n)
        integer, intent(in) :: n
        integer :: i, stat
        integer(int32) :: val, a, b
        integer(int32), volatile :: sink

        a = 1
        b = 2

        do i = 1, n
            call native_add_i32(a, b, val, stat)
            if (stat /= 0) error stop "Fail"
            sink = val
        end do
    end subroutine

    subroutine native_add_i32(a, b, res, stat)
        integer(int32), intent(in) :: a, b
        integer(int32), intent(out) :: res
        integer, intent(out) :: stat
        res = a + b
        stat = 0 ! Success
    end subroutine

    subroutine bench_i32_generic(n)
        integer, intent(in) :: n
        integer :: i
        type(result_type) :: res
        class(*), pointer :: ptr
        integer(int32) :: val, a, b
        integer(int32), volatile :: sink

        a = 1
        b = 2

        do i = 1, n
            res = formerr_add_i32_generic(a, b)
            if (.not. res%is_ok()) error stop "Fail"
            ptr => res%unwrap()
            select type (ptr)
            type is (integer(int32))
                val = ptr
                ! Simple check to prevent DCE

                if (val < 0) stop

                sink = val
            end select
        end do
    end subroutine

    subroutine bench_i32_specialized(n)
        integer, intent(in) :: n
        integer :: i
        type(result_type) :: res
        integer(int32) :: val, a, b
        integer(int32), volatile :: sink

        a = 1
        b = 2

        do i = 1, n
            res = formerr_add_i32_generic(a, b)
            if (.not. res%is_ok()) error stop "Fail"
            val = res%unwrap_i32()

            if (val < 0) stop

            sink = val
        end do
    end subroutine

    subroutine bench_i32_fully_specialized(n)
        integer, intent(in) :: n
        integer :: i
        integer(int32) :: val, a, b
        type(result_type) :: res
        integer(int32), volatile :: sink

        a = 1
        b = 2

        do i = 1, n
            res = ok_i32(a + b)
            if (.not. res%is_ok()) error stop "Fail"
            val = res%unwrap_i32()

            if (val < 0) stop

            sink = val
        end do
    end subroutine

    function formerr_add_i32_generic(a, b) result(res)
        integer(int32), intent(in) :: a, b
        type(result_type) :: res
        res = ok(a + b)
    end function

    ! --- integer(int64) Benchmarks ---
    subroutine bench_native_i64(n)
        integer, intent(in) :: n
        integer :: i, stat
        integer(int64) :: val, a, b
        integer(int64), volatile :: sink

        a = 1
        b = 2

        do i = 1, n
            call native_add_i64(a, b, val, stat)
            if (stat /= 0) error stop "Fail"
            sink = val
        end do
    end subroutine

    subroutine native_add_i64(a, b, res, stat)
        integer(int64), intent(in) :: a, b
        integer(int64), intent(out) :: res
        integer, intent(out) :: stat
        res = a + b
        stat = 0 ! Success
    end subroutine

    subroutine bench_i64_generic(n)
        integer, intent(in) :: n
        integer :: i
        type(result_type) :: res
        class(*), pointer :: ptr
        integer(int64) :: val, a, b
        integer(int64), volatile :: sink

        a = 1
        b = 2

        do i = 1, n
            res = formerr_add_i64_generic(a, b)
            if (.not. res%is_ok()) error stop "Fail"
            ptr => res%unwrap()
            select type (ptr)
            type is (integer(int64))
                val = ptr
                ! Simple check to prevent DCE

                if (val < 0) stop

                sink = val
            end select
        end do
    end subroutine

    subroutine bench_i64_specialized(n)
        integer, intent(in) :: n
        integer :: i
        type(result_type) :: res
        integer(int64) :: val, a, b
        integer(int64), volatile :: sink

        a = 1
        b = 2

        do i = 1, n
            res = formerr_add_i64_generic(a, b)
            if (.not. res%is_ok()) error stop "Fail"
            val = res%unwrap_i64()

            if (val < 0) stop

            sink = val
        end do
    end subroutine

    subroutine bench_i64_fully_specialized(n)
        integer, intent(in) :: n
        integer :: i
        integer(int64) :: val, a, b
        type(result_type) :: res
        integer(int64), volatile :: sink

        a = 1
        b = 2

        do i = 1, n
            res = ok_i64(a + b)
            if (.not. res%is_ok()) error stop "Fail"
            val = res%unwrap_i64()

            if (val < 0) stop

            sink = val
        end do
    end subroutine

    function formerr_add_i64_generic(a, b) result(res)
        integer(int64), intent(in) :: a, b
        type(result_type) :: res
        res = ok(a + b)
    end function

    ! --- integer(int128) Benchmarks ---
    subroutine bench_native_i128(n)
        integer, intent(in) :: n
        integer :: i, stat
        integer(int128) :: val, a, b
        integer(int128), volatile :: sink

        a = 1
        b = 2

        do i = 1, n
            call native_add_i128(a, b, val, stat)
            if (stat /= 0) error stop "Fail"
            sink = val
        end do
    end subroutine

    subroutine native_add_i128(a, b, res, stat)
        integer(int128), intent(in) :: a, b
        integer(int128), intent(out) :: res
        integer, intent(out) :: stat
        res = a + b
        stat = 0 ! Success
    end subroutine

    subroutine bench_i128_generic(n)
        integer, intent(in) :: n
        integer :: i
        type(result_type) :: res
        class(*), pointer :: ptr
        integer(int128) :: val, a, b
        integer(int128), volatile :: sink

        a = 1
        b = 2

        do i = 1, n
            res = formerr_add_i128_generic(a, b)
            if (.not. res%is_ok()) error stop "Fail"
            ptr => res%unwrap()
            select type (ptr)
            type is (integer(int128))
                val = ptr
                ! Simple check to prevent DCE

                if (val < 0) stop

                sink = val
            end select
        end do
    end subroutine

    subroutine bench_i128_specialized(n)
        integer, intent(in) :: n
        integer :: i
        type(result_type) :: res
        integer(int128) :: val, a, b
        integer(int128), volatile :: sink

        a = 1
        b = 2

        do i = 1, n
            res = formerr_add_i128_generic(a, b)
            if (.not. res%is_ok()) error stop "Fail"
            val = res%unwrap_i128()

            if (val < 0) stop

            sink = val
        end do
    end subroutine

    subroutine bench_i128_fully_specialized(n)
        integer, intent(in) :: n
        integer :: i
        integer(int128) :: val, a, b
        type(result_type) :: res
        integer(int128), volatile :: sink

        a = 1
        b = 2

        do i = 1, n
            res = ok_i128(a + b)
            if (.not. res%is_ok()) error stop "Fail"
            val = res%unwrap_i128()

            if (val < 0) stop

            sink = val
        end do
    end subroutine

    function formerr_add_i128_generic(a, b) result(res)
        integer(int128), intent(in) :: a, b
        type(result_type) :: res
        res = ok(a + b)
    end function

    ! --- complex(real8) Benchmarks ---
    subroutine bench_native_c8(n)
        integer, intent(in) :: n
        integer :: i, stat
        complex(real8) :: val, a, b
        complex(real8), volatile :: sink

        a = (1.0_real8, 2.0_real8)
        b = (1.0_real8, 2.0_real8)

        do i = 1, n
            call native_add_c8(a, b, val, stat)
            if (stat /= 0) error stop "Fail"
            sink = val
        end do
    end subroutine

    subroutine native_add_c8(a, b, res, stat)
        complex(real8), intent(in) :: a, b
        complex(real8), intent(out) :: res
        integer, intent(out) :: stat
        res = a + b
        stat = 0 ! Success
    end subroutine

    subroutine bench_c8_generic(n)
        integer, intent(in) :: n
        integer :: i
        type(result_type) :: res
        class(*), pointer :: ptr
        complex(real8) :: val, a, b
        complex(real8), volatile :: sink

        a = (1.0_real8, 2.0_real8)
        b = (1.0_real8, 2.0_real8)

        do i = 1, n
            res = formerr_add_c8_generic(a, b)
            if (.not. res%is_ok()) error stop "Fail"
            ptr => res%unwrap()
            select type (ptr)
            type is (complex(real8))
                val = ptr
                ! Simple check to prevent DCE

                if (real(val) < 0) stop

                sink = val
            end select
        end do
    end subroutine

    subroutine bench_c8_specialized(n)
        integer, intent(in) :: n
        integer :: i
        type(result_type) :: res
        complex(real8) :: val, a, b
        complex(real8), volatile :: sink

        a = (1.0_real8, 2.0_real8)
        b = (1.0_real8, 2.0_real8)

        do i = 1, n
            res = formerr_add_c8_generic(a, b)
            if (.not. res%is_ok()) error stop "Fail"
            val = res%unwrap_c8()

            if (real(val) < 0) stop

            sink = val
        end do
    end subroutine

    subroutine bench_c8_fully_specialized(n)
        integer, intent(in) :: n
        integer :: i
        complex(real8) :: val, a, b
        type(result_type) :: res
        complex(real8), volatile :: sink

        a = (1.0_real8, 2.0_real8)
        b = (1.0_real8, 2.0_real8)

        do i = 1, n
            res = ok_c8(a + b)
            if (.not. res%is_ok()) error stop "Fail"
            val = res%unwrap_c8()

            if (real(val) < 0) stop

            sink = val
        end do
    end subroutine

    function formerr_add_c8_generic(a, b) result(res)
        complex(real8), intent(in) :: a, b
        type(result_type) :: res
        res = ok(a + b)
    end function

    ! --- complex(real16) Benchmarks ---
    subroutine bench_native_c16(n)
        integer, intent(in) :: n
        integer :: i, stat
        complex(real16) :: val, a, b
        complex(real16), volatile :: sink

        a = (1.0_real16, 2.0_real16)
        b = (1.0_real16, 2.0_real16)

        do i = 1, n
            call native_add_c16(a, b, val, stat)
            if (stat /= 0) error stop "Fail"
            sink = val
        end do
    end subroutine

    subroutine native_add_c16(a, b, res, stat)
        complex(real16), intent(in) :: a, b
        complex(real16), intent(out) :: res
        integer, intent(out) :: stat
        res = a + b
        stat = 0 ! Success
    end subroutine

    subroutine bench_c16_generic(n)
        integer, intent(in) :: n
        integer :: i
        type(result_type) :: res
        class(*), pointer :: ptr
        complex(real16) :: val, a, b
        complex(real16), volatile :: sink

        a = (1.0_real16, 2.0_real16)
        b = (1.0_real16, 2.0_real16)

        do i = 1, n
            res = formerr_add_c16_generic(a, b)
            if (.not. res%is_ok()) error stop "Fail"
            ptr => res%unwrap()
            select type (ptr)
            type is (complex(real16))
                val = ptr
                ! Simple check to prevent DCE

                if (real(val) < 0) stop

                sink = val
            end select
        end do
    end subroutine

    subroutine bench_c16_specialized(n)
        integer, intent(in) :: n
        integer :: i
        type(result_type) :: res
        complex(real16) :: val, a, b
        complex(real16), volatile :: sink

        a = (1.0_real16, 2.0_real16)
        b = (1.0_real16, 2.0_real16)

        do i = 1, n
            res = formerr_add_c16_generic(a, b)
            if (.not. res%is_ok()) error stop "Fail"
            val = res%unwrap_c16()

            if (real(val) < 0) stop

            sink = val
        end do
    end subroutine

    subroutine bench_c16_fully_specialized(n)
        integer, intent(in) :: n
        integer :: i
        complex(real16) :: val, a, b
        type(result_type) :: res
        complex(real16), volatile :: sink

        a = (1.0_real16, 2.0_real16)
        b = (1.0_real16, 2.0_real16)

        do i = 1, n
            res = ok_c16(a + b)
            if (.not. res%is_ok()) error stop "Fail"
            val = res%unwrap_c16()

            if (real(val) < 0) stop

            sink = val
        end do
    end subroutine

    function formerr_add_c16_generic(a, b) result(res)
        complex(real16), intent(in) :: a, b
        type(result_type) :: res
        res = ok(a + b)
    end function

    ! --- complex(real32) Benchmarks ---
    subroutine bench_native_c32(n)
        integer, intent(in) :: n
        integer :: i, stat
        complex(real32) :: val, a, b
        complex(real32), volatile :: sink

        a = (1.0_real32, 2.0_real32)
        b = (1.0_real32, 2.0_real32)

        do i = 1, n
            call native_add_c32(a, b, val, stat)
            if (stat /= 0) error stop "Fail"
            sink = val
        end do
    end subroutine

    subroutine native_add_c32(a, b, res, stat)
        complex(real32), intent(in) :: a, b
        complex(real32), intent(out) :: res
        integer, intent(out) :: stat
        res = a + b
        stat = 0 ! Success
    end subroutine

    subroutine bench_c32_generic(n)
        integer, intent(in) :: n
        integer :: i
        type(result_type) :: res
        class(*), pointer :: ptr
        complex(real32) :: val, a, b
        complex(real32), volatile :: sink

        a = (1.0_real32, 2.0_real32)
        b = (1.0_real32, 2.0_real32)

        do i = 1, n
            res = formerr_add_c32_generic(a, b)
            if (.not. res%is_ok()) error stop "Fail"
            ptr => res%unwrap()
            select type (ptr)
            type is (complex(real32))
                val = ptr
                ! Simple check to prevent DCE

                if (real(val) < 0) stop

                sink = val
            end select
        end do
    end subroutine

    subroutine bench_c32_specialized(n)
        integer, intent(in) :: n
        integer :: i
        type(result_type) :: res
        complex(real32) :: val, a, b
        complex(real32), volatile :: sink

        a = (1.0_real32, 2.0_real32)
        b = (1.0_real32, 2.0_real32)

        do i = 1, n
            res = formerr_add_c32_generic(a, b)
            if (.not. res%is_ok()) error stop "Fail"
            val = res%unwrap_c32()

            if (real(val) < 0) stop

            sink = val
        end do
    end subroutine

    subroutine bench_c32_fully_specialized(n)
        integer, intent(in) :: n
        integer :: i
        complex(real32) :: val, a, b
        type(result_type) :: res
        complex(real32), volatile :: sink

        a = (1.0_real32, 2.0_real32)
        b = (1.0_real32, 2.0_real32)

        do i = 1, n
            res = ok_c32(a + b)
            if (.not. res%is_ok()) error stop "Fail"
            val = res%unwrap_c32()

            if (real(val) < 0) stop

            sink = val
        end do
    end subroutine

    function formerr_add_c32_generic(a, b) result(res)
        complex(real32), intent(in) :: a, b
        type(result_type) :: res
        res = ok(a + b)
    end function

    ! --- complex(real64) Benchmarks ---
    subroutine bench_native_c64(n)
        integer, intent(in) :: n
        integer :: i, stat
        complex(real64) :: val, a, b
        complex(real64), volatile :: sink

        a = (1.0_real64, 2.0_real64)
        b = (1.0_real64, 2.0_real64)

        do i = 1, n
            call native_add_c64(a, b, val, stat)
            if (stat /= 0) error stop "Fail"
            sink = val
        end do
    end subroutine

    subroutine native_add_c64(a, b, res, stat)
        complex(real64), intent(in) :: a, b
        complex(real64), intent(out) :: res
        integer, intent(out) :: stat
        res = a + b
        stat = 0 ! Success
    end subroutine

    subroutine bench_c64_generic(n)
        integer, intent(in) :: n
        integer :: i
        type(result_type) :: res
        class(*), pointer :: ptr
        complex(real64) :: val, a, b
        complex(real64), volatile :: sink

        a = (1.0_real64, 2.0_real64)
        b = (1.0_real64, 2.0_real64)

        do i = 1, n
            res = formerr_add_c64_generic(a, b)
            if (.not. res%is_ok()) error stop "Fail"
            ptr => res%unwrap()
            select type (ptr)
            type is (complex(real64))
                val = ptr
                ! Simple check to prevent DCE

                if (real(val) < 0) stop

                sink = val
            end select
        end do
    end subroutine

    subroutine bench_c64_specialized(n)
        integer, intent(in) :: n
        integer :: i
        type(result_type) :: res
        complex(real64) :: val, a, b
        complex(real64), volatile :: sink

        a = (1.0_real64, 2.0_real64)
        b = (1.0_real64, 2.0_real64)

        do i = 1, n
            res = formerr_add_c64_generic(a, b)
            if (.not. res%is_ok()) error stop "Fail"
            val = res%unwrap_c64()

            if (real(val) < 0) stop

            sink = val
        end do
    end subroutine

    subroutine bench_c64_fully_specialized(n)
        integer, intent(in) :: n
        integer :: i
        complex(real64) :: val, a, b
        type(result_type) :: res
        complex(real64), volatile :: sink

        a = (1.0_real64, 2.0_real64)
        b = (1.0_real64, 2.0_real64)

        do i = 1, n
            res = ok_c64(a + b)
            if (.not. res%is_ok()) error stop "Fail"
            val = res%unwrap_c64()

            if (real(val) < 0) stop

            sink = val
        end do
    end subroutine

    function formerr_add_c64_generic(a, b) result(res)
        complex(real64), intent(in) :: a, b
        type(result_type) :: res
        res = ok(a + b)
    end function

    ! --- complex(real128) Benchmarks ---
    subroutine bench_native_c128(n)
        integer, intent(in) :: n
        integer :: i, stat
        complex(real128) :: val, a, b
        complex(real128), volatile :: sink

        a = (1.0_real128, 2.0_real128)
        b = (1.0_real128, 2.0_real128)

        do i = 1, n
            call native_add_c128(a, b, val, stat)
            if (stat /= 0) error stop "Fail"
            sink = val
        end do
    end subroutine

    subroutine native_add_c128(a, b, res, stat)
        complex(real128), intent(in) :: a, b
        complex(real128), intent(out) :: res
        integer, intent(out) :: stat
        res = a + b
        stat = 0 ! Success
    end subroutine

    subroutine bench_c128_generic(n)
        integer, intent(in) :: n
        integer :: i
        type(result_type) :: res
        class(*), pointer :: ptr
        complex(real128) :: val, a, b
        complex(real128), volatile :: sink

        a = (1.0_real128, 2.0_real128)
        b = (1.0_real128, 2.0_real128)

        do i = 1, n
            res = formerr_add_c128_generic(a, b)
            if (.not. res%is_ok()) error stop "Fail"
            ptr => res%unwrap()
            select type (ptr)
            type is (complex(real128))
                val = ptr
                ! Simple check to prevent DCE

                if (real(val) < 0) stop

                sink = val
            end select
        end do
    end subroutine

    subroutine bench_c128_specialized(n)
        integer, intent(in) :: n
        integer :: i
        type(result_type) :: res
        complex(real128) :: val, a, b
        complex(real128), volatile :: sink

        a = (1.0_real128, 2.0_real128)
        b = (1.0_real128, 2.0_real128)

        do i = 1, n
            res = formerr_add_c128_generic(a, b)
            if (.not. res%is_ok()) error stop "Fail"
            val = res%unwrap_c128()

            if (real(val) < 0) stop

            sink = val
        end do
    end subroutine

    subroutine bench_c128_fully_specialized(n)
        integer, intent(in) :: n
        integer :: i
        complex(real128) :: val, a, b
        type(result_type) :: res
        complex(real128), volatile :: sink

        a = (1.0_real128, 2.0_real128)
        b = (1.0_real128, 2.0_real128)

        do i = 1, n
            res = ok_c128(a + b)
            if (.not. res%is_ok()) error stop "Fail"
            val = res%unwrap_c128()

            if (real(val) < 0) stop

            sink = val
        end do
    end subroutine

    function formerr_add_c128_generic(a, b) result(res)
        complex(real128), intent(in) :: a, b
        type(result_type) :: res
        res = ok(a + b)
    end function

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
