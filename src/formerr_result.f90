module formerr_result
    use formerr_either
    use stdlib_error, only: check
    use, intrinsic :: iso_fortran_env, only: real32, real64, real128, int8, int16, int32, int64
    implicit none
    private

    integer, parameter :: real8 = selected_real_kind(2, 2)
    integer, parameter :: real16 = selected_real_kind(4, 4)
    integer, parameter :: int128 = selected_int_kind(38)

    logical, parameter :: DO_CHECKS = .false.

    public :: result_type, ok, err, is_ok, is_err, unwrap, unwrap_err, unwrap_or
    public :: ok_move, err_move, unwrap_move_to_err, unsafe_unwrap_move

    public :: ok_int, err_int

    public :: ok_real, err_real

    public :: ok_log, err_log

    public :: ok_cpx, err_cpx

    public :: ok_r8, err_r8

    public :: ok_r16, err_r16

    public :: ok_r32, err_r32

    public :: ok_r64, err_r64

    public :: ok_r128, err_r128

    public :: ok_i8, err_i8

    public :: ok_i16, err_i16

    public :: ok_i32, err_i32

    public :: ok_i64, err_i64

    public :: ok_i128, err_i128

    public :: ok_c8, err_c8

    public :: ok_c16, err_c16

    public :: ok_c32, err_c32

    public :: ok_c64, err_c64

    public :: ok_c128, err_c128

    type, extends(either) :: result_type
    contains
        procedure :: is_ok
        procedure :: is_err
        procedure :: unwrap
        procedure :: unwrap_err
        procedure :: unwrap_or
        procedure :: unwrap_move_to_err
        procedure :: unsafe_unwrap_move

        procedure :: unwrap_int
        procedure :: unwrap_err_int
        procedure :: unwrap_or_int

        procedure :: unwrap_real
        procedure :: unwrap_err_real
        procedure :: unwrap_or_real

        procedure :: unwrap_log
        procedure :: unwrap_err_log
        procedure :: unwrap_or_log

        procedure :: unwrap_cpx
        procedure :: unwrap_err_cpx
        procedure :: unwrap_or_cpx

        procedure :: unwrap_r8
        procedure :: unwrap_err_r8
        procedure :: unwrap_or_r8

        procedure :: unwrap_r16
        procedure :: unwrap_err_r16
        procedure :: unwrap_or_r16

        procedure :: unwrap_r32
        procedure :: unwrap_err_r32
        procedure :: unwrap_or_r32

        procedure :: unwrap_r64
        procedure :: unwrap_err_r64
        procedure :: unwrap_or_r64

        procedure :: unwrap_r128
        procedure :: unwrap_err_r128
        procedure :: unwrap_or_r128

        procedure :: unwrap_i8
        procedure :: unwrap_err_i8
        procedure :: unwrap_or_i8

        procedure :: unwrap_i16
        procedure :: unwrap_err_i16
        procedure :: unwrap_or_i16

        procedure :: unwrap_i32
        procedure :: unwrap_err_i32
        procedure :: unwrap_or_i32

        procedure :: unwrap_i64
        procedure :: unwrap_err_i64
        procedure :: unwrap_or_i64

        procedure :: unwrap_i128
        procedure :: unwrap_err_i128
        procedure :: unwrap_or_i128

        procedure :: unwrap_c8
        procedure :: unwrap_err_c8
        procedure :: unwrap_or_c8

        procedure :: unwrap_c16
        procedure :: unwrap_err_c16
        procedure :: unwrap_or_c16

        procedure :: unwrap_c32
        procedure :: unwrap_err_c32
        procedure :: unwrap_or_c32

        procedure :: unwrap_c64
        procedure :: unwrap_err_c64
        procedure :: unwrap_or_c64

        procedure :: unwrap_c128
        procedure :: unwrap_err_c128
        procedure :: unwrap_or_c128

    end type result_type

contains

    pure function ok(val) result(res)
        class(*), intent(in) :: val
        type(result_type) :: res
        call res%set_right(val)
    end function ok

    pure function err(val) result(res)
        class(*), intent(in) :: val
        type(result_type) :: res
        call res%set_left(val)
    end function err

    function ok_move(val) result(res)
        class(*), allocatable, intent(inout) :: val
        type(result_type) :: res
        call res%move_right(val)
    end function ok_move

    function err_move(val) result(res)
        class(*), allocatable, intent(inout) :: val
        type(result_type) :: res
        call res%move_left(val)
    end function err_move

    pure elemental logical function is_ok(this)
        class(result_type), intent(in) :: this
        is_ok = this%is_right()
    end function is_ok

    pure elemental logical function is_err(this)
        class(result_type), intent(in) :: this
        is_err = this%is_left()
    end function is_err

    function unwrap(this) result(ptr)
        class(result_type), intent(in), target :: this
        class(*), pointer :: ptr

        if (DO_CHECKS) call check(this%is_ok(), "unwrap called on Err value")

        ptr => this%get_right()
    end function unwrap

    function unwrap_err(this) result(ptr)
        class(result_type), intent(in), target :: this
        class(*), pointer :: ptr

        if (DO_CHECKS) call check(this%is_err(), "unwrap_err called on Ok value")

        ptr => this%get_left()
    end function unwrap_err

    function unwrap_or(this, default_val) result(res)
        class(result_type), intent(in) :: this
        class(*), intent(in) :: default_val
        class(*), allocatable :: res
        class(*), pointer :: ptr

        if (this%is_ok()) then
            ptr => this%get_right()
            allocate (res, source=ptr)
        else
            allocate (res, source=default_val)
        end if
    end function unwrap_or

    !> Extracts the Ok value by moving it to 'dest', and replacing it with 'replacement_err'.
    !> This ensures the Result object remains valid (now containing an Err).
    subroutine unwrap_move_to_err(this, dest, replacement_err)
        class(result_type), intent(inout) :: this
        class(*), allocatable, intent(out) :: dest
        class(*), intent(in) :: replacement_err
        class(*), allocatable :: temp_err

        if (DO_CHECKS) call check(this%is_ok(), "unwrap_move_to_err called on Err value")

        ! 1. Allocate the replacement error first to ensure safety if alloc fails
        allocate (temp_err, source=replacement_err)

        ! 2. Perform the swap
        ! Move the Right (Ok) value to dest (leaves Right unallocated)
        call this%move_right(dest)

        ! 3. Restore invariant: Move the temp error into Left
        call this%move_left(temp_err)
    end subroutine unwrap_move_to_err

    subroutine unsafe_unwrap_move(this, dest)
        class(result_type), intent(inout) :: this
        class(*), allocatable, intent(out) :: dest

        if (DO_CHECKS) call check(this%is_ok(), "unsafe_unwrap_move called on Err value")

        ! Zero-cost pointer swap. No allocations.
        call this%move_right(dest)

        ! 'this' is now a Zombie (Left=Unallocated, Right=Unallocated).
        ! This breaks our safety contract, but this move is *very* fast
    end subroutine unsafe_unwrap_move

    ! -- Specialized Implementations --

    pure elemental function ok_int(val) result(res) !GCC$ attributes always_inline :: ok_int
        integer, intent(in) :: val
        type(result_type) :: res
        call res%set_right_int(val)
    end function ok_int

    pure elemental function err_int(val) result(res) !GCC$ attributes always_inline :: err_int
        integer, intent(in) :: val
        type(result_type) :: res
        call res%set_left_int(val)
    end function err_int

    pure elemental function unwrap_int(this) result(val) !GCC$ attributes always_inline :: unwrap_int
        class(result_type), intent(in) :: this
        integer :: val
        if (this%is_err()) then
            if (DO_CHECKS) error stop "unwrap_int called on Err value"
        end if
        val = this%get_right_int()
    end function unwrap_int

    pure elemental function unwrap_err_int(this) result(val) !GCC$ attributes always_inline :: unwrap_err_int
        class(result_type), intent(in) :: this
        integer :: val
        if (this%is_ok()) then
            if (DO_CHECKS) error stop "unwrap_err_int called on Ok value"
        end if
        val = this%get_left_int()
    end function unwrap_err_int

    pure elemental function unwrap_or_int(this, default_val) result(val) !GCC$ attributes always_inline :: unwrap_or_int
        class(result_type), intent(in) :: this
        integer, intent(in) :: default_val
        integer :: val
        if (this%is_ok()) then
            val = this%get_right_int()
        else
            val = default_val
        end if
    end function unwrap_or_int

    pure elemental function ok_real(val) result(res) !GCC$ attributes always_inline :: ok_real
        real, intent(in) :: val
        type(result_type) :: res
        call res%set_right_real(val)
    end function ok_real

    pure elemental function err_real(val) result(res) !GCC$ attributes always_inline :: err_real
        real, intent(in) :: val
        type(result_type) :: res
        call res%set_left_real(val)
    end function err_real

    pure elemental function unwrap_real(this) result(val) !GCC$ attributes always_inline :: unwrap_real
        class(result_type), intent(in) :: this
        real :: val
        if (this%is_err()) then
            if (DO_CHECKS) error stop "unwrap_real called on Err value"
        end if
        val = this%get_right_real()
    end function unwrap_real

    pure elemental function unwrap_err_real(this) result(val) !GCC$ attributes always_inline :: unwrap_err_real
        class(result_type), intent(in) :: this
        real :: val
        if (this%is_ok()) then
            if (DO_CHECKS) error stop "unwrap_err_real called on Ok value"
        end if
        val = this%get_left_real()
    end function unwrap_err_real

    pure elemental function unwrap_or_real(this, default_val) result(val) !GCC$ attributes always_inline :: unwrap_or_real
        class(result_type), intent(in) :: this
        real, intent(in) :: default_val
        real :: val
        if (this%is_ok()) then
            val = this%get_right_real()
        else
            val = default_val
        end if
    end function unwrap_or_real

    pure elemental function ok_log(val) result(res) !GCC$ attributes always_inline :: ok_log
        logical, intent(in) :: val
        type(result_type) :: res
        call res%set_right_log(val)
    end function ok_log

    pure elemental function err_log(val) result(res) !GCC$ attributes always_inline :: err_log
        logical, intent(in) :: val
        type(result_type) :: res
        call res%set_left_log(val)
    end function err_log

    pure elemental function unwrap_log(this) result(val) !GCC$ attributes always_inline :: unwrap_log
        class(result_type), intent(in) :: this
        logical :: val
        if (this%is_err()) then
            if (DO_CHECKS) error stop "unwrap_log called on Err value"
        end if
        val = this%get_right_log()
    end function unwrap_log

    pure elemental function unwrap_err_log(this) result(val) !GCC$ attributes always_inline :: unwrap_err_log
        class(result_type), intent(in) :: this
        logical :: val
        if (this%is_ok()) then
            if (DO_CHECKS) error stop "unwrap_err_log called on Ok value"
        end if
        val = this%get_left_log()
    end function unwrap_err_log

    pure elemental function unwrap_or_log(this, default_val) result(val) !GCC$ attributes always_inline :: unwrap_or_log
        class(result_type), intent(in) :: this
        logical, intent(in) :: default_val
        logical :: val
        if (this%is_ok()) then
            val = this%get_right_log()
        else
            val = default_val
        end if
    end function unwrap_or_log

    pure elemental function ok_cpx(val) result(res) !GCC$ attributes always_inline :: ok_cpx
        complex, intent(in) :: val
        type(result_type) :: res
        call res%set_right_cpx(val)
    end function ok_cpx

    pure elemental function err_cpx(val) result(res) !GCC$ attributes always_inline :: err_cpx
        complex, intent(in) :: val
        type(result_type) :: res
        call res%set_left_cpx(val)
    end function err_cpx

    pure elemental function unwrap_cpx(this) result(val) !GCC$ attributes always_inline :: unwrap_cpx
        class(result_type), intent(in) :: this
        complex :: val
        if (this%is_err()) then
            if (DO_CHECKS) error stop "unwrap_cpx called on Err value"
        end if
        val = this%get_right_cpx()
    end function unwrap_cpx

    pure elemental function unwrap_err_cpx(this) result(val) !GCC$ attributes always_inline :: unwrap_err_cpx
        class(result_type), intent(in) :: this
        complex :: val
        if (this%is_ok()) then
            if (DO_CHECKS) error stop "unwrap_err_cpx called on Ok value"
        end if
        val = this%get_left_cpx()
    end function unwrap_err_cpx

    pure elemental function unwrap_or_cpx(this, default_val) result(val) !GCC$ attributes always_inline :: unwrap_or_cpx
        class(result_type), intent(in) :: this
        complex, intent(in) :: default_val
        complex :: val
        if (this%is_ok()) then
            val = this%get_right_cpx()
        else
            val = default_val
        end if
    end function unwrap_or_cpx

    pure elemental function ok_r8(val) result(res) !GCC$ attributes always_inline :: ok_r8
        real(8), intent(in) :: val
        type(result_type) :: res
        call res%set_right_r8(val)
    end function ok_r8

    pure elemental function err_r8(val) result(res) !GCC$ attributes always_inline :: err_r8
        real(8), intent(in) :: val
        type(result_type) :: res
        call res%set_left_r8(val)
    end function err_r8

    pure elemental function unwrap_r8(this) result(val) !GCC$ attributes always_inline :: unwrap_r8
        class(result_type), intent(in) :: this
        real(8) :: val
        if (this%is_err()) then
            if (DO_CHECKS) error stop "unwrap_r8 called on Err value"
        end if
        val = this%get_right_r8()
    end function unwrap_r8

    pure elemental function unwrap_err_r8(this) result(val) !GCC$ attributes always_inline :: unwrap_err_r8
        class(result_type), intent(in) :: this
        real(8) :: val
        if (this%is_ok()) then
            if (DO_CHECKS) error stop "unwrap_err_r8 called on Ok value"
        end if
        val = this%get_left_r8()
    end function unwrap_err_r8

    pure elemental function unwrap_or_r8(this, default_val) result(val) !GCC$ attributes always_inline :: unwrap_or_r8
        class(result_type), intent(in) :: this
        real(8), intent(in) :: default_val
        real(8) :: val
        if (this%is_ok()) then
            val = this%get_right_r8()
        else
            val = default_val
        end if
    end function unwrap_or_r8

    pure elemental function ok_r16(val) result(res) !GCC$ attributes always_inline :: ok_r16
        real(16), intent(in) :: val
        type(result_type) :: res
        call res%set_right_r16(val)
    end function ok_r16

    pure elemental function err_r16(val) result(res) !GCC$ attributes always_inline :: err_r16
        real(16), intent(in) :: val
        type(result_type) :: res
        call res%set_left_r16(val)
    end function err_r16

    pure elemental function unwrap_r16(this) result(val) !GCC$ attributes always_inline :: unwrap_r16
        class(result_type), intent(in) :: this
        real(16) :: val
        if (this%is_err()) then
            if (DO_CHECKS) error stop "unwrap_r16 called on Err value"
        end if
        val = this%get_right_r16()
    end function unwrap_r16

    pure elemental function unwrap_err_r16(this) result(val) !GCC$ attributes always_inline :: unwrap_err_r16
        class(result_type), intent(in) :: this
        real(16) :: val
        if (this%is_ok()) then
            if (DO_CHECKS) error stop "unwrap_err_r16 called on Ok value"
        end if
        val = this%get_left_r16()
    end function unwrap_err_r16

    pure elemental function unwrap_or_r16(this, default_val) result(val) !GCC$ attributes always_inline :: unwrap_or_r16
        class(result_type), intent(in) :: this
        real(16), intent(in) :: default_val
        real(16) :: val
        if (this%is_ok()) then
            val = this%get_right_r16()
        else
            val = default_val
        end if
    end function unwrap_or_r16

    pure elemental function ok_r32(val) result(res) !GCC$ attributes always_inline :: ok_r32
        real(real32), intent(in) :: val
        type(result_type) :: res
        call res%set_right_r32(val)
    end function ok_r32

    pure elemental function err_r32(val) result(res) !GCC$ attributes always_inline :: err_r32
        real(real32), intent(in) :: val
        type(result_type) :: res
        call res%set_left_r32(val)
    end function err_r32

    pure elemental function unwrap_r32(this) result(val) !GCC$ attributes always_inline :: unwrap_r32
        class(result_type), intent(in) :: this
        real(real32) :: val
        if (this%is_err()) then
            if (DO_CHECKS) error stop "unwrap_r32 called on Err value"
        end if
        val = this%get_right_r32()
    end function unwrap_r32

    pure elemental function unwrap_err_r32(this) result(val) !GCC$ attributes always_inline :: unwrap_err_r32
        class(result_type), intent(in) :: this
        real(real32) :: val
        if (this%is_ok()) then
            if (DO_CHECKS) error stop "unwrap_err_r32 called on Ok value"
        end if
        val = this%get_left_r32()
    end function unwrap_err_r32

    pure elemental function unwrap_or_r32(this, default_val) result(val) !GCC$ attributes always_inline :: unwrap_or_r32
        class(result_type), intent(in) :: this
        real(real32), intent(in) :: default_val
        real(real32) :: val
        if (this%is_ok()) then
            val = this%get_right_r32()
        else
            val = default_val
        end if
    end function unwrap_or_r32

    pure elemental function ok_r64(val) result(res) !GCC$ attributes always_inline :: ok_r64
        real(real64), intent(in) :: val
        type(result_type) :: res
        call res%set_right_r64(val)
    end function ok_r64

    pure elemental function err_r64(val) result(res) !GCC$ attributes always_inline :: err_r64
        real(real64), intent(in) :: val
        type(result_type) :: res
        call res%set_left_r64(val)
    end function err_r64

    pure elemental function unwrap_r64(this) result(val) !GCC$ attributes always_inline :: unwrap_r64
        class(result_type), intent(in) :: this
        real(real64) :: val
        if (this%is_err()) then
            if (DO_CHECKS) error stop "unwrap_r64 called on Err value"
        end if
        val = this%get_right_r64()
    end function unwrap_r64

    pure elemental function unwrap_err_r64(this) result(val) !GCC$ attributes always_inline :: unwrap_err_r64
        class(result_type), intent(in) :: this
        real(real64) :: val
        if (this%is_ok()) then
            if (DO_CHECKS) error stop "unwrap_err_r64 called on Ok value"
        end if
        val = this%get_left_r64()
    end function unwrap_err_r64

    pure elemental function unwrap_or_r64(this, default_val) result(val) !GCC$ attributes always_inline :: unwrap_or_r64
        class(result_type), intent(in) :: this
        real(real64), intent(in) :: default_val
        real(real64) :: val
        if (this%is_ok()) then
            val = this%get_right_r64()
        else
            val = default_val
        end if
    end function unwrap_or_r64

    pure elemental function ok_r128(val) result(res) !GCC$ attributes always_inline :: ok_r128
        real(real128), intent(in) :: val
        type(result_type) :: res
        call res%set_right_r128(val)
    end function ok_r128

    pure elemental function err_r128(val) result(res) !GCC$ attributes always_inline :: err_r128
        real(real128), intent(in) :: val
        type(result_type) :: res
        call res%set_left_r128(val)
    end function err_r128

    pure elemental function unwrap_r128(this) result(val) !GCC$ attributes always_inline :: unwrap_r128
        class(result_type), intent(in) :: this
        real(real128) :: val
        if (this%is_err()) then
            if (DO_CHECKS) error stop "unwrap_r128 called on Err value"
        end if
        val = this%get_right_r128()
    end function unwrap_r128

    pure elemental function unwrap_err_r128(this) result(val) !GCC$ attributes always_inline :: unwrap_err_r128
        class(result_type), intent(in) :: this
        real(real128) :: val
        if (this%is_ok()) then
            if (DO_CHECKS) error stop "unwrap_err_r128 called on Ok value"
        end if
        val = this%get_left_r128()
    end function unwrap_err_r128

    pure elemental function unwrap_or_r128(this, default_val) result(val) !GCC$ attributes always_inline :: unwrap_or_r128
        class(result_type), intent(in) :: this
        real(real128), intent(in) :: default_val
        real(real128) :: val
        if (this%is_ok()) then
            val = this%get_right_r128()
        else
            val = default_val
        end if
    end function unwrap_or_r128

    pure elemental function ok_i8(val) result(res) !GCC$ attributes always_inline :: ok_i8
        integer(int8), intent(in) :: val
        type(result_type) :: res
        call res%set_right_i8(val)
    end function ok_i8

    pure elemental function err_i8(val) result(res) !GCC$ attributes always_inline :: err_i8
        integer(int8), intent(in) :: val
        type(result_type) :: res
        call res%set_left_i8(val)
    end function err_i8

    pure elemental function unwrap_i8(this) result(val) !GCC$ attributes always_inline :: unwrap_i8
        class(result_type), intent(in) :: this
        integer(int8) :: val
        if (this%is_err()) then
            if (DO_CHECKS) error stop "unwrap_i8 called on Err value"
        end if
        val = this%get_right_i8()
    end function unwrap_i8

    pure elemental function unwrap_err_i8(this) result(val) !GCC$ attributes always_inline :: unwrap_err_i8
        class(result_type), intent(in) :: this
        integer(int8) :: val
        if (this%is_ok()) then
            if (DO_CHECKS) error stop "unwrap_err_i8 called on Ok value"
        end if
        val = this%get_left_i8()
    end function unwrap_err_i8

    pure elemental function unwrap_or_i8(this, default_val) result(val) !GCC$ attributes always_inline :: unwrap_or_i8
        class(result_type), intent(in) :: this
        integer(int8), intent(in) :: default_val
        integer(int8) :: val
        if (this%is_ok()) then
            val = this%get_right_i8()
        else
            val = default_val
        end if
    end function unwrap_or_i8

    pure elemental function ok_i16(val) result(res) !GCC$ attributes always_inline :: ok_i16
        integer(int16), intent(in) :: val
        type(result_type) :: res
        call res%set_right_i16(val)
    end function ok_i16

    pure elemental function err_i16(val) result(res) !GCC$ attributes always_inline :: err_i16
        integer(int16), intent(in) :: val
        type(result_type) :: res
        call res%set_left_i16(val)
    end function err_i16

    pure elemental function unwrap_i16(this) result(val) !GCC$ attributes always_inline :: unwrap_i16
        class(result_type), intent(in) :: this
        integer(int16) :: val
        if (this%is_err()) then
            if (DO_CHECKS) error stop "unwrap_i16 called on Err value"
        end if
        val = this%get_right_i16()
    end function unwrap_i16

    pure elemental function unwrap_err_i16(this) result(val) !GCC$ attributes always_inline :: unwrap_err_i16
        class(result_type), intent(in) :: this
        integer(int16) :: val
        if (this%is_ok()) then
            if (DO_CHECKS) error stop "unwrap_err_i16 called on Ok value"
        end if
        val = this%get_left_i16()
    end function unwrap_err_i16

    pure elemental function unwrap_or_i16(this, default_val) result(val) !GCC$ attributes always_inline :: unwrap_or_i16
        class(result_type), intent(in) :: this
        integer(int16), intent(in) :: default_val
        integer(int16) :: val
        if (this%is_ok()) then
            val = this%get_right_i16()
        else
            val = default_val
        end if
    end function unwrap_or_i16

    pure elemental function ok_i32(val) result(res) !GCC$ attributes always_inline :: ok_i32
        integer(int32), intent(in) :: val
        type(result_type) :: res
        call res%set_right_i32(val)
    end function ok_i32

    pure elemental function err_i32(val) result(res) !GCC$ attributes always_inline :: err_i32
        integer(int32), intent(in) :: val
        type(result_type) :: res
        call res%set_left_i32(val)
    end function err_i32

    pure elemental function unwrap_i32(this) result(val) !GCC$ attributes always_inline :: unwrap_i32
        class(result_type), intent(in) :: this
        integer(int32) :: val
        if (this%is_err()) then
            if (DO_CHECKS) error stop "unwrap_i32 called on Err value"
        end if
        val = this%get_right_i32()
    end function unwrap_i32

    pure elemental function unwrap_err_i32(this) result(val) !GCC$ attributes always_inline :: unwrap_err_i32
        class(result_type), intent(in) :: this
        integer(int32) :: val
        if (this%is_ok()) then
            if (DO_CHECKS) error stop "unwrap_err_i32 called on Ok value"
        end if
        val = this%get_left_i32()
    end function unwrap_err_i32

    pure elemental function unwrap_or_i32(this, default_val) result(val) !GCC$ attributes always_inline :: unwrap_or_i32
        class(result_type), intent(in) :: this
        integer(int32), intent(in) :: default_val
        integer(int32) :: val
        if (this%is_ok()) then
            val = this%get_right_i32()
        else
            val = default_val
        end if
    end function unwrap_or_i32

    pure elemental function ok_i64(val) result(res) !GCC$ attributes always_inline :: ok_i64
        integer(int64), intent(in) :: val
        type(result_type) :: res
        call res%set_right_i64(val)
    end function ok_i64

    pure elemental function err_i64(val) result(res) !GCC$ attributes always_inline :: err_i64
        integer(int64), intent(in) :: val
        type(result_type) :: res
        call res%set_left_i64(val)
    end function err_i64

    pure elemental function unwrap_i64(this) result(val) !GCC$ attributes always_inline :: unwrap_i64
        class(result_type), intent(in) :: this
        integer(int64) :: val
        if (this%is_err()) then
            if (DO_CHECKS) error stop "unwrap_i64 called on Err value"
        end if
        val = this%get_right_i64()
    end function unwrap_i64

    pure elemental function unwrap_err_i64(this) result(val) !GCC$ attributes always_inline :: unwrap_err_i64
        class(result_type), intent(in) :: this
        integer(int64) :: val
        if (this%is_ok()) then
            if (DO_CHECKS) error stop "unwrap_err_i64 called on Ok value"
        end if
        val = this%get_left_i64()
    end function unwrap_err_i64

    pure elemental function unwrap_or_i64(this, default_val) result(val) !GCC$ attributes always_inline :: unwrap_or_i64
        class(result_type), intent(in) :: this
        integer(int64), intent(in) :: default_val
        integer(int64) :: val
        if (this%is_ok()) then
            val = this%get_right_i64()
        else
            val = default_val
        end if
    end function unwrap_or_i64

    pure elemental function ok_i128(val) result(res) !GCC$ attributes always_inline :: ok_i128
        integer(int128), intent(in) :: val
        type(result_type) :: res
        call res%set_right_i128(val)
    end function ok_i128

    pure elemental function err_i128(val) result(res) !GCC$ attributes always_inline :: err_i128
        integer(int128), intent(in) :: val
        type(result_type) :: res
        call res%set_left_i128(val)
    end function err_i128

    pure elemental function unwrap_i128(this) result(val) !GCC$ attributes always_inline :: unwrap_i128
        class(result_type), intent(in) :: this
        integer(int128) :: val
        if (this%is_err()) then
            if (DO_CHECKS) error stop "unwrap_i128 called on Err value"
        end if
        val = this%get_right_i128()
    end function unwrap_i128

    pure elemental function unwrap_err_i128(this) result(val) !GCC$ attributes always_inline :: unwrap_err_i128
        class(result_type), intent(in) :: this
        integer(int128) :: val
        if (this%is_ok()) then
            if (DO_CHECKS) error stop "unwrap_err_i128 called on Ok value"
        end if
        val = this%get_left_i128()
    end function unwrap_err_i128

    pure elemental function unwrap_or_i128(this, default_val) result(val) !GCC$ attributes always_inline :: unwrap_or_i128
        class(result_type), intent(in) :: this
        integer(int128), intent(in) :: default_val
        integer(int128) :: val
        if (this%is_ok()) then
            val = this%get_right_i128()
        else
            val = default_val
        end if
    end function unwrap_or_i128

    pure elemental function ok_c8(val) result(res) !GCC$ attributes always_inline :: ok_c8
        complex(real8), intent(in) :: val
        type(result_type) :: res
        call res%set_right_c8(val)
    end function ok_c8

    pure elemental function err_c8(val) result(res) !GCC$ attributes always_inline :: err_c8
        complex(real8), intent(in) :: val
        type(result_type) :: res
        call res%set_left_c8(val)
    end function err_c8

    pure elemental function unwrap_c8(this) result(val) !GCC$ attributes always_inline :: unwrap_c8
        class(result_type), intent(in) :: this
        complex(real8) :: val
        if (this%is_err()) then
            if (DO_CHECKS) error stop "unwrap_c8 called on Err value"
        end if
        val = this%get_right_c8()
    end function unwrap_c8

    pure elemental function unwrap_err_c8(this) result(val) !GCC$ attributes always_inline :: unwrap_err_c8
        class(result_type), intent(in) :: this
        complex(real8) :: val
        if (this%is_ok()) then
            if (DO_CHECKS) error stop "unwrap_err_c8 called on Ok value"
        end if
        val = this%get_left_c8()
    end function unwrap_err_c8

    pure elemental function unwrap_or_c8(this, default_val) result(val) !GCC$ attributes always_inline :: unwrap_or_c8
        class(result_type), intent(in) :: this
        complex(real8), intent(in) :: default_val
        complex(real8) :: val
        if (this%is_ok()) then
            val = this%get_right_c8()
        else
            val = default_val
        end if
    end function unwrap_or_c8

    pure elemental function ok_c16(val) result(res) !GCC$ attributes always_inline :: ok_c16
        complex(real16), intent(in) :: val
        type(result_type) :: res
        call res%set_right_c16(val)
    end function ok_c16

    pure elemental function err_c16(val) result(res) !GCC$ attributes always_inline :: err_c16
        complex(real16), intent(in) :: val
        type(result_type) :: res
        call res%set_left_c16(val)
    end function err_c16

    pure elemental function unwrap_c16(this) result(val) !GCC$ attributes always_inline :: unwrap_c16
        class(result_type), intent(in) :: this
        complex(real16) :: val
        if (this%is_err()) then
            if (DO_CHECKS) error stop "unwrap_c16 called on Err value"
        end if
        val = this%get_right_c16()
    end function unwrap_c16

    pure elemental function unwrap_err_c16(this) result(val) !GCC$ attributes always_inline :: unwrap_err_c16
        class(result_type), intent(in) :: this
        complex(real16) :: val
        if (this%is_ok()) then
            if (DO_CHECKS) error stop "unwrap_err_c16 called on Ok value"
        end if
        val = this%get_left_c16()
    end function unwrap_err_c16

    pure elemental function unwrap_or_c16(this, default_val) result(val) !GCC$ attributes always_inline :: unwrap_or_c16
        class(result_type), intent(in) :: this
        complex(real16), intent(in) :: default_val
        complex(real16) :: val
        if (this%is_ok()) then
            val = this%get_right_c16()
        else
            val = default_val
        end if
    end function unwrap_or_c16

    pure elemental function ok_c32(val) result(res) !GCC$ attributes always_inline :: ok_c32
        complex(real32), intent(in) :: val
        type(result_type) :: res
        call res%set_right_c32(val)
    end function ok_c32

    pure elemental function err_c32(val) result(res) !GCC$ attributes always_inline :: err_c32
        complex(real32), intent(in) :: val
        type(result_type) :: res
        call res%set_left_c32(val)
    end function err_c32

    pure elemental function unwrap_c32(this) result(val) !GCC$ attributes always_inline :: unwrap_c32
        class(result_type), intent(in) :: this
        complex(real32) :: val
        if (this%is_err()) then
            if (DO_CHECKS) error stop "unwrap_c32 called on Err value"
        end if
        val = this%get_right_c32()
    end function unwrap_c32

    pure elemental function unwrap_err_c32(this) result(val) !GCC$ attributes always_inline :: unwrap_err_c32
        class(result_type), intent(in) :: this
        complex(real32) :: val
        if (this%is_ok()) then
            if (DO_CHECKS) error stop "unwrap_err_c32 called on Ok value"
        end if
        val = this%get_left_c32()
    end function unwrap_err_c32

    pure elemental function unwrap_or_c32(this, default_val) result(val) !GCC$ attributes always_inline :: unwrap_or_c32
        class(result_type), intent(in) :: this
        complex(real32), intent(in) :: default_val
        complex(real32) :: val
        if (this%is_ok()) then
            val = this%get_right_c32()
        else
            val = default_val
        end if
    end function unwrap_or_c32

    pure elemental function ok_c64(val) result(res) !GCC$ attributes always_inline :: ok_c64
        complex(real64), intent(in) :: val
        type(result_type) :: res
        call res%set_right_c64(val)
    end function ok_c64

    pure elemental function err_c64(val) result(res) !GCC$ attributes always_inline :: err_c64
        complex(real64), intent(in) :: val
        type(result_type) :: res
        call res%set_left_c64(val)
    end function err_c64

    pure elemental function unwrap_c64(this) result(val) !GCC$ attributes always_inline :: unwrap_c64
        class(result_type), intent(in) :: this
        complex(real64) :: val
        if (this%is_err()) then
            if (DO_CHECKS) error stop "unwrap_c64 called on Err value"
        end if
        val = this%get_right_c64()
    end function unwrap_c64

    pure elemental function unwrap_err_c64(this) result(val) !GCC$ attributes always_inline :: unwrap_err_c64
        class(result_type), intent(in) :: this
        complex(real64) :: val
        if (this%is_ok()) then
            if (DO_CHECKS) error stop "unwrap_err_c64 called on Ok value"
        end if
        val = this%get_left_c64()
    end function unwrap_err_c64

    pure elemental function unwrap_or_c64(this, default_val) result(val) !GCC$ attributes always_inline :: unwrap_or_c64
        class(result_type), intent(in) :: this
        complex(real64), intent(in) :: default_val
        complex(real64) :: val
        if (this%is_ok()) then
            val = this%get_right_c64()
        else
            val = default_val
        end if
    end function unwrap_or_c64

    pure elemental function ok_c128(val) result(res) !GCC$ attributes always_inline :: ok_c128
        complex(real128), intent(in) :: val
        type(result_type) :: res
        call res%set_right_c128(val)
    end function ok_c128

    pure elemental function err_c128(val) result(res) !GCC$ attributes always_inline :: err_c128
        complex(real128), intent(in) :: val
        type(result_type) :: res
        call res%set_left_c128(val)
    end function err_c128

    pure elemental function unwrap_c128(this) result(val) !GCC$ attributes always_inline :: unwrap_c128
        class(result_type), intent(in) :: this
        complex(real128) :: val
        if (this%is_err()) then
            if (DO_CHECKS) error stop "unwrap_c128 called on Err value"
        end if
        val = this%get_right_c128()
    end function unwrap_c128

    pure elemental function unwrap_err_c128(this) result(val) !GCC$ attributes always_inline :: unwrap_err_c128
        class(result_type), intent(in) :: this
        complex(real128) :: val
        if (this%is_ok()) then
            if (DO_CHECKS) error stop "unwrap_err_c128 called on Ok value"
        end if
        val = this%get_left_c128()
    end function unwrap_err_c128

    pure elemental function unwrap_or_c128(this, default_val) result(val) !GCC$ attributes always_inline :: unwrap_or_c128
        class(result_type), intent(in) :: this
        complex(real128), intent(in) :: default_val
        complex(real128) :: val
        if (this%is_ok()) then
            val = this%get_right_c128()
        else
            val = default_val
        end if
    end function unwrap_or_c128

end module formerr_result
