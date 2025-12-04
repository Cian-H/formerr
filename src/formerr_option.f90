module formerr_option
    use formerr_either
    use stdlib_error, only: check
    use, intrinsic :: iso_fortran_env, only: int8, int16, int32, int64, real32, real64, real128
    implicit none
    private

    integer, parameter :: int128 = selected_int_kind(38)

    logical, parameter :: DO_CHECKS = .false.

    public :: option, some, none, is_some, is_none, unwrap, unwrap_or
    public :: some_move, unwrap_move
    public :: some_int
    public :: some_i8
    public :: some_i16
    public :: some_i32
    public :: some_i64
    public :: some_i128
    public :: some_real
    public :: some_r32
    public :: some_r64
    public :: some_r128
    public :: some_r8
    public :: some_r16
    public :: some_log
    public :: some_cpx
    public :: some_c64
    public :: some_c128

    type :: unit_type
    end type unit_type

    type, extends(either) :: option
    contains
        procedure :: is_some
        procedure :: is_none
        procedure :: unwrap
        procedure :: unwrap_or
        procedure :: unwrap_move
        procedure :: unwrap_int
        procedure :: unwrap_or_int
        procedure :: unwrap_i8
        procedure :: unwrap_or_i8
        procedure :: unwrap_i16
        procedure :: unwrap_or_i16
        procedure :: unwrap_i32
        procedure :: unwrap_or_i32
        procedure :: unwrap_i64
        procedure :: unwrap_or_i64
        procedure :: unwrap_i128
        procedure :: unwrap_or_i128
        procedure :: unwrap_real
        procedure :: unwrap_or_real
        procedure :: unwrap_r32
        procedure :: unwrap_or_r32
        procedure :: unwrap_r64
        procedure :: unwrap_or_r64
        procedure :: unwrap_r128
        procedure :: unwrap_or_r128
        procedure :: unwrap_r8
        procedure :: unwrap_or_r8
        procedure :: unwrap_r16
        procedure :: unwrap_or_r16
        procedure :: unwrap_log
        procedure :: unwrap_or_log
        procedure :: unwrap_cpx
        procedure :: unwrap_or_cpx
        procedure :: unwrap_c64
        procedure :: unwrap_or_c64
        procedure :: unwrap_c128
        procedure :: unwrap_or_c128

    end type option

contains

    pure function some(val) result(res)
        class(*), intent(in) :: val
        type(option) :: res
        call res%set_right(val)
    end function some

    pure function none() result(res)
        type(option) :: res
        type(unit_type) :: u
        call res%set_left(u)
    end function none

    function some_move(val) result(res)
        class(*), allocatable, intent(inout) :: val
        type(option) :: res
        call res%move_right(val)
    end function some_move

    pure elemental logical function is_some(this)
        class(option), intent(in) :: this
        is_some = this%is_right()
    end function is_some

    pure elemental logical function is_none(this)
        class(option), intent(in) :: this
        is_none = this%is_left()
    end function is_none

    function unwrap(this) result(ptr)
        class(option), intent(in), target :: this
        class(*), pointer :: ptr

        if (DO_CHECKS) call check(this%is_some(), "unwrap called on None value")

        ptr => this%get_right()
    end function unwrap

    function unwrap_or(this, default_val) result(res)
        class(option), intent(in) :: this
        class(*), intent(in) :: default_val
        class(*), allocatable :: res
        class(*), pointer :: ptr

        if (this%is_some()) then
            ptr => this%get_right()
            allocate (res, source=ptr)
        else
            allocate (res, source=default_val)
        end if
    end function unwrap_or

    !> Extracts the value by moving it to 'dest'.
    !> The Option immediately becomes None.
    subroutine unwrap_move(this, dest)
        class(option), intent(inout) :: this
        class(*), allocatable, intent(out) :: dest
        type(unit_type) :: u
        class(*), allocatable :: temp_none

        if (DO_CHECKS) call check(this%is_some(), "unwrap_move called on None value")

        ! 1. Prepare the None state (Unit type)
        allocate (temp_none, source=u)

        ! 2. Perform the swap
        ! Move the Right (Some) value to dest
        call this%move_right(dest)

        ! 3. Restore invariant: Move the unit type into Left (None)
        call this%move_left(temp_none)
    end subroutine unwrap_move

    ! -- Specialized Implementations --

    pure elemental function some_int(val) result(res) !GCC$ attributes always_inline :: some_int
        integer, intent(in) :: val
        type(option) :: res
        call res%set_right_int(val)
    end function some_int

    pure elemental function unwrap_int(this) result(val) !GCC$ attributes always_inline :: unwrap_int
        class(option), intent(in) :: this
        integer :: val
        if (this%is_none()) then
            if (DO_CHECKS) error stop "unwrap_int called on None value"
        end if
        ! We call the specialized getter from either
        val = this%get_right_int()
    end function unwrap_int

    pure elemental function unwrap_or_int(this, default_val) result(val) !GCC$ attributes always_inline :: unwrap_or_int
        class(option), intent(in) :: this
        integer, intent(in) :: default_val
        integer :: val
        if (this%is_some()) then
            val = this%get_right_int()
        else
            val = default_val
        end if
    end function unwrap_or_int

    pure elemental function some_i8(val) result(res) !GCC$ attributes always_inline :: some_i8
        integer(int8), intent(in) :: val
        type(option) :: res
        call res%set_right_i8(val)
    end function some_i8

    pure elemental function unwrap_i8(this) result(val) !GCC$ attributes always_inline :: unwrap_i8
        class(option), intent(in) :: this
        integer(int8) :: val
        if (this%is_none()) then
            if (DO_CHECKS) error stop "unwrap_i8 called on None value"
        end if
        ! We call the specialized getter from either
        val = this%get_right_i8()
    end function unwrap_i8

    pure elemental function unwrap_or_i8(this, default_val) result(val) !GCC$ attributes always_inline :: unwrap_or_i8
        class(option), intent(in) :: this
        integer(int8), intent(in) :: default_val
        integer(int8) :: val
        if (this%is_some()) then
            val = this%get_right_i8()
        else
            val = default_val
        end if
    end function unwrap_or_i8

    pure elemental function some_i16(val) result(res) !GCC$ attributes always_inline :: some_i16
        integer(int16), intent(in) :: val
        type(option) :: res
        call res%set_right_i16(val)
    end function some_i16

    pure elemental function unwrap_i16(this) result(val) !GCC$ attributes always_inline :: unwrap_i16
        class(option), intent(in) :: this
        integer(int16) :: val
        if (this%is_none()) then
            if (DO_CHECKS) error stop "unwrap_i16 called on None value"
        end if
        ! We call the specialized getter from either
        val = this%get_right_i16()
    end function unwrap_i16

    pure elemental function unwrap_or_i16(this, default_val) result(val) !GCC$ attributes always_inline :: unwrap_or_i16
        class(option), intent(in) :: this
        integer(int16), intent(in) :: default_val
        integer(int16) :: val
        if (this%is_some()) then
            val = this%get_right_i16()
        else
            val = default_val
        end if
    end function unwrap_or_i16

    pure elemental function some_i32(val) result(res) !GCC$ attributes always_inline :: some_i32
        integer(int32), intent(in) :: val
        type(option) :: res
        call res%set_right_i32(val)
    end function some_i32

    pure elemental function unwrap_i32(this) result(val) !GCC$ attributes always_inline :: unwrap_i32
        class(option), intent(in) :: this
        integer(int32) :: val
        if (this%is_none()) then
            if (DO_CHECKS) error stop "unwrap_i32 called on None value"
        end if
        ! We call the specialized getter from either
        val = this%get_right_i32()
    end function unwrap_i32

    pure elemental function unwrap_or_i32(this, default_val) result(val) !GCC$ attributes always_inline :: unwrap_or_i32
        class(option), intent(in) :: this
        integer(int32), intent(in) :: default_val
        integer(int32) :: val
        if (this%is_some()) then
            val = this%get_right_i32()
        else
            val = default_val
        end if
    end function unwrap_or_i32

    pure elemental function some_i64(val) result(res) !GCC$ attributes always_inline :: some_i64
        integer(int64), intent(in) :: val
        type(option) :: res
        call res%set_right_i64(val)
    end function some_i64

    pure elemental function unwrap_i64(this) result(val) !GCC$ attributes always_inline :: unwrap_i64
        class(option), intent(in) :: this
        integer(int64) :: val
        if (this%is_none()) then
            if (DO_CHECKS) error stop "unwrap_i64 called on None value"
        end if
        ! We call the specialized getter from either
        val = this%get_right_i64()
    end function unwrap_i64

    pure elemental function unwrap_or_i64(this, default_val) result(val) !GCC$ attributes always_inline :: unwrap_or_i64
        class(option), intent(in) :: this
        integer(int64), intent(in) :: default_val
        integer(int64) :: val
        if (this%is_some()) then
            val = this%get_right_i64()
        else
            val = default_val
        end if
    end function unwrap_or_i64

    pure elemental function some_i128(val) result(res) !GCC$ attributes always_inline :: some_i128
        integer(int128), intent(in) :: val
        type(option) :: res
        call res%set_right_i128(val)
    end function some_i128

    pure elemental function unwrap_i128(this) result(val) !GCC$ attributes always_inline :: unwrap_i128
        class(option), intent(in) :: this
        integer(int128) :: val
        if (this%is_none()) then
            if (DO_CHECKS) error stop "unwrap_i128 called on None value"
        end if
        ! We call the specialized getter from either
        val = this%get_right_i128()
    end function unwrap_i128

    pure elemental function unwrap_or_i128(this, default_val) result(val) !GCC$ attributes always_inline :: unwrap_or_i128
        class(option), intent(in) :: this
        integer(int128), intent(in) :: default_val
        integer(int128) :: val
        if (this%is_some()) then
            val = this%get_right_i128()
        else
            val = default_val
        end if
    end function unwrap_or_i128

    pure elemental function some_real(val) result(res) !GCC$ attributes always_inline :: some_real
        real, intent(in) :: val
        type(option) :: res
        call res%set_right_real(val)
    end function some_real

    pure elemental function unwrap_real(this) result(val) !GCC$ attributes always_inline :: unwrap_real
        class(option), intent(in) :: this
        real :: val
        if (this%is_none()) then
            if (DO_CHECKS) error stop "unwrap_real called on None value"
        end if
        ! We call the specialized getter from either
        val = this%get_right_real()
    end function unwrap_real

    pure elemental function unwrap_or_real(this, default_val) result(val) !GCC$ attributes always_inline :: unwrap_or_real
        class(option), intent(in) :: this
        real, intent(in) :: default_val
        real :: val
        if (this%is_some()) then
            val = this%get_right_real()
        else
            val = default_val
        end if
    end function unwrap_or_real

    pure elemental function some_r32(val) result(res) !GCC$ attributes always_inline :: some_r32
        real(real32), intent(in) :: val
        type(option) :: res
        call res%set_right_r32(val)
    end function some_r32

    pure elemental function unwrap_r32(this) result(val) !GCC$ attributes always_inline :: unwrap_r32
        class(option), intent(in) :: this
        real(real32) :: val
        if (this%is_none()) then
            if (DO_CHECKS) error stop "unwrap_r32 called on None value"
        end if
        ! We call the specialized getter from either
        val = this%get_right_r32()
    end function unwrap_r32

    pure elemental function unwrap_or_r32(this, default_val) result(val) !GCC$ attributes always_inline :: unwrap_or_r32
        class(option), intent(in) :: this
        real(real32), intent(in) :: default_val
        real(real32) :: val
        if (this%is_some()) then
            val = this%get_right_r32()
        else
            val = default_val
        end if
    end function unwrap_or_r32

    pure elemental function some_r64(val) result(res) !GCC$ attributes always_inline :: some_r64
        real(real64), intent(in) :: val
        type(option) :: res
        call res%set_right_r64(val)
    end function some_r64

    pure elemental function unwrap_r64(this) result(val) !GCC$ attributes always_inline :: unwrap_r64
        class(option), intent(in) :: this
        real(real64) :: val
        if (this%is_none()) then
            if (DO_CHECKS) error stop "unwrap_r64 called on None value"
        end if
        ! We call the specialized getter from either
        val = this%get_right_r64()
    end function unwrap_r64

    pure elemental function unwrap_or_r64(this, default_val) result(val) !GCC$ attributes always_inline :: unwrap_or_r64
        class(option), intent(in) :: this
        real(real64), intent(in) :: default_val
        real(real64) :: val
        if (this%is_some()) then
            val = this%get_right_r64()
        else
            val = default_val
        end if
    end function unwrap_or_r64

    pure elemental function some_r128(val) result(res) !GCC$ attributes always_inline :: some_r128
        real(real128), intent(in) :: val
        type(option) :: res
        call res%set_right_r128(val)
    end function some_r128

    pure elemental function unwrap_r128(this) result(val) !GCC$ attributes always_inline :: unwrap_r128
        class(option), intent(in) :: this
        real(real128) :: val
        if (this%is_none()) then
            if (DO_CHECKS) error stop "unwrap_r128 called on None value"
        end if
        ! We call the specialized getter from either
        val = this%get_right_r128()
    end function unwrap_r128

    pure elemental function unwrap_or_r128(this, default_val) result(val) !GCC$ attributes always_inline :: unwrap_or_r128
        class(option), intent(in) :: this
        real(real128), intent(in) :: default_val
        real(real128) :: val
        if (this%is_some()) then
            val = this%get_right_r128()
        else
            val = default_val
        end if
    end function unwrap_or_r128

    pure elemental function some_r8(val) result(res) !GCC$ attributes always_inline :: some_r8
        real(8), intent(in) :: val
        type(option) :: res
        call res%set_right_r8(val)
    end function some_r8

    pure elemental function unwrap_r8(this) result(val) !GCC$ attributes always_inline :: unwrap_r8
        class(option), intent(in) :: this
        real(8) :: val
        if (this%is_none()) then
            if (DO_CHECKS) error stop "unwrap_r8 called on None value"
        end if
        ! We call the specialized getter from either
        val = this%get_right_r8()
    end function unwrap_r8

    pure elemental function unwrap_or_r8(this, default_val) result(val) !GCC$ attributes always_inline :: unwrap_or_r8
        class(option), intent(in) :: this
        real(8), intent(in) :: default_val
        real(8) :: val
        if (this%is_some()) then
            val = this%get_right_r8()
        else
            val = default_val
        end if
    end function unwrap_or_r8

    pure elemental function some_r16(val) result(res) !GCC$ attributes always_inline :: some_r16
        real(16), intent(in) :: val
        type(option) :: res
        call res%set_right_r16(val)
    end function some_r16

    pure elemental function unwrap_r16(this) result(val) !GCC$ attributes always_inline :: unwrap_r16
        class(option), intent(in) :: this
        real(16) :: val
        if (this%is_none()) then
            if (DO_CHECKS) error stop "unwrap_r16 called on None value"
        end if
        ! We call the specialized getter from either
        val = this%get_right_r16()
    end function unwrap_r16

    pure elemental function unwrap_or_r16(this, default_val) result(val) !GCC$ attributes always_inline :: unwrap_or_r16
        class(option), intent(in) :: this
        real(16), intent(in) :: default_val
        real(16) :: val
        if (this%is_some()) then
            val = this%get_right_r16()
        else
            val = default_val
        end if
    end function unwrap_or_r16

    pure elemental function some_log(val) result(res) !GCC$ attributes always_inline :: some_log
        logical, intent(in) :: val
        type(option) :: res
        call res%set_right_log(val)
    end function some_log

    pure elemental function unwrap_log(this) result(val) !GCC$ attributes always_inline :: unwrap_log
        class(option), intent(in) :: this
        logical :: val
        if (this%is_none()) then
            if (DO_CHECKS) error stop "unwrap_log called on None value"
        end if
        ! We call the specialized getter from either
        val = this%get_right_log()
    end function unwrap_log

    pure elemental function unwrap_or_log(this, default_val) result(val) !GCC$ attributes always_inline :: unwrap_or_log
        class(option), intent(in) :: this
        logical, intent(in) :: default_val
        logical :: val
        if (this%is_some()) then
            val = this%get_right_log()
        else
            val = default_val
        end if
    end function unwrap_or_log

    pure elemental function some_cpx(val) result(res) !GCC$ attributes always_inline :: some_cpx
        complex, intent(in) :: val
        type(option) :: res
        call res%set_right_cpx(val)
    end function some_cpx

    pure elemental function unwrap_cpx(this) result(val) !GCC$ attributes always_inline :: unwrap_cpx
        class(option), intent(in) :: this
        complex :: val
        if (this%is_none()) then
            if (DO_CHECKS) error stop "unwrap_cpx called on None value"
        end if
        ! We call the specialized getter from either
        val = this%get_right_cpx()
    end function unwrap_cpx

    pure elemental function unwrap_or_cpx(this, default_val) result(val) !GCC$ attributes always_inline :: unwrap_or_cpx
        class(option), intent(in) :: this
        complex, intent(in) :: default_val
        complex :: val
        if (this%is_some()) then
            val = this%get_right_cpx()
        else
            val = default_val
        end if
    end function unwrap_or_cpx

    pure elemental function some_c64(val) result(res) !GCC$ attributes always_inline :: some_c64
        complex(real64), intent(in) :: val
        type(option) :: res
        call res%set_right_c64(val)
    end function some_c64

    pure elemental function unwrap_c64(this) result(val) !GCC$ attributes always_inline :: unwrap_c64
        class(option), intent(in) :: this
        complex(real64) :: val
        if (this%is_none()) then
            if (DO_CHECKS) error stop "unwrap_c64 called on None value"
        end if
        ! We call the specialized getter from either
        val = this%get_right_c64()
    end function unwrap_c64

    pure elemental function unwrap_or_c64(this, default_val) result(val) !GCC$ attributes always_inline :: unwrap_or_c64
        class(option), intent(in) :: this
        complex(real64), intent(in) :: default_val
        complex(real64) :: val
        if (this%is_some()) then
            val = this%get_right_c64()
        else
            val = default_val
        end if
    end function unwrap_or_c64

    pure elemental function some_c128(val) result(res) !GCC$ attributes always_inline :: some_c128
        complex(real128), intent(in) :: val
        type(option) :: res
        call res%set_right_c128(val)
    end function some_c128

    pure elemental function unwrap_c128(this) result(val) !GCC$ attributes always_inline :: unwrap_c128
        class(option), intent(in) :: this
        complex(real128) :: val
        if (this%is_none()) then
            if (DO_CHECKS) error stop "unwrap_c128 called on None value"
        end if
        ! We call the specialized getter from either
        val = this%get_right_c128()
    end function unwrap_c128

    pure elemental function unwrap_or_c128(this, default_val) result(val) !GCC$ attributes always_inline :: unwrap_or_c128
        class(option), intent(in) :: this
        complex(real128), intent(in) :: default_val
        complex(real128) :: val
        if (this%is_some()) then
            val = this%get_right_c128()
        else
            val = default_val
        end if
    end function unwrap_or_c128

end module formerr_option
