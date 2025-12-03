module formerr_result
    use formerr_either
    use stdlib_error, only: check
    use, intrinsic :: iso_fortran_env, only: int8, int16, int32, int64, real32, real64
    implicit none
    private

    public :: result_type, ok, err, is_ok, is_err, unwrap, unwrap_err, unwrap_or
    public :: ok_move, err_move, unwrap_move_to_err, unsafe_unwrap_move
        public :: ok_int, err_int
    public :: ok_i8, err_i8
    public :: ok_i16, err_i16
    public :: ok_i64, err_i64
    public :: ok_real, err_real
    public :: ok_r64, err_r64
    public :: ok_log, err_log
    public :: ok_cpx, err_cpx
    public :: ok_c64, err_c64


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
        procedure :: unwrap_i8
        procedure :: unwrap_err_i8
        procedure :: unwrap_i16
        procedure :: unwrap_err_i16
        procedure :: unwrap_i64
        procedure :: unwrap_err_i64
        procedure :: unwrap_real
        procedure :: unwrap_err_real
        procedure :: unwrap_r64
        procedure :: unwrap_err_r64
        procedure :: unwrap_log
        procedure :: unwrap_err_log
        procedure :: unwrap_cpx
        procedure :: unwrap_err_cpx
        procedure :: unwrap_c64
        procedure :: unwrap_err_c64

    end type result_type

contains

    function ok(val) result(res)
        class(*), intent(in) :: val
        type(result_type) :: res
        call res%set_right(val)
    end function ok

    function err(val) result(res)
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

    logical function is_ok(this)
        class(result_type), intent(in) :: this
        is_ok = this%is_right()
    end function is_ok

    logical function is_err(this)
        class(result_type), intent(in) :: this
        is_err = this%is_left()
    end function is_err

    function unwrap(this) result(ptr)
        class(result_type), intent(in), target :: this
        class(*), pointer :: ptr

        call check(this%is_ok(), "unwrap called on Err value")

        ptr => this%get_right()
    end function unwrap

    function unwrap_err(this) result(ptr)
        class(result_type), intent(in), target :: this
        class(*), pointer :: ptr

        call check(this%is_err(), "unwrap_err called on Ok value")

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

        call check(this%is_ok(), "unwrap_move_to_err called on Err value")

        ! 1. Allocate the replacement error first to ensure safety if alloc fails
        allocate(temp_err, source=replacement_err)

        ! 2. Perform the swap
        ! Move the Right (Ok) value to dest (leaves Right unallocated)
        call this%move_right(dest)

        ! 3. Restore invariant: Move the temp error into Left
        call this%move_left(temp_err)
    end subroutine unwrap_move_to_err

    subroutine unsafe_unwrap_move(this, dest)
        class(result_type), intent(inout) :: this
        class(*), allocatable, intent(out) :: dest

        call check(this%is_ok(), "unsafe_unwrap_move called on Err value")

        ! Zero-cost pointer swap. No allocations.
        call this%move_right(dest)

        ! 'this' is now a Zombie (Left=Unallocated, Right=Unallocated).
        ! This breaks our safety contract, but this move is *very* fast
    end subroutine unsafe_unwrap_move

    ! -- Specialized Implementations --
    
    function ok_int(val) result(res)
        integer, intent(in) :: val
        type(result_type) :: res
        call res%set_right_int(val)
    end function ok_int

    function err_int(val) result(res)
        integer, intent(in) :: val
        type(result_type) :: res
        call res%set_left_int(val)
    end function err_int

    function unwrap_int(this) result(val)
        class(result_type), intent(in) :: this
        integer :: val
        if (this%is_err()) error stop "unwrap_int called on Err value"
        val = this%get_right_int()
    end function unwrap_int

    function unwrap_err_int(this) result(val)
        class(result_type), intent(in) :: this
        integer :: val
        if (this%is_ok()) error stop "unwrap_err_int called on Ok value"
        val = this%get_left_int()
    end function unwrap_err_int

    function ok_i8(val) result(res)
        integer(int8), intent(in) :: val
        type(result_type) :: res
        call res%set_right_i8(val)
    end function ok_i8

    function err_i8(val) result(res)
        integer(int8), intent(in) :: val
        type(result_type) :: res
        call res%set_left_i8(val)
    end function err_i8

    function unwrap_i8(this) result(val)
        class(result_type), intent(in) :: this
        integer(int8) :: val
        if (this%is_err()) error stop "unwrap_i8 called on Err value"
        val = this%get_right_i8()
    end function unwrap_i8

    function unwrap_err_i8(this) result(val)
        class(result_type), intent(in) :: this
        integer(int8) :: val
        if (this%is_ok()) error stop "unwrap_err_i8 called on Ok value"
        val = this%get_left_i8()
    end function unwrap_err_i8

    function ok_i16(val) result(res)
        integer(int16), intent(in) :: val
        type(result_type) :: res
        call res%set_right_i16(val)
    end function ok_i16

    function err_i16(val) result(res)
        integer(int16), intent(in) :: val
        type(result_type) :: res
        call res%set_left_i16(val)
    end function err_i16

    function unwrap_i16(this) result(val)
        class(result_type), intent(in) :: this
        integer(int16) :: val
        if (this%is_err()) error stop "unwrap_i16 called on Err value"
        val = this%get_right_i16()
    end function unwrap_i16

    function unwrap_err_i16(this) result(val)
        class(result_type), intent(in) :: this
        integer(int16) :: val
        if (this%is_ok()) error stop "unwrap_err_i16 called on Ok value"
        val = this%get_left_i16()
    end function unwrap_err_i16

    function ok_i64(val) result(res)
        integer(int64), intent(in) :: val
        type(result_type) :: res
        call res%set_right_i64(val)
    end function ok_i64

    function err_i64(val) result(res)
        integer(int64), intent(in) :: val
        type(result_type) :: res
        call res%set_left_i64(val)
    end function err_i64

    function unwrap_i64(this) result(val)
        class(result_type), intent(in) :: this
        integer(int64) :: val
        if (this%is_err()) error stop "unwrap_i64 called on Err value"
        val = this%get_right_i64()
    end function unwrap_i64

    function unwrap_err_i64(this) result(val)
        class(result_type), intent(in) :: this
        integer(int64) :: val
        if (this%is_ok()) error stop "unwrap_err_i64 called on Ok value"
        val = this%get_left_i64()
    end function unwrap_err_i64

    function ok_real(val) result(res)
        real, intent(in) :: val
        type(result_type) :: res
        call res%set_right_real(val)
    end function ok_real

    function err_real(val) result(res)
        real, intent(in) :: val
        type(result_type) :: res
        call res%set_left_real(val)
    end function err_real

    function unwrap_real(this) result(val)
        class(result_type), intent(in) :: this
        real :: val
        if (this%is_err()) error stop "unwrap_real called on Err value"
        val = this%get_right_real()
    end function unwrap_real

    function unwrap_err_real(this) result(val)
        class(result_type), intent(in) :: this
        real :: val
        if (this%is_ok()) error stop "unwrap_err_real called on Ok value"
        val = this%get_left_real()
    end function unwrap_err_real

    function ok_r64(val) result(res)
        real(real64), intent(in) :: val
        type(result_type) :: res
        call res%set_right_r64(val)
    end function ok_r64

    function err_r64(val) result(res)
        real(real64), intent(in) :: val
        type(result_type) :: res
        call res%set_left_r64(val)
    end function err_r64

    function unwrap_r64(this) result(val)
        class(result_type), intent(in) :: this
        real(real64) :: val
        if (this%is_err()) error stop "unwrap_r64 called on Err value"
        val = this%get_right_r64()
    end function unwrap_r64

    function unwrap_err_r64(this) result(val)
        class(result_type), intent(in) :: this
        real(real64) :: val
        if (this%is_ok()) error stop "unwrap_err_r64 called on Ok value"
        val = this%get_left_r64()
    end function unwrap_err_r64

    function ok_log(val) result(res)
        logical, intent(in) :: val
        type(result_type) :: res
        call res%set_right_log(val)
    end function ok_log

    function err_log(val) result(res)
        logical, intent(in) :: val
        type(result_type) :: res
        call res%set_left_log(val)
    end function err_log

    function unwrap_log(this) result(val)
        class(result_type), intent(in) :: this
        logical :: val
        if (this%is_err()) error stop "unwrap_log called on Err value"
        val = this%get_right_log()
    end function unwrap_log

    function unwrap_err_log(this) result(val)
        class(result_type), intent(in) :: this
        logical :: val
        if (this%is_ok()) error stop "unwrap_err_log called on Ok value"
        val = this%get_left_log()
    end function unwrap_err_log

    function ok_cpx(val) result(res)
        complex, intent(in) :: val
        type(result_type) :: res
        call res%set_right_cpx(val)
    end function ok_cpx

    function err_cpx(val) result(res)
        complex, intent(in) :: val
        type(result_type) :: res
        call res%set_left_cpx(val)
    end function err_cpx

    function unwrap_cpx(this) result(val)
        class(result_type), intent(in) :: this
        complex :: val
        if (this%is_err()) error stop "unwrap_cpx called on Err value"
        val = this%get_right_cpx()
    end function unwrap_cpx

    function unwrap_err_cpx(this) result(val)
        class(result_type), intent(in) :: this
        complex :: val
        if (this%is_ok()) error stop "unwrap_err_cpx called on Ok value"
        val = this%get_left_cpx()
    end function unwrap_err_cpx

    function ok_c64(val) result(res)
        complex(real64), intent(in) :: val
        type(result_type) :: res
        call res%set_right_c64(val)
    end function ok_c64

    function err_c64(val) result(res)
        complex(real64), intent(in) :: val
        type(result_type) :: res
        call res%set_left_c64(val)
    end function err_c64

    function unwrap_c64(this) result(val)
        class(result_type), intent(in) :: this
        complex(real64) :: val
        if (this%is_err()) error stop "unwrap_c64 called on Err value"
        val = this%get_right_c64()
    end function unwrap_c64

    function unwrap_err_c64(this) result(val)
        class(result_type), intent(in) :: this
        complex(real64) :: val
        if (this%is_ok()) error stop "unwrap_err_c64 called on Ok value"
        val = this%get_left_c64()
    end function unwrap_err_c64


end module formerr_result