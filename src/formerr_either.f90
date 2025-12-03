module formerr_either
    use, intrinsic :: iso_fortran_env, only: int8, int16, int32, int64, real32, real64
    implicit none
    private

    public :: either, new_left, new_right
    

    ! Type constants
    integer, parameter :: TYPE_NONE = 0
    integer, parameter :: TYPE_DYN = 1
        integer, parameter :: TYPE_INT = 2
    integer, parameter :: TYPE_I8 = 3
    integer, parameter :: TYPE_I16 = 4
    integer, parameter :: TYPE_I64 = 5
    integer, parameter :: TYPE_REAL = 6
    integer, parameter :: TYPE_R64 = 7
    integer, parameter :: TYPE_LOG = 8
    integer, parameter :: TYPE_CPX = 9
    integer, parameter :: TYPE_C64 = 10


    type :: either
        private
        integer :: active_l = TYPE_NONE
        integer :: active_r = TYPE_NONE

        ! Dynamic fallback
        class(*), allocatable :: l_val_dyn
        class(*), allocatable :: r_val_dyn

        ! Monomorphised storage (embedded, non-allocatable)
                integer :: l_int
        integer :: r_int
        integer(int8) :: l_i8
        integer(int8) :: r_i8
        integer(int16) :: l_i16
        integer(int16) :: r_i16
        integer(int64) :: l_i64
        integer(int64) :: r_i64
        real :: l_real
        real :: r_real
        real(real64) :: l_r64
        real(real64) :: r_r64
        logical :: l_log
        logical :: r_log
        complex :: l_cpx
        complex :: r_cpx
        complex(real64) :: l_c64
        complex(real64) :: r_c64

    contains
        procedure :: is_left
        procedure :: is_right

        procedure :: get_left
        procedure :: get_right

        procedure :: set_left
        procedure :: set_right

        procedure :: move_left
        procedure :: move_right

        ! Helper to clear internal state
        procedure, private :: clear_left
        procedure, private :: clear_right

        ! Specialized Procedures
                procedure :: get_left_int
        procedure :: get_right_int
        procedure :: set_left_int
        procedure :: set_right_int
        procedure :: get_left_i8
        procedure :: get_right_i8
        procedure :: set_left_i8
        procedure :: set_right_i8
        procedure :: get_left_i16
        procedure :: get_right_i16
        procedure :: set_left_i16
        procedure :: set_right_i16
        procedure :: get_left_i64
        procedure :: get_right_i64
        procedure :: set_left_i64
        procedure :: set_right_i64
        procedure :: get_left_real
        procedure :: get_right_real
        procedure :: set_left_real
        procedure :: set_right_real
        procedure :: get_left_r64
        procedure :: get_right_r64
        procedure :: set_left_r64
        procedure :: set_right_r64
        procedure :: get_left_log
        procedure :: get_right_log
        procedure :: set_left_log
        procedure :: set_right_log
        procedure :: get_left_cpx
        procedure :: get_right_cpx
        procedure :: set_left_cpx
        procedure :: set_right_cpx
        procedure :: get_left_c64
        procedure :: get_right_c64
        procedure :: set_left_c64
        procedure :: set_right_c64

    end type either

    interface either
        module procedure new_either
    end interface

contains

    function new_either(left_val, right_val) result(res)
        class(*), intent(in), optional :: left_val
        class(*), intent(in), optional :: right_val
        type(either) :: res

        if (present(left_val) .and. .not. present(right_val)) then
            res = new_left(left_val)
        else if (present(right_val) .and. .not. present(left_val)) then
            res = new_right(right_val)
        else
            error stop "either constructor requires exactly one argument"
        end if
    end function new_either

    function new_left(val) result(res)
        class(*), intent(in) :: val
        type(either) :: res
        call res%set_left(val)
    end function new_left

    function new_right(val) result(res)
        class(*), intent(in) :: val
        type(either) :: res
        call res%set_right(val)
    end function new_right

    logical function is_left(this)
        class(either), intent(in) :: this
        is_left = (this%active_l /= TYPE_NONE)
    end function is_left

    logical function is_right(this)
        class(either), intent(in) :: this
        is_right = (this%active_r /= TYPE_NONE)
    end function is_right

    subroutine clear_left(this)
        class(either), intent(inout) :: this
        if (allocated(this%l_val_dyn)) deallocate(this%l_val_dyn)
        ! Monomorphised fields are embedded, no deallocation needed
        this%active_l = TYPE_NONE
    end subroutine clear_left

    subroutine clear_right(this)
        class(either), intent(inout) :: this
        if (allocated(this%r_val_dyn)) deallocate(this%r_val_dyn)
        ! Monomorphised fields are embedded, no deallocation needed
        this%active_r = TYPE_NONE
    end subroutine clear_right

    function get_left(this) result(ptr)
        class(either), intent(in), target :: this
        class(*), pointer :: ptr

        ptr => null()
        select case (this%active_l)
        case (TYPE_DYN)
            if (allocated(this%l_val_dyn)) ptr => this%l_val_dyn
                case (TYPE_INT)
            ptr => this%l_int
        case (TYPE_I8)
            ptr => this%l_i8
        case (TYPE_I16)
            ptr => this%l_i16
        case (TYPE_I64)
            ptr => this%l_i64
        case (TYPE_REAL)
            ptr => this%l_real
        case (TYPE_R64)
            ptr => this%l_r64
        case (TYPE_LOG)
            ptr => this%l_log
        case (TYPE_CPX)
            ptr => this%l_cpx
        case (TYPE_C64)
            ptr => this%l_c64

        end select
    end function get_left

    function get_right(this) result(ptr)
        class(either), intent(in), target :: this
        class(*), pointer :: ptr

        ptr => null()
        select case (this%active_r)
        case (TYPE_DYN)
            if (allocated(this%r_val_dyn)) ptr => this%r_val_dyn
                case (TYPE_INT)
            ptr => this%r_int
        case (TYPE_I8)
            ptr => this%r_i8
        case (TYPE_I16)
            ptr => this%r_i16
        case (TYPE_I64)
            ptr => this%r_i64
        case (TYPE_REAL)
            ptr => this%r_real
        case (TYPE_R64)
            ptr => this%r_r64
        case (TYPE_LOG)
            ptr => this%r_log
        case (TYPE_CPX)
            ptr => this%r_cpx
        case (TYPE_C64)
            ptr => this%r_c64

        end select
    end function get_right

    subroutine set_left(this, val)
        class(either), intent(inout) :: this
        class(*), intent(in) :: val

        ! Clear both sides (either is exclusive)
        call this%clear_left()
        call this%clear_right()

        select type(val)
                type is (integer)
            this%l_int = val
            this%active_l = TYPE_INT
        type is (integer(int8))
            this%l_i8 = val
            this%active_l = TYPE_I8
        type is (integer(int16))
            this%l_i16 = val
            this%active_l = TYPE_I16
        type is (integer(int64))
            this%l_i64 = val
            this%active_l = TYPE_I64
        type is (real)
            this%l_real = val
            this%active_l = TYPE_REAL
        type is (real(real64))
            this%l_r64 = val
            this%active_l = TYPE_R64
        type is (logical)
            this%l_log = val
            this%active_l = TYPE_LOG
        type is (complex)
            this%l_cpx = val
            this%active_l = TYPE_CPX
        type is (complex(real64))
            this%l_c64 = val
            this%active_l = TYPE_C64

        class default
            allocate(this%l_val_dyn, source=val)
            this%active_l = TYPE_DYN
        end select
    end subroutine set_left

    subroutine set_right(this, val)
        class(either), intent(inout) :: this
        class(*), intent(in) :: val

        call this%clear_left()
        call this%clear_right()

        select type(val)
                type is (integer)
            this%r_int = val
            this%active_r = TYPE_INT
        type is (integer(int8))
            this%r_i8 = val
            this%active_r = TYPE_I8
        type is (integer(int16))
            this%r_i16 = val
            this%active_r = TYPE_I16
        type is (integer(int64))
            this%r_i64 = val
            this%active_r = TYPE_I64
        type is (real)
            this%r_real = val
            this%active_r = TYPE_REAL
        type is (real(real64))
            this%r_r64 = val
            this%active_r = TYPE_R64
        type is (logical)
            this%r_log = val
            this%active_r = TYPE_LOG
        type is (complex)
            this%r_cpx = val
            this%active_r = TYPE_CPX
        type is (complex(real64))
            this%r_c64 = val
            this%active_r = TYPE_C64

        class default
            allocate(this%r_val_dyn, source=val)
            this%active_r = TYPE_DYN
        end select
    end subroutine set_right

    subroutine move_left(this, val)
        class(either), intent(inout) :: this
        class(*), allocatable, intent(inout) :: val

        call this%clear_left()
        call this%clear_right()

        ! If val is not allocated, do nothing (or error?)
        if (.not. allocated(val)) return

        select type(val)
                type is (integer)
            this%l_int = val
            this%active_l = TYPE_INT
        type is (integer(int8))
            this%l_i8 = val
            this%active_l = TYPE_I8
        type is (integer(int16))
            this%l_i16 = val
            this%active_l = TYPE_I16
        type is (integer(int64))
            this%l_i64 = val
            this%active_l = TYPE_I64
        type is (real)
            this%l_real = val
            this%active_l = TYPE_REAL
        type is (real(real64))
            this%l_r64 = val
            this%active_l = TYPE_R64
        type is (logical)
            this%l_log = val
            this%active_l = TYPE_LOG
        type is (complex)
            this%l_cpx = val
            this%active_l = TYPE_CPX
        type is (complex(real64))
            this%l_c64 = val
            this%active_l = TYPE_C64

        class default
            call move_alloc(from=val, to=this%l_val_dyn)
            this%active_l = TYPE_DYN
        end select

        ! Deallocate source 'val' if it was allocated (and not moved via move_alloc)
        if (allocated(val)) deallocate(val)

    end subroutine move_left

    subroutine move_right(this, val)
        class(either), intent(inout) :: this
        class(*), allocatable, intent(inout) :: val

        call this%clear_left()
        call this%clear_right()

        if (.not. allocated(val)) return

        select type(val)
                type is (integer)
            this%r_int = val
            this%active_r = TYPE_INT
        type is (integer(int8))
            this%r_i8 = val
            this%active_r = TYPE_I8
        type is (integer(int16))
            this%r_i16 = val
            this%active_r = TYPE_I16
        type is (integer(int64))
            this%r_i64 = val
            this%active_r = TYPE_I64
        type is (real)
            this%r_real = val
            this%active_r = TYPE_REAL
        type is (real(real64))
            this%r_r64 = val
            this%active_r = TYPE_R64
        type is (logical)
            this%r_log = val
            this%active_r = TYPE_LOG
        type is (complex)
            this%r_cpx = val
            this%active_r = TYPE_CPX
        type is (complex(real64))
            this%r_c64 = val
            this%active_r = TYPE_C64

        class default
            call move_alloc(from=val, to=this%r_val_dyn)
            this%active_r = TYPE_DYN
        end select

        if (allocated(val)) deallocate(val)
    end subroutine move_right

    ! -- Specialized Implementations --
    
    function get_left_int(this) result(val)
        class(either), intent(in) :: this
        integer :: val
        if (this%active_l == TYPE_INT) then
            val = this%l_int
        else
            error stop "get_left_int called on wrong type"
        end if
    end function get_left_int
    function get_right_int(this) result(val)
        class(either), intent(in) :: this
        integer :: val
        if (this%active_r == TYPE_INT) then
            val = this%r_int
        else
            error stop "get_right_int called on wrong type"
        end if
    end function get_right_int
    subroutine set_left_int(this, val)
        class(either), intent(inout) :: this
        integer, intent(in) :: val
        call this%clear_left()
        call this%clear_right()
        this%l_int = val
        this%active_l = TYPE_INT
    end subroutine set_left_int
    subroutine set_right_int(this, val)
        class(either), intent(inout) :: this
        integer, intent(in) :: val
        call this%clear_left()
        call this%clear_right()
        this%r_int = val
        this%active_r = TYPE_INT
    end subroutine set_right_int
    function get_left_i8(this) result(val)
        class(either), intent(in) :: this
        integer(int8) :: val
        if (this%active_l == TYPE_I8) then
            val = this%l_i8
        else
            error stop "get_left_i8 called on wrong type"
        end if
    end function get_left_i8
    function get_right_i8(this) result(val)
        class(either), intent(in) :: this
        integer(int8) :: val
        if (this%active_r == TYPE_I8) then
            val = this%r_i8
        else
            error stop "get_right_i8 called on wrong type"
        end if
    end function get_right_i8
    subroutine set_left_i8(this, val)
        class(either), intent(inout) :: this
        integer(int8), intent(in) :: val
        call this%clear_left()
        call this%clear_right()
        this%l_i8 = val
        this%active_l = TYPE_I8
    end subroutine set_left_i8
    subroutine set_right_i8(this, val)
        class(either), intent(inout) :: this
        integer(int8), intent(in) :: val
        call this%clear_left()
        call this%clear_right()
        this%r_i8 = val
        this%active_r = TYPE_I8
    end subroutine set_right_i8
    function get_left_i16(this) result(val)
        class(either), intent(in) :: this
        integer(int16) :: val
        if (this%active_l == TYPE_I16) then
            val = this%l_i16
        else
            error stop "get_left_i16 called on wrong type"
        end if
    end function get_left_i16
    function get_right_i16(this) result(val)
        class(either), intent(in) :: this
        integer(int16) :: val
        if (this%active_r == TYPE_I16) then
            val = this%r_i16
        else
            error stop "get_right_i16 called on wrong type"
        end if
    end function get_right_i16
    subroutine set_left_i16(this, val)
        class(either), intent(inout) :: this
        integer(int16), intent(in) :: val
        call this%clear_left()
        call this%clear_right()
        this%l_i16 = val
        this%active_l = TYPE_I16
    end subroutine set_left_i16
    subroutine set_right_i16(this, val)
        class(either), intent(inout) :: this
        integer(int16), intent(in) :: val
        call this%clear_left()
        call this%clear_right()
        this%r_i16 = val
        this%active_r = TYPE_I16
    end subroutine set_right_i16
    function get_left_i64(this) result(val)
        class(either), intent(in) :: this
        integer(int64) :: val
        if (this%active_l == TYPE_I64) then
            val = this%l_i64
        else
            error stop "get_left_i64 called on wrong type"
        end if
    end function get_left_i64
    function get_right_i64(this) result(val)
        class(either), intent(in) :: this
        integer(int64) :: val
        if (this%active_r == TYPE_I64) then
            val = this%r_i64
        else
            error stop "get_right_i64 called on wrong type"
        end if
    end function get_right_i64
    subroutine set_left_i64(this, val)
        class(either), intent(inout) :: this
        integer(int64), intent(in) :: val
        call this%clear_left()
        call this%clear_right()
        this%l_i64 = val
        this%active_l = TYPE_I64
    end subroutine set_left_i64
    subroutine set_right_i64(this, val)
        class(either), intent(inout) :: this
        integer(int64), intent(in) :: val
        call this%clear_left()
        call this%clear_right()
        this%r_i64 = val
        this%active_r = TYPE_I64
    end subroutine set_right_i64
    function get_left_real(this) result(val)
        class(either), intent(in) :: this
        real :: val
        if (this%active_l == TYPE_REAL) then
            val = this%l_real
        else
            error stop "get_left_real called on wrong type"
        end if
    end function get_left_real
    function get_right_real(this) result(val)
        class(either), intent(in) :: this
        real :: val
        if (this%active_r == TYPE_REAL) then
            val = this%r_real
        else
            error stop "get_right_real called on wrong type"
        end if
    end function get_right_real
    subroutine set_left_real(this, val)
        class(either), intent(inout) :: this
        real, intent(in) :: val
        call this%clear_left()
        call this%clear_right()
        this%l_real = val
        this%active_l = TYPE_REAL
    end subroutine set_left_real
    subroutine set_right_real(this, val)
        class(either), intent(inout) :: this
        real, intent(in) :: val
        call this%clear_left()
        call this%clear_right()
        this%r_real = val
        this%active_r = TYPE_REAL
    end subroutine set_right_real
    function get_left_r64(this) result(val)
        class(either), intent(in) :: this
        real(real64) :: val
        if (this%active_l == TYPE_R64) then
            val = this%l_r64
        else
            error stop "get_left_r64 called on wrong type"
        end if
    end function get_left_r64
    function get_right_r64(this) result(val)
        class(either), intent(in) :: this
        real(real64) :: val
        if (this%active_r == TYPE_R64) then
            val = this%r_r64
        else
            error stop "get_right_r64 called on wrong type"
        end if
    end function get_right_r64
    subroutine set_left_r64(this, val)
        class(either), intent(inout) :: this
        real(real64), intent(in) :: val
        call this%clear_left()
        call this%clear_right()
        this%l_r64 = val
        this%active_l = TYPE_R64
    end subroutine set_left_r64
    subroutine set_right_r64(this, val)
        class(either), intent(inout) :: this
        real(real64), intent(in) :: val
        call this%clear_left()
        call this%clear_right()
        this%r_r64 = val
        this%active_r = TYPE_R64
    end subroutine set_right_r64
    function get_left_log(this) result(val)
        class(either), intent(in) :: this
        logical :: val
        if (this%active_l == TYPE_LOG) then
            val = this%l_log
        else
            error stop "get_left_log called on wrong type"
        end if
    end function get_left_log
    function get_right_log(this) result(val)
        class(either), intent(in) :: this
        logical :: val
        if (this%active_r == TYPE_LOG) then
            val = this%r_log
        else
            error stop "get_right_log called on wrong type"
        end if
    end function get_right_log
    subroutine set_left_log(this, val)
        class(either), intent(inout) :: this
        logical, intent(in) :: val
        call this%clear_left()
        call this%clear_right()
        this%l_log = val
        this%active_l = TYPE_LOG
    end subroutine set_left_log
    subroutine set_right_log(this, val)
        class(either), intent(inout) :: this
        logical, intent(in) :: val
        call this%clear_left()
        call this%clear_right()
        this%r_log = val
        this%active_r = TYPE_LOG
    end subroutine set_right_log
    function get_left_cpx(this) result(val)
        class(either), intent(in) :: this
        complex :: val
        if (this%active_l == TYPE_CPX) then
            val = this%l_cpx
        else
            error stop "get_left_cpx called on wrong type"
        end if
    end function get_left_cpx
    function get_right_cpx(this) result(val)
        class(either), intent(in) :: this
        complex :: val
        if (this%active_r == TYPE_CPX) then
            val = this%r_cpx
        else
            error stop "get_right_cpx called on wrong type"
        end if
    end function get_right_cpx
    subroutine set_left_cpx(this, val)
        class(either), intent(inout) :: this
        complex, intent(in) :: val
        call this%clear_left()
        call this%clear_right()
        this%l_cpx = val
        this%active_l = TYPE_CPX
    end subroutine set_left_cpx
    subroutine set_right_cpx(this, val)
        class(either), intent(inout) :: this
        complex, intent(in) :: val
        call this%clear_left()
        call this%clear_right()
        this%r_cpx = val
        this%active_r = TYPE_CPX
    end subroutine set_right_cpx
    function get_left_c64(this) result(val)
        class(either), intent(in) :: this
        complex(real64) :: val
        if (this%active_l == TYPE_C64) then
            val = this%l_c64
        else
            error stop "get_left_c64 called on wrong type"
        end if
    end function get_left_c64
    function get_right_c64(this) result(val)
        class(either), intent(in) :: this
        complex(real64) :: val
        if (this%active_r == TYPE_C64) then
            val = this%r_c64
        else
            error stop "get_right_c64 called on wrong type"
        end if
    end function get_right_c64
    subroutine set_left_c64(this, val)
        class(either), intent(inout) :: this
        complex(real64), intent(in) :: val
        call this%clear_left()
        call this%clear_right()
        this%l_c64 = val
        this%active_l = TYPE_C64
    end subroutine set_left_c64
    subroutine set_right_c64(this, val)
        class(either), intent(inout) :: this
        complex(real64), intent(in) :: val
        call this%clear_left()
        call this%clear_right()
        this%r_c64 = val
        this%active_r = TYPE_C64
    end subroutine set_right_c64

end module formerr_either