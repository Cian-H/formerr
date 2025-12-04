module formerr_either
    use, intrinsic :: iso_fortran_env, only: int8, int16, int32, int64, real32, real64
    implicit none
    private

    public :: either, new_left, new_right
    

    ! Type constants
    integer, parameter :: TYPE_NONE = 0
    integer, parameter :: TYPE_DYN = 1
    logical, parameter :: DO_CHECKS = .false.
        integer, parameter :: STORAGE_SIZE = 16
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
                integer(int8) :: l_bytes(STORAGE_SIZE)
        integer(int8) :: r_bytes(STORAGE_SIZE)

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
            if (DO_CHECKS) error stop "either constructor requires exactly one argument"
        end if
    end function new_either

    pure function new_left(val) result(res)
        class(*), intent(in) :: val
        type(either) :: res
        call res%set_left(val)
    end function new_left

    pure function new_right(val) result(res)
        class(*), intent(in) :: val
        type(either) :: res
        call res%set_right(val)
    end function new_right

    pure elemental logical function is_left(this)
        class(either), intent(in) :: this
        is_left = (this%active_l /= TYPE_NONE)
    end function is_left

    pure elemental logical function is_right(this)
        class(either), intent(in) :: this
        is_right = (this%active_r /= TYPE_NONE)
    end function is_right

    pure subroutine clear_left(this)
        class(either), intent(inout) :: this
        if (allocated(this%l_val_dyn)) deallocate(this%l_val_dyn)
        ! Monomorphised fields are embedded, no deallocation needed
        this%active_l = TYPE_NONE
    end subroutine clear_left

    pure subroutine clear_right(this)
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
            if (DO_CHECKS) error stop "get_left (Generic) cannot return pointer to Specialized (Union) value (integer)."
        case (TYPE_I8)
            if (DO_CHECKS) error stop "get_left (Generic) cannot return pointer to Specialized (Union) value (integer(int8))."
        case (TYPE_I16)
            if (DO_CHECKS) error stop "get_left (Generic) cannot return pointer to Specialized (Union) value (integer(int16))."
        case (TYPE_I64)
            if (DO_CHECKS) error stop "get_left (Generic) cannot return pointer to Specialized (Union) value (integer(int64))."
        case (TYPE_REAL)
            if (DO_CHECKS) error stop "get_left (Generic) cannot return pointer to Specialized (Union) value (real)."
        case (TYPE_R64)
            if (DO_CHECKS) error stop "get_left (Generic) cannot return pointer to Specialized (Union) value (real(real64))."
        case (TYPE_LOG)
            if (DO_CHECKS) error stop "get_left (Generic) cannot return pointer to Specialized (Union) value (logical)."
        case (TYPE_CPX)
            if (DO_CHECKS) error stop "get_left (Generic) cannot return pointer to Specialized (Union) value (complex)."
        case (TYPE_C64)
            if (DO_CHECKS) error stop "get_left (Generic) cannot return pointer to Specialized (Union) value (complex(real64))."

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
            if (DO_CHECKS) error stop "get_right (Generic) cannot return pointer to Specialized (Union) value (integer)."
        case (TYPE_I8)
            if (DO_CHECKS) error stop "get_right (Generic) cannot return pointer to Specialized (Union) value (integer(int8))."
        case (TYPE_I16)
            if (DO_CHECKS) error stop "get_right (Generic) cannot return pointer to Specialized (Union) value (integer(int16))."
        case (TYPE_I64)
            if (DO_CHECKS) error stop "get_right (Generic) cannot return pointer to Specialized (Union) value (integer(int64))."
        case (TYPE_REAL)
            if (DO_CHECKS) error stop "get_right (Generic) cannot return pointer to Specialized (Union) value (real)."
        case (TYPE_R64)
            if (DO_CHECKS) error stop "get_right (Generic) cannot return pointer to Specialized (Union) value (real(real64))."
        case (TYPE_LOG)
            if (DO_CHECKS) error stop "get_right (Generic) cannot return pointer to Specialized (Union) value (logical)."
        case (TYPE_CPX)
            if (DO_CHECKS) error stop "get_right (Generic) cannot return pointer to Specialized (Union) value (complex)."
        case (TYPE_C64)
            if (DO_CHECKS) error stop "get_right (Generic) cannot return pointer to Specialized (Union) value (complex(real64))."

        end select
    end function get_right

    pure subroutine set_left(this, val)
        class(either), intent(inout) :: this
        class(*), intent(in) :: val

        ! Clear both sides (either is exclusive)
        call this%clear_left()
        call this%clear_right()

        select type(val)
        
        class default
            allocate(this%l_val_dyn, source=val)
            this%active_l = TYPE_DYN
        end select
    end subroutine set_left

    pure subroutine set_right(this, val)
        class(either), intent(inout) :: this
        class(*), intent(in) :: val

        call this%clear_left()
        call this%clear_right()

        select type(val)
        
        class default
            allocate(this%r_val_dyn, source=val)
            this%active_r = TYPE_DYN
        end select
    end subroutine set_right

    pure subroutine move_left(this, val)
        class(either), intent(inout) :: this
        class(*), allocatable, intent(inout) :: val

        call this%clear_left()
        call this%clear_right()

        ! If val is not allocated, do nothing (or error?)
        if (.not. allocated(val)) return

        select type(val)
        
        class default
            call move_alloc(from=val, to=this%l_val_dyn)
            this%active_l = TYPE_DYN
        end select

        ! Deallocate source 'val' if it was allocated (and not moved via move_alloc)
        if (allocated(val)) deallocate(val)

    end subroutine move_left

    pure subroutine move_right(this, val)
        class(either), intent(inout) :: this
        class(*), allocatable, intent(inout) :: val

        call this%clear_left()
        call this%clear_right()

        if (.not. allocated(val)) return

        select type(val)
        
        class default
            call move_alloc(from=val, to=this%r_val_dyn)
            this%active_r = TYPE_DYN
        end select

        if (allocated(val)) deallocate(val)
    end subroutine move_right

    ! -- Specialized Implementations --
    
    pure elemental function get_left_int(this) result(val) !GCC$ attributes always_inline :: get_left_int
        class(either), intent(in) :: this
        integer :: val
        if (this%active_l == TYPE_INT) then
            val = transfer(this%l_bytes, val)
        else if (this%active_l == TYPE_DYN) then
             select type (v => this%l_val_dyn)
             type is (integer)
                 val = v
             class default
                 if (DO_CHECKS) error stop "get_left_int type mismatch in dynamic storage"
             end select
        else
            if (DO_CHECKS) error stop "get_left_int called on wrong type"
        end if
    end function get_left_int
    pure elemental function get_right_int(this) result(val) !GCC$ attributes always_inline :: get_right_int
        class(either), intent(in) :: this
        integer :: val
        if (this%active_r == TYPE_INT) then
            val = transfer(this%r_bytes, val)
        else if (this%active_r == TYPE_DYN) then
             select type (v => this%r_val_dyn)
             type is (integer)
                 val = v
             class default
                 if (DO_CHECKS) error stop "get_right_int type mismatch in dynamic storage"
             end select
        else
            if (DO_CHECKS) error stop "get_right_int called on wrong type"
        end if
    end function get_right_int
    pure elemental subroutine set_left_int(this, val) !GCC$ attributes always_inline :: set_left_int
        class(either), intent(inout) :: this
        integer, intent(in) :: val
        ! Inlined clear_left (optimized: no active_l assignment)
        if (allocated(this%l_val_dyn)) deallocate(this%l_val_dyn)
        ! Inlined clear_right
        if (allocated(this%r_val_dyn)) deallocate(this%r_val_dyn)
        this%active_r = TYPE_NONE
        
        this%l_bytes = transfer(val, this%l_bytes)
        this%active_l = TYPE_INT
    end subroutine set_left_int
    pure elemental subroutine set_right_int(this, val) !GCC$ attributes always_inline :: set_right_int
        class(either), intent(inout) :: this
        integer, intent(in) :: val
        ! Inlined clear_left
        if (allocated(this%l_val_dyn)) deallocate(this%l_val_dyn)
        this%active_l = TYPE_NONE
        ! Inlined clear_right (optimized: no active_r assignment)
        if (allocated(this%r_val_dyn)) deallocate(this%r_val_dyn)
        
        this%r_bytes = transfer(val, this%r_bytes)
        this%active_r = TYPE_INT
    end subroutine set_right_int
    pure elemental function get_left_i8(this) result(val) !GCC$ attributes always_inline :: get_left_i8
        class(either), intent(in) :: this
        integer(int8) :: val
        if (this%active_l == TYPE_I8) then
            val = transfer(this%l_bytes, val)
        else if (this%active_l == TYPE_DYN) then
             select type (v => this%l_val_dyn)
             type is (integer(int8))
                 val = v
             class default
                 if (DO_CHECKS) error stop "get_left_i8 type mismatch in dynamic storage"
             end select
        else
            if (DO_CHECKS) error stop "get_left_i8 called on wrong type"
        end if
    end function get_left_i8
    pure elemental function get_right_i8(this) result(val) !GCC$ attributes always_inline :: get_right_i8
        class(either), intent(in) :: this
        integer(int8) :: val
        if (this%active_r == TYPE_I8) then
            val = transfer(this%r_bytes, val)
        else if (this%active_r == TYPE_DYN) then
             select type (v => this%r_val_dyn)
             type is (integer(int8))
                 val = v
             class default
                 if (DO_CHECKS) error stop "get_right_i8 type mismatch in dynamic storage"
             end select
        else
            if (DO_CHECKS) error stop "get_right_i8 called on wrong type"
        end if
    end function get_right_i8
    pure elemental subroutine set_left_i8(this, val) !GCC$ attributes always_inline :: set_left_i8
        class(either), intent(inout) :: this
        integer(int8), intent(in) :: val
        ! Inlined clear_left (optimized: no active_l assignment)
        if (allocated(this%l_val_dyn)) deallocate(this%l_val_dyn)
        ! Inlined clear_right
        if (allocated(this%r_val_dyn)) deallocate(this%r_val_dyn)
        this%active_r = TYPE_NONE
        
        this%l_bytes = transfer(val, this%l_bytes)
        this%active_l = TYPE_I8
    end subroutine set_left_i8
    pure elemental subroutine set_right_i8(this, val) !GCC$ attributes always_inline :: set_right_i8
        class(either), intent(inout) :: this
        integer(int8), intent(in) :: val
        ! Inlined clear_left
        if (allocated(this%l_val_dyn)) deallocate(this%l_val_dyn)
        this%active_l = TYPE_NONE
        ! Inlined clear_right (optimized: no active_r assignment)
        if (allocated(this%r_val_dyn)) deallocate(this%r_val_dyn)
        
        this%r_bytes = transfer(val, this%r_bytes)
        this%active_r = TYPE_I8
    end subroutine set_right_i8
    pure elemental function get_left_i16(this) result(val) !GCC$ attributes always_inline :: get_left_i16
        class(either), intent(in) :: this
        integer(int16) :: val
        if (this%active_l == TYPE_I16) then
            val = transfer(this%l_bytes, val)
        else if (this%active_l == TYPE_DYN) then
             select type (v => this%l_val_dyn)
             type is (integer(int16))
                 val = v
             class default
                 if (DO_CHECKS) error stop "get_left_i16 type mismatch in dynamic storage"
             end select
        else
            if (DO_CHECKS) error stop "get_left_i16 called on wrong type"
        end if
    end function get_left_i16
    pure elemental function get_right_i16(this) result(val) !GCC$ attributes always_inline :: get_right_i16
        class(either), intent(in) :: this
        integer(int16) :: val
        if (this%active_r == TYPE_I16) then
            val = transfer(this%r_bytes, val)
        else if (this%active_r == TYPE_DYN) then
             select type (v => this%r_val_dyn)
             type is (integer(int16))
                 val = v
             class default
                 if (DO_CHECKS) error stop "get_right_i16 type mismatch in dynamic storage"
             end select
        else
            if (DO_CHECKS) error stop "get_right_i16 called on wrong type"
        end if
    end function get_right_i16
    pure elemental subroutine set_left_i16(this, val) !GCC$ attributes always_inline :: set_left_i16
        class(either), intent(inout) :: this
        integer(int16), intent(in) :: val
        ! Inlined clear_left (optimized: no active_l assignment)
        if (allocated(this%l_val_dyn)) deallocate(this%l_val_dyn)
        ! Inlined clear_right
        if (allocated(this%r_val_dyn)) deallocate(this%r_val_dyn)
        this%active_r = TYPE_NONE
        
        this%l_bytes = transfer(val, this%l_bytes)
        this%active_l = TYPE_I16
    end subroutine set_left_i16
    pure elemental subroutine set_right_i16(this, val) !GCC$ attributes always_inline :: set_right_i16
        class(either), intent(inout) :: this
        integer(int16), intent(in) :: val
        ! Inlined clear_left
        if (allocated(this%l_val_dyn)) deallocate(this%l_val_dyn)
        this%active_l = TYPE_NONE
        ! Inlined clear_right (optimized: no active_r assignment)
        if (allocated(this%r_val_dyn)) deallocate(this%r_val_dyn)
        
        this%r_bytes = transfer(val, this%r_bytes)
        this%active_r = TYPE_I16
    end subroutine set_right_i16
    pure elemental function get_left_i64(this) result(val) !GCC$ attributes always_inline :: get_left_i64
        class(either), intent(in) :: this
        integer(int64) :: val
        if (this%active_l == TYPE_I64) then
            val = transfer(this%l_bytes, val)
        else if (this%active_l == TYPE_DYN) then
             select type (v => this%l_val_dyn)
             type is (integer(int64))
                 val = v
             class default
                 if (DO_CHECKS) error stop "get_left_i64 type mismatch in dynamic storage"
             end select
        else
            if (DO_CHECKS) error stop "get_left_i64 called on wrong type"
        end if
    end function get_left_i64
    pure elemental function get_right_i64(this) result(val) !GCC$ attributes always_inline :: get_right_i64
        class(either), intent(in) :: this
        integer(int64) :: val
        if (this%active_r == TYPE_I64) then
            val = transfer(this%r_bytes, val)
        else if (this%active_r == TYPE_DYN) then
             select type (v => this%r_val_dyn)
             type is (integer(int64))
                 val = v
             class default
                 if (DO_CHECKS) error stop "get_right_i64 type mismatch in dynamic storage"
             end select
        else
            if (DO_CHECKS) error stop "get_right_i64 called on wrong type"
        end if
    end function get_right_i64
    pure elemental subroutine set_left_i64(this, val) !GCC$ attributes always_inline :: set_left_i64
        class(either), intent(inout) :: this
        integer(int64), intent(in) :: val
        ! Inlined clear_left (optimized: no active_l assignment)
        if (allocated(this%l_val_dyn)) deallocate(this%l_val_dyn)
        ! Inlined clear_right
        if (allocated(this%r_val_dyn)) deallocate(this%r_val_dyn)
        this%active_r = TYPE_NONE
        
        this%l_bytes = transfer(val, this%l_bytes)
        this%active_l = TYPE_I64
    end subroutine set_left_i64
    pure elemental subroutine set_right_i64(this, val) !GCC$ attributes always_inline :: set_right_i64
        class(either), intent(inout) :: this
        integer(int64), intent(in) :: val
        ! Inlined clear_left
        if (allocated(this%l_val_dyn)) deallocate(this%l_val_dyn)
        this%active_l = TYPE_NONE
        ! Inlined clear_right (optimized: no active_r assignment)
        if (allocated(this%r_val_dyn)) deallocate(this%r_val_dyn)
        
        this%r_bytes = transfer(val, this%r_bytes)
        this%active_r = TYPE_I64
    end subroutine set_right_i64
    pure elemental function get_left_real(this) result(val) !GCC$ attributes always_inline :: get_left_real
        class(either), intent(in) :: this
        real :: val
        if (this%active_l == TYPE_REAL) then
            val = transfer(this%l_bytes, val)
        else if (this%active_l == TYPE_DYN) then
             select type (v => this%l_val_dyn)
             type is (real)
                 val = v
             class default
                 if (DO_CHECKS) error stop "get_left_real type mismatch in dynamic storage"
             end select
        else
            if (DO_CHECKS) error stop "get_left_real called on wrong type"
        end if
    end function get_left_real
    pure elemental function get_right_real(this) result(val) !GCC$ attributes always_inline :: get_right_real
        class(either), intent(in) :: this
        real :: val
        if (this%active_r == TYPE_REAL) then
            val = transfer(this%r_bytes, val)
        else if (this%active_r == TYPE_DYN) then
             select type (v => this%r_val_dyn)
             type is (real)
                 val = v
             class default
                 if (DO_CHECKS) error stop "get_right_real type mismatch in dynamic storage"
             end select
        else
            if (DO_CHECKS) error stop "get_right_real called on wrong type"
        end if
    end function get_right_real
    pure elemental subroutine set_left_real(this, val) !GCC$ attributes always_inline :: set_left_real
        class(either), intent(inout) :: this
        real, intent(in) :: val
        ! Inlined clear_left (optimized: no active_l assignment)
        if (allocated(this%l_val_dyn)) deallocate(this%l_val_dyn)
        ! Inlined clear_right
        if (allocated(this%r_val_dyn)) deallocate(this%r_val_dyn)
        this%active_r = TYPE_NONE
        
        this%l_bytes = transfer(val, this%l_bytes)
        this%active_l = TYPE_REAL
    end subroutine set_left_real
    pure elemental subroutine set_right_real(this, val) !GCC$ attributes always_inline :: set_right_real
        class(either), intent(inout) :: this
        real, intent(in) :: val
        ! Inlined clear_left
        if (allocated(this%l_val_dyn)) deallocate(this%l_val_dyn)
        this%active_l = TYPE_NONE
        ! Inlined clear_right (optimized: no active_r assignment)
        if (allocated(this%r_val_dyn)) deallocate(this%r_val_dyn)
        
        this%r_bytes = transfer(val, this%r_bytes)
        this%active_r = TYPE_REAL
    end subroutine set_right_real
    pure elemental function get_left_r64(this) result(val) !GCC$ attributes always_inline :: get_left_r64
        class(either), intent(in) :: this
        real(real64) :: val
        if (this%active_l == TYPE_R64) then
            val = transfer(this%l_bytes, val)
        else if (this%active_l == TYPE_DYN) then
             select type (v => this%l_val_dyn)
             type is (real(real64))
                 val = v
             class default
                 if (DO_CHECKS) error stop "get_left_r64 type mismatch in dynamic storage"
             end select
        else
            if (DO_CHECKS) error stop "get_left_r64 called on wrong type"
        end if
    end function get_left_r64
    pure elemental function get_right_r64(this) result(val) !GCC$ attributes always_inline :: get_right_r64
        class(either), intent(in) :: this
        real(real64) :: val
        if (this%active_r == TYPE_R64) then
            val = transfer(this%r_bytes, val)
        else if (this%active_r == TYPE_DYN) then
             select type (v => this%r_val_dyn)
             type is (real(real64))
                 val = v
             class default
                 if (DO_CHECKS) error stop "get_right_r64 type mismatch in dynamic storage"
             end select
        else
            if (DO_CHECKS) error stop "get_right_r64 called on wrong type"
        end if
    end function get_right_r64
    pure elemental subroutine set_left_r64(this, val) !GCC$ attributes always_inline :: set_left_r64
        class(either), intent(inout) :: this
        real(real64), intent(in) :: val
        ! Inlined clear_left (optimized: no active_l assignment)
        if (allocated(this%l_val_dyn)) deallocate(this%l_val_dyn)
        ! Inlined clear_right
        if (allocated(this%r_val_dyn)) deallocate(this%r_val_dyn)
        this%active_r = TYPE_NONE
        
        this%l_bytes = transfer(val, this%l_bytes)
        this%active_l = TYPE_R64
    end subroutine set_left_r64
    pure elemental subroutine set_right_r64(this, val) !GCC$ attributes always_inline :: set_right_r64
        class(either), intent(inout) :: this
        real(real64), intent(in) :: val
        ! Inlined clear_left
        if (allocated(this%l_val_dyn)) deallocate(this%l_val_dyn)
        this%active_l = TYPE_NONE
        ! Inlined clear_right (optimized: no active_r assignment)
        if (allocated(this%r_val_dyn)) deallocate(this%r_val_dyn)
        
        this%r_bytes = transfer(val, this%r_bytes)
        this%active_r = TYPE_R64
    end subroutine set_right_r64
    pure elemental function get_left_log(this) result(val) !GCC$ attributes always_inline :: get_left_log
        class(either), intent(in) :: this
        logical :: val
        if (this%active_l == TYPE_LOG) then
            val = transfer(this%l_bytes, val)
        else if (this%active_l == TYPE_DYN) then
             select type (v => this%l_val_dyn)
             type is (logical)
                 val = v
             class default
                 if (DO_CHECKS) error stop "get_left_log type mismatch in dynamic storage"
             end select
        else
            if (DO_CHECKS) error stop "get_left_log called on wrong type"
        end if
    end function get_left_log
    pure elemental function get_right_log(this) result(val) !GCC$ attributes always_inline :: get_right_log
        class(either), intent(in) :: this
        logical :: val
        if (this%active_r == TYPE_LOG) then
            val = transfer(this%r_bytes, val)
        else if (this%active_r == TYPE_DYN) then
             select type (v => this%r_val_dyn)
             type is (logical)
                 val = v
             class default
                 if (DO_CHECKS) error stop "get_right_log type mismatch in dynamic storage"
             end select
        else
            if (DO_CHECKS) error stop "get_right_log called on wrong type"
        end if
    end function get_right_log
    pure elemental subroutine set_left_log(this, val) !GCC$ attributes always_inline :: set_left_log
        class(either), intent(inout) :: this
        logical, intent(in) :: val
        ! Inlined clear_left (optimized: no active_l assignment)
        if (allocated(this%l_val_dyn)) deallocate(this%l_val_dyn)
        ! Inlined clear_right
        if (allocated(this%r_val_dyn)) deallocate(this%r_val_dyn)
        this%active_r = TYPE_NONE
        
        this%l_bytes = transfer(val, this%l_bytes)
        this%active_l = TYPE_LOG
    end subroutine set_left_log
    pure elemental subroutine set_right_log(this, val) !GCC$ attributes always_inline :: set_right_log
        class(either), intent(inout) :: this
        logical, intent(in) :: val
        ! Inlined clear_left
        if (allocated(this%l_val_dyn)) deallocate(this%l_val_dyn)
        this%active_l = TYPE_NONE
        ! Inlined clear_right (optimized: no active_r assignment)
        if (allocated(this%r_val_dyn)) deallocate(this%r_val_dyn)
        
        this%r_bytes = transfer(val, this%r_bytes)
        this%active_r = TYPE_LOG
    end subroutine set_right_log
    pure elemental function get_left_cpx(this) result(val) !GCC$ attributes always_inline :: get_left_cpx
        class(either), intent(in) :: this
        complex :: val
        if (this%active_l == TYPE_CPX) then
            val = transfer(this%l_bytes, val)
        else if (this%active_l == TYPE_DYN) then
             select type (v => this%l_val_dyn)
             type is (complex)
                 val = v
             class default
                 if (DO_CHECKS) error stop "get_left_cpx type mismatch in dynamic storage"
             end select
        else
            if (DO_CHECKS) error stop "get_left_cpx called on wrong type"
        end if
    end function get_left_cpx
    pure elemental function get_right_cpx(this) result(val) !GCC$ attributes always_inline :: get_right_cpx
        class(either), intent(in) :: this
        complex :: val
        if (this%active_r == TYPE_CPX) then
            val = transfer(this%r_bytes, val)
        else if (this%active_r == TYPE_DYN) then
             select type (v => this%r_val_dyn)
             type is (complex)
                 val = v
             class default
                 if (DO_CHECKS) error stop "get_right_cpx type mismatch in dynamic storage"
             end select
        else
            if (DO_CHECKS) error stop "get_right_cpx called on wrong type"
        end if
    end function get_right_cpx
    pure elemental subroutine set_left_cpx(this, val) !GCC$ attributes always_inline :: set_left_cpx
        class(either), intent(inout) :: this
        complex, intent(in) :: val
        ! Inlined clear_left (optimized: no active_l assignment)
        if (allocated(this%l_val_dyn)) deallocate(this%l_val_dyn)
        ! Inlined clear_right
        if (allocated(this%r_val_dyn)) deallocate(this%r_val_dyn)
        this%active_r = TYPE_NONE
        
        this%l_bytes = transfer(val, this%l_bytes)
        this%active_l = TYPE_CPX
    end subroutine set_left_cpx
    pure elemental subroutine set_right_cpx(this, val) !GCC$ attributes always_inline :: set_right_cpx
        class(either), intent(inout) :: this
        complex, intent(in) :: val
        ! Inlined clear_left
        if (allocated(this%l_val_dyn)) deallocate(this%l_val_dyn)
        this%active_l = TYPE_NONE
        ! Inlined clear_right (optimized: no active_r assignment)
        if (allocated(this%r_val_dyn)) deallocate(this%r_val_dyn)
        
        this%r_bytes = transfer(val, this%r_bytes)
        this%active_r = TYPE_CPX
    end subroutine set_right_cpx
    pure elemental function get_left_c64(this) result(val) !GCC$ attributes always_inline :: get_left_c64
        class(either), intent(in) :: this
        complex(real64) :: val
        if (this%active_l == TYPE_C64) then
            val = transfer(this%l_bytes, val)
        else if (this%active_l == TYPE_DYN) then
             select type (v => this%l_val_dyn)
             type is (complex(real64))
                 val = v
             class default
                 if (DO_CHECKS) error stop "get_left_c64 type mismatch in dynamic storage"
             end select
        else
            if (DO_CHECKS) error stop "get_left_c64 called on wrong type"
        end if
    end function get_left_c64
    pure elemental function get_right_c64(this) result(val) !GCC$ attributes always_inline :: get_right_c64
        class(either), intent(in) :: this
        complex(real64) :: val
        if (this%active_r == TYPE_C64) then
            val = transfer(this%r_bytes, val)
        else if (this%active_r == TYPE_DYN) then
             select type (v => this%r_val_dyn)
             type is (complex(real64))
                 val = v
             class default
                 if (DO_CHECKS) error stop "get_right_c64 type mismatch in dynamic storage"
             end select
        else
            if (DO_CHECKS) error stop "get_right_c64 called on wrong type"
        end if
    end function get_right_c64
    pure elemental subroutine set_left_c64(this, val) !GCC$ attributes always_inline :: set_left_c64
        class(either), intent(inout) :: this
        complex(real64), intent(in) :: val
        ! Inlined clear_left (optimized: no active_l assignment)
        if (allocated(this%l_val_dyn)) deallocate(this%l_val_dyn)
        ! Inlined clear_right
        if (allocated(this%r_val_dyn)) deallocate(this%r_val_dyn)
        this%active_r = TYPE_NONE
        
        this%l_bytes = transfer(val, this%l_bytes)
        this%active_l = TYPE_C64
    end subroutine set_left_c64
    pure elemental subroutine set_right_c64(this, val) !GCC$ attributes always_inline :: set_right_c64
        class(either), intent(inout) :: this
        complex(real64), intent(in) :: val
        ! Inlined clear_left
        if (allocated(this%l_val_dyn)) deallocate(this%l_val_dyn)
        this%active_l = TYPE_NONE
        ! Inlined clear_right (optimized: no active_r assignment)
        if (allocated(this%r_val_dyn)) deallocate(this%r_val_dyn)
        
        this%r_bytes = transfer(val, this%r_bytes)
        this%active_r = TYPE_C64
    end subroutine set_right_c64

end module formerr_either