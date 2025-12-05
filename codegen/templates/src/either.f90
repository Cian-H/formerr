module formerr_either
    {% include "shared/iso_uses.f90" %}
    implicit none
    private

    {% include "shared/selected_kinds.f90" %}

    public :: either, new_left, new_right
    public :: TYPE_SSO_STRING, TYPE_DYN, TYPE_NONE

    logical, parameter :: DO_CHECKS = .true.
    integer, parameter :: BUFFER_SIZE = {{ supported_types.buffer_size() }}

    ! Type constants
    integer, parameter :: TYPE_NONE = -1
    integer, parameter :: TYPE_DYN = 0
    {% for t in supported_types.SUPPORTED_TYPES %}
        integer, parameter :: {{ t.const_name }} = {{ loop.index }}
    {% endfor %}
    integer, parameter :: TYPE_SSO_STRING = 127

    type :: either
        integer :: active_l = TYPE_NONE
        integer :: active_r = TYPE_NONE

        ! Dynamic fallback
        class(*), allocatable :: l_val_dyn
        class(*), allocatable :: r_val_dyn

        ! Monomorphised storage (embedded, non-allocatable)
        integer(int64) :: l_bytes(BUFFER_SIZE)
        integer(int64) :: r_bytes(BUFFER_SIZE)
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
        {% for t in supported_types.SUPPORTED_TYPES %}
            {% include "src/either/generic_procedures.f90" %}
        {% endfor %}
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
        if (allocated(this%l_val_dyn)) deallocate (this%l_val_dyn)
        ! Monomorphised fields are embedded, no deallocation needed
        this%active_l = TYPE_NONE
    end subroutine clear_left

    pure subroutine clear_right(this)
        class(either), intent(inout) :: this
        if (allocated(this%r_val_dyn)) deallocate (this%r_val_dyn)
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
        {% for t in supported_types.SUPPORTED_TYPES %}
            case ({{ t.const_name }})
                if (DO_CHECKS) error stop "get_left (Generic) cannot return pointer to Specialized (Union) value ({{ t.type_def }})."
        {% endfor %}
        end select
    end function get_left

    function get_right(this) result(ptr)
        class(either), intent(in), target :: this
        class(*), pointer :: ptr

        ptr => null()
        select case (this%active_r)
        case (TYPE_DYN)
            if (allocated(this%r_val_dyn)) ptr => this%r_val_dyn
        {% for t in supported_types.SUPPORTED_TYPES %}
            case ({{ t.const_name }})
                if (DO_CHECKS) error stop "get_left (Generic) cannot return pointer to Specialized (Union) value ({{ t.type_def }})."
        {% endfor %}
        end select
    end function get_right

    pure subroutine set_left(this, val)
        class(either), intent(inout) :: this
        class(*), intent(in) :: val

        ! Clear both sides (either is exclusive)
        call this%clear_left()
        call this%clear_right()

        select type (val)
        class default
            allocate (this%l_val_dyn, source=val)
            this%active_l = TYPE_DYN
        end select
    end subroutine set_left

    pure subroutine set_right(this, val)
        class(either), intent(inout) :: this
        class(*), intent(in) :: val

        call this%clear_left()
        call this%clear_right()

        select type (val)
        class default
            allocate (this%r_val_dyn, source=val)
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

        select type (val)
        class default
            call move_alloc(from=val, to=this%l_val_dyn)
            this%active_l = TYPE_DYN
        end select

        ! Deallocate source 'val' if it was allocated (and not moved via move_alloc)
        if (allocated(val)) deallocate (val)

    end subroutine move_left

    pure subroutine move_right(this, val)
        class(either), intent(inout) :: this
        class(*), allocatable, intent(inout) :: val

        call this%clear_left()
        call this%clear_right()

        if (.not. allocated(val)) return

        select type (val)
        class default
            call move_alloc(from=val, to=this%r_val_dyn)
            this%active_r = TYPE_DYN
        end select

        if (allocated(val)) deallocate (val)
    end subroutine move_right

    ! -- Specialized Implementations --
    {% for t in supported_types.SUPPORTED_TYPES %}
        {% include "src/either/generic_impls.f90" %}
    {% endfor %}

end module formerr_either
