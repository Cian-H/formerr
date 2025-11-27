module formerr_option
    use formerr_either
    use stdlib_error, only: check
    implicit none
    private

    public :: option, some, none, is_some, is_none, unwrap, unwrap_or
    public :: some_move, unwrap_move

    type :: unit_type
    end type unit_type

    type, extends(either) :: option
    contains
        procedure :: is_some
        procedure :: is_none
        procedure :: unwrap
        procedure :: unwrap_or
        procedure :: unwrap_move
    end type option

contains

    function some(val) result(res)
        class(*), intent(in) :: val
        type(option) :: res
        call res%set_right(val)
    end function some

    function none() result(res)
        type(option) :: res
        type(unit_type) :: u
        call res%set_left(u)
    end function none

    function some_move(val) result(res)
        class(*), allocatable, intent(inout) :: val
        type(option) :: res
        call res%move_right(val)
    end function some_move

    logical function is_some(this)
        class(option), intent(in) :: this
        is_some = this%is_right()
    end function is_some

    logical function is_none(this)
        class(option), intent(in) :: this
        is_none = this%is_left()
    end function is_none

    function unwrap(this) result(ptr)
        class(option), intent(in), target :: this
        class(*), pointer :: ptr

        call check(this%is_some(), "unwrap called on None value")

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

        call check(this%is_some(), "unwrap_move called on None value")

        ! 1. Prepare the None state (Unit type)
        allocate(temp_none, source=u)

        ! 2. Perform the swap
        ! Move the Right (Some) value to dest
        call this%move_right(dest)

        ! 3. Restore invariant: Move the unit type into Left (None)
        call this%move_left(temp_none)
    end subroutine unwrap_move

end module formerr_option
