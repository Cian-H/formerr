module formerr_option
    use formerr_either
    use stdlib_error, only: check
    implicit none
    private

    public :: option, some, none, is_some, is_none, unwrap

    type :: unit_type
    end type unit_type

    type, extends(either) :: option
    contains
        procedure :: is_some
        procedure :: is_none
        procedure :: unwrap
        procedure :: unwrap_or
    end type option

contains

    function some(val) result(res)
        class(*), intent(in) :: val
        type(option) :: res
        res%either = right(val)
    end function some

    function none() result(res)
        type(option) :: res
        type(unit_type) :: u
        res%either = left(u)
    end function none

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

end module formerr_option
