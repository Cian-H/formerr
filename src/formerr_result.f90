module formerr_result
    use formerr_either
    use stdlib_error, only: check
    implicit none
    private

    public :: result_type, ok, err, is_ok, is_err, unwrap, unwrap_err, unwrap_or

    type, extends(either) :: result_type
    contains
        procedure :: is_ok
        procedure :: is_err
        procedure :: unwrap
        procedure :: unwrap_err
        procedure :: unwrap_or
    end type result_type

contains

    function ok(val) result(res)
        class(*), intent(in) :: val
        type(result_type) :: res
        res%either = right(val)
    end function ok

    function err(val) result(res)
        class(*), intent(in) :: val
        type(result_type) :: res
        res%either = left(val)
    end function err

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

end module formerr_result
