module formerr_either
    implicit none
    private

    public :: either, new_left, new_right

    type :: either
        private
        class(*), allocatable :: l_val
        class(*), allocatable :: r_val
    contains
        procedure :: is_left
        procedure :: is_right

        procedure :: get_left
        procedure :: get_right

        procedure :: set_left
        procedure :: set_right

        procedure :: move_left
        procedure :: move_right
    end type either

    interface either
        module procedure new_either
    end interface

contains
    function new_either(left_val, right_val) result(res)
        class(*), intent(in), optional :: left_val
        class(*), intent(in), optional :: right_val
        type(either) :: res

        ! Validation: Ensure strict XOR logic
        if (present(left_val) .and. .not. present(right_val)) then
            res = new_left(left_val)
        else if (present(right_val) .and. .not. present(left_val)) then
            res = new_right(right_val)
        else
            error stop "either constructor requires exactly one argument (left=... OR right=...)"
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
        is_left = allocated(this%l_val)
    end function is_left

    logical function is_right(this)
        class(either), intent(in) :: this
        is_right = allocated(this%r_val)
    end function is_right

    function get_left(this) result(ptr)
        ! Return pointer to internal data.
        ! This allows the user to get result without copying the data.
        class(either), intent(in), target :: this
        class(*), pointer :: ptr

        ptr => null()
        if (allocated(this%l_val)) ptr => this%l_val
    end function get_left

    function get_right(this) result(ptr)
        class(either), intent(in), target :: this
        class(*), pointer :: ptr

        ptr => null()
        if (allocated(this%r_val)) ptr => this%r_val
    end function get_right

    subroutine set_left(this, val)
        class(either), intent(inout) :: this
        class(*), intent(in) :: val
        if (allocated(this%r_val)) deallocate(this%r_val)
        if (allocated(this%l_val)) deallocate(this%l_val)
        allocate (this%l_val, source=val)
    end subroutine set_left

    subroutine set_right(this, val)
        class(either), intent(inout) :: this
        class(*), intent(in) :: val
        if (allocated(this%l_val)) deallocate(this%l_val)
        if (allocated(this%r_val)) deallocate(this%r_val)
        allocate (this%r_val, source=val)
    end subroutine set_right

    subroutine move_left(this, val)
        class(either), intent(inout) :: this
        class(*), allocatable, intent(inout) :: val
        if (allocated(this%r_val)) deallocate(this%r_val)
        if (allocated(this%l_val)) deallocate(this%l_val)
        call move_alloc(from=val, to=this%l_val)
    end subroutine move_left

    subroutine move_right(this, val)
        class(either), intent(inout) :: this
        class(*), allocatable, intent(inout) :: val
        if (allocated(this%l_val)) deallocate(this%l_val)
        if (allocated(this%r_val)) deallocate(this%r_val)
        call move_alloc(from=val, to=this%r_val)
    end subroutine move_right

end module formerr_either
