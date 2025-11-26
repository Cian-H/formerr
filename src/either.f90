module either_mod
    implicit none
    private

    public :: either, left, right

    type :: either
        private
        class(*), allocatable :: l_val
        class(*), allocatable :: r_val
    contains
        procedure :: is_left
        procedure :: is_right

        procedure :: get_left
        procedure :: get_right
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
            res = left(left_val)
        else if (present(right_val) .and. .not. present(left_val)) then
            res = right(right_val)
        else
            error stop "either constructor requires exactly one argument (left=... OR right=...)"
        end if
    end function new_either

    function left(val) result(res)
        class(*), intent(in) :: val
        type(either) :: res

        ! Enforce invariant: allocate left, ensure right is dead
        allocate (res%l_val, source=val)
    end function left

    function right(val) result(res)
        class(*), intent(in) :: val
        type(either) :: res

        ! Enforce invariant: allocate right, ensure left is dead
        allocate (res%r_val, source=val)
    end function right

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

end module either_mod
