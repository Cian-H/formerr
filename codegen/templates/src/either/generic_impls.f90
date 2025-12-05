pure elemental function get_left_{{ t.suffix }} (this) result(val) !GCC$ attributes always_inline :: get_left_{{ t.suffix }}
class(either), intent(in) :: this
{{ t.type_def }} :: val
if (this%active_l == {{ t.const_name }}) then
    val = transfer(this%l_bytes, val)
else if (this%active_l == TYPE_DYN) then
    select type (v => this%l_val_dyn)
    type is ({{ t.type_def }})
        val = v
    class default
        if (DO_CHECKS) error stop "get_left_{{ t.suffix }} type mismatch in dynamic storage"
    end select
else
    if (DO_CHECKS) error stop "get_left_{{ t.suffix }} called on wrong type"
end if
end function get_left_{{ t.suffix }}

pure elemental function get_right_{{ t.suffix }} (this) result(val) !GCC$ attributes always_inline :: get_right_{{ t.suffix }}
class(either), intent(in) :: this
{{ t.type_def }} :: val
if (this%active_r == {{ t.const_name }}) then
    val = transfer(this%r_bytes, val)
else if (this%active_r == TYPE_DYN) then
    select type (v => this%r_val_dyn)
    type is ({{ t.type_def }})
        val = v
    class default
        if (DO_CHECKS) error stop "get_right_{{ t.suffix }} type mismatch in dynamic storage"
    end select
else
    if (DO_CHECKS) error stop "get_right_{{ t.suffix }} called on wrong type"
end if
end function get_right_{{ t.suffix }}

pure elemental subroutine set_left_{{ t.suffix }} (this, val) !GCC$ attributes always_inline :: set_left_{{ t.suffix }}
class(either), intent(inout) :: this
{{ t.type_def }}, intent(in) :: val

if (this%active_l == TYPE_DYN) deallocate (this%l_val_dyn)
if (this%active_r == TYPE_DYN) deallocate (this%r_val_dyn)

this%active_r = TYPE_NONE

this%l_bytes = transfer(val, this%l_bytes)
this%active_l = {{ t.const_name }}
end subroutine set_left_{{ t.suffix }}

pure elemental subroutine set_right_{{ t.suffix }} (this, val) !GCC$ attributes always_inline :: set_right_{{ t.suffix }}
class(either), intent(inout) :: this
{{ t.type_def }}, intent(in) :: val

if (this%active_l == TYPE_DYN) deallocate (this%l_val_dyn)
this%active_l = TYPE_NONE

if (this%active_r == TYPE_DYN) deallocate (this%r_val_dyn)

this%r_bytes = transfer(val, this%r_bytes)
this%active_r = {{ t.const_name }}
end subroutine set_right_{{ t.suffix }}
