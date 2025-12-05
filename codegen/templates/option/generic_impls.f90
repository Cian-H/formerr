pure elemental function some_{{ t.suffix }}(val) result(res) !gcc$ attributes always_inline :: some_{{ t.suffix }}
    {{ t.type_def }}, intent(in) :: val
    type(option) :: res
    call res%set_right_{{ t.suffix }}(val)
end function some_{{ t.suffix }}

pure elemental function unwrap_{{ t.suffix }}(this) result(val) !gcc$ attributes always_inline :: unwrap_{{ t_suffix }}
    class(option), intent(in) :: this
    {{ t.type_def }} :: val
    if (this%is_none()) then
        if (do_checks) error stop "unwrap_{{ t.suffix }} called on none value"
    end if
    ! we call the specialized getter from either
    val = this%get_right_{{ t.suffix }}()
end function unwrap_{{ t.suffix }}

pure elemental function unwrap_or_{{ t.suffix }}(this, default_val) result(val) !gcc$ attributes always_inline :: unwrap_or_{{ t.suffix }}
    class(option), intent(in) :: this
    {{ t.type_def }}, intent(in) :: default_val
    {{ t.type_def }} :: val
    if (this%is_some()) then
        val = this%get_right_{{ t.suffix }}()
    else
        val = default_val
    end if
end function unwrap_or_{{ t.suffix }}
