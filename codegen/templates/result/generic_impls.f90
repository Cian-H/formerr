    pure elemental function ok_{{ t.suffix }}(val) result(res) !GCC$ attributes always_inline :: ok_{{ t.suffix }}
        {{ t.type_def }}, intent(in) :: val
        type(result_type) :: res
        call res%set_right_{{ t.suffix }}(val)
    end function ok_{{ t.suffix }}

    pure elemental function err_{{ t.suffix }}(val) result(res) !GCC$ attributes always_inline :: err_{{ t.suffix }}
        {{ t.type_def }}, intent(in) :: val
        type(result_type) :: res
        call res%set_left_{{ t.suffix }}(val)
    end function err_{{ t.suffix }}

    pure elemental function unwrap_{{ t.suffix }}(this) result(val) !GCC$ attributes always_inline :: unwrap_{{ t.suffix }}
        class(result_type), intent(in) :: this
        {{ t.type_def }} :: val
        if (this%is_err()) then
            if (DO_CHECKS) error stop "unwrap_{{ t.suffix }} called on Err value"
        end if
        val = this%get_right_{{ t.suffix }}()
    end function unwrap_{{ t.suffix }}

    pure elemental function unwrap_err_{{ t.suffix }}(this) result(val) !GCC$ attributes always_inline :: unwrap_err_{{ t.suffix }}
        class(result_type), intent(in) :: this
        {{ t.type_def }} :: val
        if (this%is_ok()) then
            if (DO_CHECKS) error stop "unwrap_err_{{ t.suffix }} called on Ok value"
        end if
        val = this%get_left_{{ t.suffix }}()
    end function unwrap_err_{{ t.suffix }}

    pure elemental function unwrap_or_{{ t.suffix }}(this, default_val) result(val) !GCC$ attributes always_inline :: unwrap_or_{{ t.suffix }}
        class(result_type), intent(in) :: this
        {{ t.type_def }}, intent(in) :: default_val
        {{ t.type_def }} :: val
        if (this%is_ok()) then
            val = this%get_right_{{ t.suffix }}()
        else
            val = default_val
        end if
    end function unwrap_or_{{ t.suffix }}
