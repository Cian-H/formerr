pure function ok_string(val) result(res)
    character(*), intent(in) :: val
    type(result_type) :: res
    character(len={{ supported_types.max_size() }}) :: temp

    ! We need 1 byte for length, so max string len is max_size - 1.
    if (len(val) < {{ supported_types.max_size() }}) then
        ! Create padded string with length prefix
        temp(1:1) = char(len(val))
        temp(2:1+len(val)) = val

        ! Transfer 32 bytes directly to 4xInt64 array. Very fast copy.
        res%r_bytes = transfer(temp, res%r_bytes)
        res%active_r = TYPE_SSO_STRING
    else
        ! --- SLOW PATH (Allocatable) ---
        call res%set_right(val)
    end if
end function

pure function err_string(val) result(res)
    character(*), intent(in) :: val
    type(result_type) :: res
    character(len={{ supported_types.max_size() }}) :: temp

    if (len(val) < {{ supported_types.max_size() }}) then
        temp(1:1) = char(len(val))
        temp(2:1+len(val)) = val
        res%l_bytes = transfer(temp, res%l_bytes)
        res%active_l = TYPE_SSO_STRING
    else
        call res%set_left(val)
    end if
end function

pure function unwrap_string(this) result(val)
    class(result_type), intent(in) :: this
    character(:), allocatable :: val
    integer :: n
    character(len={{ supported_types.max_size() }}) :: temp

    if (this%is_err()) error stop "Called unwrap on Err"

    if (this%active_r == TYPE_SSO_STRING) then
        ! --- FAST PATH ---
        ! Bulk transfer back to character buffer
        temp = transfer(this%r_bytes, temp)
        n = ichar(temp(1:1))

        allocate(character(len=n) :: val)
        val = temp(2 : 1+n)

    else if (this%active_r == TYPE_DYN) then
        ! --- SLOW PATH ---
        select type(v => this%r_val_dyn)
        type is (character(*))
            val = v
        end select
    end if
end function

pure function unwrap_err_string(this) result(val)
    class(result_type), intent(in) :: this
    character(:), allocatable :: val
    integer :: n
    character(len={{ supported_types.max_size() }}) :: temp

    if (this%is_ok()) error stop "Called unwrap_err on Ok"

    if (this%active_l == TYPE_SSO_STRING) then
        temp = transfer(this%l_bytes, temp)
        n = ichar(temp(1:1))
        allocate(character(len=n) :: val)
        val = temp(2 : 1+n)

    else if (this%active_l == TYPE_DYN) then
        select type(v => this%l_val_dyn)
        type is (character(*))
            val = v
        end select
    end if
end function

pure function unwrap_or_string(this, default_val) result(val)
    class(result_type), intent(in) :: this
    character(*), intent(in) :: default_val
    character(:), allocatable :: val
    integer :: n
    character(len={{ supported_types.max_size() }}) :: temp

    if (this%active_r == TYPE_SSO_STRING) then
        temp = transfer(this%r_bytes, temp)
        n = ichar(temp(1:1))
        allocate(character(len=n) :: val)
        val = temp(2 : 1+n)

    else if (this%active_r == TYPE_DYN) then
        select type(v => this%r_val_dyn)
        type is (character(*))
            val = v
        end select
    else
        val = default_val
    end if
end function
