pure function ok_string(val) result(res)
    character(*), intent(in) :: val
    type(result_type) :: res
    integer(int8) :: length_byte

    ! We need 1 byte for length, so max string len is max_size - 1.
    if (len(val) < {{ supported_types.max_size() }}) then
        length_byte = int(len(val), int8)
        res%l_bytes(1) = length_byte
        ! We transfer the string bits into the integer(int8) array starting at index 2
        res%l_bytes(2 : 1 + len(val)) = transfer(val, res%l_bytes(1:len(val)))
        res%active_l = TYPE_SSO_STRING
    else
        ! --- SLOW PATH (Allocatable) ---
        call res%set_left(val)
    end if
end function

pure function unwrap_string(this) result(val)
    class(result_type), intent(in) :: this
    character(:), allocatable :: val
    integer :: n

    if (this%is_err()) stop "Called unwrap on Err"

    if (this%active_r == TYPE_SSO_STRING) then
        ! --- FAST PATH ---
        n = int(this%r_bytes(1)) ! Read the length from the first byte
        allocate(character(len=n) :: val)
        val = transfer(this%r_bytes(2 : 1+n), val)

    else if (this%active_r == TYPE_DYN) then
        ! --- SLOW PATH ---
        select type(v => this%r_val_dyn)
        type is (character(*))
            val = v
        end select
    end if
end function
