from pathlib import Path

from supported_types import SUPPORTED_TYPES


EITHER_TEMPLATE_PATH = Path(__file__).parent / "templates/either.fyyp"

with EITHER_TEMPLATE_PATH.open("rt") as f:
    EITHER_TEMPLATE = f.read()


def generate_either():
    # Reverting to int8 storage as int64 proved slower for small types
    # Storage size increased to 32 to support complex(real128) (16+16 bytes)
    type_constants = "    integer, parameter :: STORAGE_SIZE = 32\n"

    # Define the union storage buffer with int8
    type_fields = "        integer(int8) :: l_bytes(STORAGE_SIZE)\n"
    type_fields += "        integer(int8) :: r_bytes(STORAGE_SIZE)\n"

    get_left_cases = ""
    get_right_cases = ""
    set_left_cases = ""
    set_right_cases = ""
    move_left_cases = ""
    move_right_cases = ""

    public_procedures = ""
    type_bound_procedures = ""
    specialized_impls = ""

    for i, t in enumerate(SUPPORTED_TYPES):
        type_id = i + 2  # Start after TYPE_DYN
        t_type = t["type"]
        t_suffix = t["suffix"]

        const_name = f"TYPE_{t_suffix.upper()}"
        type_constants += f"    integer, parameter :: {const_name} = {type_id}\n"

        # Type bound procedures
        type_bound_procedures += f"        procedure :: get_left_{t_suffix}\n"
        type_bound_procedures += f"        procedure :: get_right_{t_suffix}\n"
        type_bound_procedures += f"        procedure :: set_left_{t_suffix}\n"
        type_bound_procedures += f"        procedure :: set_right_{t_suffix}\n"

        # Generic Dispatch Cases - Error on Union types for pointer return
        get_left_cases += f"        case ({const_name})\n"
        get_left_cases += f'            if (DO_CHECKS) error stop "get_left (Generic) cannot return pointer to Specialized (Union) value ({t_type})."\n'

        get_right_cases += f"        case ({const_name})\n"
        get_right_cases += f'            if (DO_CHECKS) error stop "get_right (Generic) cannot return pointer to Specialized (Union) value ({t_type})."\n'

        # Specialized Implementation Code

        # GET LEFT
        specialized_impls += f"""
    pure elemental function get_left_{t_suffix}(this) result(val) !GCC$ attributes always_inline :: get_left_{t_suffix}
        class(either), intent(in) :: this
        {t_type} :: val
        if (this%active_l == {const_name}) then
            val = transfer(this%l_bytes, val)
        else if (this%active_l == TYPE_DYN) then
             select type (v => this%l_val_dyn)
             type is ({t_type})
                 val = v
             class default
                 if (DO_CHECKS) error stop "get_left_{t_suffix} type mismatch in dynamic storage"
             end select
        else
            if (DO_CHECKS) error stop "get_left_{t_suffix} called on wrong type"
        end if
    end function get_left_{t_suffix}"""

        # GET RIGHT
        specialized_impls += f"""
    pure elemental function get_right_{t_suffix}(this) result(val) !GCC$ attributes always_inline :: get_right_{t_suffix}
        class(either), intent(in) :: this
        {t_type} :: val
        if (this%active_r == {const_name}) then
            val = transfer(this%r_bytes, val)
        else if (this%active_r == TYPE_DYN) then
             select type (v => this%r_val_dyn)
             type is ({t_type})
                 val = v
             class default
                 if (DO_CHECKS) error stop "get_right_{t_suffix} type mismatch in dynamic storage"
             end select
        else
            if (DO_CHECKS) error stop "get_right_{t_suffix} called on wrong type"
        end if
    end function get_right_{t_suffix}"""

        # SET LEFT
        specialized_impls += f"""
    pure elemental subroutine set_left_{t_suffix}(this, val) !GCC$ attributes always_inline :: set_left_{t_suffix}
        class(either), intent(inout) :: this
        {t_type}, intent(in) :: val
        
        if (this%active_l == TYPE_DYN) deallocate(this%l_val_dyn)
        if (this%active_r == TYPE_DYN) deallocate(this%r_val_dyn)
        
        this%active_r = TYPE_NONE
        
        this%l_bytes = transfer(val, this%l_bytes)
        this%active_l = {const_name}
    end subroutine set_left_{t_suffix}"""

        # SET RIGHT
        specialized_impls += f"""
    pure elemental subroutine set_right_{t_suffix}(this, val) !GCC$ attributes always_inline :: set_right_{t_suffix}
        class(either), intent(inout) :: this
        {t_type}, intent(in) :: val
        
        if (this%active_l == TYPE_DYN) deallocate(this%l_val_dyn)
        this%active_l = TYPE_NONE
        
        if (this%active_r == TYPE_DYN) deallocate(this%r_val_dyn)
        
        this%r_bytes = transfer(val, this%r_bytes)
        this%active_r = {const_name}
    end subroutine set_right_{t_suffix}"""

    content = EITHER_TEMPLATE.format(
        type_constants=type_constants,
        type_fields=type_fields,
        get_left_cases=get_left_cases,
        get_right_cases=get_right_cases,
        set_left_cases=set_left_cases,
        set_right_cases=set_right_cases,
        move_left_cases=move_left_cases,
        move_right_cases=move_right_cases,
        public_procedures=public_procedures,
        type_bound_procedures=type_bound_procedures,
        specialized_impls=specialized_impls,
    )

    with Path("src/formerr_either.f90").open("w+") as f:
        f.write(content)


if __name__ == "__main__":
    generate_either()
