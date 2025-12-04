from pathlib import Path

from supported_types import SUPPORTED_TYPES


EITHER_TEMPLATE_PATH = Path(__file__).parent / "templates/either.fyyp"

with EITHER_TEMPLATE_PATH.open("rt") as f:
    EITHER_TEMPLATE = f.read()


def generate_either():
    type_constants = ""
    type_fields = ""
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

        # Embedded fields
        type_fields += f"        {t_type} :: l_{t_suffix}\n"
        type_fields += f"        {t_type} :: r_{t_suffix}\n"

        # Type bound procedures
        type_bound_procedures += f"        procedure :: get_left_{t_suffix}\n"
        type_bound_procedures += f"        procedure :: get_right_{t_suffix}\n"
        type_bound_procedures += f"        procedure :: set_left_{t_suffix}\n"
        type_bound_procedures += f"        procedure :: set_right_{t_suffix}\n"

        # Generic Dispatch Cases
        get_left_cases += f"        case ({const_name})\n"
        get_left_cases += f"            ptr => this%l_{t_suffix}\n"

        get_right_cases += f"        case ({const_name})\n"
        get_right_cases += f"            ptr => this%r_{t_suffix}\n"

        set_left_cases += f"        type is ({t_type})\n"
        set_left_cases += f"            this%l_{t_suffix} = val\n"
        set_left_cases += f"            this%active_l = {const_name}\n"

        set_right_cases += f"        type is ({t_type})\n"
        set_right_cases += f"            this%r_{t_suffix} = val\n"
        set_right_cases += f"            this%active_r = {const_name}\n"

        move_left_cases += f"        type is ({t_type})\n"
        move_left_cases += f"            this%l_{t_suffix} = val\n"
        move_left_cases += f"            this%active_l = {const_name}\n"

        move_right_cases += f"        type is ({t_type})\n"
        move_right_cases += f"            this%r_{t_suffix} = val\n"
        move_right_cases += f"            this%active_r = {const_name}\n"

        # Specialized Implementation Code

        # GET LEFT
        specialized_impls += f'\n    function get_left_{
            t_suffix
        }(this) result(val)\n        class(either), intent(in) :: this\n        {
            t_type
        } :: val\n        if (this%active_l == {
            const_name
        }) then\n            val = this%l_{
            t_suffix
        }\n        else\n            error stop "get_left_{
            t_suffix
        } called on wrong type"\n        end if\n    end function get_left_{t_suffix}'
        # GET RIGHT
        specialized_impls += f'\n    function get_right_{
            t_suffix
        }(this) result(val)\n        class(either), intent(in) :: this\n        {
            t_type
        } :: val\n        if (this%active_r == {
            const_name
        }) then\n            val = this%r_{
            t_suffix
        }\n        else\n            error stop "get_right_{
            t_suffix
        } called on wrong type"\n        end if\n    end function get_right_{t_suffix}'
        # SET LEFT
        specialized_impls += f"\n    subroutine set_left_{
            t_suffix
        }(this, val)\n        class(either), intent(inout) :: this\n        {
            t_type
        }, intent(in) :: val\n        call this%clear_left()\n        call this%clear_right()\n        this%l_{
            t_suffix
        } = val\n        this%active_l = {const_name}\n    end subroutine set_left_{
            t_suffix
        }"
        # SET RIGHT
        specialized_impls += f"\n    subroutine set_right_{
            t_suffix
        }(this, val)\n        class(either), intent(inout) :: this\n        {
            t_type
        }, intent(in) :: val\n        call this%clear_left()\n        call this%clear_right()\n        this%r_{
            t_suffix
        } = val\n        this%active_r = {const_name}\n    end subroutine set_right_{
            t_suffix
        }"

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
