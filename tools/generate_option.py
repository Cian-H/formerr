from pathlib import Path

from supported_types import SUPPORTED_TYPES


OPTION_TEMPLATE_PATH = Path(__file__).parent / "templates/option.fyyp"

with OPTION_TEMPLATE_PATH.open("rt") as f:
    OPTION_TEMPLATE = f.read()


def generate_option():
    public_procedures = ""
    type_bound_procedures = ""
    specialized_impls = ""

    for t in SUPPORTED_TYPES:
        t_type = t["type"]
        t_suffix = t["suffix"]

        # Public module procedures
        public_procedures += f"    public :: some_{t_suffix}\n"

        # Type bound procedures
        type_bound_procedures += f"        procedure :: unwrap_{t_suffix}\n"

        # some_{suffix}
        specialized_impls += f"""
    pure elemental function some_{t_suffix}(val) result(res)
        {t_type}, intent(in) :: val
        type(option) :: res
        call res%set_right_{t_suffix}(val)
    end function some_{t_suffix}
"""

        # unwrap_{suffix}
        specialized_impls += f"""
    pure elemental function unwrap_{t_suffix}(this) result(val)
        class(option), intent(in) :: this
        {t_type} :: val
        if (this%is_none()) error stop "unwrap_{t_suffix} called on None value"
        ! We call the specialized getter from either
        val = this%get_right_{t_suffix}()
    end function unwrap_{t_suffix}
"""

    content = OPTION_TEMPLATE.format(
        public_procedures=public_procedures,
        type_bound_procedures=type_bound_procedures,
        specialized_impls=specialized_impls,
    )

    with Path("src/formerr_option.f90").open("w+") as f:
        f.write(content)


if __name__ == "__main__":
    generate_option()
