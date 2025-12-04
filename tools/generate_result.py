from pathlib import Path

from supported_types import SUPPORTED_TYPES


RESULT_TEMPLATE_PATH = Path(__file__).parent / "templates/result.fyyp"

with RESULT_TEMPLATE_PATH.open("rt") as f:
    RESULT_TEMPLATE = f.read()


def generate_result():
    public_procedures = ""
    type_bound_procedures = ""
    specialized_impls = ""

    for t in SUPPORTED_TYPES:
        t_type = t["type"]
        t_suffix = t["suffix"]

        # Public procedures
        public_procedures += f"    public :: ok_{t_suffix}, err_{t_suffix}\n"

        # Type bound procedures
        type_bound_procedures += f"        procedure :: unwrap_{t_suffix}\n"
        type_bound_procedures += f"        procedure :: unwrap_err_{t_suffix}\n"
        type_bound_procedures += f"        procedure :: unwrap_or_{t_suffix}\n"

        # ok_{suffix}
        specialized_impls += f"""
    pure elemental function ok_{t_suffix}(val) result(res) !GCC$ attributes always_inline :: ok_{t_suffix}
        {t_type}, intent(in) :: val
        type(result_type) :: res
        call res%set_right_{t_suffix}(val)
    end function ok_{t_suffix}
"""
        # err_{suffix}
        specialized_impls += f"""
    pure elemental function err_{t_suffix}(val) result(res) !GCC$ attributes always_inline :: err_{t_suffix}
        {t_type}, intent(in) :: val
        type(result_type) :: res
        call res%set_left_{t_suffix}(val)
    end function err_{t_suffix}
"""

        # unwrap_{suffix} (for Ok)
        specialized_impls += f"""
    pure elemental function unwrap_{t_suffix}(this) result(val) !GCC$ attributes always_inline :: unwrap_{t_suffix}
        class(result_type), intent(in) :: this
        {t_type} :: val
        if (this%is_err()) then
            if (DO_CHECKS) error stop \"unwrap_{t_suffix} called on Err value\"
        end if
        val = this%get_right_{t_suffix}()
    end function unwrap_{t_suffix}
"""

        # unwrap_err_{suffix} (for Err)
        specialized_impls += f"""
    pure elemental function unwrap_err_{t_suffix}(this) result(val) !GCC$ attributes always_inline :: unwrap_err_{t_suffix}
        class(result_type), intent(in) :: this
        {t_type} :: val
        if (this%is_ok()) then
            if (DO_CHECKS) error stop \"unwrap_err_{t_suffix} called on Ok value\"
        end if
        val = this%get_left_{t_suffix}()
    end function unwrap_err_{t_suffix}
"""

        # unwrap_or_{suffix}
        specialized_impls += f"""
    pure elemental function unwrap_or_{t_suffix}(this, default_val) result(val) !GCC$ attributes always_inline :: unwrap_or_{t_suffix}
        class(result_type), intent(in) :: this
        {t_type}, intent(in) :: default_val
        {t_type} :: val
        if (this%is_ok()) then
            val = this%get_right_{t_suffix}()
        else
            val = default_val
        end if
    end function unwrap_or_{t_suffix}
"""

    content = RESULT_TEMPLATE.format(
        public_procedures=public_procedures,
        type_bound_procedures=type_bound_procedures,
        specialized_impls=specialized_impls,
    )

    with Path("src/formerr_result.f90").open("w+") as f:
        f.write(content)


if __name__ == "__main__":
    generate_result()
