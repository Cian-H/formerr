import math

from ftypes import FortranType


SUPPORTED_TYPES = [
    # --- Standard Intrinsics ---
    FortranType("integer", "integer", "int", size=4, is_iso=False),
    FortranType("real", "real", "real", size=4, is_iso=False),
    FortranType("logical", "logical", "log", size=4, is_iso=False),
    FortranType("complex", "complex", "cpx", size=8, is_iso=False),
    # Reals
    # --- Legacy / Hardcoded Kinds ---
    FortranType("real(8)", "real(8)", "r8", size=8, is_iso=False),
    FortranType("real(16)", "real(16)", "r16", size=16, is_iso=False),
    # --- ISO_FORTRAN_ENV Types ---
    FortranType("real(real32)", "real(real32)", "r32", size=4, is_iso=True),
    FortranType("real(real64)", "real(real64)", "r64", size=8, is_iso=True),
    FortranType("real(real128)", "real(real128)", "r128", size=16, is_iso=True),
    # Integers
    # --- ISO_FORTRAN_ENV Types ---
    FortranType("integer(int8)", "integer(int8)", "i8", size=1, is_iso=True),
    FortranType("integer(int16)", "integer(int16)", "i16", size=2, is_iso=True),
    FortranType("integer(int32)", "integer(int32)", "i32", size=4, is_iso=True),
    FortranType("integer(int64)", "integer(int64)", "i64", size=8, is_iso=True),
    # --- Legacy / Hardcoded Kinds ---
    FortranType("integer(int128)", "integer(int128)", "i128", size=16, is_iso=False),
    # Complex
    FortranType("complex(real8)", "complex(real8)", "c8", size=2, is_iso=False),
    FortranType("complex(real16)", "complex(real16)", "c16", size=4, is_iso=False),
    FortranType("complex(real32)", "complex(real32)", "c32", size=8, is_iso=False),
    FortranType("complex(real64)", "complex(real64)", "c64", size=16, is_iso=False),
    FortranType("complex(real128)", "complex(real128)", "c128", size=32, is_iso=False),
]


def max_size() -> int:
    return max(t.size for t in SUPPORTED_TYPES)


def buffer_size() -> int:
    return math.ceil(max_size() / 8)


def iso_uses() -> str:
    return ", ".join(
        t.kind_parameter
        for t in SUPPORTED_TYPES
        if t.is_iso and t.kind_parameter is not None
    )


def get_test_value(t: FortranType) -> str:
    if "integer" in t.name:
        kind = (
            t.kind_parameter
            if t.is_iso
            else ("int128" if "int128" in t.type_def else None)
        )
        suffix = f"_{kind}" if kind else ""
        return f"42{suffix}"
    elif "complex" in t.name:
        # Extract inner real kind
        if "(" in t.type_def:
            inner_kind = t.type_def.split("(")[1].split(")")[0]
            return f"(1.0_{inner_kind}, 2.0_{inner_kind})"
        return "(1.0, 2.0)"
    elif "real" in t.name:
        kind = t.kind_parameter
        if not kind:
            # Handle legacy kinds like real(8) -> 8
            if "(" in t.type_def:
                kind = t.type_def.split("(")[1].split(")")[0]

        suffix = f"_{kind}" if kind else ""
        return f"1.23{suffix}"
    elif "logical" in t.name:
        return ".true."
    return "0"


def get_err_value(t: FortranType) -> str:
    if "integer" in t.name:
        kind = (
            t.kind_parameter
            if t.is_iso
            else ("int128" if "int128" in t.type_def else None)
        )
        suffix = f"_{kind}" if kind else ""
        return f"-1{suffix}"
    elif "complex" in t.name:
        if "(" in t.type_def:
            inner_kind = t.type_def.split("(")[1].split(")")[0]
            return f"(-1.0_{inner_kind}, -2.0_{inner_kind})"
        return "(-1.0, -2.0)"
    elif "real" in t.name:
        kind = t.kind_parameter
        if not kind:
            if "(" in t.type_def:
                kind = t.type_def.split("(")[1].split(")")[0]
        suffix = f"_{kind}" if kind else ""
        return f"-9.99{suffix}"
    elif "logical" in t.name:
        return ".false."
    return "0"


def is_numeric(t: FortranType) -> bool:
    return "integer" in t.name or "real" in t.name or "complex" in t.name
