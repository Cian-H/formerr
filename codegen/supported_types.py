from ftypes import FortranType


SUPPORTED_TYPES = [
    # --- Standard Intrinsics ---
    FortranType("integer", "integer", "int", size=4, is_iso=False),
    FortranType("real", "real", "real", size=4, is_iso=False),
    FortranType("logical", "logical", "log", size=4, is_iso=False),
    FortranType("complex", "complex", "cpx", size=8, is_iso=False),
    # --- Legacy / Hardcoded Kinds ---
    FortranType("real(8)", "real(8)", "r8", size=8, is_iso=False),
    FortranType("real(16)", "real(16)", "r16", size=16, is_iso=False),
    # --- ISO_FORTRAN_ENV Types ---
    # Integers
    FortranType("integer(int8)", "integer(int8)", "i8", size=1, is_iso=True),
    FortranType("integer(int16)", "integer(int16)", "i16", size=2, is_iso=True),
    FortranType("integer(int32)", "integer(int32)", "i32", size=4, is_iso=True),
    FortranType("integer(int64)", "integer(int64)", "i64", size=8, is_iso=True),
    FortranType("integer(int128)", "integer(int128)", "i128", size=16, is_iso=False),
    # Reals
    FortranType("real(real32)", "real(real32)", "r32", size=4, is_iso=True),
    FortranType("real(real64)", "real(real64)", "r64", size=8, is_iso=True),
    FortranType("real(real128)", "real(real128)", "r128", size=16, is_iso=True),
    # Complex
    FortranType("complex(real64)", "complex(real64)", "c64", size=16, is_iso=False),
    FortranType("complex(real128)", "complex(real128)", "c128", size=32, is_iso=False),
]


def max_size():
    return max(t.size for t in SUPPORTED_TYPES)


def iso_uses():
    return ", ".join(
        t.kind_parameter
        for t in SUPPORTED_TYPES
        if t.is_iso and t.kind_parameter is not None
    )
