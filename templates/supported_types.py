from dataclasses import dataclass
import re


@dataclass(frozen=True)
class FortranType:
    name: str
    type_def: str
    suffix: str
    size: int
    is_iso: bool = False

    def __post_init__(self):
        """
        Validates the type definition immediately upon creation.
        Ensures consistency between the type name string and the declared byte size.
        """
        if self.size <= 0:
            raise ValueError(f"Type '{self.name}': Size must be positive.")

        if self.is_iso:
            self._validate_iso_consistency()

    def _validate_iso_consistency(self):
        """
        Parses the type definition string to ensure the declared 'size' matches
        the bit-width implied by the ISO name.
        """
        match = re.search(r"(int|real)(\d+)", self.type_def)

        if not match:
            msg = f"Type '{self.name}' is marked is_iso=True but does not contain"
            msg += "a standard ISO kind parameter (e.g., int32, real64)."
            raise ValueError(msg)

        bits = int(match.group(2))
        expected_bytes = bits // 8

        if "complex" in self.type_def:
            expected_bytes *= 2

        if self.size != expected_bytes:
            msg = f"Consistency Error for '{self.name}':\n"
            msg += f"  - Definition implies: {bits} bits ({expected_bytes} bytes)\n"
            msg += f"  - User provided:      {self.size} bytes\n"
            msg += f"  - Fix: Change size to {expected_bytes}."
            raise ValueError(msg)

    @property
    def kind_parameter(self) -> str | None:
        if not self.is_iso:
            return None
        match = re.search(r"\((.*?)\)", self.type_def)
        return match.group(1) if match else None


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
    FortranType("integer(int128)", "integer(int128)", "i128", size=16, is_iso=True),
    # Reals
    FortranType("real(real32)", "real(real32)", "r32", size=4, is_iso=True),
    FortranType("real(real64)", "real(real64)", "r64", size=8, is_iso=True),
    FortranType("real(real128)", "real(real128)", "r128", size=16, is_iso=True),
    # Complex
    FortranType("complex(real64)", "complex(real64)", "c64", size=16, is_iso=True),
    FortranType("complex(real128)", "complex(real128)", "c128", size=32, is_iso=True),
]
