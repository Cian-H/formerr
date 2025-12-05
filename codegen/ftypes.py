from dataclasses import dataclass, field
import re


@dataclass(frozen=True)
class FortranType:
    name: str = field(repr=True)
    type_def: str = field(repr=False)
    suffix: str = field(repr=False)
    size: int = field(repr=False)
    is_iso: bool = field(default=False, repr=False)

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

    @property
    def const_name(self) -> str:
        return f"TYPE_{self.suffix.upper()}"
