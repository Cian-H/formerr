from pathlib import Path

CODEGEN_DIR = Path(__file__).parent
TEMPLATES_DIR = CODEGEN_DIR / "templates"

PROJECT_ROOT_DIR = CODEGEN_DIR.parent
SRC_DIR = PROJECT_ROOT_DIR / "src"
