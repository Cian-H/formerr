from jinja2 import Environment, FileSystemLoader, select_autoescape

import constants
import supported_types

env = Environment(
    loader=FileSystemLoader(constants.TEMPLATES_DIR),
    autoescape=select_autoescape(),
)
env.globals.update(supported_types=supported_types)
template_files = ["either.f90", "option.f90", "result.f90"]
templates = {p: env.get_template(p) for p in template_files}

for p, t in templates.items():
    out_p = constants.SRC_DIR / f"formerr_{p}"
    with out_p.open("wt+") as f:
        f.write(t.render())
