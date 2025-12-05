from jinja2 import Environment, FileSystemLoader, select_autoescape

import constants
import supported_types

env = Environment(
    loader=FileSystemLoader(constants.TEMPLATES_DIR),
    autoescape=select_autoescape(),
)
env.globals.update(supported_types=supported_types)

# Generate Source Files
source_templates = ["either.f90", "option.f90", "result.f90"]
for p in source_templates:
    template = env.get_template(f"src/{p}")
    out_p = constants.SRC_DIR / f"formerr_{p}"
    with out_p.open("wt+") as f:
        f.write(template.render())

# Generate Test Files
test_templates = [
    "check.f90",
    "check_result.f90",
    "check_either.f90",
    "check_option.f90",
    "check_properties.f90",
]
for p in test_templates:
    template = env.get_template(f"test/{p}")
    out_p = constants.TEST_DIR / p
    with out_p.open("wt+") as f:
        f.write(template.render())

# Generate Benchmark Files
bench_templates = ["bench.f90"]
for p in bench_templates:
    template = env.get_template(f"bench/{p}")
    out_p = constants.BENCH_DIR / p
    with out_p.open("wt+") as f:
        f.write(template.render())
