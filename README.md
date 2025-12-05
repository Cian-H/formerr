# Formerr

## The FORtran Monadic ERRor system

**Formerr** is a Fortran library that brings functional-style monadic error handling to the Fortran
ecosystem. It provides a robust alternative to the traditional "status code" pattern (e.g., ierr),
allowing developers to write cleaner, safer, and more predictable code for modular and distributed
systems.

## Why Formerr?

Standard Fortran error handling often relies on passing integer status codes or checking global
state. This approach can lead to:

- Inconsistent error contracts between functions.
- "GOTO hell" when handling multiple failure points.
- Silent failures if status codes are ignored.

**Formerr** introduces the Result, Option, and Either monads—concepts popularized by functional
languages and modern systems languages like Rust—adapted for Modern Fortran.

It leverages `jinja` templates to generate **specialized, zero-allocation** implementations for
common types, while providing a generic fallback for complex objects.

## Installation

Formerr is designed to be used with the
[Fortran Package Manager (fpm)](https://fpm.fortran-lang.org/).

Add the following dependency to your fpm.toml:

```toml
[dependencies]
formerr = { git = "https://github.com/cian-h/formerr" }
```

## Usage

### 1. The Result Type (ok / err)

Use Result when an operation might succeed or fail. It wraps either a success value (Ok) or an
error value (Err).

#### Specialized (High Performance)

For primitive types, use the specialized constructors and accessors (suffixed with type, e.g.,
`_real`, `_int`, `_r64`). These avoid dynamic allocation.

```fortran
program demo_result_fast
    use formerr_result
    implicit none

    type(result_type) :: res

    ! 1. Successful operation (Specialized for real)
    res = ok_real(5.0 / 2.0)

    if (res%is_ok()) then
        ! Zero-allocation access
        print *, "Result is:", res%unwrap_real()
    else
        ! Handle error
        print *, "Error code:", res%unwrap_err_int() ! Assuming error is an int
    end if
end program demo_result_fast
```

#### Generic (Flexible)

For objects, arrays, or when performance is secondary to flexibility.

```fortran
program demo_result_generic
    use formerr_result
    implicit none

    type(result_type) :: res
    class(*), pointer :: val

    ! 1. Successful operation
    res = divide(10.0, 2.0)

    if (res%is_ok()) then
        ! Unwrapping returns a polymorphic pointer (class(*), pointer)
        ! You must use `select type` to access the concrete data.
        val => res%unwrap()
        select type (val)
        type is (real)
            print *, "Result is:", val
        end select
    else
        ! Handle error
        val => res%unwrap_err()
        select type (val)
        type is (character(*))
            print *, "Error occurred: ", val
        end select
    end if

    ! 2. Failed operation
    res = divide(10.0, 0.0)

    ! You can also use unwrap_or to provide a default value
    print *, "Safe result:", res%unwrap_or(-1.0)

contains

    function divide(n, d) result(r)
        real, intent(in) :: n, d
        type(result_type) :: r

        if (d == 0.0) then
            r = err("Division by zero")
        else
            r = ok(n / d)
        end if
    end function divide

end program demo_result_generic
```

### 2. The Option Type (some / none)

Use Option when a value might be present or absent.

```fortran
program demo_option
    use formerr_option
    implicit none

    type(option) :: user_id

    ! Specialized usage
    user_id = some_int(1234)

    if (user_id%is_some()) then
        print *, "User ID found:", user_id%unwrap_int()
    else
        print *, "User not found."
    end if

    ! Defaults
    user_id = none()
    call print_id(user_id%unwrap_or_int(0))

contains
    subroutine print_id(val)
        integer, intent(in) :: val
        print *, "ID is ", val
    end subroutine
end program demo_option
```

## Safety & Pitfalls (Read This!)

While this library aims to improve safety, the nature of Fortran and performance optimizations
introduces some pitfalls you **must** be aware of. **This library DOES NOT protect you from the
following if you are careless:**

### 1. Mixing Generic and Specialized APIs

**This is the most dangerous pitfall.**

- **Using purely generic APIs is safe:** If you construct a result using a generic constructor
  (e.g., `ok(my_real_variable)`), then calling the generic `unwrap()` method will correctly access
  the value.
- **The danger arises when mixing:** If you construct a result using a **specialized** constructor
  (e.g., `ok_int(5)` to maximize performance by avoiding allocations), you **MUST** use the
  corresponding specialized unwrap (e.g., `unwrap_int()`).
- Calling the **generic** `unwrap()` method on a value created with a **specialized** constructor
  will return a **NULL POINTER** (silent failure if `DO_CHECKS` is off, or crash if `DO_CHECKS`
  is on). This occurs because the generic method attempts to access dynamic storage, which is
  empty for specialized values.
- **Rule of thumb:** Pair your constructors and unwrappers:
  `ok_X(...)` -> `unwrap_X()`. `ok(...)` -> `unwrap()`.

### 2. Default Safety Checks are ON

To prioritize safety, the library defines `DO_CHECKS = .true.` by default.

- `unwrap()` on an `Err` value **WILL** stop the program with an informative error message.
- This introduces a small branching overhead.
- For maximum performance in tight loops (where logic is proven correct), you can disable this by
  recompiling with `DO_CHECKS = .false.` in the source modules.

### 3. Pointer Lifetimes (Generic API)

The generic `unwrap()` returns a pointer (`class(*), pointer`) to internal storage.

**Why?**
This design avoids the performance penalty of deep-copying potentially large polymorphic objects
when accessing them.

**Mitigation:**

- **Scope it:** Use `select type` or `associate` blocks to limit the pointer's usage to the
  immediate scope where the `Result` is valid.
- **Copy it:** If you need the value to outlive the `Result`, allocate a new copy:
  `allocate(my_copy, source=res%unwrap())`.
- **Beware:** The pointer becomes a **dangling pointer** immediately after the parent `Result`
  object is destroyed. The goal of this library is to *minimize* the likelihood of dangling
  pointers, with some kind of compiler modifications (e.g: a borrow checker) *eliminiating*
  these mistakes is impossible for a mere library to achieve.

### 4. Polymorphic Allocation Overhead

The generic `ok(val)` and `unwrap_or(default)` functions involve allocation
(`allocate(..., source=val)`).

- In tight loops, use the specialized `_int`, `_real`, `_r64` variants. These use purely
  stack/in-place memory (`transfer`) and are significantly faster.

## API Reference

### formerr_result

- **Generic Constructors**: `ok(val)`, `err(val)` (Allocates)
- **Specialized Constructors**: `ok_int(v)`, `ok_real(v)`, `ok_r64(v)`, `ok_log(v)`, `ok_string(v)`
  (Fast)
- **Methods**:
  - `is_ok()`, `is_err()`: Logical checks.
  - `unwrap()`, `unwrap_err()`: Generic pointer access (Careful!).
  - `unwrap_int()`, `unwrap_real()`, ...: Specialized value access (Fast).
  - `unwrap_or(default)`: Returns allocatable clone of value or default.
  - `unwrap_or_int(default)`, ...: Returns value or default (Fast).

### formerr_option

- **Constructors**: `some(val)`, `none()`, `some_int(v)`, etc.
- **Methods**: Same patterns as Result (`is_some`, `is_none`, `unwrap_X`).

## Development Environment

This project supports **Nix** and **devenv** for a reproducible development environment.

1. Install [Nix](https://nixos.org/download.html).

1. Install [direnv](https://direnv.net/).

1. Enter the directory and allow the environment:

   ```sh
   direnv allow
   ```

### Running Tests

To run the test suite:

```sh
fortran-fpm test
```

## License

This project is licensed under the MIT License - see the LICENSE.md file for details.
