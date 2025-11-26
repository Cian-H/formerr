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

```fortran
program demo_result
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
        type is (character(\*))
            print *, "Error occurred: ", val
        end select
    end if

    ! 2. Failed operation
    res = divide(10.0, 0.0)

    ! You can also use unwrap_or to provide a default value if it failed
    ! (Note: unwrap_or returns an allocatable, not a pointer)
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

end program demo_result
```

### 2. The Option Type (some / none)

Use Option when a value might be present or absent (avoiding null pointer exceptions or magic
numbers like -1).

```fortran
program demo_option
    use formerr_option
    implicit none

    type(option) :: user_id
    class(\*), pointer :: p

    user_id = find_user("alice")

    if (user_id%is_some()) then
        p => user_id%unwrap()
        select type (p)
        type is (integer)
            print *, "User ID found:", p
        end select
    else
        print *, "User not found."
    end if

    ! defaults
    user_id = none()
    ! Returns 0 if none, otherwise the contained value
    call print_id(user_id%unwrap_or(0))

contains

    function find_user(name) result(opt)
        character(\*), intent(in) :: name
        type(option) :: opt

        if (name == "alice") then
            opt = some(1234)
        else
            opt = none()
        end if
    end function find_user

    subroutine print_id(val)
        class(*), intent(in) :: val
        select type (val)
        type is (integer)
            print *, "ID is ", val
        end select
    end subroutine

end program demo_option
```

## API Reference

### formerr_result

- **Constructors**:
  - `ok(val)`: Creates a `success` result containing `val`.
  - `err(val)`: Creates a `failure` result containing `val`.
- **Methods**:
  - `is_ok()`: Logical. `.TRUE.` if `success`.
  - `is_err()`: Logical. `.TRUE.` if `failure`.
  - `unwrap()`: Returns `class(*)`, pointer to the Ok value. Fails if Err.
  - `unwrap_err()`: Returns `class(*)`, pointer to the Err value. Fails if Ok.
  - `unwrap_or(default)`: Returns `class(*), allocatable`. Returns content if Ok, default if Err.

### formerr_option

- **Constructors**:
  - `some(val)`: Creates an option containing `val`.
  - `none()`: Creates an empty option.
- **Methods**:
  - `is_some()`: Logical. `.TRUE.` if value exists.
  - `is_none()`: Logical. `.TRUE.` if empty.
  - `unwrap()`: Returns `class(*)`, pointer to the value. Fails if None.
  - `unwrap_or(default)`: Returns `class(*), allocatable`. Returns content if Some, default if None.

### formerr_either

The base type for Option and Result. Useful for generic "Left vs Right" logic.

- **Constructors**: `left(val)`, `right(val)`.
- **Methods**: `is_left()`, `is_right()`, `get_left()`, `get_right()`.

## Development Environment

This project supports **Nix** and **devenv** for a reproducible development environment.

1. Install [Nix](https://nixos.org/download.html).

1. Install [direnv](https://direnv.net/).

1. Enter the directory and allow the environment:

   ```sh
   direnv allow
   ```

   This will automatically install gfortran, fortls (Language Server), fpm, and formatting tools
   defined in devenv.nix.

### Running Tests

To run the test suite (powered by test-drive):

```sh
fortran-fpm test
```

## License

This project is licensed under the MIT License - see the
[LICENSE.md](http://docs.google.com/LICENSE.md) file for details.

## Contributing

Contributions are welcome! Please feel free to submit a Pull Request.

1. Fork the repository.
1. Create your feature branch (`git checkout -b feature/AmazingFeature`).
1. Commit your changes (`git commit -m 'Add some AmazingFeature'`).
1. Push to the branch (`git push origin feature/AmazingFeature`).
1. Open a Pull Request.
