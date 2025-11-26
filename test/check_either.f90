module either_tests
    use testdrive, only: new_unittest, check, error_type
    use formerr_either
    implicit none

contains

    subroutine test_integer_left(error)
        type(error_type), allocatable, intent(out) :: error
        type(either) :: e
        class(*), pointer :: p

        e = left(42)

        call check(error, e%is_left(), "is_left() should be true")
        if (allocated(error)) return

        call check(error,.not. e%is_right(), "is_right() should be false")
        if (allocated(error)) return

        p => e%get_left()
        call check(error, associated(p), "get_left() pointer should be associated")
        if (allocated(error)) return

        select type (p)
        type is (integer)
            call check(error, p == 42, "Value inside left should be 42")
        class default
            call check(error, .false., "Wrong type stored in Left (expected Integer)")
        end select
    end subroutine test_integer_left

    subroutine test_real_right(error)
        type(error_type), allocatable, intent(out) :: error
        type(either) :: e
        class(*), pointer :: p

        e = right(3.14)

        call check(error, e%is_right(), "is_right() should be true")
        if (allocated(error)) return

        p => e%get_left()
        call check(error,.not. associated(p), "get_left() on a Right value should return null")
        if (allocated(error)) return

        p => e%get_right()
        select type (p)
        type is (real)
            ! Using a small epsilon for float comparison
            call check(error, abs(p - 3.14) < 0.0001, "Value inside right should be approx 3.14")
        class default
            call check(error, .false., "Wrong type stored in Right (expected Real)")
        end select
    end subroutine test_real_right

    subroutine test_string_logic(error)
        type(error_type), allocatable, intent(out) :: error
        type(either) :: e
        class(*), pointer :: p

        e = left("Fortran")
        p => e%get_left()

        select type (p)
        type is (character(*))
            call check(error, p == "Fortran", "String content should match 'Fortran'")
        class default
            call check(error, .false., "Wrong type stored (expected Character)")
        end select
    end subroutine test_string_logic

    subroutine test_generic_constructor(error)
        type(error_type), allocatable, intent(out) :: error
        type(either) :: e

        e = either(left_val=100)
        call check(error, e%is_left(), "either(left_val=...) creates Left")
        if (allocated(error)) return

        e = either(right_val=200)
        call check(error, e%is_right(), "either(right_val=...) creates Right")
    end subroutine test_generic_constructor

end module either_tests

module either_suite
    use testdrive, only: new_unittest, unittest_type, error_type, check
    use either_tests
    implicit none
    private

    public :: collect_either_suite

contains

    subroutine collect_either_suite(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
                    new_unittest("integer_left", test_integer_left), &
                    new_unittest("real_right", test_real_right), &
                    new_unittest("string_logic", test_string_logic), &
                    new_unittest("generic_constructor", test_generic_constructor) &
                    ]

    end subroutine collect_either_suite

end module either_suite
