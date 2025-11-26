module test_utils
    implicit none
    private

    public :: assert, report_and_finish

    integer :: failures = 0
    integer :: total_tests = 0

contains

    subroutine assert(condition, msg)
        logical, intent(in) :: condition
        character(*), intent(in) :: msg

        total_tests = total_tests + 1

        if (.not. condition) then
            print *, "[FAIL] ", msg
            failures = failures + 1
        else
            print *, "[PASS] ", msg
        end if
    end subroutine assert

    subroutine report_and_finish()
        print *, "------------------------------------------------"
        print *, "Test Summary"
        print *, "------------------------------------------------"
        print *, "Total checked: ", total_tests
        print *, "Passed:        ", total_tests - failures
        print *, "Failed:        ", failures
        print *, "------------------------------------------------"

        if (failures > 0) then
            print *, "RESULT: FAILED"
            error stop 1
        else
            print *, "RESULT: OK"
        end if
    end subroutine report_and_finish

end module test_utils
