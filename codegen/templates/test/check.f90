program check
    use, intrinsic :: iso_fortran_env, only: error_unit
    use testdrive, only: new_testsuite, testsuite_type, collect_interface, unittest_type, error_type
    use either_suite, only: collect_either_suite
    use option_suite, only: collect_option_suite
    use result_suite, only: collect_result_suite
    use property_suite, only: collect_property_suite
    implicit none
    integer :: stat, is
    type(testsuite_type), allocatable :: testsuites(:)

    stat = 0

    testsuites = [ &
                 new_testsuite("either_suite", collect_either_suite), &
                 new_testsuite("option_suite", collect_option_suite), &
                 new_testsuite("result_suite", collect_result_suite), &
                 new_testsuite("property_suite", collect_property_suite) &
                 ]

    print '(A)', repeat("-", 80)
    print '(A20, 1X, A40, 1X, A10)', "Suite", "Test", "Status"
    print '(A)', repeat("-", 80)

    do is = 1, size(testsuites)
        call run_testsuite_table(testsuites(is)%name, testsuites(is)%collect, stat)
    end do

    print '(A)', repeat("-", 80)

    if (stat > 0) then
        write (error_unit, '(i0, 1x, a)') stat, "test(s) failed!"
        error stop
    end if

contains

    subroutine run_testsuite_table(suite_name, collect, stat)
        character(len=*), intent(in) :: suite_name
        procedure(collect_interface) :: collect
        integer, intent(inout) :: stat

        type(unittest_type), allocatable :: testsuite(:)
        type(error_type), allocatable :: error
        integer :: i
        character(len=10) :: status_str
        logical :: passed

        call collect(testsuite)

        do i = 1, size(testsuite)
            call testsuite(i)%test(error)

            passed = .true.
            if (allocated(error)) then
                if (error%stat /= 0) passed = .false. ! 0 is success, 77 is skipped
                if (error%stat == 77) passed = .true. ! treat skipped as pass/skip
            end if

            ! Check should_fail logic
            if (testsuite(i)%should_fail) then
                passed = .not. passed
            end if

            if (allocated(error)) then
                 if (error%stat == 77) then
                     status_str = "[SKIPPED]"
                 elseif (passed) then
                     status_str = "[PASS]"
                 else
                     status_str = "[FAIL]"
                     stat = stat + 1
                 end if
            else
                 if (passed) then
                     status_str = "[PASS]"
                 else
                     status_str = "[FAIL]"
                     stat = stat + 1
                 end if
            end if

            print '(A20, 1X, A40, 1X, A10)', &
                  trim(suite_name), &
                  trim(testsuite(i)%name), &
                  status_str

            if (.not. passed .and. allocated(error)) then
                if (allocated(error%message)) then
                    print '(4X, "Message: ", A)', error%message
                end if
            end if

            if (allocated(error)) deallocate(error)
        end do
    end subroutine run_testsuite_table

end program check
