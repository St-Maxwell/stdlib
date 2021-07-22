program test_to_string
    
    use stdlib_ascii, only: to_string
    use stdlib_strings, only: format_to_string
    use, intrinsic :: iso_fortran_env, only: int64, real64
    implicit none
    
    integer :: i(10000)
    logical :: l(10000)
    integer(int64) :: tic, toc, trate
    real(real64) :: t_ms
    character(10) :: string(10000)
    integer :: j

    print "(A, t60, A)", "Item", "Time /ms"

    !> Tests for integer `i = 1`
    i = 1
    call system_clock(count_rate=trate)

    call system_clock(count=tic)
    do j = 1, 10000
        string(j) = to_string(i(j))
    end do
    call system_clock(count=toc)
    t_ms = (toc-tic) * 1000._real64 / trate
    print "(A, t60, g0)", "`to_string` time for integer `i=1` : ", t_ms

    call system_clock(count=tic)
    do j = 1, 10000
        string(j) = format_to_string(i(j))
    end do
    call system_clock(count=toc)
    t_ms = (toc-tic) * 1000._real64 / trate
    print "(A, t60, g0)", "`format_to_string` time for integer `i=1` : ", t_ms

    !> Tests for integer `i = huge(i(1))`
    i = huge(i(1))
    call system_clock(count_rate=trate)

    call system_clock(count=tic)
    do j = 1, 10000
        string(j) = to_string(i(j))
    end do
    call system_clock(count=toc)
    t_ms = (toc-tic) * 1000._real64 / trate
    print "(A, t60, g0)", "`to_string` time for integer `i=huge(i(1))` : ", t_ms

    call system_clock(count=tic)
    do j = 1, 10000
        string(j) = format_to_string(i(j))
    end do
    call system_clock(count=toc)
    t_ms = (toc-tic) * 1000._real64 / trate
    print "(A, t60, g0)", "`format_to_string` time for integer `i=huge(i(1))` : ", t_ms

    !> Tests for integer `l = .true.`
    l = .true.
    call system_clock(count_rate=trate)

    call system_clock(count=tic)
    do j = 1, 10000
        string(j) = to_string(l(j))
    end do
    call system_clock(count=toc)
    t_ms = (toc-tic) * 1000._real64 / trate
    print "(A, t60, g0)", "`to_string` time for logical `l=.true.` : ", t_ms

    call system_clock(count=tic)
    do j = 1, 10000
        string(j) = format_to_string(l(j))
    end do
    call system_clock(count=toc)
    t_ms = (toc-tic) * 1000._real64 / trate
    print "(A, t60, g0)", "`format_to_string` time for logical `l=.true.` : ", t_ms

    !> RESULTS
    !-------------------------------------------------------------------------------
    !> Item                                                       Time /ms
    !> `to_string` time for integer `i=1` :                       1.1416999999999999
    !> `format_to_string` time for integer `i=1` :                63.093699999999998
    !> `to_string` time for integer `i=huge(i(1))` :              1.5802000000000000
    !> `format_to_string` time for integer `i=huge(i(1))` :       60.947099999999999
    !> `to_string` time for logical `l=.true.` :                  0.92299999999999993E-1
    !> `format_to_string` time for logical `l=.true.` :           59.209699999999998

end program test_to_string
