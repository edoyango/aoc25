program pt1

    use iso_fortran_env, only: iostat_end, int64

    implicit none
    character(100):: line
    integer:: io, iostat, top(12), i, num, line_len, j
    integer(int64):: max_jolt, total_jolt

    open (newunit=io, file="input.txt", status="old", action="read")

    total_jolt = 0
    do
        ! read line and handle eof
        read (io, "(A)", iostat=iostat) line
        if (iostat == iostat_end) exit

        ! start looping through digits
        line_len = len_trim(line)
        top(:) = 0 ! initialize best number
        do i = 1, line_len
            num = read_dig(line(i:i))
            ! see which digit in the max_jolt number this is best suited for
            ! highest digit encountered should be top(1), the next highest after that is top(2) etc.
            ! if within 12 digits of the end of the line, skip over first irrelevant digits
            do j = 1, 12
                if (num > top(j) .and. i < line_len - (12 - j) + 1) then
                    top(j) = num
                    top(j + 1:12) = 0
                    exit
                end if
            end do
        end do
        ! calculate max jolt for the line
        max_jolt = 0
        do i = 1, 12
            max_jolt = max_jolt + 10_int64**(12_int64 - int(i, kind=int64))*top(i)
        end do
        ! add max_jolt to total
        total_jolt = total_jolt + max_jolt
    end do
    close (io)

    print *, total_jolt

contains

    ! helper function to convert character digit to integer
    integer function read_dig(char)
        character, intent(in):: char
        read (char, "(I1)") read_dig
    end function read_dig
end program pt1
