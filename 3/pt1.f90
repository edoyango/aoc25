program pt1

    use iso_fortran_env, only: iostat_end

    implicit none
    character(100):: line
    integer:: io, iostat, top2(2), i, num, max_jolt, line_len, total_jolt

    open(newunit=io, file="input.txt", status="old", action="read")

    total_jolt = 0
    do
        read(io, "(A)", iostat=iostat) line
        if (iostat == iostat_end) exit
        line_len = len_trim(line)
        top2(:) = 0
        do i = 1, line_len
            num = read_dig(line(i:i))
            ! see if encountered digit is higher than best first digit seen so far. Highest digit
            ! encountered should be first digit. Skip updating first digit if at last digit of line.
            if (num > top2(1) .and. i < line_len) then
                top2(1) = num
                top2(2) = 0
            elseif (num > top2(2)) then
                top2(2) = num
            endif
        enddo
        ! calculate max jolt for the line
        max_jolt = 10*top2(1) + top2(2)
        ! add max_jolt to total
        total_jolt = total_jolt + max_jolt
    enddo
    close(io)

    print *, total_jolt

contains

    integer function read_dig(char)
        character, intent(in):: char
        read(char, "(I1)") read_dig
    end function read_dig
end program pt1