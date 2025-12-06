program pt2
    use iso_fortran_env, only: iostat_end, int64
    implicit none
    type:: prefix_suffix_t
        integer(int64):: prefix, suffix
    end type prefix_suffix_t
    character(:), allocatable:: line, num1, num2, num
    character(20):: buffer
    integer:: io, iostat, sz, istart, iend
    integer(int64):: rangestart, rangeend, i, nvalid
    type(prefix_suffix_t):: pf
    logical:: repeats

    ! read in the line of content
    open (newunit=io, file="input.txt", status="old", action="read")
    line = ""
    do
        read (io, "(A)", advance="no", iostat=iostat) buffer
        line = line//trim(buffer)
        if (iostat < 0) exit
    end do
    close (io)

    ! process the content
    num = ""
    sz = len_trim(line)
    istart = 1
    iend = 1
    nvalid = 0_int64
    do while (iend /= 0)
        ! find the next hyphen
        iend = index(line(istart:sz), '-')
        ! collect the number before the hyphen - accounting for iend being relative to istart
        num1 = line(istart:istart + iend - 2)
        ! update istart
        istart = istart + iend
        ! find the second number in the range using an apostrophe
        iend = index(line(istart:sz), ',')
        ! index failing to find ',' means we're at the end - update the second number.
        if (iend == 0) then
            num2 = line(istart:sz)
        else
            num2 = line(istart:istart + iend - 2)
        end if
        istart = istart + iend

        ! convert chars to ints
        read (num1, "(I10)") rangestart
        read (num2, "(I10)") rangeend

        ! loop over all numbers between ranges - this could be done faster by skipping ranges but iiwii.
        do i = rangestart, rangeend
            nvalid = nvalid + merge(i, 0_int64, is_repeating(i))
        end do
    end do

    print *, nvalid

contains

    ! function to get number of decimal digits in x
    integer(int64) function dec_digits(x)
        integer(int64), intent(in):: x
        character(20):: x_char
        write (x_char, "(I20)") x
        dec_digits = int(log10(dble(x)), kind=int64) + 1_int64
    end function dec_digits

    logical function is_repeating(a)
        integer(int64), intent(in):: a
        integer(int64):: nd, max_repeats, i, nchunks, j
        character(20):: a_char, prev_chunk
        nd = dec_digits(a)
        write (a_char, "(I20)") a
        a_char = adjustl(a_char)
        is_repeating = .false.
        max_repeats = nd/2_int64
        loop_repeats: do i = 1, max_repeats
            if (mod(nd, i) == 0) then
                nchunks = nd/i
                prev_chunk = a_char(1:i)
                do j = 2, nchunks
                    ! if non-repeating pattern found, try next no. of repeats
                    if (a_char((j - 1)*i + 1:j*i) /= trim(prev_chunk)) cycle loop_repeats
                end do
                ! if above loop completes, that means the num is repeating
                is_repeating = .true.
                exit
            end if
        end do loop_repeats

    end function is_repeating

end program pt2
