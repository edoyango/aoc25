program pt2

    use iso_fortran_env, only: iostat_end, int64

    implicit none
    integer:: io, iostat, nranges, hyphen_idx, line_len, i, j
    character(50):: line
    integer(int64):: ranges(2, 194), nfresh

    open(newunit=io, file="input.txt", status="old", action="read")
    nranges = 0
    do
        read(io, "(A)", iostat=iostat) line
        line_len = len_trim(line)
        if (line_len == 0) exit
        hyphen_idx = index(line, '-')
        nranges = nranges + 1
        read(line(1:hyphen_idx-1), "(I32)") ranges(1, nranges)
        read(line(hyphen_idx+1:line_len), "(I32)") ranges(2, nranges)
    enddo

    loopi: do i = 1, nranges-1
        if (all(ranges(:,i) > 0)) then
            do j = i+1, nranges
                if (all(ranges(:,j) > 0) .and. .not.(ranges(1, j) > ranges(2, i) .or. ranges(2, j) < ranges(1, i))) then
                    ! j is completely inside i - wipeout j
                    if (ranges(1, j) >= ranges(1, i) .and. ranges(2, j) <= ranges(2, i)) then
                        ranges(1, j) = 0
                        ranges(2, j) = -1
                    ! i is completely inside j - wipeout i
                    elseif (ranges(1, j) < ranges(1, i) .and. ranges(2, j) > ranges(2, i)) then
                        ranges(1, i) = 0
                        ranges(2, i) = -1
                        cycle loopi
                    ! left overhang - ajdust j's upper
                    elseif (ranges(1, j) < ranges(1, i) .and. ranges(2, j) >= ranges(1, i)) then
                        ranges(2, j) = ranges(1, i) - 1
                    ! right overhang - adjust j's lower
                    elseif (ranges(2, j) > ranges(2, i) .and. ranges(1, j) <= ranges(2,i)) then
                        ranges(1, j) = ranges(2, i) + 1
                    endif
                endif
            enddo
        endif
    enddo loopi

    ! count numbers between ranges
    nfresh = 0
    do i = 1, nranges
        nfresh = nfresh + max(0, ranges(2, i) - ranges(1, i) + 1)
    enddo

    print *, nfresh

    close(io)

end program pt2