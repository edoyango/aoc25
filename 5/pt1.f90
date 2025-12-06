program pt1

    use iso_fortran_env, only: iostat_end, int64

    implicit none
    integer:: io, iostat, nfresh, nranges, hyphen_idx, line_len, i
    character(50):: line
    integer(int64):: ranges(2, 194), ingredient

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

    nfresh = 0
    do
        read(io, "(I16)", iostat=iostat) ingredient
        if (iostat == iostat_end) exit
        do i = 1, nranges
            if (ranges(1, i) <= ingredient .and. ingredient <= ranges(2,i)) then
                nfresh = nfresh + 1
                exit
            endif
        enddo
    enddo
    print *, nfresh

end program pt1