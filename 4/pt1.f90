program pt1
    use iso_fortran_env, only: iostat_end
    implicit none
    character(140):: line
    integer:: io, iostat, line_len, i, j, npapers, naccessible, grid(0:141, 0:141) ! body in 1:140, 0, 141 are buffers

    ! initiliaze grid
    grid(:, :) = 0
    open(newunit=io, file="input.txt", status="old", action="read")

    ! populate grid
    i = 0
    do
        read(io, "(A)", iostat=iostat) line
        if (iostat == iostat_end) exit

        i = i + 1
        line_len = len_trim(line)
        do j = 1, line_len
            if (line(j:j) == '@') grid(i, j) = 1
        enddo
    enddo

    ! count how how many papers are accessible by traversing grid
    ! could be made faster and more memory efficient by doing the count while parsing the lines.
    naccessible = 0
    do i = 1, line_len
        do j = 1, line_len
            npapers = sum(grid(i-1:i+1, j-1:j+1)) - grid(i, j)
            naccessible = naccessible + merge(grid(i,j), 0, npapers < 4)
        enddo
    enddo
    print *, naccessible

end program pt1