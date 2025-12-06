program pt1
    use iso_fortran_env, only: iostat_end
    implicit none
    character(140):: line
    integer:: io, iostat, line_len, i, j, npapers, nremoved_prev, nremoved, grid(0:141, 0:141) ! body in 1:140, 0, 141 are buffers

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

    ! first pass
    nremoved_prev = 0
    nremoved = remove()
    ! keep removing accessible papers until no additional papers are accessible
    do while (nremoved > nremoved_prev)
        nremoved_prev = nremoved
        nremoved = nremoved + remove()
    enddo

    print *, nremoved


contains

    integer function remove()
        integer:: grid_tmp(0:141, 0:141)
        logical:: is_accessible
        grid_tmp(:, :) = grid(:, :)
        remove = 0
        do i = 1, line_len
            do j = 1, line_len
                npapers = sum(grid(i-1:i+1, j-1:j+1)) - grid(i, j)
                is_accessible = npapers < 4
                remove = remove + merge(grid(i,j), 0, is_accessible)
                if (is_accessible) grid_tmp(i, j) = 0
            enddo
        enddo
        grid(:, :) = grid_tmp(:, :)
    end function remove

end program pt1