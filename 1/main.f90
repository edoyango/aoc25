program day1

    use iso_fortran_env, only: iostat_end

    implicit none
    character(4):: row
    integer:: io, iostat, cursor, sign, dist, nzeros

    ! intiailize
    cursor = 50 ! starting position
    nzeros = 0  ! number of zeros

    ! input file with the rotations
    open(newunit=io, file="input.txt", status="old", action="read")

    do
        ! read in the row
        read(io, '(A)', iostat=iostat) row

        ! exit once end of line reached
        if (iostat == iostat_end) then
            print *, "done, counted ", nzeros, " zeros."
            exit
        else
            ! subtract from cursor if L rotation, else add.
            sign = merge(-1, 1, row(1:1) == "L")
            ! read the distance to move the cursor.
            read(row(2:4), "(I3)") dist
            ! update cursor, circling it back to 0 if > 99.
            cursor = mod(cursor + sign*dist, 100)
            ! handle case when cursor < 0.
            cursor = merge(100+cursor, cursor, cursor<0)
            ! update zero counter.
            nzeros = nzeros + merge(1, 0, cursor == 0)
        endif
    enddo
    close(io)

end program day1