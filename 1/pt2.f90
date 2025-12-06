program pt2

    use iso_fortran_env, only: iostat_end

    implicit none
    character(4):: row
    integer:: io, iostat, cursor, sign, dist, nzeros

    ! intiailize
    cursor = 50 ! starting position
    nzeros = 0  ! number of zeros

    ! input file with the rotations
    open (newunit=io, file="input.txt", status="old", action="read")

    do
        ! read in the row
        read (io, '(A)', iostat=iostat) row

        ! exit once end of line reached
        if (iostat == iostat_end) then
            print *, "done, counted ", nzeros, " zeros."
            exit
        else
            ! subtract from cursor if L rotation, else add.
            sign = merge(-1, 1, row(1:1) == "L")
            ! read the distance to move the cursor.
            read (row(2:4), "(I3)") dist
            ! update cursor with movement distance
            cursor = cursor + sign*dist
            ! increment
            nzeros = nzeros + &
                     abs(cursor)/100 + & ! increment for each time going over 100 in abs terms.
                     merge(1, 0, cursor < 1 .and. cursor /= sign*dist) ! add one extra if at between -99 and 0 inclusive, but not if starting at 0.
            ! circular update cursor.
            cursor = mod(cursor, 100)
            cursor = merge(100 + cursor, cursor, cursor < 0)

        end if
    end do
    close (io)

end program pt2
