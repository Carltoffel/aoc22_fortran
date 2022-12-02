program day02
    use file_reader_m, only: file_reader_t
    implicit none

    integer :: res

    print "('Task 2')"

    res = total_score("input/day02/example1.txt")
    print "('Example 1: ', g0)", res

    res = total_score("input/day02/input.txt")
    print "('Solution : ', g0)", res

contains


    elemental impure integer function total_score(filename)
        use rps_game_m, only: rps_game_t
        character(*), intent(in) :: filename

        type(rps_game_t) :: rps_game
        integer          :: score

        rps_game    = rps_game_t(filename)
        total_score = 0
        score       = rps_game%next_score()
        do while (score>0)
            total_score = total_score + score
            score       = rps_game%next_score()
        end do
    end function total_score


end program day02
