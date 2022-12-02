module rps_game_m
    use file_reader_m, only: file_reader_t
    implicit none
    private
    public :: rps_game_t

    type, extends(file_reader_t) :: rps_game_t
    contains
        private
        procedure, public :: next_score
    end type rps_game_t

    interface rps_game_t
        module procedure :: rps_game_constructor
    end interface rps_game_t

contains


    elemental impure function rps_game_constructor(filename) result(rps_game)
        character(*), intent(in) :: filename
        type(rps_game_t) :: rps_game

        call rps_game%constructor(filename)
    end function rps_game_constructor


    elemental impure function next_score(self) result(score)
        class(rps_game_t), intent(inout) :: self
        integer :: score

        character(:), allocatable :: line
        line = self%next_line()
        if (.not. self%eof) then
            score = eval_game(line)
        else
            score = 0
        end if
        deallocate(line)
    end function next_score


    elemental function eval_game(line) result(score)
        character(3), intent(in) :: line
        integer :: score

        integer :: player1, player2

        player1 = ichar(line(1:1)) - ichar('A') + 1
        player2 = ichar(line(3:3)) - ichar('X') + 1

        score = player2 + modulo(player2-player1+1, 3)*3
    end function eval_game


end module rps_game_m
