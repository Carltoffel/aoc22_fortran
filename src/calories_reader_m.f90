module calories_reader_m
    use file_reader_m, only: file_reader_t
    implicit none
    private
    public :: calories_reader_t

    type, extends(file_reader_t) :: calories_reader_t
    contains
        private
        procedure, public :: next_calories
    end type calories_reader_t

    interface calories_reader_t
        module procedure calories_reader_constructor
    end interface calories_reader_t

contains


    elemental impure function calories_reader_constructor(filename) result(calories_reader)
        character(*), intent(in) :: filename
        type(calories_reader_t)  :: calories_reader

        call calories_reader%constructor(filename)
    end function calories_reader_constructor


    elemental impure function next_calories(self) result(calories)
        class(calories_reader_t), intent(inout) :: self
        integer                                 :: calories

        integer :: item

        calories = 0
        item = self%next_int()
        do while (item>0)
            calories = calories + item
            item = self%next_int()
        end do
    end function next_calories


end module calories_reader_m
