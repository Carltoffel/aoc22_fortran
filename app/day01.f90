program day01
    use file_reader_m, only: file_reader_t
    use calories_reader_m, only: calories_reader_t
    implicit none

    integer :: res

    print "('Task 1')"

    res = max_calories("input/day01/example1.txt")
    print "('Example 1: ', g0)", res

    res = max_calories("input/day01/input.txt")
    print "('Solution : ', g0)", res


    print *
    print "('Task 2')"

    res = max_three_calories("input/day01/example1.txt")
    print "('Example 1: ', g0)", res

    res = max_three_calories("input/day01/input.txt")
    print "('Solution : ', g0)", res

contains

    integer function max_calories(filename)
        character(*), intent(in) :: filename

        type(calories_reader_t) :: calories_reader
        integer                 :: calories

        calories_reader = calories_reader_t(filename)
        max_calories = 0

        do while(.not. calories_reader%eof)
            calories = calories_reader%next_calories()
            max_calories = max(max_calories, calories)
        end do
    end function max_calories


    integer function max_three_calories(filename)
        character(*), intent(in) :: filename

        type(calories_reader_t) :: calories_reader
        integer                 :: calories
        integer                 :: max_calories(3)

        calories_reader = calories_reader_t(filename)
        max_calories = 0

        do while(.not. calories_reader%eof)
            calories = calories_reader%next_calories()
            if (calories >= max_calories(1)) then
                max_calories     = [calories, max_calories(1:2)]
            else if (calories >= max_calories(2)) then
                max_calories(2:) = [calories, max_calories(2)]
            else if (calories >= max_calories(3)) then
                max_calories(3)  = calories
            end if
        end do
        max_three_calories = sum(max_calories)
    end function max_three_calories

end program day01
