module file_reader_m
    implicit none
    private
    public :: file_reader_t

    type file_reader_t
        private
        logical, public           :: eof = .false.
        character(:), allocatable :: filename
        integer                   :: unit
    contains
        private
        procedure, public :: constructor
        procedure, public :: next_line
        procedure, public :: next_int
    end type file_reader_t

    interface file_reader_t
        module procedure file_reader_constructor
    end interface file_reader_t

contains


    elemental impure subroutine constructor(self, filename)
        class(file_reader_t), intent(inout) :: self
        character(*),         intent(in)    :: filename

        allocate(self%filename, source=filename)
        open(newunit=self%unit, file=filename)
    end subroutine constructor


    elemental impure function file_reader_constructor(filename) result(file_reader)
        character(*), intent(in) :: filename
        type(file_reader_t)      :: file_reader

        call file_reader%constructor(filename)
    end function file_reader_constructor


    function next_line(self) result(line)
        class(file_reader_t), intent(inout) :: self
        character(:), allocatable           :: line

        integer         :: ierr
        character(1024) :: tmp_line

        read(self%unit, "(a)", iostat=ierr) tmp_line
        if (ierr == 0) then
            allocate(line, source=trim(tmp_line))
        else
            allocate(character(0) :: line)
            self%eof = .true.
        end if
    end function next_line


    function next_int(self) result(i)
        class(file_reader_t), intent(inout) :: self
        integer                             :: i

        character(:), allocatable :: line

        line = self%next_line()
        if (len(line) > 0) then
            read(line, *) i
        else
            i = 0
        end if
        deallocate(line)
    end function next_int


end module file_reader_m
