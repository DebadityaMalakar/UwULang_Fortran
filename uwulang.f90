program UwULang
  implicit none
  integer, parameter :: mem_size = 32768  ! 2^15 size memory array
  integer :: memory(mem_size) = 0
  integer :: ptr = 1
  integer :: i, code_len
  character(len=10000) :: code
  character :: char_in
  logical :: continue
  integer :: loop_depth
  real :: temp  ! Temporary variable for random number generation

  print *, "Enter UwULang code:"
  read *, code
  code_len = len_trim(code)

  i = 1
  continue = .true.

  do while (continue .and. i <= code_len)
    select case (code(i:i))
      case ("👆")  ! Increment
        memory(ptr) = memory(ptr) + 1
      case ("👇")  ! Decrement
        memory(ptr) = memory(ptr) - 1
      case ("👉")  ! Move pointer right
        ptr = ptr + 1
        if (ptr > mem_size) ptr = 1  ! Wrap around at mem_size
      case ("👈")  ! Move pointer left
        ptr = ptr - 1
        if (ptr < 1) ptr = mem_size  ! Wrap around at 1
      case ("🥺")  ! Output character
        write(*, '(A)', advance='no') char(memory(ptr))
      case ("😳")  ! Input character
        read(*, '(A)') char_in
        memory(ptr) = ichar(char_in)
      case ("🥴")  ! Random short integer (0 to 32767)
        call random_number(temp)  ! Generate a random number in the range [0, 1)
        memory(ptr) = int(temp * 32767)  ! Scale to [0, 32767] and convert to integer
      case ("😒")  ! Jump to 😡 if memory(ptr) == 0
        if (memory(ptr) == 0) then
          loop_depth = 1
          do
            i = i + 1
            if (i > code_len) exit
            if (code(i:i) == "😒") loop_depth = loop_depth + 1
            if (code(i:i) == "😡") loop_depth = loop_depth - 1
            if (loop_depth == 0) exit
          end do
        end if
      case ("😡")  ! Jump back to 😒 if memory(ptr) != 0
        if (memory(ptr) /= 0) then
          loop_depth = 1
          do
            i = i - 1
            if (i < 1) exit
            if (code(i:i) == "😡") loop_depth = loop_depth + 1
            if (code(i:i) == "😒") loop_depth = loop_depth - 1
            if (loop_depth == 0) exit
          end do
        end if
    end select

    i = i + 1
  end do

end program UwULang
