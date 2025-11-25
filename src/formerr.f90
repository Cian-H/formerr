module formerr
   implicit none
   private

   public :: say_hello
contains
   subroutine say_hello
      print *, "Hello, formerr!"
   end subroutine say_hello
end module formerr
