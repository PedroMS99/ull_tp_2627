module read_input

   use iso_fortran_env, only: real64, iostat_end
   use geometry
   use particle
   
   implicit none
   
contains
   
   function input(Name_txt) result(particles)

		character(len=*), intent(in) :: Name_txt
		type(particle3d), allocatable :: particles(:)
		integer :: i, ios, ios_end, N, ident

		! Read the set up: for the moment particles position, velocity an mass
		open(unit=1, file=Name_txt, status="old", action="read")

		read(1, *) ! Skip Header

		! Read number of particles
		N = 0 

		do
			read(1, *, iostat=ios)
			if (ios == iostat_end) exit
			N = N + 1
		end do

		! Allocate acceleration and particles:
		allocate(particles(N))

		rewind(1) ! Return to beginning of file


		read(1, *)    ! Skip header

		! Loop to read particles from the file
		do i = 1, N

		   read(1, *) ident, &
					   particles(i)%p%x, &
					   particles(i)%p%y, &
					   particles(i)%p%z, &
					   particles(i)%v%x, &
					   particles(i)%v%y, &
					   particles(i)%v%z, &
					   particles(i)%m

		end do
		close(1) ! close the file, dont nedded forward


   end function input
   
end module read_input






