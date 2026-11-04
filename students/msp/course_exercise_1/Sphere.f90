! this program create a IC in a sphere with radius 1, initial position 
! inside the sphere, initial velocities in three coordinates in betewn
! 0 to 1 and each particle with mass equal to 1 .

! it's create 7 random numbers for a single particle. r
! define the radius, the first 3, are the three define the three coordinates
! from the vector position, the last 3 are the velocities.

! to calculate de position of a single particle the next calculation is donne:
! r * position/|position|. ussing this we assure the particle position 
! is on a sphere with radius 1 and furthermore, it's homogenious.

program sphere

   use iso_fortran_env, only: real64
   implicit none

   integer :: i, s, n, ident, unit
   integer, allocatable :: seed(:)
   real(real64), allocatable :: x(:,:), r(:)

   n = 1000

   call random_seed(size=s)
   allocate(seed(s))

   seed = 10
   call random_seed(put=seed)

   allocate(x(n,7))
   allocate(r(n))

   x(:,7) = 1.0_real64 ! de momento la masa es toda la misma y vale 1

   call random_number(x(:,1:6))
   x(:,1:6) = 2*x(:,1:6) - 1.0_real64

   !cambio la semilla para los radios
   seed = 11
   call random_seed(put=seed)

   call random_number(r(:))

   do i = 1, n
      x(i,1:3) = r(i) * x(i,1:3) / &
      sqrt(x(i,1)**2 + x(i,2)**2 + x(i,3)**2)
   end do

   open(newunit=unit, file="sphere.txt", status="replace", action="write")
   write (unit , *) "ID	", "X	", "Y	", "Z	", "VX	", "VY	", "VZ	", "M	"
   ! Segundo bucle sobre el número de partículas
   do ident = 1, n
      write (unit , *) ident, x(ident,:) ! guardo resultados
   end do

   close(unit)

end program sphere
