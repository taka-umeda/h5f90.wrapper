program main
  use h5f90_wrapper
  
  implicit none
  ! by Takayuki Umeda
  ! Feburary 26, 2024: ver.1.0
  !
  ! This program includes samples to use subroutines
  !    in module "h5f90_wrapper"

! integer(kind=4), parameter :: kr = 4 ! single precision
  integer(kind=4), parameter :: kr = 8 ! double precision
  
  integer(kind=8), parameter :: N=10
  
  real(kind=kr) :: r1(N)
  real(kind=kr) :: r2(N,N)
  real(kind=kr) :: r3(N,N,N)
  real(kind=kr) :: r4(N,N,N,N)
  real(kind=kr) :: r5(N,N,N,N,N)
  real(kind=kr) :: r6(N,N,N,N,N,N)
  
  integer(kind=kr) :: h1(N)
  integer(kind=kr) :: h2(N,N)
  integer(kind=kr) :: h3(N,N,N)
  
  integer(kind=8) :: msize(6)
!******************************************************************
  msize(1) = N
  msize(2) = N
  msize(3) = N
  msize(4) = N
  msize(5) = N
  msize(6) = N

  call random_number(r1)
  call random_number(r2)
  call random_number(r3)
  call random_number(r4)
  call random_number(r5)
  call random_number(r6)

  h1 = r1*real(N,kind=kr)
  h2 = r2*real(N,kind=kr)
  h3 = r3*real(N,kind=kr)
  
  ! initialize HDF5
  call hdf_start

  ! create a file
  call hdf_create('filename.h5')

  ! write an array to a dataset in the file
  call hdf_write('R1','filename.h5',msize(1:1),r1)
  call hdf_write('R2','filename.h5',msize(1:2),r2)
  call hdf_write('R3','filename.h5',msize(1:3),r3)
  call hdf_write('R4','filename.h5',msize(1:4),r4)
  call hdf_write('R5','filename.h5',msize(1:5),r5)
  call hdf_write('R6','filename.h5',msize(1:6),r6)

  call hdf_write('H1','filename.h5',msize(1:1),h1)
  call hdf_write('H2','filename.h5',msize(1:2),h2)
  call hdf_write('H3','filename.h5',msize(1:3),h3)

  ! read a dataset in the file to an array 
  call hdf_read('R1','filename.h5',msize(1:1),r1)
  call hdf_read('R2','filename.h5',msize(1:2),r2)
  call hdf_read('R3','filename.h5',msize(1:3),r3)
  call hdf_read('R4','filename.h5',msize(1:4),r4)
  call hdf_read('R5','filename.h5',msize(1:5),r5)
  call hdf_read('R6','filename.h5',msize(1:6),r6)

  call hdf_read('H1','filename.h5',msize(1:1),h1)
  call hdf_read('H2','filename.h5',msize(1:2),h2)
  call hdf_read('H3','filename.h5',msize(1:3),h3)
  
  ! finilize HDF5
  call hdf_end
!******************************************************************
end program main
