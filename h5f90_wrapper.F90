module H5F90_WRAPPER
  use hdf5 ! HDF5 is necessary
  
  implicit none
  private
  ! by Takayuki Umeda
  ! Feburary 26, 2024: ver.1.0
  !
  ! This module includes F90 wrappers for write/read HDF5 data: 

  ! subroutine to initialize hdf5
  public hdf_start

  ! subroutine to finalize hdf5
  public hdf_end

  ! subroutine to create a file in hdf5 format
  public hdf_create ! ('filename.h5')

  ! subroutine to write an array to a dataset in a hdf5 file
  public hdf_write ! ('data_name','file_name',size(1:N),array(N))
  interface hdf_write
     module procedure wrt1r4
     module procedure wrt2r4
     module procedure wrt3r4
     module procedure wrt4r4
     module procedure wrt5r4
     module procedure wrt6r4
     
     module procedure wrt1r8
     module procedure wrt2r8
     module procedure wrt3r8
     module procedure wrt4r8
     module procedure wrt5r8
     module procedure wrt6r8
     
     module procedure wrt1i4
     module procedure wrt2i4
     module procedure wrt3i4
     
     module procedure wrt1i8
     module procedure wrt2i8
     module procedure wrt3i8
  end interface
  
  ! subroutine to read a dataset in a hdf5 file to an array
  public hdf_read ! ('data_name','file_name',size(1:N),array(N))
  interface hdf_read
     module procedure read1r4
     module procedure read2r4
     module procedure read3r4
     module procedure read4r4
     module procedure read5r4
     module procedure read6r4
     
     module procedure read1r8
     module procedure read2r8
     module procedure read3r8
     module procedure read4r8
     module procedure read5r8
     module procedure read6r8
     
     module procedure read1i4
     module procedure read2i4
     module procedure read3i4
     
     module procedure read1i8
     module procedure read2i8
     module procedure read3i8
  end interface

 
!#define H5T_NATIVE_INTEGER8 H5T_NATIVE_INTEGER_8 ! for HDF5 < 1-10
#define H5T_NATIVE_INTEGER8 H5T_NATIVE_INTEGER_KIND(4) ! for HDF5 >= 1.10

  integer(HID_T) :: file_id, space_id, type_id, data_id
  integer(kind=4) :: ierr
  
contains
!*******************************************************************
  subroutine hdf_start
    call H5open_f(ierr)
  end subroutine hdf_start
!*******************************************************************
  subroutine hdf_end
    call H5close_f(ierr)
  end subroutine hdf_end
!*******************************************************************
  subroutine hdf_create(filename)
    character(len=*),intent(in) :: filename
    call H5Fcreate_f(filename,H5F_ACC_TRUNC_F,file_id,ierr)
    print*, ierr,' - ', ' create ', file_id, ' : ', filename
    call H5Fclose_f(file_id,ierr)
  end subroutine hdf_create
!*******************************************************************
  subroutine wrt1r4(data_name,filename,dims,data_set)
    character(len=*),intent(in) :: data_name
    character(len=*),intent(in) :: filename
    integer(HSIZE_T),intent(in) :: dims(1)
    real(kind=4),intent(in) :: data_set(dims(1))

    call H5open_f(ierr)
    call H5Fopen_f(filename,H5F_ACC_RDWR_F,file_id,ierr)

    call H5Screate_simple_f(1,dims,space_id,ierr)
    call H5Dcreate_f(file_id,data_name,H5T_IEEE_F32BE,space_id,data_id,ierr)
    call H5Dwrite_f(data_id,H5T_NATIVE_REAL,data_set,dims,ierr)
    call H5Dclose_f(data_id,ierr)
    call H5Sclose_f(space_id,ierr)
    
    print*, ierr,' - ', ' write ', data_name

    call H5Fclose_f(file_id,ierr)
  end subroutine wrt1r4

  subroutine wrt2r4(data_name,filename,dims,data_set)
    character(len=*),intent(in) :: data_name
    character(len=*),intent(in) :: filename
    integer(HSIZE_T),intent(in) :: dims(2)
    real(kind=4),intent(in) :: data_set(dims(1),dims(2))

    call H5open_f(ierr)
    call H5Fopen_f(filename,H5F_ACC_RDWR_F,file_id,ierr)

    call H5Screate_simple_f(2,dims,space_id,ierr)
    call H5Dcreate_f(file_id,data_name,H5T_IEEE_F32BE,space_id,data_id,ierr)
    call H5Dwrite_f(data_id,H5T_NATIVE_REAL,data_set,dims,ierr)
    call H5Dclose_f(data_id,ierr)
    call H5Sclose_f(space_id,ierr)
    
    print*, ierr,' - ', ' write ', data_name

    call H5Fclose_f(file_id,ierr)
  end subroutine wrt2r4

  subroutine wrt3r4(data_name,filename,dims,data_set)
    character(len=*),intent(in) :: data_name
    character(len=*),intent(in) :: filename
    integer(HSIZE_T),intent(in) :: dims(3)
    real(kind=4),intent(in) :: data_set(dims(1),dims(2),dims(3))

    call H5open_f(ierr)
    call H5Fopen_f(filename,H5F_ACC_RDWR_F,file_id,ierr)

    call H5Screate_simple_f(3,dims,space_id,ierr)
    call H5Dcreate_f(file_id,data_name,H5T_IEEE_F32BE,space_id,data_id,ierr)
    call H5Dwrite_f(data_id,H5T_NATIVE_REAL,data_set,dims,ierr)
    call H5Dclose_f(data_id,ierr)
    call H5Sclose_f(space_id,ierr)
    
    print*, ierr,' - ', ' write ', data_name

    call H5Fclose_f(file_id,ierr)
  end subroutine wrt3r4

  subroutine wrt4r4(data_name,filename,dims,data_set)
    character(len=*),intent(in) :: data_name
    character(len=*),intent(in) :: filename
    integer(HSIZE_T),intent(in) :: dims(4)
    real(kind=4),intent(in) :: data_set(dims(1),dims(2),dims(3),dims(4))

    call H5open_f(ierr)
    call H5Fopen_f(filename,H5F_ACC_RDWR_F,file_id,ierr)

    call H5Screate_simple_f(4,dims,space_id,ierr)
    call H5Dcreate_f(file_id,data_name,H5T_IEEE_F32BE,space_id,data_id,ierr)
    call H5Dwrite_f(data_id,H5T_NATIVE_REAL,data_set,dims,ierr)
    call H5Dclose_f(data_id,ierr)
    call H5Sclose_f(space_id,ierr)
    
    print*, ierr,' - ', ' write ', data_name

    call H5Fclose_f(file_id,ierr)
  end subroutine wrt4r4

  subroutine wrt5r4(data_name,filename,dims,data_set)
    character(len=*),intent(in) :: data_name
    character(len=*),intent(in) :: filename
    integer(HSIZE_T),intent(in) :: dims(5)
    real(kind=4),intent(in) :: data_set(dims(1),dims(2),dims(3),dims(4),dims(5))

    call H5open_f(ierr)
    call H5Fopen_f(filename,H5F_ACC_RDWR_F,file_id,ierr)

    call H5Screate_simple_f(5,dims,space_id,ierr)
    call H5Dcreate_f(file_id,data_name,H5T_IEEE_F32BE,space_id,data_id,ierr)
    call H5Dwrite_f(data_id,H5T_NATIVE_REAL,data_set,dims,ierr)
    call H5Dclose_f(data_id,ierr)
    call H5Sclose_f(space_id,ierr)
    
    print*, ierr,' - ', ' write ', data_name

    call H5Fclose_f(file_id,ierr)
  end subroutine wrt5r4

  subroutine wrt6r4(data_name,filename,dims,data_set)
    character(len=*),intent(in) :: data_name
    character(len=*),intent(in) :: filename
    integer(HSIZE_T),intent(in) :: dims(6)
    real(kind=4),intent(in) :: data_set(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6))

    call H5open_f(ierr)
    call H5Fopen_f(filename,H5F_ACC_RDWR_F,file_id,ierr)

    call H5Screate_simple_f(6,dims,space_id,ierr)
    call H5Dcreate_f(file_id,data_name,H5T_IEEE_F32BE,space_id,data_id,ierr)
    call H5Dwrite_f(data_id,H5T_NATIVE_REAL,data_set,dims,ierr)
    call H5Dclose_f(data_id,ierr)
    call H5Sclose_f(space_id,ierr)
    
    print*, ierr,' - ', ' write ', data_name

    call H5Fclose_f(file_id,ierr)
  end subroutine wrt6r4
!******************************************************************************
  subroutine wrt1r8(data_name,filename,dims,data_set)
    character(len=*),intent(in) :: data_name
    character(len=*),intent(in) :: filename
    integer(HSIZE_T),intent(in) :: dims(1)
    real(kind=8),intent(in) :: data_set(dims(1))

    call H5open_f(ierr)
    call H5Fopen_f(filename,H5F_ACC_RDWR_F,file_id,ierr)

    call H5Screate_simple_f(1,dims,space_id,ierr)
    call H5Dcreate_f(file_id,data_name,H5T_IEEE_F64BE,space_id,data_id,ierr)
    call H5Dwrite_f(data_id,H5T_NATIVE_DOUBLE,data_set,dims,ierr)
    call H5Dclose_f(data_id,ierr)
    call H5Sclose_f(space_id,ierr)
    
    print*, ierr,' - ', ' write ', data_name

    call H5Fclose_f(file_id,ierr)
  end subroutine wrt1r8

  subroutine wrt2r8(data_name,filename,dims,data_set)
    character(len=*),intent(in) :: data_name
    character(len=*),intent(in) :: filename
    integer(HSIZE_T),intent(in) :: dims(2)
    real(kind=8),intent(in) :: data_set(dims(1),dims(2))

    call H5open_f(ierr)
    call H5Fopen_f(filename,H5F_ACC_RDWR_F,file_id,ierr)

    call H5Screate_simple_f(2,dims,space_id,ierr)
    call H5Dcreate_f(file_id,data_name,H5T_IEEE_F64BE,space_id,data_id,ierr)
    call H5Dwrite_f(data_id,H5T_NATIVE_DOUBLE,data_set,dims,ierr)
    call H5Dclose_f(data_id,ierr)
    call H5Sclose_f(space_id,ierr)
    
    print*, ierr,' - ', ' write ', data_name

    call H5Fclose_f(file_id,ierr)
  end subroutine wrt2r8

  subroutine wrt3r8(data_name,filename,dims,data_set)
    character(len=*),intent(in) :: data_name
    character(len=*),intent(in) :: filename
    integer(HSIZE_T),intent(in) :: dims(3)
    real(kind=8),intent(in) :: data_set(dims(1),dims(2),dims(3))

    call H5open_f(ierr)
    call H5Fopen_f(filename,H5F_ACC_RDWR_F,file_id,ierr)

    call H5Screate_simple_f(3,dims,space_id,ierr)
    call H5Dcreate_f(file_id,data_name,H5T_IEEE_F64BE,space_id,data_id,ierr)
    call H5Dwrite_f(data_id,H5T_NATIVE_DOUBLE,data_set,dims,ierr)
    call H5Dclose_f(data_id,ierr)
    call H5Sclose_f(space_id,ierr)
    
    print*, ierr,' - ', ' write ', data_name

    call H5Fclose_f(file_id,ierr)
  end subroutine wrt3r8

  subroutine wrt4r8(data_name,filename,dims,data_set)
    character(len=*),intent(in) :: data_name
    character(len=*),intent(in) :: filename
    integer(HSIZE_T),intent(in) :: dims(4)
    real(kind=8),intent(in) :: data_set(dims(1),dims(2),dims(3),dims(4))

    call H5open_f(ierr)
    call H5Fopen_f(filename,H5F_ACC_RDWR_F,file_id,ierr)

    call H5Screate_simple_f(4,dims,space_id,ierr)
    call H5Dcreate_f(file_id,data_name,H5T_IEEE_F64BE,space_id,data_id,ierr)
    call H5Dwrite_f(data_id,H5T_NATIVE_DOUBLE,data_set,dims,ierr)
    call H5Dclose_f(data_id,ierr)
    call H5Sclose_f(space_id,ierr)
    
    print*, ierr,' - ', ' write ', data_name

    call H5Fclose_f(file_id,ierr)
  end subroutine wrt4r8

  subroutine wrt5r8(data_name,filename,dims,data_set)
    character(len=*),intent(in) :: data_name
    character(len=*),intent(in) :: filename
    integer(HSIZE_T),intent(in) :: dims(5)
    real(kind=8),intent(in) :: data_set(dims(1),dims(2),dims(3),dims(4),dims(5))

    call H5open_f(ierr)
    call H5Fopen_f(filename,H5F_ACC_RDWR_F,file_id,ierr)

    call H5Screate_simple_f(5,dims,space_id,ierr)
    call H5Dcreate_f(file_id,data_name,H5T_IEEE_F64BE,space_id,data_id,ierr)
    call H5Dwrite_f(data_id,H5T_NATIVE_DOUBLE,data_set,dims,ierr)
    call H5Dclose_f(data_id,ierr)
    call H5Sclose_f(space_id,ierr)
    
    print*, ierr,' - ', ' write ', data_name

    call H5Fclose_f(file_id,ierr)
  end subroutine wrt5r8

  subroutine wrt6r8(data_name,filename,dims,data_set)
    character(len=*),intent(in) :: data_name
    character(len=*),intent(in) :: filename
    integer(HSIZE_T),intent(in) :: dims(6)
    real(kind=8),intent(in) :: data_set(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6))

    call H5open_f(ierr)
    call H5Fopen_f(filename,H5F_ACC_RDWR_F,file_id,ierr)

    call H5Screate_simple_f(6,dims,space_id,ierr)
    call H5Dcreate_f(file_id,data_name,H5T_IEEE_F64BE,space_id,data_id,ierr)
    call H5Dwrite_f(data_id,H5T_NATIVE_DOUBLE,data_set,dims,ierr)
    call H5Dclose_f(data_id,ierr)
    call H5Sclose_f(space_id,ierr)
    
    print*, ierr,' - ', ' write ', data_name

    call H5Fclose_f(file_id,ierr)
  end subroutine wrt6r8
!**********************************************************************
  subroutine wrt1i4(data_name,filename,dims,data_set)
    character(len=*),intent(in) :: data_name
    character(len=*),intent(in) :: filename
    integer(HSIZE_T),intent(in) :: dims(1)
    integer(kind=4),intent(in) :: data_set(dims(1))

    call H5open_f(ierr)
    call H5Fopen_f(filename,H5F_ACC_RDWR_F,file_id,ierr)

    call H5Screate_simple_f(1,dims,space_id,ierr)
    call H5Dcreate_f(file_id,data_name,H5T_STD_I32BE,space_id,data_id,ierr)
    call H5Dwrite_f(data_id,H5T_NATIVE_INTEGER,data_set,dims,ierr)
    call H5Dclose_f(data_id,ierr)
    call H5Sclose_f(space_id,ierr)
    
    print*, ierr,' - ', ' write ', data_name

    call H5Fclose_f(file_id,ierr)
  end subroutine wrt1i4

  subroutine wrt2i4(data_name,filename,dims,data_set)
    character(len=*),intent(in) :: data_name
    character(len=*),intent(in) :: filename
    integer(HSIZE_T),intent(in) :: dims(2)
    integer(kind=4),intent(in) :: data_set(dims(1),dims(2))

    call H5open_f(ierr)
    call H5Fopen_f(filename,H5F_ACC_RDWR_F,file_id,ierr)

    call H5Screate_simple_f(2,dims,space_id,ierr)
    call H5Dcreate_f(file_id,data_name,H5T_STD_I32BE,space_id,data_id,ierr)
    call H5Dwrite_f(data_id,H5T_NATIVE_INTEGER,data_set,dims,ierr)
    call H5Dclose_f(data_id,ierr)
    call H5Sclose_f(space_id,ierr)
    
    print*, ierr,' - ', ' write ', data_name

    call H5Fclose_f(file_id,ierr)
  end subroutine wrt2i4

  subroutine wrt3i4(data_name,filename,dims,data_set)
    character(len=*),intent(in) :: data_name
    character(len=*),intent(in) :: filename
    integer(HSIZE_T),intent(in) :: dims(3)
    integer(kind=4),intent(in) :: data_set(dims(1),dims(2),dims(3))

    call H5open_f(ierr)
    call H5Fopen_f(filename,H5F_ACC_RDWR_F,file_id,ierr)

    call H5Screate_simple_f(3,dims,space_id,ierr)
    call H5Dcreate_f(file_id,data_name,H5T_STD_I32BE,space_id,data_id,ierr)
    call H5Dwrite_f(data_id,H5T_NATIVE_INTEGER,data_set,dims,ierr)
    call H5Dclose_f(data_id,ierr)
    call H5Sclose_f(space_id,ierr)
    
    print*, ierr,' - ', ' write ', data_name

    call H5Fclose_f(file_id,ierr)
  end subroutine wrt3i4
!**********************************************************************
  subroutine wrt1i8(data_name,filename,dims,data_set)
    character(len=*),intent(in) :: data_name
    character(len=*),intent(in) :: filename
    integer(HSIZE_T),intent(in) :: dims(1)
    integer(kind=8),intent(in) :: data_set(dims(1))

    call H5open_f(ierr)
    call H5Fopen_f(filename,H5F_ACC_RDWR_F,file_id,ierr)

    call H5Screate_simple_f(1,dims,space_id,ierr)
    call H5Dcreate_f(file_id,data_name,H5T_STD_I64BE,space_id,data_id,ierr)
    call H5Dwrite_f(data_id,H5T_NATIVE_INTEGER8,data_set,dims,ierr)
    call H5Dclose_f(data_id,ierr)
    call H5Sclose_f(space_id,ierr)
    
    print*, ierr,' - ', ' write ', data_name

    call H5Fclose_f(file_id,ierr)
  end subroutine wrt1i8

  subroutine wrt2i8(data_name,filename,dims,data_set)
    character(len=*),intent(in) :: data_name
    character(len=*),intent(in) :: filename
    integer(HSIZE_T),intent(in) :: dims(2)
    integer(kind=8),intent(in) :: data_set(dims(1),dims(2))

    call H5open_f(ierr)
    call H5Fopen_f(filename,H5F_ACC_RDWR_F,file_id,ierr)

    call H5Screate_simple_f(2,dims,space_id,ierr)
    call H5Dcreate_f(file_id,data_name,H5T_STD_I64BE,space_id,data_id,ierr)
    call H5Dwrite_f(data_id,H5T_NATIVE_INTEGER8,data_set,dims,ierr)
    call H5Dclose_f(data_id,ierr)
    call H5Sclose_f(space_id,ierr)
    
    print*, ierr,' - ', ' write ', data_name

    call H5Fclose_f(file_id,ierr)
  end subroutine wrt2i8

  subroutine wrt3i8(data_name,filename,dims,data_set)
    character(len=*),intent(in) :: data_name
    character(len=*),intent(in) :: filename
    integer(HSIZE_T),intent(in) :: dims(3)
    integer(kind=8),intent(in) :: data_set(dims(1),dims(2),dims(3))

    call H5open_f(ierr)
    call H5Fopen_f(filename,H5F_ACC_RDWR_F,file_id,ierr)

    call H5Screate_simple_f(3,dims,space_id,ierr)
    call H5Dcreate_f(file_id,data_name,H5T_STD_I64BE,space_id,data_id,ierr)
    call H5Dwrite_f(data_id,H5T_NATIVE_INTEGER8,data_set,dims,ierr)
    call H5Dclose_f(data_id,ierr)
    call H5Sclose_f(space_id,ierr)
    
    print*, ierr,' - ', ' write ', data_name

    call H5Fclose_f(file_id,ierr)
  end subroutine wrt3i8
!***********************************************************************
!***********************************************************************
  subroutine read1r4(data_name,filename,dims,data_set)
    character(len=*),intent(in) :: data_name
    character(len=*),intent(in) :: filename
    integer(HSIZE_T),intent(in) :: dims(1)
    real(kind=4),intent(out) :: data_set(dims(1))

    call H5open_f(ierr)
    call H5Fopen_f(filename,H5F_ACC_RDWR_F,file_id,ierr)

    call H5Dopen_f(file_id,data_name,data_id,ierr)
    call H5Dread_f(data_id,H5T_NATIVE_REAL,data_set,dims,ierr)
    call H5Dclose_f(data_id,ierr)

    print*, ierr,' - ', ' read ', data_name

    call H5Fclose_f(file_id,ierr)
  end subroutine read1r4

  subroutine read2r4(data_name,filename,dims,data_set)
    character(len=*),intent(in) :: data_name
    character(len=*),intent(in) :: filename
    integer(HSIZE_T),intent(in) :: dims(2)
    real(kind=4),intent(out) :: data_set(dims(1),dims(2))

    call H5open_f(ierr)
    call H5Fopen_f(filename,H5F_ACC_RDWR_F,file_id,ierr)

    call H5Dopen_f(file_id,data_name,data_id,ierr)
    call H5Dread_f(data_id,H5T_NATIVE_REAL,data_set,dims,ierr)
    call H5Dclose_f(data_id,ierr)

    print*, ierr,' - ', ' read ', data_name

    call H5Fclose_f(file_id,ierr)
  end subroutine read2r4

  subroutine read3r4(data_name,filename,dims,data_set)
    character(len=*),intent(in) :: data_name
    character(len=*),intent(in) :: filename
    integer(HSIZE_T),intent(in) :: dims(3)
    real(kind=4),intent(out) :: data_set(dims(1),dims(2),dims(3))

    call H5open_f(ierr)
    call H5Fopen_f(filename,H5F_ACC_RDWR_F,file_id,ierr)

    call H5Dopen_f(file_id,data_name,data_id,ierr)
    call H5Dread_f(data_id,H5T_NATIVE_REAL,data_set,dims,ierr)
    call H5Dclose_f(data_id,ierr)

    print*, ierr,' - ', ' read ', data_name

    call H5Fclose_f(file_id,ierr)
  end subroutine read3r4

  subroutine read4r4(data_name,filename,dims,data_set)
    character(len=*),intent(in) :: data_name
    character(len=*),intent(in) :: filename
    integer(HSIZE_T),intent(in) :: dims(4)
    real(kind=4),intent(out) :: data_set(dims(1),dims(2),dims(3),dims(4))

    call H5open_f(ierr)
    call H5Fopen_f(filename,H5F_ACC_RDWR_F,file_id,ierr)

    call H5Dopen_f(file_id,data_name,data_id,ierr)
    call H5Dread_f(data_id,H5T_NATIVE_REAL,data_set,dims,ierr)
    call H5Dclose_f(data_id,ierr)

    print*, ierr,' - ', ' read ', data_name

    call H5Fclose_f(file_id,ierr)
  end subroutine read4r4

  subroutine read5r4(data_name,filename,dims,data_set)
    character(len=*),intent(in) :: data_name
    character(len=*),intent(in) :: filename
    integer(HSIZE_T),intent(in) :: dims(5)
    real(kind=4),intent(out) :: data_set(dims(1),dims(2),dims(3),dims(4),dims(5))

    call H5open_f(ierr)
    call H5Fopen_f(filename,H5F_ACC_RDWR_F,file_id,ierr)

    call H5Dopen_f(file_id,data_name,data_id,ierr)
    call H5Dread_f(data_id,H5T_NATIVE_REAL,data_set,dims,ierr)
    call H5Dclose_f(data_id,ierr)

    print*, ierr,' - ', ' read ', data_name

    call H5Fclose_f(file_id,ierr)
  end subroutine read5r4

  subroutine read6r4(data_name,filename,dims,data_set)
    character(len=*),intent(in) :: data_name
    character(len=*),intent(in) :: filename
    integer(HSIZE_T),intent(in) :: dims(6)
    real(kind=4),intent(out) :: data_set(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6))

    call H5open_f(ierr)
    call H5Fopen_f(filename,H5F_ACC_RDWR_F,file_id,ierr)

    call H5Dopen_f(file_id,data_name,data_id,ierr)
    call H5Dread_f(data_id,H5T_NATIVE_REAL,data_set,dims,ierr)
    call H5Dclose_f(data_id,ierr)

    print*, ierr,' - ', ' read ', data_name

    call H5Fclose_f(file_id,ierr)
  end subroutine read6r4
!***********************************************************************
  subroutine read1r8(data_name,filename,dims,data_set)
    character(len=*),intent(in) :: data_name
    character(len=*),intent(in) :: filename
    integer(HSIZE_T),intent(in) :: dims(1)
    real(kind=8),intent(out) :: data_set(dims(1))

    call H5open_f(ierr)
    call H5Fopen_f(filename,H5F_ACC_RDWR_F,file_id,ierr)

    call H5Dopen_f(file_id,data_name,data_id,ierr)
    call H5Dread_f(data_id,H5T_NATIVE_DOUBLE,data_set,dims,ierr)
    call H5Dclose_f(data_id,ierr)

    print*, ierr,' - ', ' read ', data_name

    call H5Fclose_f(file_id,ierr)
  end subroutine read1r8

  subroutine read2r8(data_name,filename,dims,data_set)
    character(len=*),intent(in) :: data_name
    character(len=*),intent(in) :: filename
    integer(HSIZE_T),intent(in) :: dims(2)
    real(kind=8),intent(out) :: data_set(dims(1),dims(2))

    call H5open_f(ierr)
    call H5Fopen_f(filename,H5F_ACC_RDWR_F,file_id,ierr)

    call H5Dopen_f(file_id,data_name,data_id,ierr)
    call H5Dread_f(data_id,H5T_NATIVE_DOUBLE,data_set,dims,ierr)
    call H5Dclose_f(data_id,ierr)

    print*, ierr,' - ', ' read ', data_name

    call H5Fclose_f(file_id,ierr)
  end subroutine read2r8

  subroutine read3r8(data_name,filename,dims,data_set)
    character(len=*),intent(in) :: data_name
    character(len=*),intent(in) :: filename
    integer(HSIZE_T),intent(in) :: dims(3)
    real(kind=8),intent(out) :: data_set(dims(1),dims(2),dims(3))

    call H5open_f(ierr)
    call H5Fopen_f(filename,H5F_ACC_RDWR_F,file_id,ierr)

    call H5Dopen_f(file_id,data_name,data_id,ierr)
    call H5Dread_f(data_id,H5T_NATIVE_DOUBLE,data_set,dims,ierr)
    call H5Dclose_f(data_id,ierr)

    print*, ierr,' - ', ' read ', data_name

    call H5Fclose_f(file_id,ierr)
  end subroutine read3r8

  subroutine read4r8(data_name,filename,dims,data_set)
    character(len=*),intent(in) :: data_name
    character(len=*),intent(in) :: filename
    integer(HSIZE_T),intent(in) :: dims(4)
    real(kind=8),intent(out) :: data_set(dims(1),dims(2),dims(3),dims(4))

    call H5open_f(ierr)
    call H5Fopen_f(filename,H5F_ACC_RDWR_F,file_id,ierr)

    call H5Dopen_f(file_id,data_name,data_id,ierr)
    call H5Dread_f(data_id,H5T_NATIVE_DOUBLE,data_set,dims,ierr)
    call H5Dclose_f(data_id,ierr)

    print*, ierr,' - ', ' read ', data_name

    call H5Fclose_f(file_id,ierr)
  end subroutine read4r8

  subroutine read5r8(data_name,filename,dims,data_set)
    character(len=*),intent(in) :: data_name
    character(len=*),intent(in) :: filename
    integer(HSIZE_T),intent(in) :: dims(5)
    real(kind=8),intent(out) :: data_set(dims(1),dims(2),dims(3),dims(4),dims(5))

    call H5open_f(ierr)
    call H5Fopen_f(filename,H5F_ACC_RDWR_F,file_id,ierr)

    call H5Dopen_f(file_id,data_name,data_id,ierr)
    call H5Dread_f(data_id,H5T_NATIVE_DOUBLE,data_set,dims,ierr)
    call H5Dclose_f(data_id,ierr)

    print*, ierr,' - ', ' read ', data_name

    call H5Fclose_f(file_id,ierr)
  end subroutine read5r8

  subroutine read6r8(data_name,filename,dims,data_set)
    character(len=*),intent(in) :: data_name
    character(len=*),intent(in) :: filename
    integer(HSIZE_T),intent(in) :: dims(6)
    real(kind=8),intent(out) :: data_set(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6))

    call H5open_f(ierr)
    call H5Fopen_f(filename,H5F_ACC_RDWR_F,file_id,ierr)

    call H5Dopen_f(file_id,data_name,data_id,ierr)
    call H5Dread_f(data_id,H5T_NATIVE_DOUBLE,data_set,dims,ierr)
    call H5Dclose_f(data_id,ierr)

    print*, ierr,' - ', ' read ', data_name

    call H5Fclose_f(file_id,ierr)
  end subroutine read6r8
!***********************************************************************
  subroutine read1i4(data_name,filename,dims,data_set)
    character(len=*),intent(in) :: data_name
    character(len=*),intent(in) :: filename
    integer(HSIZE_T),intent(in) :: dims(1)
    integer(kind=4),intent(out) :: data_set(dims(1))

    call H5open_f(ierr)
    call H5Fopen_f(filename,H5F_ACC_RDWR_F,file_id,ierr)

    call H5Dopen_f(file_id,data_name,data_id,ierr)
    call H5Dread_f(data_id,H5T_NATIVE_INTEGER,data_set,dims,ierr)
    call H5Dclose_f(data_id,ierr)

    print*, ierr,' - ', ' read ', data_name

    call H5Fclose_f(file_id,ierr)
  end subroutine read1i4

  subroutine read2i4(data_name,filename,dims,data_set)
    character(len=*),intent(in) :: data_name
    character(len=*),intent(in) :: filename
    integer(HSIZE_T),intent(in) :: dims(2)
    integer(kind=4),intent(out) :: data_set(dims(1),dims(2))

    call H5open_f(ierr)
    call H5Fopen_f(filename,H5F_ACC_RDWR_F,file_id,ierr)

    call H5Dopen_f(file_id,data_name,data_id,ierr)
    call H5Dread_f(data_id,H5T_NATIVE_INTEGER,data_set,dims,ierr)
    call H5Dclose_f(data_id,ierr)

    print*, ierr,' - ', ' read ', data_name

    call H5Fclose_f(file_id,ierr)
  end subroutine read2i4

  subroutine read3i4(data_name,filename,dims,data_set)
    character(len=*),intent(in) :: data_name
    character(len=*),intent(in) :: filename
    integer(HSIZE_T),intent(in) :: dims(3)
    integer(kind=4),intent(out) :: data_set(dims(1),dims(2),dims(3))

    call H5open_f(ierr)
    call H5Fopen_f(filename,H5F_ACC_RDWR_F,file_id,ierr)

    call H5Dopen_f(file_id,data_name,data_id,ierr)
    call H5Dread_f(data_id,H5T_NATIVE_INTEGER,data_set,dims,ierr)
    call H5Dclose_f(data_id,ierr)

    print*, ierr,' - ', ' read ', data_name

    call H5Fclose_f(file_id,ierr)
  end subroutine read3i4
!***********************************************************************
  subroutine read1i8(data_name,filename,dims,data_set)
    character(len=*),intent(in) :: data_name
    character(len=*),intent(in) :: filename
    integer(HSIZE_T),intent(in) :: dims(1)
    integer(kind=8),intent(out) :: data_set(dims(1))

    call H5open_f(ierr)
    call H5Fopen_f(filename,H5F_ACC_RDWR_F,file_id,ierr)

    call H5Dopen_f(file_id,data_name,data_id,ierr)
    call H5Dread_f(data_id,H5T_NATIVE_INTEGER8,data_set,dims,ierr)
    call H5Dclose_f(data_id,ierr)

    print*, ierr,' - ', ' read ', data_name

    call H5Fclose_f(file_id,ierr)
  end subroutine read1i8

  subroutine read2i8(data_name,filename,dims,data_set)
    character(len=*),intent(in) :: data_name
    character(len=*),intent(in) :: filename
    integer(HSIZE_T),intent(in) :: dims(2)
    integer(kind=8),intent(out) :: data_set(dims(1),dims(2))

    call H5open_f(ierr)
    call H5Fopen_f(filename,H5F_ACC_RDWR_F,file_id,ierr)

    call H5Dopen_f(file_id,data_name,data_id,ierr)
    call H5Dread_f(data_id,H5T_NATIVE_INTEGER8,data_set,dims,ierr)
    call H5Dclose_f(data_id,ierr)

    print*, ierr,' - ', ' read ', data_name

    call H5Fclose_f(file_id,ierr)
  end subroutine read2i8

  subroutine read3i8(data_name,filename,dims,data_set)
    character(len=*),intent(in) :: data_name
    character(len=*),intent(in) :: filename
    integer(HSIZE_T),intent(in) :: dims(3)
    integer(kind=8),intent(out) :: data_set(dims(1),dims(2),dims(3))

    call H5open_f(ierr)
    call H5Fopen_f(filename,H5F_ACC_RDWR_F,file_id,ierr)

    call H5Dopen_f(file_id,data_name,data_id,ierr)
    call H5Dread_f(data_id,H5T_NATIVE_INTEGER8,data_set,dims,ierr)
    call H5Dclose_f(data_id,ierr)

    print*, ierr,' - ', ' read ', data_name

    call H5Fclose_f(file_id,ierr)
  end subroutine read3i8
!***********************************************************************

end module H5F90_WRAPPER
