FC              = mpiifort -mcmodel=large -ip -ipo
#OPT             = -O3 -xICELAKE-SERVER
LINKER          = $(FC) $(OPT)

H5I             = -I/usr/hdf5/include
H5L             = -L/usr/hdf5/lib -lhdf5_fortran -lhdf5 -lz
H5I             = -I/lustre/home/g3/umeda/hdf5/include
H5L             = -L/lustre/home/g3/umeda/hdf5/lib -lhdf5_fortran -lhdf5 -lz

PROGRAM         = a.out
MODU            = h5f90_wrapper.o
MAIN            = main.o

all:            $(PROGRAM)

$(MODU):        %.o : %.F90
		$(FC) $(OPT) $(H5I) -c $<

$(MAIN):        %.o : %.f90 $(MODU)
		$(FC) $(OPT) -c $<

$(PROGRAM):     $(MAIN) $(MODU)
		$(LINKER) -o $@ $(MAIN) $(MODU) $(H5L)
