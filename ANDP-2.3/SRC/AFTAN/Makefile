#
INST_DIR = /usr/local/bin

BIN = AFTAN_PROG

fflags = -O -Wall -ffixed-line-length-none

LDLIBS =  -lfftw3

FFLAGS = $(DBG) $(fflags)

FC = gfortran

DBG = -g

all: addition.mod

addition.mod: addition.f90
	$(FC) -c $^

FOBJS = driver.o swapn.o aftanpg.o aftanipg.o addition.o ftfilt.o fmax.o taper.o trigger.o tapers.o tgauss.o mspline.o pred_cur.o misc.o phtovel.o

all : $(BIN)

$(BIN) : $(FOBJS)
	$(FC) $(FFLAGS) $(FOBJS) -o $(BIN) $(LDLIBS)


install :: $(BIN)
	install -s $(BIN) $(INST_DIR)

clean ::
	rm -f $(FOBJS) *.mod

cleanall: clean
	rm -rf $(BIN)
