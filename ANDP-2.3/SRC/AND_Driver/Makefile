
INST_DIR = /usr/local/bin/

FC = mpifort -Wall

MAIN = AND_Driver

LDLIBS = -lfftw3


all: my_definition.mod string.mod sac_io.mod date_time.mod my_data_io.mod math.mod Main_Proc.mod $(MAIN)


my_definition.mod: my_definition.f90
	$(FC) -c $^

string.mod: string.f90
	$(FC) -c $^

sac_io.mod: sac_io.f90
	$(FC) -c $^

date_time.mod: date_time.f90
	$(FC) -c $^

my_data_io.mod: my_data_io.f90
	$(FC) -c $^

math.mod: math.f90
	$(FC) -c $^

Main_Proc.mod: Main_Proc.f90
	$(FC) -c $^


%.o: %.f90
	$(FC) -c $^


$(MAIN): %: %.o my_definition.o string.o sac_io.o date_time.o my_data_io.o math.o Main_Proc.o
	$(FC) $^ -o $@ $(LDLIBS)

install :: $(MAIN)
	install -s $(MAIN) $(INST_DIR)

clean:
	-rm -rf *.o *.mod

cleanall: clean
	-rm -rf $(MAIN)
