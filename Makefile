FC = gfortran
FFLAGS = -O2 -Wall -Wextra -std=f2008
SRC = src/mikrosim_mnl.f90
BIN = bin/mikrosim

.PHONY: all build run clean

all: build

build:
	mkdir -p bin
	$(FC) $(FFLAGS) $(SRC) -o $(BIN)

run: build
	$(BIN)

clean:
	rm -rf bin
