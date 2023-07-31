code_dir := 16bit_cpu
bin_dir := orig
prog_dir := programs

includes := -include cstdint
flags := ${includes} -std=c++20

all: clean asm sim programs

clean: bindir
	rm -f ${bin_dir}/asm ${bin_dir}/sim 

asm: bindir
	g++ -o ${bin_dir}/asm ${flags} ${code_dir}/assembly/assembler.cpp

sim: bindir
	g++ -o ${bin_dir}/sim ${flags} ${code_dir}/simulator/simulator.cpp

bindir:
	mkdir -p orig

programs: progdir
	./programs.sh ./${bin_dir}/asm ${code_dir}/assembly ${prog_dir}

progdir:
	mkdir -p programs
