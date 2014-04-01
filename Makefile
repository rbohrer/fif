compiler: compiler.f
	cpp compiler.f -o compiler2.f
	gfortran compiler2.f -o compiler -g -Wall
clean:
	rm compiler
	rm compiler2.f
