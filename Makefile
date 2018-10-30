all: program

runtime.o: runtime.c
	gcc -c runtime.c

program.ll: scheme.scm run.scm
	echo "$(file)"
	cat $(file) | guile run.scm > program.ll

program.o: program.ll
	llc -filetype=obj program.ll

program: runtime.o program.o
	gcc -no-pie runtime.o program.o -o program

clean:
	rm -f runtime.o program.o program.ll program

.PHONY: guile
guile:
	guile -l scheme.scm

.PHONY: rm_test_dir
rm_test_dir:
	rm -f tests/*.scm tests/runtime.c tests/Makefile
	rm -r tests

.PHONY: mk_test_dir
mk_test_dir: Makefile run.scm runtime.c scheme.scm
	mkdir -p tests
	cp Makefile run.scm runtime.c scheme.scm test_driver.scm tests/
	
.PHONY: test
test:
	guile test_driver.scm -no-auto-compile




	 
