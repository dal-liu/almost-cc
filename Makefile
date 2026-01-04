test:
	./scripts/tests.sh

clean:
	cd l1 ; make clean ; 
	cd l2 ; make clean ; 
	cd l3 ; make clean ;
	cd ir ; make clean ;

.PHONY: tests clean
