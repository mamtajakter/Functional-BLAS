build:
	gcc -L${MKLROOT}/lib/intel64 -Wl,--no-as-needed -lmkl_intel_ilp64 -lmkl_sequential -lmkl_core -lpthread -lm -ldl cgm_10.c && srun --cpus-per-task=1 --time=60 ./a.out 1>mat_10.txt && gcc -L${MKLROOT}/lib/intel64 -Wl,--no-as-needed -lmkl_intel_ilp64 -lmkl_sequential -lmkl_core -lpthread -lm -ldl cgm_100.c && srun --cpus-per-task=1 --time=60 ./a.out 1>mat_100.txt && gcc -L${MKLROOT}/lib/intel64 -Wl,--no-as-needed -lmkl_intel_ilp64 -lmkl_sequential -lmkl_core -lpthread -lm -ldl cgm_1000.c && srun --cpus-per-task=1 --time=60 ./a.out 1>mat_1000.txt && gcc -L${MKLROOT}/lib/intel64 -Wl,--no-as-needed -lmkl_intel_ilp64 -lmkl_sequential -lmkl_core -lpthread -lm -ldl cgm_10000.c && srun --cpus-per-task=1 --time=60 ./a.out 1>mat_10000.txt 

search: 
	cat foo.txt | grep stream
clean: 
	rm *.out
