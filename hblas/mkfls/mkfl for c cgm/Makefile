&&build:
	gcc cgm_10.c -lcblas && ./a.out 1>mat_10.txt && gcc cgm_100.c -lcblas && ./a.out 1>mat_100.txt && gcc cgm_1000.c -lcblas && ./a.out 1>mat_1000.txt && gcc cgm_10000.c -lcblas && ./a.out 1>mat_10000.txt 

search: 
	cat foo.txt | grep stream
clean: 
	rm *.out
