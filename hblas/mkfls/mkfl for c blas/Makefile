build:
	gcc cblas_10_1.c -lcblas && ./a.out 1>vec_10_1.txt && gcc cblas_10_2.c -lcblas && ./a.out 1>vec_10_2.txt && gcc cblas_10_3.c -lcblas && ./a.out 1>vec_10_3.txt && gcc cblas_10_4.c -lcblas && ./a.out 1>vec_10_4.txt && gcc cblas_10_5.c -lcblas && ./a.out 1>vec_10_5.txt && gcc cblas_10_6.c -lcblas && ./a.out 1>vec_10_6.txt && gcc cblas_10_7.c -lcblas && ./a.out 1>vec_10_7.txt && gcc cblas_10_8.c -lcblas && ./a.out 1>vec_10_8.txt

search: 
	cat foo.txt | grep stream
clean: 
	rm *.out
