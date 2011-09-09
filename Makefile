bf: bf.c
	gcc -Os bf.c -o bf

bf.c: bfck.hs
	runghc bfck.hs > bf.c
