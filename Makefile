FBC=fbc32 -lang fb
ABC=ab

bc:
	$(FBC) -O s -w all  -v  -s console  -entry my_main  bc.bas
bc_d:
	$(FBC) -w all  -v  -s console  -e  -entry my_main  bc.bas
bc_ab:
	$(ABC)  bc.abp
