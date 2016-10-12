e_so=./nif_test.so
e_flag= -g -Wno-pointer-arith -I./ -I./header -I/usr/local/erlang/lib/erlang/usr/include/

all:  $(e_so)

$(e_so): nif_test.c 
	g++ -fPIC -shared $(e_flag) -o $@ nif_test.c


clean:
	rm $(e_so)
  
