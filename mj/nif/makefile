
flag= -g -DEXE -std=c++0x

OBJDIR:=./obj
mkdir $(OBJ)

e_so=./nif.so
e_flag= -g -Wno-pointer-arith -I./ -I./header -I/usr/local/erlang/lib/erlang/usr/include/


SRC:=$(wildcard *.cpp )
OBJ:=$(patsubst %.cpp,$(OBJDIR)/%.o,$(notdir $(SRC)))
all:  client $(e_so) 

client: ${OBJ} 
	g++ $(flag) -o $@ $^

$(e_so): ${OBJ} 
	g++ -fPIC -shared $(e_flag) -o $@ $^

clean:
	rm client
	rm $(e_so)
	rm $(OBJDIR)/*.o
  
$(OBJDIR)/%.o:%.cpp
	g++ -c $< -o $@ $(flag)
