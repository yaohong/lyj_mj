
flag= -g -DEXE -fPIC -std=c++0x

OBJDIR:=./obj

e_so=./nif.so
e_flag= -g -fPIC -shared -I./ -I/usr/local/erlang/lib/erlang/usr/include/


SRC:=$(wildcard *.cpp )
OBJ:=$(patsubst %.cpp,$(OBJDIR)/%.o,$(notdir $(SRC)))
all:  client $(e_so) 

client: ${OBJ} 
	g++ $(flag) -o $@ $^

$(e_so): ${OBJ} 
	g++ -fPIC -shared $(e_flag) -o $@ $^

clean:
	rm -rf $(OBJDIR)/*.o
	rm -rf client
	rm -rf $(e_so)
  
$(OBJDIR)/%.o:%.cpp
	g++ -c $< -o $@ $(flag)
