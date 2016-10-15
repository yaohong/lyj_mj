
flag= -g -DEXE -fPIC -std=c++0x

OBJDIR:=./obj

e_so=./mj_nif.so
e_flag= -g -fPIC -shared -I./ -I/usr/local/erlang/lib/erlang/usr/include/


SRC:=$(wildcard *.cpp )
CPP_OBJ:=$(patsubst %.cpp,$(OBJDIR)/%.o,$(notdir $(SRC)))
C_OBJ:=$(patsubst %.c,$(OBJDIR)/%.o,$(notdir $(SRC)))
OBJ:= ${CPP_OBJ} ${C_OBJ}
all:  client $(e_so) beam

client: ${OBJ} 
	g++ $(flag) -o $@ $^

beam: mj_nif.erl 
	erlc mj_nif.erl

$(e_so): ${OBJ} 
	g++ -fPIC -shared $(e_flag) -o $@ $^

clean:
	rm -rf $(OBJDIR)/*.o
	rm -rf client
	rm -rf $(e_so)
	rm -rf mj_nif.beam
  
$(OBJDIR)/%.o:%.cpp
	g++ -c $< -o $@ $(flag)
