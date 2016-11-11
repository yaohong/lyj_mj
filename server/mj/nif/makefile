
flag= -g -DHH_EXE -I./include -fPIC -std=c++0x

OBJDIR:=./obj

e_so=./mj_nif.so
e_flag= -g -fPIC -shared -I./ -I/usr/local/erlang/lib/erlang/usr/include/


CPP_SRC:=$(wildcard *.cpp )
C_SRC:=$(wildcard *.c)

CPP_OBJ:=$(patsubst %.cpp,$(OBJDIR)/%.o,$(notdir $(CPP_SRC)))
C_OBJ:=$(patsubst %.c,$(OBJDIR)/%.o,$(notdir $(C_SRC)))
OBJ:= ${CPP_OBJ} ${C_OBJ}
all: client $(e_so) beam

client: ${CPP_OBJ} 
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

$(OBJDIR)/%.o:%.c
	g++ -c $< -o $@ $(e_flag)
