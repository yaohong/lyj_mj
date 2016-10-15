
flag= -g -std=c++0x
OBJDIR:=./obj
SRC:=$(wildcard *.cpp )
OBJ:=$(patsubst %.cpp,$(OBJDIR)/%.o,$(notdir $(SRC)))
all:  client

client: ${OBJ} 
	g++ $(flag) -o $@ $^


clean:
	rm client
	rm $(OBJDIR)/*.o
  
$(OBJDIR)/%.o:%.cpp
	g++ -c $< -o $@ $(flag)
