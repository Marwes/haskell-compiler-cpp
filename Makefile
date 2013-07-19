CC=gcc
CXX=g++
RM=rm -f

INCLUDEDIRS=-Iinclude

CFLAGS=-Wall -g
CPPFLAGS=-g $(INCLUDEDIRS) --std=c++0x $(shell root-config --cflags)
LDFLAGS=-g $(shell root-config --ldflags)
LDLIBS=$(shell root-config --libs)

SOURCES=src/main.cpp src/VM.cpp src/Instruction.cpp
OBJS=$(subst .cpp,.o, $(SOURCES))

all: vm

vm: $(OBJS)
	g++ $(LDFLAGS) -o main $(OBJS) $(LDLIBS)

depend: .depend

.depend: $(SRCS)
	rm -f ./.depend
	$(CXX) $(CPPFLAGS) -MM $^>>./.depend;

clean:
	$(RM) $(OBJS)
include .depend
