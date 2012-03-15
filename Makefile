.SUFFIXES:.ml .cmx

CC=g++
RM=rm -f
OUT=go
CPP=init.cpp output.cpp main.cpp
OBJS= ${CPP:.cpp=.o}


all: go

go: ${OBJS}
	${CC} -o $@ ${OBJS}

clean:
	${RM} *~ *.o *.swp ~* go
