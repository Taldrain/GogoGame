.SUFFIXES:.cpp .o

CC=g++
RM=rm -f
OUT=go
CPP=goban.cpp main.cpp
OBJS= ${CPP:.cpp=.o}


all: go

go: ${OBJS}
	${CC} -o $@ ${OBJS}

clean:
	${RM} *~ *.o *.swp ~* go
