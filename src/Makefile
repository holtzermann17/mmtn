CXX = g++
CXXFLAGS = -pthread -Wall -g -pipe -I.
LDADD = -lpthread
OBJECTS = main.o listener.o logger.o
EXECUTABLE = mmtnd

.c.o :
	$(CC) -c $(CFLAGS) $<

.PHONY : all clean

all : $(EXECUTABLE)

clean :
	rm -f $(EXECUTABLE) $(OBJECTS)

$(EXECUTABLE) : $(OBJECTS)
	$(CXX) -o $(EXECUTABLE) $(CXXFLAGS) $(LDADD) $(OBJECTS)
