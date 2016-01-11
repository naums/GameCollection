CC=g++
CPPFLAGS=-Wall -pedantic -g
LIBS=-lmysqlpp
FILES=main.o interface.o game.o base/date.o base/date_parser.o mysql/mysql.o base/debug.o base/file.o base/dictionary.o mysql/store.o
OUT=gamecollection

all: $(FILES)
	$(CC) $(FILES) -o $(OUT) $(LIBS) $(CPPFLAGS)
%.o: %.cpp
	$(CC) $(CPPFLAGS) -c -o $@ $<
	
doc: 
	doxygen doc_template
	
clean:
	rm $(FILES) $(OUT)
