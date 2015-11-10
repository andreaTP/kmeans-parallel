
default: all

clean:
	rm *.beam
	rm *.dump

all:
	erl -make

run:
	erl +P 134217727 -noshell -eval 'main:run().'
