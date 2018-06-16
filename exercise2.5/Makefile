all:
	bnfc CPP.cf
	happy -gca ParCPP.y
	alex -g LexCPP.x
	stack build
	ln -f -s `stack exec which lab3` lab3

clean:
	rm -f lab3
	stack clean

distclean: clean
	rm -f DocCPP.* LexCPP.* ParCPP.* LayoutCPP.* SkelCPP.* PrintCPP.* TestCPP.* AbsCPP.* TestCPP ErrM.* SharedString.* CPP.dtd XMLCPP.*

test: all
	./tests/driver.sh
