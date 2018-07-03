all:
	bnfc CPP.cf
	happy -gca ParCPP.y
	alex -g LexCPP.x
	rm ParCPP.y LexCPP.x
	stack build --ghc-options -Wall
	./link.sh

clean:
	rm -f lab4
	stack clean

distclean: clean
	rm -f DocCPP.* LexCPP.* ParCPP.* LayoutCPP.* SkelCPP.* PrintCPP.* TestCPP.* AbsCPP.* TestCPP ErrM.* SharedString.* CPP.dtd XMLCPP.*

test: all
	./tests/driver.sh
