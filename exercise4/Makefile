all:
	bnfc CPP.cf
	happy -gca ParCPP.y
	alex -g LexCPP.x
	stack build
	./link.sh

clean:
	rm -f lab3
	stack clean

distclean: clean
	rm -f DocCPP.* LexCPP.* ParCPP.* LayoutCPP.* SkelCPP.* PrintCPP.* TestCPP.* AbsCPP.* TestCPP ErrM.* SharedString.* CPP.dtd XMLCPP.*

test: all
	./tests/driver.sh
