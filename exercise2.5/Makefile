all:
	bnfc CPP.cf
	happy -gca ParCPP.y
	alex -g LexCPP.x
	stack build
	ln -s .stack-work/install/x86_64-linux-nopie/lts-9.21/8.0.2/bin/lab3 lab3

clean:
	rm -f lab3
	stack clean

distclean: clean
	rm -f DocCPP.* LexCPP.* ParCPP.* LayoutCPP.* SkelCPP.* PrintCPP.* TestCPP.* AbsCPP.* TestCPP ErrM.* SharedString.* CPP.dtd XMLCPP.*

