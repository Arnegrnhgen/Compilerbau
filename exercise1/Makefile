BUILDDIR=build_dir

COMP=${BUILDDIR}/TestGrammar


check: check_hello check_greet check_med check_grade check_palin check_grammar
	@echo "Success!"

check_hello: lab1-testsuite-larger/good/hello.cc ${COMP}
	${COMP} $<

check_greet: lab1-testsuite-larger/good/greet.cc ${COMP}
	${COMP} $<

check_med: lab1-testsuite-larger/good/med.cc ${COMP}
	${COMP} $<

check_grade: lab1-testsuite-larger/good/grade.cc ${COMP}
	${COMP} $<

check_palin: lab1-testsuite-larger/good/palin.cc ${COMP}
	${COMP} $<

check_grammar: lab1-testsuite-larger/good/grammar.cc ${COMP}
	${COMP} $<

${COMP}: ${BUILDDIR}/Makefile ${BUILDDIR}/*.hs
	${MAKE} -C ${BUILDDIR}

.INTERMEDIATE: interm

${BUILDDIR}/Makefile ${BUILDDIR}/*.hs: interm

interm: grammar.cf
	bnfc -m -o ${BUILDDIR} $^


clean:
	rm -Rf ${BUILDDIR}
