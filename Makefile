EMACS = emacs

check: compile
	$(EMACS) -q -batch -eval "(check-declare-file \"slideview.el\")" 2>&1 | grep -e "Checking"
	$(EMACS) -q -batch -l slideview.el -l slideview-test.el \
		-f ert-run-tests-batch-and-exit
	$(EMACS) -q -batch -l slideview.elc -l slideview-test.el \
		-f ert-run-tests-batch-and-exit

compile:
	$(EMACS) -q -batch -f batch-byte-compile slideview.el

clean:
	rm -f *.elc
