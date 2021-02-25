export CASK ?= cask
export EMACS ?= $(shell which emacs)
export CASK_DIR := $(shell EMACS=$(EMACS) $(CASK) package-directory)

PKBUILD=2.3
TESTSSRC = $(shell ls tests/*.el)
ELCTESTS = $(TESTSSRC:.el=.elc)
.DEFAULT_GOAL := compile

.PHONY: test
test: cask compile
	$(CASK) emacs -Q --batch -L . -L tests --eval "(setq package-user-dir \"$(CASK_DIR)\")" --eval "(package-initialize)" -l test-patch-melpa-requires.el -f ert-run-tests-batch-and-exit

README.rst: README.in.rst patch-melpa-requires.el
	grep ';;' patch-melpa-requires.el \
	    | awk '/;;;\s*Commentary/{within=1;next}/;;;\s*/{within=0}within' \
	    | sed -e 's/^\s*;;*\s*//g' \
	    | tools/readme-sed.sh "COMMENTARY" README.in.rst > README.rst

.PHONY: cask
cask: $(CASK_DIR)

$(CASK_DIR): Cask
	$(CASK) install
	touch $(CASK_DIR)

.PHONY: compile
compile: cask
	! ($(CASK) eval \
	      "(cl-letf (((symbol-function (quote cask-files)) (lambda (&rest _args) (mapcar (function symbol-name) (quote ($(TESTSSRC))))))) \
	          (let ((byte-compile-error-on-warn t)) (cask-cli/build)))" 2>&1 | egrep -a "(Warning|Error):") ; (ret=$$? ; rm -f $(ELCTESTS) && exit $$ret)
	! ($(CASK) eval "(let ((byte-compile-error-on-warn t)) (cask-cli/build))" 2>&1 | egrep -a "(Warning|Error):") ; (ret=$$? ; $(CASK) clean-elc && exit $$ret)

.PHONY: lint
lint: compile
	bash -ex tools/melpazoid.sh

.PHONY: clean
clean:
	$(CASK) clean-elc
	rm -rf tests/test-install
	rm -rf melpazoid-master/patch-melpa-requires

.PHONY: dist-clean
dist-clean:
	rm -rf dist

.PHONY: dist
dist: dist-clean
	$(CASK) package

.PHONY: install
install: compile dist
	$(EMACS) -Q --batch --eval "(package-initialize)" \
	  --eval "(package-install-file \"dist/patch-melpa-requires-$(shell $(CASK) version).el\")"

define SET_GITHUB_ACTOR =
GITHUB_ACTOR := $(shell if [ -z ${GITHUB_ACTOR} ]; then git config user.name; else echo ${GITHUB_ACTOR} ; fi)
endef

define SET_GITHUB_ACTOR_REPOSITORY =
GITHUB_ACTOR_REPOSITORY := $(GITHUB_ACTOR)/$(shell basename `git rev-parse --show-toplevel`)
endef

define SET_GITHUB_HEAD_REF =
GITHUB_HEAD_REF := $(shell if [ -z ${GITHUB_HEAD_REF} ]; then git rev-parse --abbrev-ref HEAD; else echo ${GITHUB_HEAD_REF} ; fi)
endef

define SET_GITHUB_SHA =
GITHUB_SHA := $(shell if [ -z ${GITHUB_SHA} ] ; then git rev-parse origin/${GITHUB_HEAD_REF}; else echo ${GITHUB_SHA}; fi)
endef

define SET_GITHUB_COMMIT =
GITHUB_COMMIT := $(shell if git show -s --format=%s "${GITHUB_SHA}" | egrep -q "^Merge .* into" ; then git show -s --format=%s "${GITHUB_SHA}" | cut -d " " -f2 ; else echo "${GITHUB_SHA}" ; fi)
endef

.PHONY: test-install-vars
test-install-vars:
	$(eval $(call SET_GITHUB_ACTOR))
	$(eval $(call SET_GITHUB_ACTOR_REPOSITORY))
	$(eval $(call SET_GITHUB_HEAD_REF))
	$(eval $(call SET_GITHUB_SHA))
	$(eval $(call SET_GITHUB_COMMIT))
	git show -s --format=%s $(GITHUB_COMMIT)
	git show -s --format=%s $(GITHUB_SHA)

.PHONY: test-install
test-install: test-install-vars
	mkdir -p tests/test-install
	if [ ! -s "tests/test-install/$(PKBUILD).tar.gz" ] ; then \
	  cd tests/test-install ; curl -sLOk https://github.com/melpa/package-build/archive/$(PKBUILD).tar.gz ; fi
	cd tests/test-install ; tar xfz $(PKBUILD).tar.gz
	cd tests/test-install ; rm -f $(PKBUILD).tar.gz
	cd tests/test-install/package-build-$(PKBUILD) ; make -s loaddefs
	mkdir -p tests/test-install/recipes
	cd tests/test-install/recipes ; curl -sfLOk https://raw.githubusercontent.com/melpa/melpa/master/recipes/patch-melpa-requires || cp -f ../../../tools/recipe ./patch-melpa-requires
	! ( $(EMACS) -Q --batch -L tests/test-install/package-build-$(PKBUILD) \
	--eval "(require 'package-build)" \
	--eval "(require 'subr-x)" \
	--eval "(package-initialize)" \
	--eval "(add-to-list 'package-archives '(\"melpa\" . \"http://melpa.org/packages/\"))" \
	--eval "(package-refresh-contents)" \
	--eval "(setq rcp (package-recipe-lookup \"patch-melpa-requires\"))" \
	--eval "(unless (file-exists-p package-build-archive-dir) \
	           (make-directory package-build-archive-dir))" \
	--eval "(let* ((my-repo \"$(GITHUB_ACTOR_REPOSITORY)\") \
	               (my-branch \"$(GITHUB_HEAD_REF)\") \
	               (my-commit \"$(GITHUB_COMMIT)\")) \
	           (oset rcp :repo my-repo) \
	           (oset rcp :branch my-branch) \
	           (oset rcp :commit my-commit))" \
	--eval "(package-build--package rcp (package-build--checkout rcp))" \
	--eval "(package-install-file (car (file-expand-wildcards (concat package-build-archive-dir \"patch-melpa-requires*.tar\"))))" 2>&1 | egrep -ia "error: |fatal" )
