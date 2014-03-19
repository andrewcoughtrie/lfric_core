export COMPILER?=gfortran
.PHONY: test
test: all
	$(MAKE) -C src/test

.PHONY: all
all:
	$(MAKE) -C src/dynamo

.PHONY: run
run: test
	$(MAKE) -C run

.PHONY: doc docs
doc docs:
	$(MAKE) -C Docs

.PHONY: clean
clean:
	$(MAKE) -C src/dynamo clean
	$(MAKE) -C src/test clean
