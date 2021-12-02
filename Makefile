SUBDIRS:=$(wildcard day??)

all:
	for d in $(SUBDIRS); do $(MAKE) -C $$d all; done

clean:
	for d in $(SUBDIRS); do $(MAKE) -C $$d clean; done

run-all:
	for d in $(SUBDIRS); do $(MAKE) -C $$d run-all; done
