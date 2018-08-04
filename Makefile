UNAME_S := $(shell uname -s)
ifeq ($(UNAME_S),Linux)
	CLEAN_BUILD = "https://ftp.cs.ru.nl/Clean/builds/linux-x64/clean-bundle-complete-linux-x64-latest.tgz"
endif
ifeq ($(UNAME_S),Darwin)
	CLEAN_BUILD = "https://ftp.cs.ru.nl/Clean/builds/macos-x64/clean-bundle-complete-macos-x64-latest.tgz"
endif
ifeq ($(OS), Windows_NT)
DETECTED_OS=Windows
else
DETECTED_OS=POSIX
endif

CLEAN_HOME := clean-bundle-complete
CS_HOME := mTask/library/CleanSerial/
CPM := $(CLEAN_HOME)/bin/cpm
CLM := $(CLEAN_HOME)//bin/clm

all: fetch build

fetch:
	curl -o clean.tgz $(CLEAN_BUILD)
	tar -xvzf clean.tgz
	rm clean.tgz

build: build_clean_serial build_autohouse

build_clean_serial:
	mkdir -p $(CS_HOME)/Clean\ System\ Files
	gcc -c $(CS_HOME)/$(DETECTED_OS)/tty.c -o $(CS_HOME)/Clean\ System\ Files/ctty.o

build_autohouse:
	$(CPM) project AutoHouse create
	$(CPM) project AutoHouse.prj target iTasks
	$(CPM) project AutoHouse.prj path add "$$PWD/mTask/library"
	$(CPM) project AutoHouse.prj path add "$$PWD/mTask/library/CleanSerial"
	$(CPM) project AutoHouse.prj path add "$$PWD/mTask/library/CleanSerial/POSIX"
	$(CPM) project AutoHouse.prj set -dynamics -h 2000M -s 20M
	$(CPM) AutoHouse.prj

clean:
	rm -r $(CLEAN_HOME)
