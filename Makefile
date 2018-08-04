UNAME_S := $(shell uname -s)
ifeq ($(UNAME_S),Linux)
	CLEAN_BUILD = "https://ftp.cs.ru.nl/Clean/builds/linux-x64/clean-bundle-complete-linux-x64-latest.tgz"
endif
ifeq ($(UNAME_S),Darwin)
	CLEAN_BUILD = "https://ftp.cs.ru.nl/Clean/builds/macos-x64/clean-bundle-complete-macos-x64-latest.tgz"
endif

CPM := clean-bundle-complete/bin/cpm

all: fetch build

fetch:
	curl -o clean.tgz $(CLEAN_BUILD)
	tar -xvzf clean.tgz
	rm clean.tgz

build:
	$(CPM) project AutoHouse create
	$(CPM) project AutoHouse.prj target iTasks
	$(CPM) project AutoHouse.prj path add "$$PWD/mTask/library"
	$(CPM) project AutoHouse.prj path add "$$PWD/mTask/library/CleanSerial"
	$(CPM) project AutoHouse.prj path add "$$PWD/mTask/library/CleanSerial/POSIX"
	$(CPM) project AutoHouse.prj set -dynamics -h 2000M -s 20M
	$(CPM) AutoHouse.prj
