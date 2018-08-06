ifeq ($(OS), Windows_NT)
DETECTED_OS=Windows
else
DETECTED_OS=POSIX
endif

CS_HOME := mTask/library/CleanSerial/

all: build

build: build_clean_serial build_autohouse

build_clean_serial:
	make -C $(CS_HOME)

build_autohouse:
	cpm project AutoHouse create
	cpm project AutoHouse.prj target iTasks
	cpm project AutoHouse.prj path add "$$PWD/mTask/library"
	cpm project AutoHouse.prj path add "$$PWD/mTask/library/CleanSerial"
	cpm project AutoHouse.prj path add "$$PWD/mTask/library/CleanSerial/POSIX"
	cpm project AutoHouse.prj set -dynamics -h 2000M -s 20M
	cpm AutoHouse.prj

clean:
	make -C $(CS_HOME) clean
	find . -name "Clean System Files" -exec rm -r {} \;
	find . -name "*sapl" -exec rm -r {} \;
	find . -name "*www" -exec rm -r {} \;
	rm AutoHouse
