MHSDIR=../..
BIN=../../bin
TOOLS=../../Tools

build: build/blinky.uf2

build/blinky.uf2: build/blinky.c build/main.c
	cmake -B build -DPICO_SDK_PATH=deps/pico-sdk/
	cmake --build build
# Generate combinator file
build/blinky.c:	src/Blinky.hs $(BIN)/mhs
	@mkdir -p build
	MHSDIR=$(MHSDIR) $(BIN)/mhs -isrc Blinky -z -obuild/blinky.c
	sed -i s#mhsffi.h#../../../src/runtime/mhsffi.h# build/blinky.c
	sed -i '/mhsffi.h/ a #include "../../../src/runtime/config-raspberry-pico.h"' build/blinky.c

$(BIN)/mhs:	$(MHSDIR)/src/runtime/*.c $(MHSDIR)/src/runtime/*.h
	make -C $(MHSDIR) bin/mhs

build/main.c:
	@mkdir -p build
	echo "#include \"blinky.c\"" > build/main.c
	echo "#include \"$(MHSDIR)/../src/runtime/eval-raspberry-pico.c\"" >> build/main.c

pico-sdk:
	git clone https://github.com/raspberrypi/pico-sdk.git deps/pico-sdk


.PHONY:	pico-sdk build/main.c build
