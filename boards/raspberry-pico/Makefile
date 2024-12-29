MHSDIR=../..
BIN=../../bin
TOOLS=../../Tools

build: build/blinky_comb.c build/blinky.c build/blinky_btn_comb.c build/blinky_btn.c
	cmake -B build -DPICO_SDK_PATH=deps/pico-sdk/
	cmake --build build

# Generate combinator file
build/blinky_comb.c:	src/Blinky.hs $(BIN)/mhs
	@mkdir -p build
	MHSDIR=$(MHSDIR) $(BIN)/mhs -isrc Blinky -z -obuild/blinky_comb.c
	sed -i s#mhsffi.h#../../../src/runtime/mhsffi.h# build/blinky_comb.c
	sed -i '/mhsffi.h/ a #include "../../../src/runtime/config-raspberry-pico.h"' build/blinky_comb.c

build/blinky_btn_comb.c:	src/BlinkyBtn.hs $(BIN)/mhs
	@mkdir -p build
	MHSDIR=$(MHSDIR) $(BIN)/mhs -isrc BlinkyBtn -z -obuild/blinky_btn_comb.c
	sed -i s#mhsffi.h#../../../src/runtime/mhsffi.h# build/blinky_btn_comb.c
	sed -i '/mhsffi.h/ a #include "../../../src/runtime/config-raspberry-pico.h"' build/blinky_btn_comb.c

$(BIN)/mhs:	$(MHSDIR)/src/runtime/*.c $(MHSDIR)/src/runtime/*.h
	make -C $(MHSDIR) bin/mhs

build/blinky.c:
	@mkdir -p build
	echo "#include \"blinky_comb.c\"" > build/blinky.c
	echo "#include \"$(MHSDIR)/../src/runtime/eval-raspberry-pico.c\"" >> build/blinky.c

build/blinky_btn.c:
	@mkdir -p build
	echo "#include \"blinky_btn_comb.c\"" > build/blinky_btn.c
	echo "#include \"$(MHSDIR)/../src/runtime/eval-raspberry-pico.c\"" >> build/blinky_btn.c


pico-sdk:
	git clone https://github.com/raspberrypi/pico-sdk.git deps/pico-sdk


.PHONY:	pico-sdk build/blinky.c build/blinky_btn.c build
