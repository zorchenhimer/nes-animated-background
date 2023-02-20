
# TODO: don't do this
CA = ../../cc65/bin/ca65
LD = ../../cc65/bin/ld65

CHRUTIL = go-nes/bin/chrutil

CAFLAGS = -g -t nes --color-messages
LDFLAGS = -C $(NESCFG) --dbgfile bin/$(NAME).dbg -m bin/$(NAME).map --color-messages

# Mapper configuration for linker
NESCFG = nes_snrom.cfg

NAME = animated-background
CHR = waves.chr

SOURCES := main.asm \
		   nes2header.inc \
		   bg_anim.asm

WAVE_FRAMES = waves_1 \
			  waves_2 \
			  waves_3 \
			  waves_4 \
			  waves_5 \
			  waves_6 \
			  waves_7 \
			  waves_8 \
			  waves_9 \
			  waves_10 \
			  waves_11 \
			  waves_12 \
			  waves_13 \
			  waves_14 \
			  waves_15

WAVE_BMP := $(addprefix images/,$(addsuffix .bmp,$(WAVE_FRAMES)))

all: $(CHRUTIL) waves.chr bin/ bin/$(NAME).nes

clean:
	-rm bin/*.nes bin/*.o bin/*.dbg bin/*.map

cleanall: clean
	-rm *.chr images/*.bmp

bin/:
	-mkdir bin/

waves.chr: $(WAVE_BMP)
	$(CHRUTIL) -o $@ $^

bin/main.o: $(SOURCES) $(CHR)
	$(CA) $(CAFLAGS) -o $@ main.asm

bin/$(NAME).nes: bin/main.o
	$(LD) $(LDFLAGS) -o $@ $^

images/%.bmp: images/%.aseprite
	aseprite -b $< --save-as $@

$(WAVE_BMP): images/waves_e.aseprite
	aseprite -b $< --save-as images/waves_{frame1}.bmp

$(CHRUTIL):
	$(MAKE) -C go-nes/ bin/chrutil
