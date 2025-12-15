##----------------------------------------------------------------------------##
#   Project Settings                                                           #
##----------------------------------------------------------------------------##

NAME = Blink
TOP = topEntity
BUILDDIR= _build
PNR_FLAGS= \
	--85k \
	--package CSFBGA285 \
	--lpf orangecrab.pcf \

-include build.cfg
-include build.cfg.local

##----------------------------------------------------------------------------##
#   Build Rules                                                                #
##----------------------------------------------------------------------------##

default: bitstream

upload: ${BUILDDIR}/04-bitstream/${TOP}.bit
	${PROG} -S $<

synth: ${BUILDDIR}/03-net/$(TOP).json

netlist: ${BUILDDIR}/04-bitstream/$(TOP).config

bitstream: ${BUILDDIR}/04-bitstream/$(TOP).bit

${BUILDDIR}/04-bitstream/${TOP}.dfu: ${BUILDDIR}/04-bitstream/${TOP}.bit
	cp -a $< $@
	${DFUSUFFIX} -v 1209 -p 5af0 -a $@

${BUILDDIR}/04-bitstream/${TOP}.bit: ${BUILDDIR}/04-bitstream/${TOP}.config
	${PACK} --compress --freq 38.8 --input $< --bit $@

${BUILDDIR}/04-bitstream/$(TOP).config: ${BUILDDIR}/03-net/$(TOP).json
	mkdir -p ${BUILDDIR}/04-bitstream
	${PNR} --json $< --textcfg $@ ${PNR_FLAGS}

${BUILDDIR}/03-net/$(TOP).json: ${BUILDDIR}/02-hdl/${TOP}.v
	mkdir -p ${BUILDDIR}/03-net ${BUILDDIR}/log
	$(YOSYS) \
		-l ${BUILDDIR}/log/synth.log \
		-p 'read_verilog $(dir $^)/*.v; synth_ecp5 -top $(TOP) -json $@'

${BUILDDIR}/02-hdl/${TOP}.v: verilog/${NAME}.${TOP}/${TOP}.v
        mkdir -p ${BUILDDIR}
        rm -Rf ${BUILDDIR}/02-hdl
				cp -a verilog/${NAME}.${TOP} ${BUILDDIR}/02-hdl

##----------------------------------------------------------------------------##
#   Cleanup                                                                    #
##----------------------------------------------------------------------------##

clean:
	rm -Rf ${BUILDDIR}

##----------------------------------------------------------------------------##
#   Special Targets                                                            #
##----------------------------------------------------------------------------##

.PHONY: clean upload
.SECONDARY:
.SILENT:
