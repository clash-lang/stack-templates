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

hdl: ${BUILDDIR}/02-hdl/${TOP}.v

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

${BUILDDIR}/02-hdl/${TOP}.v: src/${NAME}.hs dist-newstyle/packagedb src/*
	${CABAL} build
	mkdir -p ${BUILDDIR}/01-clash
	echo
	${CABAL} run clash -- \
		-package-env $(abspath).ghc.environment.* \
		--verilog \
		-outputdir ${BUILDDIR}/01-clash \
		$<
	rm -Rf ${BUILDDIR}/02-hdl
	mv ${BUILDDIR}/01-clash/${NAME}.topEntity ${BUILDDIR}/02-hdl

dist-newstyle/packagedb:
	${CABAL} build

# Conditionally override to build with stack
ifeq ($(USE_STACK), yes)
CABAL=stack
endif

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
