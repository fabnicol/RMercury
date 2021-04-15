MMC = /usr/local/mercury-DEV/bin/mmc
PARALLEL =
files = $(wildcard *.m)


test0: $(files) Mercury.options 
	@$(MMC) --make $(PARALLEL) $@ && touch $@
