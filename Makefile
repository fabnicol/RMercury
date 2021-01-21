MMC = mmc
PARALLEL =
files = $(wildcard *.m)


test0: $(files) Mercury.options 
	@$(MMC) --make $(PARALLEL) $@ && touch $@
