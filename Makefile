MMC = /usr/local/mercury-DEV/bin/mmc
PARALLEL =
files = test0.m ri.m


test0: $(files) Mercury.options 
	@$(MMC) --make $(PARALLEL) $@ && touch $@
