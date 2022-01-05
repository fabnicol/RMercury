MMC = /usr/local/mercury-DEV/bin/mmc
PARALLEL =
files = test1.m ri.m


test1: $(files) Mercury.options 
	@$(MMC) --make $(PARALLEL) $@ && touch $@
