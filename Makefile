RSCRIPT ?= Rscript
# Example Windows override: make RSCRIPT="C:/Program Files/R/R-4.5.3/bin/Rscript.exe"

.PHONY: all clean-data clean-host clean-refugee rf105-reviewed rf105 drdid

all: clean-data rf105-reviewed

clean-data: clean-host clean-refugee

clean-host:
	"$(RSCRIPT)" 1_run_clean_host_20260805_2141.R

clean-refugee:
	"$(RSCRIPT)" 1_run_clean_refugee_20260805_2141.R

rf105-reviewed: clean-data
	"$(RSCRIPT)" 5_analysis_RF105/reviewed/00_run_RF105_20260805_2213.R

rf105: rf105-reviewed

drdid:
	"$(RSCRIPT)" 5_analysis_RF105/reviewed/5_drDiD_comparison_20260805_2213.R

.PHONY: geocene-reviewed
geocene-reviewed:
	"$(RSCRIPT)" --vanilla 1_run_geocene.R
