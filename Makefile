# Search path
VPATH = data data-raw eda reports scripts

# Processed data files
# .rds files in data folder
DATA =

# EDA studies
# .md files in eda folder
EDA =

# Reports
# .md files in report folder
REPORTS =

# All targets
all : $(DATA) $(EDA) $(REPORTS)

# Data dependencies
# [target file] : [dependency file 1] [dependency file 2] [dependency file 3]

# EDA study and report dependencies
# [knitted file] : [cleaned data 1] [ cleaned data 2]


# Pattern rules
%.rds : %.R
	Rscript $<
%.md : %.Rmd
	Rscript -e 'rmarkdown::render(input = "$<", output_options = list(html_preview = FALSE))'
