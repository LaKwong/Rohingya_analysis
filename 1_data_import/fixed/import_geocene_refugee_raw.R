# Import both fuel-specific event exports for each analysis variant.
# Inputs: 2_data_raw/geocene_{biomass,lpg}_100_80_5_{20,30}/*.csv
# Output and mission provenance: 8_restricted/geocene_pipeline/
source(file.path("1_data_import", "fixed", "geocene_pipeline_helpers.R"))
geocene_import()
