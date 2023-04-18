library(targets)
# This is an example _targets.R file. Every
# {targets} pipeline needs one.
# Use tar_script() to create _targets.R and tar_edit()
# to open it again for editing.
# Then, run tar_make() to run the pipeline
# and tar_read(summary) to view the results.

# Define custom functions and other global objects.
# This is where you write source(\"R/functions.R\")
# if you keep your functions in external scripts.
source("R/DataMaker.R")

# Set target-specific options such as packages.
tar_option_set(packages = c("tidyverse", "sf", "ggplot2", "readxl", "leaflet", "gsubfn", "r5r", "rstudioapi",
                            "otpr", "leaflet", "tidycensus", "parallel", "haven", 
                            "mlogit", "jsonlite", "VGAM", "nemsr", "modelsummary", "tigris",
                            "viridis", "ggspatial", "dplyr", "rgdal", "mice"))

options(tigris_use_cache = TRUE)

options(java.parameters = "-Xmx8G")


# End this file with a list of target objects.
list(
  tar_target(data, create_data("Data/Utah_Grocery_And_Food_Stores__UDAF_.geojson", "Utah_County_Boundaries/Counties.shp")),
  tar_target(exist_data, existing_data("Data/data_sanjuan.csv", "Data/data_utah.csv", "Data/data_saltlake.csv")),
  tar_target(combined_data, combine_data(data, exist_data))
  #tar_target(imputed_data, impute_data(data, exist_data))
)
