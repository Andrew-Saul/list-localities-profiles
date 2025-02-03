##### LOCALITY PROFILES MASTER DOC RENDER CODE #####

library(readxl)
library(janitor)
library(tidyverse)
library(knitr)
library(markdown)
library(rmarkdown)
library(here)
library(patchwork)

rm(list = ls())

## Input (Read) Project file path
ip_path <- "/conf/LIST_analytics/West Hub/02 - Scaled Up Work/RMarkdown/Locality Profiles/"
#lp_path <- "/conf/LIST_analytics/West Hub/02 - Scaled Up Work/RMarkdown/Locality Profiles/"

# Output (Write) Project file path
op_path <- "/conf/LIST_analytics/West Dunbartonshire/Locality Profiles Combined/"


# system unmask function so files have read-write permissions
Sys.umask("006")

# Source in functions code
source(here("Master RMarkdown Document & Render Code/Global Script.R"))

## Specify HSCP here
## NOTE - make sure that the formatting of the partnership's name matches the lookup
HSCP <- "West Dunbartonshire"

# Below creates locality list of all the localities in a chosen HSCP
lookup <- read_in_localities()

HSCP_list <- unique(lookup$hscp2019name)

# Create list of localities in chosen HSCP
locality_list <- lookup |> 
  filter(hscp2019name == HSCP) |> 
  pull(hscp_locality)


## Loop to create the profiles for all the localities in the list

## There are several stages to the profiles:
# 1. Producing the HSCP services map (this takes a while to run so it is produced separately)
# 2. Looping through each locality in the HSCP doing the following:
# 2a. Run each section script for that locality
# 2b. Run the Rmd for the main body of the profiles
# 2c. Run the Rmd for the summary tables

# 1. HSCP Services Map

map <- paste0(ip_path, "Master RMarkdown Document & Render Code/Output/maps/", HSCP, ".png")

stopifnot(file.exists(map)) # Error if the file path doesn't exist.

# 2. Loop through each locality to create the main body of the profiles and the summary table
#for (LOCALITY in locality_list) {
  ## 2a) Source in all the scripts for a given LOCALITY

  # demographics
  source(here("Demographics/1. Demographics - Population.R"))
  source(here("Demographics/2. Demographics - SIMD.R"))

  # housing
  source(here("Households/Households Code.R"))

  # services
  source(here("Services/2. Services data manipulation & table.R"))

  # general health
  source(here("General Health/3. General Health Outputs.R"))

  # lifestyle & risk factors
  source(here("Lifestyle & Risk Factors/2. Lifestyle & Risk Factors Outputs.R"))
  # unscheduled care
  source(here("Unscheduled Care/2. Unscheduled Care outputs.R"))

  # appendices
  source(here("Master RMarkdown Document & Render Code/Tables for Appendix.R"))

  # Remove tidylog package which messes up outputs
  detach(package:tidylog, unload = TRUE)

  ## 2b) Create the main body of the profiles

  rmarkdown::render(paste0(op_path, "Master RMarkdown Document & Render Code/Locality_Profiles_Master_Markdown.Rmd"),
    output_file = paste0(LOCALITY, " - Locality Profile.docx"),
    output_dir = paste0(op_path, "Master RMarkdown Document & Render Code/Output/")
  )

  ## 2c) Create the summary tables
  rmarkdown::render(paste0(op_path, "Summary Table/Summary-Table-Markdown.Rmd"),
    output_file = paste0(LOCALITY, " - Summary Table.docx"),
    output_dir = paste0(op_path, "Master RMarkdown Document & Render Code/Output/Summary Tables/")
  )
}
