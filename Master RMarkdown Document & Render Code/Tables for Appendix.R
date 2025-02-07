##### LOCALITY PROFILES TABLES FOR APPENDIX #####

## Reads in indicator info from "Indicator Tracker" excel doc and formats tables for profiles

# Packages
library(readxl)
library(dplyr)
library(tidyr)
library(fs)
library(glue)

# Set year of data extracts for folder
<<<<<<< HEAD
#ext_year <- 2023

# Set file path
#lp_path <- "/conf/LIST_analytics/West Hub/02 - Scaled Up Work/RMarkdown/Locality Profiles/"
filepath <- paste0(ip_path, "Project Info & Indicators")

ext_year <- list.files(filepath, "Indicator Tracker") %>% 
  str_subset(".xlsx$") %>% 
  str_extract("\\b\\d{4}\\b") 
=======
ext_year <- 2024

# Set file path
# lp_path <- path("/conf/LIST_analytics/West Hub/02 - Scaled Up Work/RMarkdown/Locality Profiles")
>>>>>>> origin/development

# testing locality
# LOCALITY <- "Forres"

indicator_workbook_path <- path(lp_path, "Project Info & Indicators", glue("Indicator Tracker {ext_year}.xlsx"))

## Indicator Definitions ----

<<<<<<< HEAD
indicator_defs <- read_excel(paste0(filepath, "/Indicator Tracker ", ext_year, ".xlsx"),
  sheet = "Definitions"
)
indicator_defs$format <- "**"
indicator_defs$Indicator <- paste0(indicator_defs$format, indicator_defs$Indicator, indicator_defs$format)

# indicator_defs <- as_tibble(indicator_defs)
indicator_defs <- dplyr::select(indicator_defs, -format)
=======
indicator_defs <- read_excel(
  path = indicator_workbook_path,
  sheet = "Definitions",
  col_types = "text"
) |>
  mutate(Indicator = glue("**{Indicator}**"))
>>>>>>> origin/development


## Data extraction dates ----

<<<<<<< HEAD
dates_extract <- read_excel(paste0(filepath, "/Indicator Tracker ", ext_year, ".xlsx"),
=======
dates_extract <- read_excel(
  path = indicator_workbook_path,
>>>>>>> origin/development
  sheet = "Overview",
  # Extract the the columns A:C (col 1:col 3) but skip the first 4 rows
  range = cell_limits(ul = c(4, 1), lr = c(NA, 3)),
  # Set the headings
  col_names = c("Section", "Indicator", "Date of data extraction"),
  # Set the col types (also parse the dates)
  col_types = c("text", "text", "date")
) |>
  replace_na(list("Date of data extraction" = Sys.Date())) |>
  mutate(Section = glue("**{Section}**"))

## PPA conditions included ----

<<<<<<< HEAD
ppa_def <- read_excel(paste0(filepath, "/Indicator Tracker ", ext_year, ".xlsx"),
  sheet = "PPA"
=======
ppa_def <- read_excel(
  path = indicator_workbook_path,
  sheet = "PPA",
  col_types = "text"
>>>>>>> origin/development
)

rm(indicator_workbook_path)
