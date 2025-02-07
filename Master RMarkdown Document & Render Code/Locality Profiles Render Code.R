##### LOCALITY PROFILES MASTER DOC RENDER CODE #####

library(knitr)
library(rmarkdown)
library(here)
library(ggtext)
library(kableExtra)
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
source(here("Master RMarkdown Document & Render Code", "Global Script.R"))


## Specify HSCP here
## NOTE - make sure that the formatting of the partnership's name matches the lookup
HSCP <- "West Dunbartonshire"
# Set file path
lp_path <- lp_path <- "/conf/LIST_analytics/West Dunbartonshire/Locality Profiles Combined/"
output_dir <- path(lp_path, "Master RMarkdown Document & Render Code", "Output")



lookup <- read_in_localities()

# Below creates locality list of all the localities in a chosen HSCP
locality_list <-
  read_in_localities(dz_level = TRUE) %>%
  filter(hscp2019name == HSCP) %>% 
  distinct(hscp_locality) %>% 
  pull()

# Specify HSCP(s) ----
# use `unique(lookup$hscp2019name)` for all
# or create a vector for multiple e.g. `c("Angus", "West Lothian")`
# For a larger test, use the below to produce profiles for HSCPs likely to cause issues.
# source("Master RMarkdown Document & Render Code/find_hscp_outliers.R")
# hscp_list <- outlier_hscps
#hscp_list <- "West Dunbartonshire"

# NOTE - This checks that it exactly matches the lookup
stopifnot(all(HSCP %in% unique(lookup[["hscp2019name"]])))

# Loop over HSCP ----
# 'looping' over one HSCP is fine.
# for (HSCP in hscp_list) {
#   # Create list of localities in chosen HSCP
#   locality_list <- lookup |>
#     filter(hscp2019name == HSCP) |>
#     pull(hscp_locality)
# 
#   # Loop to create the profiles for all the localities in the list
# 
#   # There are several stages to the profiles:
#   # 1. Looping through each locality in the HSCP doing the following:
#   # 1a. Run each section script for that locality
#   # 1b. Run the Rmd for the main body of the profiles
#   # 1c. Run the Rmd for the summary tables
# 
   #loop_env <- c(ls(), "loop_env")

map <- paste0(ip_path, "Master RMarkdown Document & Render Code/Output/maps/", HSCP, ".png")

    # Demographics ----
    source(here("Demographics", "1. Demographics - Population.R"))
    source(here("Demographics", "/2. Demographics - SIMD.R"))

    # Housing ----
    source("Households/Households Code.R")

    # Services ----
    source("Services/2. Services data manipulation & table.R")
    source("Services/3. Service HSCP map.R")

    # General Health ----
    source("General Health/3. General Health Outputs.R")

    # Lifestyle & Risk Factors ----
    source("Lifestyle & Risk Factors/2. Lifestyle & Risk Factors Outputs.R")

    # Unscheduled Care ----
    source("Unscheduled Care/2. Unscheduled Care outputs.R")

    # Appendices ----
    source("Master RMarkdown Document & Render Code/Tables for Appendix.R")

    # Render main profile content ----
    render(
      input = "Master RMarkdown Document & Render Code/Locality_Profiles_Master_Markdown.Rmd",
      output_file = glue("{LOCALITY} - Locality Profile.docx"),
      output_dir = output_dir
    )

    # Render the summary table(s) ----
    render(
      input = "Summary Table/Summary-Table-Markdown.Rmd",
      output_file = glue("{LOCALITY} - Summary Table.docx"),
      output_dir = path(output_dir, "Summary Tables")
    )

    # End of loop housekeeping ----
    # Clean up the environment by restoring it to the 'pre-loop' state.
    rm(list = setdiff(ls(), loop_env))
    # Force garbage collection to free up memory
    gc()


