################################################################################ .
#                                                                              #
#                       LOCALITY PROFILES: HOUSEHOLDS                          #
#                                                                              #
################################################################################ .
###
### The purpose of this code is to produce graphs which will be used for locality
### profiles produced in RMarkdown.
###
### When rerunning for a new year of data, manually change dates at lines 39 and 41
###
### First Created: 05/09/2019
### UPDATES
### Oct 2021  Aidan Morrison
### Sep 2022  Diane Wilson  Updated to use functions within Global Script and updated palette
###
###
### Contents:
### Section 1 - Packages, working directory etc
### Section 2 - Households Data
### Section 3 - Council Tax Band Data
### Section 4 - Objects for Summary Table


##################### Section 1 - Packages, working directory etc ########################

# load in required packages
library(tidyverse)
library(readxl)
library(janitor)
library(png)
library(gridExtra)
library(reshape2)

# Update Data Year (this is the maximum year available for both housing data sets from NRS)
#max_year_housing <- 2022
# Update Publication Year (the year marked on the Data folder)
#ext_year <- 2023

# Set Directory.
filepath <- paste0(ip_path,"Households/")

# AS: automatic detection of latest Data folder for NRS housing
# Update Publication Year (the year marked on the Data folder)
ext_year_dir <- select_latest_year_dir()

# Read in Global Script for RMarkdown (For testing only)
# source("Master RMarkdown Document & Render Code/Global Script.R")

# Set locality (for testing only)
# LOCALITY <- "Whalsay and Skerries"
# LOCALITY <- "Ayr North and Former Coalfield Communities"


##################### Section 2 - Households Data #############################

## 2a) Data imports & cleaning ----

#household file
household_est <- paste0(ext_year_dir, "/household_estimates.xlsx")

# AS: Update Data Year (this is the maximum year available for both housing data sets from NRS)
housing_sheets <- 
  str_subset(excel_sheets(household_est), "\\d{4}") 

# get historic housing data, each year is on a seperate sheet so do a for loop
  house_raw_dat <- map(housing_sheets, 
                          ~read_excel(household_est,
                                      sheet = .x, 
                                      skip = 3) %>% 
                            mutate(year = .x)
                          ) %>% 
    bind_rows() %>% 
    clean_names() %>% 
    select(year, 1:12)

# get historic housing data, each year is on a separate sheet so do a for loop



# Global Script Function to read in Localities Lookup
# lookup_dz <- read_in_localities(dz_level = TRUE) %>%
#   dplyr::select(datazone2011, hscp_locality) %>%
#   filter(hscp_locality == LOCALITY)
  lookup_dz <- read_in_localities(dz_level = TRUE) %>%
    dplyr::select(datazone2011, hscp_locality) %>%
    filter(hscp_locality %in% locality_list)


# filter housing data for locality of interest
house_dat <- 
  lookup_dz %>% 
  left_join(house_raw_dat, join_by(datazone2011 == data_zone_code))

# # aggregate data
# house_dat1 <- house_dat %>%
#   dplyr::group_by(year) %>%
#   dplyr::summarise(
#     total_dwellings = sum(total_number_of_dwellings),
#     occupied_dwellings = sum(occupied_dwellings),
#     vacant_dwellings = sum(vacant_dwellings),
#     second_homes = sum(second_homes),
#     tax_exempt = sum(occupied_dwellings_exempt_from_paying_council_tax),
#     tax_discount = sum(dwellings_with_a_single_adult_council_tax_discount)
#   ) %>%
#   dplyr::ungroup() %>%
#   dplyr::mutate_at(.vars = 3:7, .funs = funs(perc = 100 * . / total_dwellings))

#AS: updated superseeded script
# aggregate data
house_dat1 <-
  house_dat %>%
  dplyr::group_by(hscp_locality, year) %>%
  dplyr::summarise(
    total_dwellings = sum(total_number_of_dwellings),
    occupied_dwellings = sum(occupied_dwellings),
    vacant_dwellings = sum(vacant_dwellings),
    second_homes = sum(second_homes),
    tax_exempt = sum(occupied_dwellings_exempt_from_paying_council_tax),
    tax_discount = sum(dwellings_with_a_single_adult_council_tax_discount)
  ) %>%
  mutate(across(.cols = 3:7,  ~ .x * 100 / total_dwellings, .names = "{.col}_perc")) %>% 
  ungroup()



## 2b) Text objects ----
# vector form - 1st cell = "Clydebank", 2nd cell = "Dum/Alex"

# numbers 
n_houses <- format_number_for_text(filter(house_dat1, year == max(year)) %>% pull(total_dwellings))
n_occupied <- format_number_for_text(filter(house_dat1, year == max(year))%>% pull(occupied_dwellings))
n_vacant <- format_number_for_text(filter(house_dat1, year == max(year))%>% pull(vacant_dwellings))
n_single_discount <- format_number_for_text(filter(house_dat1, year == max(year))%>% pull(tax_discount))
n_exempt <- format_number_for_text(filter(house_dat1, year == max(year))%>% pull(tax_exempt))
n_second_homes <- format_number_for_text(filter(house_dat1, year == max(year))%>% pull(second_homes))

# percentages
perc_occupied <- format_number_for_text(filter(house_dat1, year == max(year)) %>% pull(occupied_dwellings_perc))
perc_vacant <- format_number_for_text(filter(house_dat1, year == max(year)) %>% pull(vacant_dwellings_perc))
perc_single_discount <- format_number_for_text(filter(house_dat1, year == max(year)) %>% pull(tax_discount_perc))
perc_exempt <- format_number_for_text(filter(house_dat1, year == max(year)) %>% pull(tax_exempt_perc))
perc_second_homes <- format_number_for_text(filter(house_dat1, year == max(year)) %>% pull(second_homes_perc))


## 2c) Plots and Tables ----
# time series function for households and council tax
plot_household_ts <- function(df, yname) {
    df %>%
    mutate(hscp_locality = factor(hscp_locality, levels = locality_list)) %>%
    ggplot(aes(x = year, y = {{yname}}, group = hscp_locality)) +
    geom_line(aes(color = hscp_locality), linewidth = 1) +
    geom_point(aes(colour = hscp_locality))+
    theme_profiles() +
    geom_text(aes(label = format({{yname}}, big.mark = ","), vjust = ifelse(hscp_locality == locality_list[1], 2, -1)),color = "#4a4a4a", size = 3.5
    ) +
    scale_y_continuous(labels = scales::comma, limits = c(0, 1.1 * max(df %>% select({{yname}})))) +
    scale_colour_manual(values = setNames(unname(phs_colors()[1:2]), locality_list)
                        )
  
}
# Total dwellings over time
# houses_ts <- 
#   house_dat1 %>% 
#   mutate(hscp_locality = factor(hscp_locality, levels = locality_list)) %>% 
#   ggplot(aes(x = year, y = total_dwellings, group = hscp_locality)) +
#   geom_line(aes(color = hscp_locality), linewidth = 1) +
#   geom_point(aes(colour = hscp_locality))+
#   theme_profiles() +
#   geom_text(aes(label = format(total_dwellings, big.mark = ","), vjust = ifelse(hscp_locality == locality_list[1], 2, -1)),color = "#4a4a4a", size = 3.5
#   ) +
#   scale_y_continuous(labels = scales::comma, limits = c(0, 1.1 * max(house_dat1$total_dwellings))) +
#   scale_colour_manual(values = setNames(unname(phs_colors()[1:2]), locality_list)
#                       )+
  
houses_ts <- 
  plot_household_ts(house_dat1, total_dwellings)+
labs(
    x = "Year", y = "Number of Dwellings",
    title = paste0("Number of Dwellings by Year by Locality ", max(house_dat1$year)),
    caption = "Source: Council Tax billing system (via NRS)"
  ) +
  theme(plot.title = element_text(size = 12))

#AS: 
# Table in list format
house_table <- 
  map(locality_list, 
  ~house_dat1 %>% 
    filter(hscp_locality == .x) %>% 
    select(year, total_dwellings, occupied_dwellings, vacant_dwellings,
    tax_discount, tax_exempt, second_homes) %>%
  mutate(
    across(
      .cols = 2:7,
      ~ format(., big.mark = ",")
      )
    )
  ) %>% 
  set_names(locality_list)



######################## Section 3 - Council Tax Band Data ############################

## 3a) Data imports & cleaning ----

# https://www.nrscotland.gov.uk/statistics-and-data/statistics/statistics-by-theme/households/household-estimates/small-area-statistics-on-households-and-dwellings


# Council tax file
council_tax <- paste0(ext_year_dir, "/council_tax.xlsx")

# Latest year council tax - taken from latest year of housing data

house_raw_dat2 <- read_excel(council_tax,
  sheet = max(housing_sheets), skip = 4

) %>%
  clean_names()

# filter housing data for locality of interest and aggregate
house_dat2 <- 
  lookup_dz %>% 
  left_join(house_raw_dat2, join_by(datazone2011 == data_zone_code)) %>% 
  select(hscp_locality, where(is.numeric)) %>%
  group_by(hscp_locality) %>% 
  summarise(across(everything(), sum))


## 3b) Plots & tables ----

ctb_list <- 
  map(locality_list, 
      ~house_dat2 %>%
        filter(hscp_locality == .x) %>% 
        select(council_tax_band_a:council_tax_band_h) %>%
        melt()
  ) %>% 
  set_names(locality_list)
  

variable <- ctb_list[[1]]$variable

pal_ctb <- phsstyles::phs_colours(c(
  "phs-magenta", "phs-magenta-80", "phs-magenta-50", "phs-magenta-10",
  "phs-purple-30", "phs-purple-50", "phs-purple-80", "phs-purple"
))


ctb_plot <- 
  map(locality_list,
      ~ctb_list[[.x]] %>% 
        ggplot(aes(
          x = value,
          y = 1,
          fill = factor(variable, levels = rev(variable))
          )) +
        geom_col(position = "fill", colour = "black", linewidth = 0.5, orientation = "y") +
        theme_classic() +
        labs(x = paste0("Proportion of Households for ", .x), y = ""#, caption = "Source: Scottish Assessors’ Association (via NRS)"
             ) +
        scale_fill_manual(
                            name = "Council Tax Band",
                            labels = paste("Band", LETTERS[8:1]),
                            values = pal_ctb,
                            drop = FALSE,
                            guide = guide_legend(reverse = TRUE)
                            ) +
        theme(
          axis.text = element_blank(),
          axis.line = element_blank(),
          axis.ticks = element_blank(),
          legend.position = "none",
          legend.background = element_blank(),
          legend.box.background = element_rect(colour = "black")
        )
  )


ctb_table <- 
  map(locality_list, 
      ~ctb_list[[.x]] %>%
        mutate(percent = paste0(format_number_for_text(100 * value / sum(value)), "%")) %>%
        select(-value) %>%
        pivot_wider(names_from = variable, values_from = percent) %>%
        mutate(`Tax Band` = "Percent of households") %>%
        select(`Tax Band`, 
               A = council_tax_band_a, 
               B = council_tax_band_b,
               C = council_tax_band_c, 
               D = council_tax_band_d, 
               E = council_tax_band_e,
               `F` = council_tax_band_f, 
               G = council_tax_band_g, 
               H = council_tax_band_h)
  ) %>% 
  set_names(locality_list)


## Objects for locality
perc_houses <- 
  house_dat2 %>% 
  group_by(hscp_locality) %>% 
  mutate(perc_houses_AC = format_number_for_text(100*rowSums(across(council_tax_band_a:council_tax_band_b))/total_number_of_dwellings),
         perc_houses_FH = format_number_for_text(100*rowSums(across(council_tax_band_f:council_tax_band_h))/total_number_of_dwellings)) %>% 
  select(hscp_locality, starts_with("perc_"))
  
# perc_houses_AC <- format_number_for_text(sum(
#   house_dat2$council_tax_band_a,
#   house_dat2$council_tax_band_b,
#   house_dat2$council_tax_band_c
# ) / house_dat2$total_number_of_dwellings * 100)

# perc_houses_FH <- format_number_for_text(sum(
#   house_dat2$council_tax_band_f,
#   house_dat2$council_tax_band_g,
#   house_dat2$council_tax_band_h
# ) / house_dat2$total_number_of_dwellings * 100)


########################## Section 4 - Objects for Summary Table ########################

## Relevant lookups for creating the table objects

# Global Script Function to read in Localities Lookup
lookup2 <- read_in_localities(dz_level = FALSE)

# # Determine HSCP and HB based on Loc
# HSCP <- as.character(filter(lookup2, hscp_locality == LOCALITY)$hscp2019name)

# Determine other localities based on LOCALITY object
other_locs <- locality_list[2]
# Find number of locs per partnership
n_loc <- count_localities(lookup2, HSCP)

rm(lookup2)


# 1. Other localities

# Global Script Function to read in Localities Lookup
other_locs_dz <- read_in_localities(dz_level = TRUE) %>%
  arrange() %>%
  dplyr::select(datazone2011, hscp_locality) %>%
 filter(hscp_locality == other_locs)

house_dat_otherlocs <- house_raw_dat %>%
  inner_join(other_locs_dz, by = c("data_zone_code" = "datazone2011")) %>%
  filter(year == max(year)) %>%
  group_by(hscp_locality) %>%
  summarise(
    total_dwellings = sum(total_number_of_dwellings),
    tax_discount = sum(dwellings_with_a_single_adult_council_tax_discount)
  ) %>%
  dplyr::ungroup() %>%
  mutate(tax_discount_perc = round_half_up(tax_discount / total_dwellings * 100, 1))

other_locs_n_houses <- house_dat_otherlocs %>%
  mutate(tot_dwellings_chr = formatC(total_dwellings, format = "d", big.mark = ",")) %>%
  arrange(hscp_locality) %>%
  select(hscp_locality, tot_dwellings_chr) %>%
  pivot_wider(names_from = hscp_locality, values_from = tot_dwellings_chr)

other_locs_perc_discount <- house_dat_otherlocs %>%
  select(hscp_locality, tax_discount_perc) %>%
  arrange(hscp_locality) %>%
  pivot_wider(names_from = hscp_locality, values_from = tax_discount_perc)


house_dat2_otherlocs <- house_raw_dat2 %>%
  inner_join(other_locs_dz, by = c("data_zone_code" = "datazone2011")) %>%
  group_by(hscp_locality) %>%
  summarise(
    total_number_of_dwellings = sum(total_number_of_dwellings),
    band_a = sum(council_tax_band_a),
    band_b = sum(council_tax_band_b),
    band_c = sum(council_tax_band_c),
    band_f = sum(council_tax_band_f),
    band_g = sum(council_tax_band_g),
    band_h = sum(council_tax_band_h)
  ) %>%
  mutate(
    perc_houses_AC = round_half_up((band_a + band_b + band_c) / total_number_of_dwellings * 100, 1),
    perc_houses_FH = round_half_up((band_f + band_g + band_h) / total_number_of_dwellings * 100, 1)
  )

other_locs_perc_housesAC <- house_dat2_otherlocs %>%
  arrange(hscp_locality) %>%
  select(hscp_locality, perc_houses_AC) %>%
  pivot_wider(names_from = hscp_locality, values_from = perc_houses_AC)

other_locs_perc_housesFH <- house_dat2_otherlocs %>%
  arrange(hscp_locality) %>%
  select(hscp_locality, perc_houses_FH) %>%
  pivot_wider(names_from = hscp_locality, values_from = perc_houses_FH)

rm(house_dat2_otherlocs, house_dat_otherlocs, other_locs_dz)


# 2. HSCP

# Global Script Function to read in Localities Lookup
hscp_dz <- read_in_localities(dz_level = TRUE) %>%
  select(datazone2011, hscp2019name) %>%
  filter(hscp2019name == HSCP)


house_dat_hscp <- house_raw_dat %>%
  inner_join(hscp_dz, by = c("data_zone_code" = "datazone2011")) %>%
  filter(year == max(year)) %>%
  group_by(year) %>%
  summarise(
    total_dwellings = sum(total_number_of_dwellings),
    tax_discount = sum(dwellings_with_a_single_adult_council_tax_discount)
  ) %>%
  ungroup() %>%
  mutate(perc_discount = round_half_up(tax_discount / total_dwellings * 100, 1))

hscp_n_houses <- format_number_for_text(house_dat_hscp$total_dwellings)
hscp_perc_discount <- house_dat_hscp$perc_discount


house_dat2_hscp <- house_raw_dat2 %>%
  inner_join(hscp_dz, by = c("data_zone_code" = "datazone2011")) %>%
  group_by(hscp2019name) %>%
  summarise(
    total_dwellings = sum(total_number_of_dwellings),
    band_a = sum(council_tax_band_a),
    band_b = sum(council_tax_band_b),
    band_c = sum(council_tax_band_c),
    band_f = sum(council_tax_band_f),
    band_g = sum(council_tax_band_g),
    band_h = sum(council_tax_band_h)
  ) %>%
  ungroup() %>%
  mutate(
    perc_houses_AC = round_half_up((band_a + band_b + band_c) / total_dwellings * 100, 1),
    perc_houses_FH = round_half_up((band_f + band_g + band_h) / total_dwellings * 100, 1)
  )

hscp_perc_housesAC <- house_dat2_hscp$perc_houses_AC
hscp_perc_housesFH <- house_dat2_hscp$perc_houses_FH

rm(hscp_dz, house_dat_hscp, house_dat2_hscp)


# 3. Scotland
scot_n_houses <- format_number_for_text(sum(filter(house_raw_dat, year == max(year))$total_number_of_dwellings, na.rm = TRUE))
scot_perc_discount <- format_number_for_text(sum(filter(house_raw_dat, year == max(year))$dwellings_with_a_single_adult_council_tax_discount, na.rm = TRUE) / sum(filter(house_raw_dat, year == max(year))$total_number_of_dwellings, na.rm = TRUE) * 100)

scot_perc_housesAC <- format_number_for_text(sum(house_raw_dat2$council_tax_band_a,
  house_raw_dat2$council_tax_band_b,
  house_raw_dat2$council_tax_band_c,
  na.rm = TRUE
) / sum(house_raw_dat2$total_number_of_dwellings, na.rm = TRUE) * 100)
scot_perc_housesFH <- format_number_for_text(sum(house_raw_dat2$council_tax_band_f,
  house_raw_dat2$council_tax_band_g,
  house_raw_dat2$council_tax_band_h,
  na.rm = TRUE
) / sum(house_raw_dat2$total_number_of_dwellings, na.rm = TRUE) * 100)

