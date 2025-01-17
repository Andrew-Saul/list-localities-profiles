##################### LOCALITY PROFILES UNSCHEDULED CARE: OUTPUTS ######################.

# Original author: Will Clayton
# Updated Oct/Nov 2022 by Adam Rennie to use Global Script functions
# Last edits Late Nov 22 by Luke Taylor and Cecilia Puech to tidy up script and change outputs

####################### SECTION 1: Packages, file paths, etc #########################

## Manually set year that the profiles are being run (year on data folder)
#ext_year <- 2023

# Set locality profiles file path
# lp_path <- "/conf/LIST_analytics/West Hub/02 - Scaled Up Work/RMarkdown/Locality Profiles/"
# import_folder <- paste0(lp_path, "Unscheduled Care/DATA ", ext_year, "/")
filepath <- paste0(ip_path, "Unscheduled Care")

ext_year_dir <- select_latest_year_dir()

## Packages
library(tidyverse)
library(janitor)
library(tidylog)
library(magrittr)
library(lubridate)
library(scales)
library(broom)
library(reshape2)
library(haven)
library(fst)


### for testing run global script and locality placeholder below 

## Functions
#source("./Master RMarkdown Document & Render Code/Global Script.R")

## Define locality
# LOCALITY <- "Stirling City with the Eastern Villages Bridge of Allan and Dunblane"
#LOCALITY <- "Inverness"
# LOCALITY <- "Ayr North and Former Coalfield Communities"
# LOCALITY <- "Whalsay and Skerries"
# LOCALITY <- "North Perthshire"

# Set date limit for financial year
# If in quarter 1
max_fy <- ifelse(quarter(Sys.Date()) == "1L", fy(Sys.Date()), fy(ymd(Sys.Date()) - years(1)))

########################## SECTION 2: Lookups & Populations ###############################

## 1. Lookups ----

localities <- read_in_localities()

HSCP <- as.character(filter(localities, hscp_locality == LOCALITY)$hscp2019name)
HB <- as.character(filter(localities, hscp_locality == LOCALITY)$hb2019name)

# Determine other localities based on LOCALITY object
other_locs <- localities %>%
  select(hscp_locality, hscp2019name) %>%
  filter(hscp2019name == HSCP & hscp_locality != LOCALITY) %>%
  arrange(hscp_locality)

# Find number of locs per partnership
n_loc <- localities %>%
  group_by(hscp2019name) %>%
  summarise(locality_n = n()) %>%
  filter(hscp2019name == HSCP) %>%
  pull(locality_n)


## 2. Populations (for rates) ----

populations <- read_in_dz_pops()

populations22 <- read_in_dz_pops22()

populations <- rbind(populations,populations22)


#pop_max_year <- max(populations$year)

# compute age bands
populations$"Pop0_17" <- rowSums(subset(populations, select = age0:age17))
populations$"Pop18_44" <- rowSums(subset(populations, select = age18:age44))
populations$"Pop45_64" <- rowSums(subset(populations, select = age45:age64))
populations$"Pop65_74" <- rowSums(subset(populations, select = age65:age74))
populations$"Pop75Plus" <- rowSums(subset(populations, select = age75:age90plus))
populations$"Pop65Plus" <- rowSums(subset(populations, select = age65:age90plus))

pops <- populations %>%
  select(
    year, hb2019name, hscp2019name, hscp_locality,
    Pop0_17, Pop18_44, Pop45_64, Pop65_74,
    Pop75Plus, Pop65Plus, total_pop
  ) %>%
  mutate(financial_year = paste0(year, "/", substr(year + 1, 3, 4))) %>%
  group_by(financial_year, year, hb2019name, hscp2019name, hscp_locality) %>%
  summarise_all(sum) %>%
  ungroup()


# Aggregate and add partnership + HB + Scotland totals

pop_areas <- pops %>%
  filter(hscp_locality == LOCALITY) %>%
  select(-hb2019name, -hscp2019name) %>%
  rename(location = hscp_locality) %>%
  # Add a partnership total
  bind_rows(
    pops %>%
      select(-hscp_locality, -hb2019name) %>%
      filter(hscp2019name == HSCP) %>%
      group_by(financial_year, year, hscp2019name) %>%
      summarise_all(sum) %>%
      ungroup() %>%
      rename(location = hscp2019name)
  ) %>%
  # Add HB total
  bind_rows(
    pops %>%
      select(-hscp_locality, -hscp2019name) %>%
      filter(hb2019name == HB) %>%
      group_by(financial_year, year, hb2019name) %>%
      summarise_all(sum) %>%
      ungroup() %>%
      rename(location = hb2019name)
  ) %>%
  # Add a Scotland total
  bind_rows(
    pops %>%
      select(-hscp_locality, -hscp2019name, -hb2019name) %>%
      group_by(financial_year, year) %>%
      summarise_all(sum) %>%
      ungroup() %>%
      mutate(location = "Scotland")
  ) %>%
  pivot_longer("Pop0_17":"total_pop", names_to = "age_group", values_to = "pop") %>%
  mutate(age_group = case_when(
    age_group == "Pop0_17" ~ "0 - 17",
    age_group == "Pop18_44" ~ "18 - 44",
    age_group == "Pop45_64" ~ "45 - 64",
    age_group == "Pop65_74" ~ "65 - 74",
    age_group == "Pop75Plus" ~ "75+",
    age_group == "Pop65Plus" ~ "65+",
    age_group == "total_pop" ~ "Total"
  ))


loc_pop <- pops %>%
  pivot_longer("Pop0_17":"total_pop", names_to = "age_group", values_to = "pop") %>%
  mutate(age_group = case_when(
    age_group == "Pop0_17" ~ "0 - 17",
    age_group == "Pop18_44" ~ "18 - 44",
    age_group == "Pop45_64" ~ "45 - 64",
    age_group == "Pop65_74" ~ "65 - 74",
    age_group == "Pop75Plus" ~ "75+",
    age_group == "Pop65Plus" ~ "65+",
    age_group == "total_pop" ~ "Total"
  ))

# populations for age group charts
loc_pop_age1 <- loc_pop %>%
  filter(
    hscp_locality == LOCALITY,
    age_group %in% c("0 - 17", "18 - 44", "45 - 64", "65 - 74", "75+")
  )

# pop for MH emergency admissions age group chart
loc_pop_age2 <- loc_pop %>%
  filter(
    hscp_locality == LOCALITY,
    age_group %in% c("0 - 17", "18 - 44", "45 - 64", "65+")
  )

# populations by area - all ages
pop_areas_all_ages <- pop_areas %>%
  filter(age_group == "Total")

# populations by area - 65+
pop_areas_65plus <- pop_areas %>%
  filter(age_group == "65+")

# populations for other localities in the HSCP (for summary table only) - all ages
pops_other_locs <- inner_join(loc_pop, other_locs) %>%
  filter(
    age_group == "Total",
    year == max(year)
  ) %>%
  select(financial_year, year, hscp_locality, pop)

# populations for other localities in the HSCP (for summary table only) - 65+
pops_other_locs_65plus <- inner_join(loc_pop, other_locs) %>%
  filter(
    age_group == "65+",
    year == max(year)
  ) %>%
  select(financial_year, year, hscp_locality, pop)


########################## SECTION 3: Functions ###############################

# Functions for aggregating data
# For this function to work, the main variable of the data (ex: number of admissions) must be renamed "n"

aggregate_usc_area_data <- function(data) {
  pts_locality <- data %>%
    filter(hscp_locality == LOCALITY) %>%
    mutate(location = hscp_locality) %>%
    group_by(financial_year, location) %>%
    summarise(n = sum(n)) %>%
    ungroup() %>%
    mutate(area_type = "Locality")

  pts_hscp <- data %>%
    filter(hscp2019name == HSCP) %>%
    mutate(location = hscp2019name) %>%
    group_by(financial_year, location) %>%
    summarise(n = sum(n)) %>%
    ungroup() %>%
    mutate(area_type = "HSCP")

  pts_hb <- data %>%
    left_join(select(localities, hscp_locality, hb2019name)) %>%
    filter(hb2019name == HB) %>%
    mutate(location = hb2019name) %>%
    group_by(financial_year, location) %>%
    summarise(n = sum(n)) %>%
    ungroup() %>%
    mutate(area_type = "HB")

  pts_scot <- data %>%
    group_by(financial_year) %>%
    summarise(n = sum(n)) %>%
    ungroup() %>%
    mutate(
      location = "Scotland",
      area_type = "Scotland"
    )

  bind_rows(pts_locality, pts_hscp, pts_hb, pts_scot) %>%
    mutate(area_type = factor(area_type, levels = c("Locality", "HSCP", "HB", "Scotland")))
}

# Functions for creating time trends
age_group_trend_usc <- function(data_for_plot, plot_title, yaxis_title, source) {
  data_for_plot %>%
    ggplot(aes(x = financial_year, y = data, group = age_group, color = age_group)) +
    geom_line(size = 1) +
    geom_point() +
    scale_colour_manual(values = c(palette)) +
    scale_x_discrete(breaks = data_for_plot$financial_year) +
    scale_y_continuous(labels = comma, limits = c(0, 1.1 * max(data_for_plot$data))) +
    theme_profiles() +
    labs(
      title = plot_title,
      y = yaxis_title,
      x = "Financial Year",
      color = "Age Group",
      caption = source
    ) +
    theme(plot.title = element_text(hjust = 0.5, size = 12))
}

area_trend_usc <- function(data_for_plot, plot_title, yaxis_title, source) {
  data_for_plot %>%
    mutate(location = fct_reorder(as.factor(str_wrap(location, 23)), as.numeric(area_type))) %>%
    ggplot() +
    aes(x = financial_year, y = data, group = location, fill = location, linetype = area_type) +
    geom_line(aes(colour = location), size = 1) +
    geom_point(aes(colour = location), size = 2) +
    scale_fill_manual(values = palette) +
    scale_colour_manual(values = palette) +
    theme_profiles() +
    expand_limits(y = 0) +
    scale_x_discrete(breaks = data_for_plot$financial_year) +
    scale_y_continuous(labels = comma, limits = c(0, 1.1 * max(data_for_plot$data))) +
    labs(
      title = plot_title,
      y = yaxis_title,
      x = "Financial Year",
      caption = source
    ) +
    theme(
      plot.title = element_text(hjust = 0.5, size = 12),
      legend.title = element_blank()
    ) +
    guides(
      linetype = "none", shape = "none", fill = "none",
      colour = guide_legend(nrow = 1, byrow = TRUE)
    )
}


####################### SECTION 4: Data manipulation & outputs #########################

# 1. Emergency Admissions ----
# _________________________________________________________________________

emergency_adm <- arrow::read_parquet(paste0(ext_year_dir, "/emergency_admissions_msg.parquet")) %>%
  filter(financial_year <= max_fy)

# Plotting by age
emergency_adm_age <- emergency_adm %>%
  filter(hscp_locality == LOCALITY) %>%
  drop_na(age_group) %>%
  group_by(financial_year, age_group) %>%
  summarise(adm = sum(admissions)) %>%
  ungroup() %>%
  left_join(loc_pop_age1) %>%
  mutate(data = round_half_up(adm / pop * 100000))


EAs_age_ts <- age_group_trend_usc(
  data_for_plot = emergency_adm_age,
  plot_title = paste("Emergency admissions per 100,000 over time by age group\n for", LOCALITY),
  yaxis_title = "Emergency admission rate\n per 100,000 population",
  source = "Source: PHS SMR01"
)

# Plotting by area
emergency_adm_areas <- emergency_adm %>%
  rename(n = admissions) %>%
  aggregate_usc_area_data() %>%
  left_join(pop_areas_all_ages) %>%
  mutate(data = round_half_up(n / pop * 100000))

EAs_loc_ts <- area_trend_usc(
  data_for_plot = emergency_adm_areas,
  plot_title = "Emergency admissions per 100,000 over time by residence",
  yaxis_title = "Emergency admission rate\n per 100,000 population",
  source = "Source: PHS SMR01"
)


# Objects for text and summary table
latest_emergency_adm_loc <- emergency_adm_areas %>%
  filter(
    location == LOCALITY,
    year == max(year)
  ) %>%
  mutate(data = format(data, big.mark = ",")) %>%
  pull(data)

hscp_emergency_adm <- emergency_adm_areas %>%
  filter(
    location == HSCP,
    year == max(year)
  ) %>%
  mutate(data = format(data, big.mark = ",")) %>%
  pull(data)

scot_emergency_adm <- emergency_adm_areas %>%
  filter(
    location == "Scotland",
    year == max(year)
  ) %>%
  mutate(data = format(data, big.mark = ",")) %>%
  pull(data)

other_loc_emergency_adm <- emergency_adm %>%
  group_by(financial_year, hscp_locality) %>%
  summarise(adm = sum(admissions)) %>%
  ungroup() %>%
  right_join(pops_other_locs) %>%
  mutate(adm = replace_na(adm, 0)) %>%
  mutate(data = round_half_up(adm / pop * 100000)) %>%
  mutate(data = format(data, big.mark = ",")) %>%
  select(hscp_locality, data) %>%
  spread(hscp_locality, data)



# 2a. Unscheduled bed days ----
# _________________________________________________________________________

bed_days <- arrow::read_parquet(paste0(ext_year_dir, "/bed_days_msg.parquet")) %>%
  filter(financial_year <= max_fy)

# Plotting by age
bed_days_age <- bed_days %>%
  filter(hscp_locality == LOCALITY) %>%
  drop_na(age_group) %>%
  group_by(financial_year, age_group) %>%
  summarise(bed_days = sum(bed_days)) %>%
  ungroup() %>%
  left_join(loc_pop_age1) %>%
  mutate(data = round_half_up(bed_days / pop * 100000))


BDs_age_ts <- age_group_trend_usc(
  data_for_plot = bed_days_age,
  plot_title = paste("Unscheduled bed days per 100,000 over time by age group\n for", LOCALITY),
  yaxis_title = "Unscheduled bed day rate\n per 100,000 population",
  source = "Source: PHS SMR01"
)


# Plotting by area
bed_days_areas <- bed_days %>%
  rename(n = bed_days) %>%
  aggregate_usc_area_data() %>%
  left_join(pop_areas_all_ages) %>%
  mutate(data = round_half_up(n / pop * 100000))

BDs_loc_ts <- area_trend_usc(
  data_for_plot = bed_days_areas,
  plot_title = "Unscheduled bed days per 100,000 over time by residence",
  yaxis_title = "Unscheduled bed day rate\n per 100,000 population",
  source = "Source: PHS SMR01"
)


# Objects for text and summary table
latest_bed_days_loc <- bed_days_areas %>%
  filter(
    location == LOCALITY,
    year == max(year)
  ) %>%
  mutate(data = format(data, big.mark = ",")) %>%
  pull(data)

hscp_bed_days <- bed_days_areas %>%
  filter(
    location == HSCP,
    year == max(year)
  ) %>%
  mutate(data = format(data, big.mark = ",")) %>%
  pull(data)

scot_bed_days <- bed_days_areas %>%
  filter(
    location == "Scotland",
    year == max(year)
  ) %>%
  mutate(data = format(data, big.mark = ",")) %>%
  pull(data)

other_loc_bed_days <- bed_days %>%
  group_by(financial_year, hscp_locality) %>%
  summarise(bed_days = sum(bed_days)) %>%
  ungroup() %>%
  right_join(pops_other_locs) %>%
  mutate(adm = replace_na(bed_days, 0)) %>%
  mutate(data = round_half_up(bed_days / pop * 100000)) %>%
  mutate(data = format(data, big.mark = ",")) %>%
  select(hscp_locality, data) %>%
  spread(hscp_locality, data)


# 2b. Unscheduled bed days - Mental Health ----
# _________________________________________________________________________

bed_days_mh <- arrow::read_parquet(paste0(ext_year_dir, "/bed_days_mh_msg.parquet")) %>%
  filter(financial_year <= max_fy)

# Plotting by age
bed_days_mh_age <- bed_days_mh %>%
  filter(hscp_locality == LOCALITY) %>%
  drop_na(age_group) %>%
  group_by(financial_year, age_group) %>%
  summarise(bed_days = sum(bed_days)) %>%
  ungroup() %>%
  left_join(loc_pop_age1) %>%
  mutate(data = round_half_up(bed_days / pop * 100000))


BDMH_age_ts <- age_group_trend_usc(
  data_for_plot = bed_days_mh_age,
  plot_title = paste("Unscheduled bed days (MH) per 100,000 over time by age group\n for", LOCALITY),
  yaxis_title = "Unscheduled bed day (MH) rate\n per 100,000 population",
  source = "Source: PHS SMR04"
)


# Plotting by area
bed_days_mh_areas <- bed_days_mh %>%
  rename(n = bed_days) %>%
  aggregate_usc_area_data() %>%
  left_join(pop_areas_all_ages) %>%
  mutate(data = round_half_up(n / pop * 100000))

BDMH_loc_ts <- area_trend_usc(
  data_for_plot = bed_days_mh_areas,
  plot_title = "Unscheduled bed days (MH) per 100,000 over time by residence",
  yaxis_title = "Unscheduled bed day (MH) rate\n per 100,000 population",
  source = "Source: PHS SMR04"
)


# Objects for text and summary table
latest_bed_days_mh_loc <- bed_days_mh_areas %>%
  filter(
    location == LOCALITY,
    year == max(year)
  ) %>%
  mutate(data = format(data, big.mark = ",")) %>%
  pull(data)

latest_bed_days_mh_loc <- ifelse(is_empty(latest_bed_days_mh_loc), "NA", latest_bed_days_mh_loc)

hscp_bed_days_mh <- bed_days_mh_areas %>%
  filter(
    location == HSCP,
    year == max(year)
  ) %>%
  mutate(data = format(data, big.mark = ",")) %>%
  pull(data)

scot_bed_days_mh <- bed_days_mh_areas %>%
  filter(
    location == "Scotland",
    year == max(year)
  ) %>%
  mutate(data = format(data, big.mark = ",")) %>%
  pull(data)

other_loc_bed_days_mh <- bed_days_mh %>%
  group_by(financial_year, hscp_locality) %>%
  summarise(bed_days = sum(bed_days)) %>%
  ungroup() %>%
  right_join(pops_other_locs) %>%
  mutate(adm = replace_na(bed_days, 0)) %>%
  mutate(data = round_half_up(bed_days / pop * 100000)) %>%
  mutate(data = format(data, big.mark = ",")) %>%
  select(hscp_locality, data) %>%
  spread(hscp_locality, data)


# 3. A&E Attendances ----
# _________________________________________________________________________

ae_attendances <- arrow::read_parquet(paste0(ext_year_dir, "/ae_attendances_msg.parquet")) %>%
  filter(financial_year <= max_fy)

# Plotting by age
ae_att_age <- ae_attendances %>%
  filter(
    hscp_locality == LOCALITY,
    age_group != "NA"
  ) %>%
  group_by(financial_year, age_group) %>%
  summarise(attendances = sum(attendances)) %>%
  ungroup() %>%
  left_join(loc_pop_age1) %>%
  mutate(data = round_half_up(attendances / pop * 100000))

AandE_age_ts <- age_group_trend_usc(
  data_for_plot = ae_att_age,
  plot_title = paste("A&E attendances per 100,000 over time by age group\n for", LOCALITY),
  yaxis_title = "A&E attendance rate\n per 100,000 population",
  source = "Source: PHS A&E Datamart"
)


# Plotting by area
ae_att_areas <- ae_attendances %>%
  rename(n = attendances) %>%
  aggregate_usc_area_data() %>%
  left_join(pop_areas_all_ages) %>%
  mutate(data = round_half_up(n / pop * 100000))

AandE_loc_ts <- area_trend_usc(
  data_for_plot = ae_att_areas,
  plot_title = paste("A&E attendances per 100,000 over time by residence"),
  yaxis_title = "A&E attendance rate\n per 100,000 population",
  source = "Source: PHS A&E Datamart"
)


# Objects for text and summary table
latest_ae_att_loc <- ae_att_areas %>%
  filter(
    location == LOCALITY,
    year == max(year)
  ) %>%
  mutate(data = format(data, big.mark = ",")) %>%
  pull(data)

hscp_ae_att <- ae_att_areas %>%
  filter(
    location == HSCP,
    year == max(year)
  ) %>%
  mutate(data = format(data, big.mark = ",")) %>%
  pull(data)

scot_ae_att <- ae_att_areas %>%
  filter(
    location == "Scotland",
    year == max(year)
  ) %>%
  mutate(data = format(data, big.mark = ",")) %>%
  pull(data)

other_loc_ae_att <- ae_attendances %>%
  group_by(financial_year, hscp_locality) %>%
  summarise(attendances = sum(attendances)) %>%
  ungroup() %>%
  right_join(pops_other_locs) %>%
  mutate(attendances = replace_na(attendances, 0)) %>%
  mutate(data = round_half_up(attendances / pop * 100000)) %>%
  mutate(data = format(data, big.mark = ",")) %>%
  select(hscp_locality, data) %>%
  spread(hscp_locality, data)


# 4. Delayed Discharges ----
# _________________________________________________________________________

delayed_disch <- arrow::read_parquet(paste0(ext_year_dir, "/delayed_discharges_msg.parquet")) %>%
  filter(financial_year <= max_fy) %>%
  filter(age_group %in% c("65 - 74", "75+")) %>%
  group_by(financial_year, hscp2019name, hscp_locality) %>%
  summarise(
    dd_people = sum(dd_people),
    dd_bed_days = sum(dd_bed_days)
  ) %>%
  ungroup()


# Plotting by area
delayed_disch_areas <- delayed_disch %>%
  rename(n = dd_bed_days) %>%
  aggregate_usc_area_data() %>%
  left_join(pop_areas_65plus) %>%
  mutate(data = round_half_up(n / pop * 100000))

DD_loc_ts <- area_trend_usc(
  data_for_plot = delayed_disch_areas,
  plot_title = paste0(
    "Delayed discharge bed days per 100,000 population aged over 65\n",
    "over time by residence"
  ),
  yaxis_title = "Delayed discharge bed day rate\n per 100,000 population aged 65+",
  source = "Source: PHS Delayed Discharges"
)


# Objects for text and summary table
latest_dd_loc <- delayed_disch_areas %>%
  filter(
    location == LOCALITY,
    year == max(year)
  ) %>%
  mutate(data = format(data, big.mark = ",")) %>%
  pull(data)

hscp_dd <- delayed_disch_areas %>%
  filter(
    location == HSCP,
    year == max(year)
  ) %>%
  mutate(data = format(data, big.mark = ",")) %>%
  pull(data)

scot_dd <- delayed_disch_areas %>%
  filter(
    location == "Scotland",
    year == max(year)
  ) %>%
  mutate(data = format(data, big.mark = ",")) %>%
  pull(data)

other_loc_dd <- delayed_disch %>%
  group_by(financial_year, hscp_locality) %>%
  summarise(dd_bed_days = sum(dd_bed_days)) %>%
  ungroup() %>%
  right_join(pops_other_locs_65plus) %>%
  mutate(dd_bed_days = replace_na(dd_bed_days, 0)) %>%
  mutate(data = round_half_up(dd_bed_days / pop * 100000)) %>%
  mutate(data = format(data, big.mark = ",")) %>%
  select(hscp_locality, data) %>%
  spread(hscp_locality, data)



# 5. Fall Admissions ----
# _________________________________________________________________________

falls <- arrow::read_parquet(paste0(ext_year_dir, "/falls_smr.parquet")) %>%
  filter(financial_year <= max_fy) %>%
  filter(age_group %in% c("65 - 74", "75+"))

# Plotting by area
falls_areas <- falls %>%
  rename(n = admissions) %>%
  aggregate_usc_area_data() %>%
  left_join(pop_areas_65plus) %>%
  mutate(data = round_half_up(n / pop * 100000))

Falls_loc_ts <- area_trend_usc(
  data_for_plot = falls_areas,
  plot_title = paste0(
    "Emergency admissions from falls per 100,000 population aged over 65\n",
    "over time by residence"
  ),
  yaxis_title = "Emergency admissions from falls rate\nper 100,000 population aged 65+",
  source = "Source: PHS SMR01"
)


# Objects for text and summary table
latest_falls_loc <- falls_areas %>%
  filter(
    location == LOCALITY,
    year == max(year)
  ) %>%
  mutate(data = format(data, big.mark = ",")) %>%
  pull(data)

scot_falls <- falls_areas %>%
  filter(
    location == "Scotland",
    year == max(year)
  ) %>%
  mutate(data = format(data, big.mark = ",")) %>%
  pull(data)


# 6. Readmissions (28 days) ----
# _________________________________________________________________________

readmissions <- arrow::read_parquet(paste0(ext_year_dir, "/readmissions_smr.parquet")) %>%
  filter(financial_year <= max_fy)

# Plotting by age
readmissions_age <- readmissions %>%
  filter(hscp_locality == LOCALITY) %>%
  drop_na(age_group) %>%
  group_by(financial_year, age_group) %>%
  summarise(
    read_28 = sum(read_28),
    discharges = sum(discharges)
  ) %>%
  ungroup() %>%
  mutate(data = round_half_up(read_28 / discharges * 1000, 1))

Read_age_ts <- age_group_trend_usc(
  data_for_plot = readmissions_age,
  plot_title = paste("Readmission rate (28 days) per 1,000 discharges by age group\n for", LOCALITY),
  yaxis_title = "Readmission rate (28 days)\n per 1,000 discharges",
  source = "Source: PHS SMR01"
)


# Plotting by area

# First use aggregating function on readmissions, then on discharges, then join

read1 <- readmissions %>%
  select(-discharges) %>%
  rename(n = read_28) %>%
  aggregate_usc_area_data() %>%
  rename(read_28 = n)

read2 <- readmissions %>%
  select(-read_28) %>%
  rename(n = discharges) %>%
  aggregate_usc_area_data() %>%
  rename(discharges = n)

readmissions_areas <- left_join(read1, read2) %>%
  left_join(pop_areas_all_ages) %>%
  mutate(data = round_half_up(read_28 / discharges * 1000, 1))

rm(read1, read2)

Read_loc_ts <- area_trend_usc(
  data_for_plot = readmissions_areas,
  plot_title = paste("Readmission rate (28 days) per 1,000 discharges over time by residence"),
  yaxis_title = "Readmission rate (28 days)\n per 1,000 discharges",
  source = "Source: PHS SMR01"
)

# Objects for text and summary table
latest_read_loc <- readmissions_areas %>%
  filter(
    location == LOCALITY,
    year == max(year)
  ) %>%
  pull(data)

scot_read <- readmissions_areas %>%
  filter(
    location == "Scotland",
    year == max(year)
  ) %>%
  pull(data)


# 7. Comm 6 months ----
# _________________________________________________________________________________
#
# comm_6mo <- readRDS(paste0(import_folder, "comm_6mo_smr.rds")) %>%
#   rename(financial_year = financial_death) %>%
#   filter(financial_year <= max_fy)
#
# # Aggregate data
# # First use aggregating function on bed days, then on deaths, then join
#
# comm_6mo1 <- comm_6mo %>%
#   select(-total_deaths) %>%
#   rename(n = total_bddys) %>%
#   aggregate_usc_area_data() %>%
#   rename(total_bddys = n)
#
# comm_6mo2 <- comm_6mo %>%
#   select(-total_bddys) %>%
#   rename(n = total_deaths) %>%
#   aggregate_usc_area_data() %>%
#   rename(total_deaths = n)
#
# comm_6mo_areas <- left_join(comm_6mo1, comm_6mo2) %>%
#   left_join(pop_areas_all_ages) %>%
#   mutate(data = round_half_up(100*(1 - total_bddys/total_deaths/182.5), 1))
#
# #Locality Table
#
# comm_6mo_loc_table <- comm_6mo_areas %>%
#   filter(location == LOCALITY) %>%
#   mutate(data = paste0(data, "%"),
#          financial_year = paste0("**", financial_year, "**"),
#          financial_year = fct_reorder(financial_year, year)) %>%
#   select(location, financial_year, data) %>%
#   pivot_wider(names_from = financial_year, values_from = data) %>%
#   select(-location)
#
# # Bar plot comparing areas in latest year
#
# comm_loc_bar <- comm_6mo_areas %>%
#   filter(year == max(year)) %>%
#   mutate(location = fct_reorder(as.factor(str_wrap(location, 28)), as.numeric(area_type))) %>%
#
#   ggplot(aes(x = location, y = data, fill = location, weight = data)) +
#   geom_col(position = position_dodge()) +
#   geom_text(aes(y = data, label = round_half_up(data, 1)),
#             position=position_dodge(width=0.9),
#             vjust=-0.25, color = "#4a4a4a", size = 4, fontface = "bold") +
#   scale_y_continuous(labels = comma, limits = c(0, 1.1*max(comm_6mo_areas$data))) +
#   scale_fill_manual(values = palette) +
#   theme_profiles() +
#   theme(panel.grid.major.y = element_line(color = "grey85"),
#         panel.grid.major.x = element_blank(),
#         axis.title.x = element_blank(),
#         axis.text.x = element_text(size = 10)) +
#   labs(y = "% Last 6 months in community",
#        caption = "Source: NRS Death Records, PHS SMR01, SMR01E and SMR04") +
#   guides(fill= "none")
#
#
# # Objects for text and summary table
# latest_comm_6mo_loc <- comm_6mo_areas %>%
#   filter(location == LOCALITY,
#          year == max(year)) %>%
#   pull(data)
#
# hscp_comm_6mo <- comm_6mo_areas %>%
#   filter(location == HSCP,
#          year == max(year)) %>%
#   pull(data)
#
# scot_comm_6mo <- comm_6mo_areas %>%
#   filter(location == "Scotland",
#          year == max(year)) %>%
#   pull(data)
#
# other_loc_comm_6mo <- comm_6mo %>%
#   group_by(financial_year, hscp_locality) %>%
#   summarise(total_bddys = sum(total_bddys),
#             total_deaths = sum(total_deaths)) %>%
#   ungroup() %>%
#   right_join(pops_other_locs) %>%
#   mutate(total_bddys = replace_na(total_bddys, 0),
#          total_deaths = replace_na(total_deaths, 0)) %>%
#   mutate(data = as.character(round_half_up(100*(1 - total_bddys/total_deaths/182.5), 1))) %>%
#   select(hscp_locality, data) %>%
#   spread(hscp_locality, data)



# 8. Potentially Preventable Admissions ----
# _______________________________________________________________________________________________________

ppa <- arrow::read_parquet(paste0(ext_year_dir, "/ppa_smr.parquet")) %>%
  filter(financial_year <= max_fy)

# % PPAs in locality under and over 65
ppa_total <- ppa %>%
  rename(n = admissions) %>%
  aggregate_usc_area_data()

ppa_65plus <- ppa %>%
  filter(age_group %in% c("65 - 74", "75+")) %>%
  rename(n = admissions) %>%
  aggregate_usc_area_data() %>%
  rename(plus65tot = n) %>%
  left_join(ppa_total, by = c("financial_year", "location")) %>%
  left_join(pop_areas_all_ages) %>%
  mutate(data = round_half_up(plus65tot / n * 100, 1))

latest_ppa_65plus <- ppa_65plus %>%
  filter(
    location == LOCALITY,
    year == max(year)
  ) %>%
  pull(data)

ppa_under65 <- ppa %>%
  filter(age_group %in% c("0 - 17", "18 - 44", "45 - 64")) %>%
  rename(n = admissions) %>%
  aggregate_usc_area_data() %>%
  rename(under65tot = n) %>%
  left_join(ppa_total, by = c("financial_year", "location")) %>%
  left_join(pop_areas_all_ages) %>%
  mutate(data = round_half_up(under65tot / n * 100, 1))

latest_ppa_under65 <- ppa_under65 %>%
  filter(
    location == LOCALITY,
    year == max(year)
  ) %>%
  pull(data)


# Plotting by area
ppa_areas <- ppa %>%
  rename(n = admissions) %>%
  aggregate_usc_area_data() %>%
  left_join(pop_areas_all_ages) %>%
  mutate(data = round_half_up(n / pop * 100000)) %>%
  mutate(location = factor(location, levels = c(LOCALITY, HSCP, HB, "Scotland"))) %>%
  arrange(location)


PPA_loc_ts <- area_trend_usc(
  data_for_plot = ppa_areas,
  plot_title = paste("Potentially Preventable Emergency Admissions per 100,000 by residence"),
  yaxis_title = "PPA rate\nper 100,000 population",
  source = "Source: PHS SMR01"
)


# Objects for text and summary table
latest_ppa_loc <- ppa_areas %>%
  filter(
    location == LOCALITY,
    year == max(year)
  ) %>%
  mutate(data = format(data, big.mark = ",")) %>%
  pull(data)

hscp_ppa <- ppa_areas %>%
  filter(
    location == HSCP,
    year == max(year)
  ) %>%
  mutate(data = format(data, big.mark = ",")) %>%
  pull(data)

scot_ppa <- ppa_areas %>%
  filter(
    location == "Scotland",
    year == max(year)
  ) %>%
  mutate(data = format(data, big.mark = ",")) %>%
  pull(data)

other_loc_ppa <- ppa %>%
  group_by(financial_year, hscp_locality) %>%
  summarise(admissions = sum(admissions)) %>%
  ungroup() %>%
  right_join(pops_other_locs) %>%
  mutate(admissions = replace_na(admissions, 0)) %>%
  mutate(data = round_half_up(admissions / pop * 100000)) %>%
  mutate(data = format(data, big.mark = ",")) %>%
  select(hscp_locality, data) %>%
  spread(hscp_locality, data)


# 9. Psychiatric hospital admissions (ScotPHO) ----
# ___________________________________________________________________________

psych_hosp <- read_csv(paste0(ext_year_dir, "/scotpho_data_extract_psychiatric_admissions.csv")) %>%
  clean_scotpho_dat() %>%
  mutate(period_short = gsub("to", "-", substr(period, 1, 18)))

check_missing_data_scotpho(psych_hosp)

## Create variables for latest year
latest_period_psych_hosp <- unique(filter(psych_hosp, year == max(psych_hosp$year))$period_short)

## Time trend
psych_hosp_time_trend <- psych_hosp %>%
  scotpho_time_trend(
    data = .,
    chart_title = "Psychiatric Patient Hospitalisations Time Trend",
    xaxis_title = "Financial Year Groups (3-year aggregates)",
    yaxis_title = "Psychiatric patient hospitalisations\n(Standardised rates per 100,000)",
    string_wrap = 10,
    rotate_xaxis = TRUE
  )



## Objects for text and summary table

# Locality latest value
psych_hosp_latest <- round_half_up(filter(
  psych_hosp,
  year == max(psych_hosp$year) &
    (area_name == LOCALITY & area_type == "Locality")
)$measure, 1)

other_locs_psych_hosp <- psych_hosp %>%
  filter(year == max(year)) %>%
  filter(area_type == "Locality") %>%
  rename("hscp_locality" = "area_name") %>%
  right_join(other_locs) %>%
  arrange(hscp_locality) %>%
  select(hscp_locality, measure) %>%
  mutate(measure = as.character(round_half_up(measure, 1))) %>%
  spread(hscp_locality, measure)

hscp_psych_hosp <- round_half_up(filter(psych_hosp, year == max(year) &
  (area_name == HSCP & area_type == "HSCP"))$measure, 1)

scot_psych_hosp <- round_half_up(filter(psych_hosp, year == max(year) & area_name == "Scotland")$measure, 1)



## Stat disclosure control

# writexl::write_xlsx(x = list("Emergency Adm" = emergency_adm,
#                              "Unsch Bed Days" = bed_days,
#                              "Unsch Bed Days (SMR4)" = bed_days_mh,
#                              "A&E Att" = ae_attendances,
#                              "Delayed Disch" = delayed_disch),
#                     path = paste0(lp_path, "Publishing/MSG Data.xlsx"))

# falls_sdc <- falls %>%
#   group_by(financial_year, hscp2019name, hscp_locality) %>%
#   summarise(falls_admission = sum(admissions)) %>%
#   ungroup()

# writexl::write_xlsx(x = list("Falls" = falls_sdc,
#                              "Readmissions 28" = readmissions,
#                              "PPA" = ppa),
#                     path = paste0(lp_path, "Publishing/SMR Data.xlsx"))
