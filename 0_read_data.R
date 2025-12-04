#===============================================================================
# Script to read soluble sugar concentrations from the Proctor Maple Research 
# Center Long-term Study
#-------------------------------------------------------------------------------

# Load dependencies ----
if(!existsFunction("read_excel")) library("readxl")
if(!existsFunction("%>%")) library("tidyverse")

# Name of data file with the sugar concentrations ----
file_name <- "../../Data/LTS-Sugar_concentrations.xlsx"

# Get sample dates from sheet names ----
dates <- excel_sheets(file_name) [-c(1:2)]

# Loop over sample dates to read data for each date ----
for (d in dates){
  
  # Read date-specific data ----
  tmp <- read_excel(
    path = file_name, 
    sheet = d, 
    range = "A9:H102",
    na = "NA",
    col_names = c("tn", "t", "tree", "ssc1", "ssc2", "ssc3", "ssc", "comments"),
    col_types = c("numeric", "text", "text", rep("numeric", 4), "text")
    ) %>% add_column (date = as_date(d))
  
  # Concatenate data ----
  if (d == dates[1]) {
    SS <- tmp
  } else {
    SS <- rbind (SS, tmp)
  }
} # end date loop

# Clean-up workspace ----
rm(tmp)

# Correct dates for a few trees (see comments of data file for reasons) ----
SS$date[SS$date == as_date("2025-03-05") & SS$tn == 3] <- 
  as_date("2025-03-06")
SS$date[SS$date == as_date("2025-03-19") & SS$tn == 3] <- 
  as_date("2025-03-20")
trees <- c("871", "873", "874","877", "879", "880", "883", "884", "886", "887", 
           "888", "AA", "AB", "AC", "AD", "AE", "AF", "AG", "AH")
SS$date[SS$date == as_date("2025-04-03") & SS$tree %in% trees ] <- 
  as_date("2025-04-04")

# Get the file name for the growth data 
file_name <- "../../Data/LTS-Sugar_yield.xlsx"

# Read mean annual sap sugar concentration data ----
# N.B.: Does not contain data for sugar control trees!
tmp <- read_excel(
    path = file_name, 
    sheet = "Tree Growth and Yield", 
    range = "A2:CQ91",
    na = "NA",
    col_names = c("t", "t_name", "o_ID", "ID", "i_DBH", "CPC", "CPS", "TH", "LC", 
                  "MCD", "CD", "CV", "ii_DBH", paste0("dbh_", as.character(2014:2025)),
                  "dg_in", "dg_%", "rg_in", "rg_%", "BAI_ft2", "BAI_%","EM1", "notes21",
                  "i_NAMP", "vigor", "CPS_18", "CPC_18", "LCR_18", "TBM_18", "TBMM_18", 
                  "FT_18", "TH_18", "LC_18", "MCD_18", "CD_18", "CV_18", "note", 
                  "CPC_24", "CPS_24","LCR_24", "TBM_24", "TBMM_24", "FT_24",
                  "EM2", paste0("tc_", as.character(2014:2017)), 
                  paste0("tc2_", as.character(2014:2017)), 
                  paste0("tc_", as.character(2022:2025)), "EM3", 
                  paste0("ssc_", as.character(2014:2025)),
                  "EM4", "EM5", "EM6", "EM7", 
                  paste0("sy_", as.character(2014:2025))),
    col_types = c("numeric", rep("text", 2), rep("numeric", 2), 
                  rep("text", 62), rep("numeric", 12), rep("text", 4), rep("numeric", 12)))

# Convert the mean-annual sap sugar concentration data into long format ----
mSS <- tmp %>% 
  select(-c(2:3, 6:67, 80:95)) %>% 
  pivot_longer(cols = 4:15, 
               names_to = "year", 
               names_prefix = "ssc_", 
               values_to = "ssc") %>%
  mutate(year = as.numeric(year))

# Convert the total annual syrup yield data into long format ----
sy <- tmp %>% 
  select(-c(2:3, 6:83)) %>% 
  pivot_longer(cols = 4:15, 
               names_to = "year", 
               names_prefix = "sy_", 
               values_to = "sy") %>%
  mutate(year = as.numeric(year))

# Calculate sap volume ----
data <- left_join(mSS, sy, by = c("t", "ID", "i_DBH", "year")) %>%
  mutate(vol_s = sy / ssc)
#===============================================================================