# ==============================================================================
# Purpose: Characteristics of counties
# Author: Amirreza Sahebi
# Last modified: 06.04.2023
# Input: ACS data for county characterstics 
# Output: Count of MOUDs
# ==============================================================================




# ================ Required libraries ================
library(readxl)

# library(data.table)
# library(stringr)
# library(zipcodeR)
# library(dplyr)
# library(lubridate)
# library(tidyr)
# library(zoo)
# library(scales)
# library(gridExtra)
# library(grid)
# library(plyr)
# library(tidycensus)

# ==============================================================================
# ================================== Read data =================================
# ==============================================================================
# General path of data
path <- "G:/My Drive/North Carolina State University/Project - Opioid 2/Data/"

# DATASET 1: ACS data sets
ACS2017DH <- read_excel(paste0(path,"Characterstics/ACS5_2017_DemographicHousing.xlsx"), sheet = "Data")






# DATASET 2: MOUDs based on https://findtreatment.gov/locator
# Suggested abbreviation: Mental Heath treatment (MH) / Substance Use Treatment (SA) - (MHSA)
MHSA <- fread(paste0(path,"TreatmentFacilityLocator_MentalAndSubatanceUseDisorder.csv"))

# Extract SC Counties
SCzips <- as.data.frame(search_state("SC")[,c("zipcode","county")])
names(SCzips) <- c("Zipcode","County")
SCzips$County <- str_replace(SCzips$County," County","")


# ==============================================================================
# ============================== Pre-process data ==============================
# ==============================================================================

# ============== #
# MOUD Dataset 1 #
# ============== #
MOUD_SC <- SCzips

SOTP$Zipcode <- as.character(SOTP$Zipcode)
tmp <- as.data.frame(table(SOTP$Zipcode))
names(tmp) <- c("Zipcode","SOTP")
MOUD_SC <- merge(MOUD_SC,tmp, by = "Zipcode", all.x = T)

tmp <- as.data.frame(table(BU$zipCode))
names(tmp) <- c("Zipcode","BU")
MOUD_SC <- merge(MOUD_SC,tmp, by = "Zipcode", all.x = T)

NU$ZIPCODE <- as.character(NU$ZIPCODE)
tmp <- as.data.frame(table(NU$ZIPCODE))
names(tmp) <- c("Zipcode","NU")
MOUD_SC <- merge(MOUD_SC,tmp, by = "Zipcode", all.x = T)

MOUD_SC[is.na(MOUD_SC)] <- 0
fwrite(MOUD_SC,paste0(path,"MOUD/MOUDCount_SCzips_Haffajee.csv"))


# ============== #
# MOUD Dataset 1 #
# ============== #
# Our research showed that only Facility Types OTPs and Buprenorphine Practitioners
# are related to our research. (https://findtreatment.gov/locator) 
# These are exactly the same ones mentioned by Haffajee et. al.
# So we may go ahead with dataset 1

