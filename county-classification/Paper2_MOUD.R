# ==============================================================================
# Purpose: Calc. availability of medications for opioid use disorder in counties 
# Author: Amirreza Sahebi
# Last modified: 06.04.2023
# Input: OTP, BPL, NPL, / All mental and substance use disorders location in SC 
# Output: Count of MOUDs
# ==============================================================================




# ================ Required libraries ================
library(data.table)
library(stringr)
library(zipcodeR)
library(dplyr)
library(sf)
library(purrr)
library(units)
library(geosphere)

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

# DATASET 1: MOUDs location based on Haffajee et al. paper source
# Acronyms are derived based on https://findtreatment.gov/locator service code
SOTP <- fread(paste0(path,"MOUD/Haffajee_28OpioidTreatmentProgram.csv"))
SOTP$FullCertification <- as.Date(SOTP$FullCertification, "%m/%d/%Y")
SOTP <- SOTP[SOTP$FullCertification <= as.Date("12/31/2021","%m/%d/%Y"),]
BU <- fread(paste0(path,"MOUD/Haffajee_45BuprenorphinePractitionerLocator.csv"))
BU <- subset(BU, state == "SC")
BU$zipCode <- sub('(?<=\\-).*$', '', BU$zipCode, perl=TRUE)
BU$zipCode <- str_replace(BU$zipCode,"-","")
NU <- fread(paste0(path,"MOUD/Haffajee_46NaltrexonePractitionerLocator.csv"))

# DATASET 2: MOUDs based on https://findtreatment.gov/locator
# Suggested abbreviation: Mental Heath treatment (MH) / Substance Use Treatment (SA) - (MHSA)
# MHSA <- fread(paste0(path,"TreatmentFacilityLocator_MentalAndSubatanceUseDisorder.csv"))

# Extract SC Counties
SCzips <- as.data.frame(search_state("SC")[,c("zipcode","county")])
names(SCzips) <- c("Zipcode","County")
SCzips$County <- str_replace(SCzips$County," County","")

# Read shapefile data
dfShapeCounty <- st_read(paste0(path,"Map/tl_2019_us_county.shp"))
dfShapeCounty <- st_transform(dfShapeCounty, 2263)
dfShapeCounty_SC <- dfShapeCounty[dfShapeCounty$STATEFP == "45", ]

dfShapeZip <- st_read(paste0(path,"Map/tl_2019_us_zcta510.shp"))
dfShapeZip <- st_transform(dfShapeZip, 2263)
dfShapeZip_SC <- st_filter(dfShapeZip, dfShapeCounty_SC)

# ==============================================================================
# ============================== Pre-process data ==============================
# ==============================================================================

# ============== #
# MOUD Dataset 1 #
# ============== #
MOUDzip <- SCzips
zip_coords <- dfShapeZip_SC %>%
  select(Zipcode = ZCTA5CE10, Latitude = INTPTLAT10, Longitude = INTPTLON10) %>%
  mutate(across(c(Latitude, Longitude), as.numeric)) # Ensure they are numeric
names(zip_coords)[2:3] <- c("Lat","Long")
zip_coords <- as.data.frame(zip_coords)
zip_coords <- zip_coords[,1:3]
MOUDzip <- merge(MOUDzip, zip_coords, by = "Zipcode", all.x = T)

tmp <- as.data.frame(table(BU$zipCode))
names(tmp) <- c("Zipcode","BU")
tmp <- merge(tmp, BU[,c("zipCode","latitude","longitude")], by.x = "Zipcode", by.y= "zipCode")
names(tmp)[3:4] <- c("Lat","Long")
tmp <- tmp[,c("Zipcode","Lat","Long","BU")]
MOUDzip <- merge(MOUDzip, tmp, by = "Zipcode", all.x = TRUE, suffixes = c("", "_BU"))
MOUDzip$Lat <- ifelse(is.na(MOUDzip$Lat), MOUDzip$Lat_BU, MOUDzip$Lat)
MOUDzip$Long <- ifelse(is.na(MOUDzip$Long), MOUDzip$Long_BU, MOUDzip$Long)
MOUDzip <- MOUDzip[, !names(MOUDzip) %in% c("Lat_BU", "Long_BU")]



SOTP$Zipcode <- as.character(SOTP$Zipcode)
tmp <- as.data.frame(table(SOTP$Zipcode))
names(tmp) <- c("Zipcode","SOTP")
MOUDzip <- merge(MOUDzip,tmp, by = "Zipcode", all.x = T)



NU$ZIPCODE <- as.character(NU$ZIPCODE)
tmp <- as.data.frame(table(NU$ZIPCODE))
names(tmp) <- c("Zipcode","NU")
MOUDzip <- merge(MOUDzip,tmp, by = "Zipcode", all.x = T)

MOUDzip <- subset(MOUDzip, !is.na(Lat))
MOUDzip[is.na(MOUDzip)] <- 0

MOUDzip <- MOUDzip %>% distinct(Zipcode, .keep_all = TRUE)

fwrite(MOUDzip[,-c(3:4)],paste0(path,"MOUD/MOUDCount_SCzips_Haffajee.csv"))


# ============== #
# MOUD Dataset 2 #
# ============== #
# centroids <- st_centroid(dfShapeCounty_SC)
# MOUDsf <- st_as_sf(MOUDzip, coords = c("Long", "Lat"), crs = 4326)
# 
# MOUDsf <- st_transform(MOUDsf, 32617) # Example UTM Zone 17N
# centroids <- st_transform(centroids, 32617)
# 
# mile_in_meters <- set_units(30 * 1609.34, "m") # 30 miles in meters
# 
# # Initialize columns
# centroids$SOTP_sum <- 0
# centroids$NU_sum <- 0
# centroids$BU_sum <- 0
# 
# # Loop through each centroid
# for(i in 1:nrow(centroids)) {
#   
#   # Get the centroid point
#   centroid_point <- centroids[i, ]
#   
#   # Find MOUD locations within a 30-mile radius
#   distances <- st_distance(centroid_point, MOUDsf)
#   
#   # Identify which points of MOUDsf are within a 30-mile radius
#   nearby_indices <- which(distances < mile_in_meters)
#   
#   
#   
#   # Sum the SOTP, NU, BU and assign them to the centroid
#   centroids$SOTP_sum[i] <- sum(MOUDsf[nearby_indices, ]$SOTP, na.rm = TRUE)
#   centroids$NU_sum[i] <- sum(MOUDsf[nearby_indices, ]$NU, na.rm = TRUE)
#   centroids$BU_sum[i] <- sum(MOUDsf[nearby_indices, ]$BU, na.rm = TRUE)
# }
# centroids <- centroids[,c("NAMELSAD","SOTP_sum","NU_sum","BU_sum")]
# centroids$NAMELSAD <- str_replace(centroids$NAMELSAD,"County","")
# centroids <- as.data.frame(centroids)
# centroids <- centroids[,1:4]
# names(centroids) <- c("County","SOTP","NU","BU")
# 




radius <- 30 * 1609.34
# Initialize columns
centroids$SOTP_sum <- 0
centroids$NU_sum <- 0
centroids$BU_sum <- 0
for(i in 1:nrow(dfShapeCounty_SC)){
  
  centroid_lat <- as.numeric(dfShapeCounty_SC$INTPTLAT[i])
  centroid_long <- as.numeric(dfShapeCounty_SC$INTPTLON[i])
  
  tmp <- MOUDzip %>% 
    rowwise() %>% 
    mutate(distance = distHaversine(c(centroid_long, centroid_lat), c(Long, Lat))) %>% 
    ungroup()
  
  # Filtering the rows that are within the radius and summarizing the values
  summary <- tmp %>% 
    filter(distance <= radius) %>% 
    summarize(SOTP_sum = sum(SOTP, na.rm = TRUE), 
              NU_sum = sum(NU, na.rm = TRUE), 
              BU_sum = sum(BU, na.rm = TRUE))
  
  # Assigning the summarized values back to the original dataframe
  dfShapeCounty_SC$SOTP_sum[i] <- summary$SOTP_sum
  dfShapeCounty_SC$NU_sum[i] <- summary$NU_sum
  dfShapeCounty_SC$BU_sum[i] <- summary$BU_sum
}

# Cleaning the Dataframe for Final Output
centroids <- dfShapeCounty_SC %>% 
  select(NAMELSAD, SOTP_sum, NU_sum, BU_sum) %>% 
  mutate(NAMELSAD = str_replace(NAMELSAD, "County", ""))
centroids <- centroids[,c("NAMELSAD","SOTP_sum","NU_sum","BU_sum")]
centroids$NAMELSAD <- str_replace(centroids$NAMELSAD,"County","")
centroids <- as.data.frame(centroids)
centroids <- centroids[,1:4]
names(centroids) <- c("County","SOTP","NU","BU")

fwrite(centroids,paste0(path,"MOUD/MOUDCount_SCcounty_30mile.csv"))
