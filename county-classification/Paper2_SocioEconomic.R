# ==============================================================================
# Purpose: Calc. socio-economic vars of Ribk et. al paper in Addiction journal - 
#          Effect modifiers section
# Author: Amirreza Sahebi
# Last modified: 08.19.2023
# Input: ACS (5-year estimate), GDL and CDC data in SocioEconomic folder 
# Output: HDI, Rey, unemeployment rate, poverty rate, income inequality
# ==============================================================================


# ==============================================================================
# ============================= Required libraries =============================
# ==============================================================================
library(readxl)
library(dplyr)
library(tidyr)
library(data.table)
library(ndi)
library(stringr)
library(tigris)
# ==============================================================================
# ============================== Read & Clean data =============================
# ==============================================================================
# General path of data
path <- "G:/My Drive/North Carolina State University/Project - Opioid 2/Data/SocioEconomic/"

# ===================================== #
# DATASET 1: Unemployment Rate from ACS #
# ===================================== #
# # Source: https://data.census.gov/table - search EMPLOYMENT STATUS (5-year estimate)
# # Function to process employment data
# process_file_emp <- function(year, path) {
#   tmp <- suppressWarnings(read_excel(paste0(path, "Employment", year, "ACS.xlsx"), sheet="Data"))
#   
#   county_cols <- seq(2, by=4, length.out=46)
#   county_names <- names(tmp)[county_cols]
#   county_names <- gsub(" County, South Carolina", "", county_names)
#   
#   # Extract unemployment rate columns 
#   var_cols <- county_cols + 3
#   var_data <- tmp[3, var_cols]
#   var_data <- as.numeric(gsub("%", "", var_data))
#   
#   df <- data.frame(County = county_names, 
#                    Year = year, 
#                    UnemploymentRate = var_data)
#   return(df)
#   rm(tmp, county_cols,county_names,var_cols, var_data)
# }
# years <- 2017:2021
# list_of_dataframes <- lapply(years, function(y) process_file_emp(y, path))
# df <- bind_rows(list_of_dataframes)
# df <- df %>% arrange(County, Year)
# rm(list_of_dataframes, years)

# Source: https://rfa.sc.gov/data-research/population-demographics/census-state-data-center/socioeconomic-data/unemployment-rates-by-county-2012-2021
# I found the above source more accurate comparing to ACS 5 year
tmp <- fread(paste0(path,"UnemploymentSCRFA.csv"), header = T)
df <- tmp[-1,c("County","2017":"2021")] %>% 
  pivot_longer(cols = "2017":"2021", names_to = "Year", values_to = "UnemploymentRate")
rm(tmp)

# ================================ #
# DATASET 2: Poverty Rate from ACS #
# ================================ #
# # Source: https://data.census.gov/table - search POVERTY STATUS (5-year estimate)
# # Function to process poverty data
# process_file_pvtr <- function(year, path) {
#   tmp <- suppressWarnings(read_excel(paste0(path, "Poverty", year, "ACS.xlsx"), sheet="Data"))
#   
#   county_cols <- seq(2, by=3, length.out=46)
#   county_names <- names(tmp)[county_cols]
#   county_names <- gsub(" County, South Carolina", "", county_names)
#   
#   # Extract unemployment rate columns 
#   var_cols <- county_cols + 2
#   var_data <- tmp[3, var_cols]
#   var_data <- as.numeric(gsub("%", "", var_data))
#   
#   df <- data.frame(County = county_names, 
#                    Year = year, 
#                    PovertyRate = var_data)
#   return(df)
#   rm(tmp, county_cols,county_names, var_cols, var_data)
# }
# years <- 2017:2021
# list_of_dataframes <- lapply(years, function(y) process_file_pvtr(y, path))
# tmp <- bind_rows(list_of_dataframes)
# tmp <- tmp %>% arrange(County, Year)
# df <- merge(df, tmp, by = c("County","Year"))
# rm(list_of_dataframes, years,tmp)

# Source: https://rfa.sc.gov/data-research/population-demographics/census-state-data-center/socioeconomic-data/Population-with-percent-in-poverty-by-county-2011-2020
# I found the above source more accurate comparing to ACS 5 year
tmp <- fread(paste0(path,"PovertyPercentageSCRFA.csv"), header = T)
df <- merge(df,
            tmp[-1,c("County","2017":"2021")] %>% 
              mutate(across(starts_with("20"), ~as.numeric(gsub("%", "", .)))) %>%
              pivot_longer(cols = "2017":"2021", names_to = "Year", values_to = "PovertyRate"), 
            by=c("County","Year"))
rm(tmp)


# =================================== #
# DATASET 3: Income inequality - Gini #
# =================================== #
# Source: https://data.census.gov/table - search GINI INDEX OF INCOME INEQUALITY	
# (5-year estimate)
# Function to process gini data
process_file_gini <- function(year, path) {
  tmp <- suppressWarnings(read_excel(paste0(path, "Gini", year, "ACS.xlsx"), sheet="Data"))
  counties <- tmp$Label[seq(1, nrow(tmp), 2)]
  values <- as.numeric(tmp$`Gini Index`[seq(2, nrow(tmp), 2)])
  df <- data.frame(
    County = gsub(" County, South Carolina", "", counties),
    Year = year,
    Gini = values)
  return(df)
  rm(tmp, counties,values)
}
years <- 2017:2021
list_of_dataframes <- lapply(years, function(y) process_file_gini(y, path))
tmp <- bind_rows(list_of_dataframes)
tmp <- tmp %>% arrange(County, Year)
df <- merge(df, tmp, by = c("County","Year"))
rm(list_of_dataframes, years,tmp)

# =============================== #
# DATASET 4: Rey Index Components #
# =============================== #

# ----------------------- #
# Median Household Income #
# ----------------------- #
# # Source: https://data.census.gov/table - search INCOME IN THE PAST 12 MONTHS
# # Function to process income data
# process_file_incm <- function(year, path) {
#   tmp <- suppressWarnings(read_excel(paste0(path, "Income", year, "ACS.xlsx"), sheet="Data"))
#   
#   county_cols <- seq(2, by=4, length.out=46)
#   county_names <- names(tmp)[county_cols]
#   county_names <- gsub(" County, South Carolina", "", county_names)
#   
#   # Extract unemployment rate columns 
#   var_cols <- county_cols
#   var_data <- tmp[14, var_cols]
#   var_data <- as.numeric(gsub(",", "", var_data))
#   
#   df <- data.frame(County = county_names, 
#                    Year = year, 
#                    IncomeMedian = var_data)
#   return(df)
#   rm(tmp, county_cols,county_names, var_cols, var_data)
# }
# years <- 2017:2021
# list_of_dataframes <- lapply(years, function(y) process_file_incm(y, path))
# tmp <- bind_rows(list_of_dataframes)
# tmp <- tmp %>% arrange(County, Year)
# df <- merge(df, tmp, by = c("County","Year"))
# rm(list_of_dataframes, years,tmp)
# Source: https://rfa.sc.gov/data-research/population-demographics/census-state-data-center/mhi-county-2011-2020
# I found the above source more accurate comparing to ACS 5 year
tmp <- fread(paste0(path,"MedianHouseholdIncomeSCRFA.csv"), header = T)
df <- merge(df,
            tmp[-1,c("County","2017":"2021")] %>% 
              mutate(across(starts_with("20"), ~as.numeric(gsub("[$,]", "", .)))) %>%
              pivot_longer(cols = "2017":"2021", names_to = "Year", values_to = "IncomeMedian"), 
            by=c("County","Year"))
rm(tmp)


# --------------------------- #
# Above High School Graduates #
# --------------------------- #
# Source: https://data.census.gov/table - search EDUCATIONAL ATTAINMENT	
# Function to process income data
process_file_edu <- function(year, path) {
  tmp <- suppressWarnings(read_excel(paste0(path, "Education", year, "ACS.xlsx"), sheet="Data"))
  
  county_cols <- seq(2, by=6, length.out=46)
  county_names <- names(tmp)[county_cols]
  county_names <- gsub(" County, South Carolina", "", county_names)
  
  # Extract unemployment rate columns 
  var_cols <- county_cols + 1
  var_data <- tmp[5, var_cols]
  var_data <- 100 - as.numeric(gsub("%", "", var_data))
  
  df <- data.frame(County = county_names, 
                   Year = year, 
                   AboveHighSchool = var_data)
  return(df)
  rm(tmp, county_cols,county_names, var_cols, var_data)
}
years <- 2017:2021
list_of_dataframes <- lapply(years, function(y) process_file_edu(y, path))
tmp <- bind_rows(list_of_dataframes)
tmp <- tmp %>% arrange(County, Year)
df <- merge(df, tmp, by = c("County","Year"))
rm(list_of_dataframes, years,tmp)


# ------------------ #
# Blue Collar Worker #
# ------------------ #
# Source: https://data.census.gov/table - search OCCUPATION BY SEX FOR THE 
# CIVILIAN EMPLOYED POPULATION 16 YEARS AND OVER	
# Blue Collars are: 
#   1- Building and grounds cleaning and maintenance occupations, row 26
#   2- Natural resources, construction, and maintenance occupations, row 31
#   3- Production, transportation, and material moving occupations, row 35
# Function to process occupation data
process_file_occu <- function(year, path) {
  tmp <- suppressWarnings(read_excel(paste0(path, "Occupation", year, "ACS.xlsx"), sheet="Data"))
  
  county_cols <- seq(2, by=5, length.out=46)
  county_names <- names(tmp)[county_cols]
  county_names <- gsub(" County, South Carolina", "", county_names)
  
  # Extract unemployment rate columns 
  var_cols <- county_cols
  var_data <- tmp[c(3,26,31,35), var_cols]
  var_data <- as.data.frame(apply(var_data, 2, function(x) as.numeric(gsub(",", "", x))))
  var_data <- colSums(var_data[2:4,]) / var_data[1,]*100
  var_data <- unname(unlist(as.list(var_data)))
  
  df <- data.frame(County = county_names, 
                   Year = year, 
                   BlueCollar = var_data)
  return(df)
  rm(tmp, county_cols,county_names, var_cols, var_data)
}
years <- 2017:2021
list_of_dataframes <- lapply(years, function(y) process_file_occu(y, path))
tmp <- bind_rows(list_of_dataframes)
tmp <- tmp %>% arrange(County, Year)
df <- merge(df, tmp, by = c("County","Year"))
rm(list_of_dataframes, years,tmp)


# ========================= #
# DATASET 5: HDI Components #
# ========================= #
# Source: https://ourworldindata.org/human-development-index

# --------------- #
# Life Expectancy #
# --------------- #
# Source: https://www.cdc.gov/nchs/data-visualization/life-expectancy/index.html
# This is based on data for years 2010-2015. So no year by year data
tmp <- fread(paste0(path,"LifeExpectency4HDI__CDC_USALEEP_20102015.csv"))
tmp <- tmp[tmp$State == "South Carolina",]
tmp$County <- sapply(strsplit(tmp$County, " "), `[`, 1)
tmp <- tmp[tmp$County != "(blank)",]
tmp <- tmp %>% group_by(County) %>% summarise(LifeExpectancy = mean(`Life Expectancy`,na.rm = T))
df <- merge(df, tmp, by = "County", all.x = T)

# --------------------------- #
# Expected years of schooling #
# --------------------------- #
# Source: https://globaldatalab.org/shdi/table/esch/USA/
# Data is available at state level. So no county difference
tmp <- fread(paste0(path,"ExpectedYearsSchooling4HDI__GDL.csv"),header = T)
tmp <- tmp[tmp$Region == "South Carolina",c("2017","2018","2019","2020","2021")]
tmp <- tmp %>% pivot_longer(
  cols = `2017`:`2021`, 
  names_to = "Year",
  values_to = "SchoolingExpectedYears"
)
df <- merge(df, tmp, by = "Year", all.x = T) %>% arrange(County,Year)


# ----------------------- #
# Mean years of schooling #
# ----------------------- #
# Source: https://globaldatalab.org/shdi/table/esch/USA/
# Data is available at state level. So no county difference
tmp <- fread(paste0(path,"MeanYearsSchooling4HDI__GDL.csv"),header = T)
tmp <- tmp[tmp$Region == "South Carolina",c("2017","2018","2019","2020","2021")]
tmp <- tmp %>% pivot_longer(
  cols = `2017`:`2021`, 
  names_to = "Year",
  values_to = "SchoolingMeanYears"
)
df <- merge(df, tmp, by = "Year", all.x = T) %>% arrange(County,Year)



# ============================================= #
# DATASET 6: Multidimensional Deprivation Index #
# ============================================= #
process_file_mdi <- function(year, counties, path) {
  tmp <- read_excel(paste0(path,"MDIACS.xls"), sheet = as.character(year), skip = 2)
  tmp$County <- str_pad(tmp$County, width = 5, side = "left", pad = "0")
  tmp <- tmp[tmp$County >= "45001" & tmp$County <= "45091", ]
  tmp <- na.omit(tmp)
  names(tmp)[1] <- "FIPS"
  tmp <- merge(tmp,counties, by = "FIPS")
  county_names <- tmp$County
  var_data <- tmp$`MDI rate`
  var_data <- as.numeric(var_data)
  df <- data.frame(County = county_names, 
                   Year = year, 
                   MDI = var_data)
  return(df)
  rm(tmp, county_cols,county_names, var_cols, var_data)
}
counties <- counties()
counties <- as.data.frame(counties)
counties$FIPS <- paste0(counties$STATEFP,counties$COUNTYFP)
counties <- counties[,c("FIPS","NAME")]
names(counties) <- c("FIPS","County")
years <- 2017:2019
list_of_dataframes <- lapply(years, function(y) process_file_mdi(y, counties, path))
tmp <- bind_rows(list_of_dataframes)
tmp <- tmp %>% arrange(County, Year)
df <- merge(df, tmp, by = c("County","Year"), all.x = T)
rm(list_of_dataframes, years,tmp)

# =================================== #
# DATASET 7: Social Deprivation Index #
# =================================== #
process_file_sdi <- function(year, counties, path) {
  tmp <- fread(paste0(path,"SDI",year,"RGC.csv"))
  names(tmp)[1] <- "FIPS"
  tmp$FIPS <- str_pad(tmp$FIPS, width = 5, side = "left", pad = "0")
  tmp <- tmp[tmp$FIPS >= "45001" & tmp$FIPS <= "45091", ]
  tmp <- tmp[,c("FIPS","SDI_score")]
  names(tmp) <- c("FIPS","SDI")
  tmp <- merge(tmp,counties, by = "FIPS")
  county_names <- tmp$County
  var_data <- tmp$SDI
  var_data <- as.numeric(var_data)
  df <- data.frame(County = county_names, 
                   Year = year, 
                   SDI = var_data)
  return(df)
  rm(tmp, county_cols,county_names, var_cols, var_data)
}
counties <- counties()
counties <- as.data.frame(counties)
counties$FIPS <- paste0(counties$STATEFP,counties$COUNTYFP)
counties <- counties[,c("FIPS","NAME")]
names(counties) <- c("FIPS","County")
years <- 2017:2019
list_of_dataframes <- lapply(years, function(y) process_file_sdi(y, counties, path))
tmp <- bind_rows(list_of_dataframes)
tmp <- tmp %>% arrange(County, Year)
df <- merge(df, tmp, by = c("County","Year"), all.x = T)
rm(list_of_dataframes, years,tmp,counties)

# ==============================================================================
# ================================ Process Data ================================
# ==============================================================================

# ------------------- #
# Calculate Rey Index #
# ------------------- #
# Source: https://bmcpublichealth.biomedcentral.com/articles/10.1186/1471-2458-9-33
ReyVars <- c("IncomeMedian", "AboveHighSchool", "BlueCollar", "UnemploymentRate")
ReyPCAresult <- prcomp(df[,ReyVars], scale. = TRUE, center = TRUE)
Reyloadings <- ReyPCAresult$rotation[, 1]
df$ReyIndex <- rowSums(scale(df[,ReyVars]) %*% matrix(Reyloadings))
rm(ReyVars,Reyloadings,ReyPCAresult)

# ---------------------- #
# Calculate NDI - Messer #
# ---------------------- #
# Source: https://link.springer.com/article/10.1007/s11524-006-9094-x
#         https://cran.r-project.org/web/packages/ndi/vignettes/vignette.html
NDImesser <- data.frame()
for (yr in 2017:2021){
  tmp <- messer(geo = "county", year = yr, state = "SC")
  tmp <- tmp$ndi
  tmp <- tmp[,c("county","NDI","NDIQuart")]
  tmp$county <- gsub(" County","",tmp$county)
  names(tmp) <- c("County","NDI_messer","NDIQuart_messer")
  tmp$Year <- yr
  NDImesser <- rbind(NDImesser,tmp)
}
df <- merge(df,NDImesser,by=c('County','Year'))
rm(NDImesser,yr)

# ---------------------------- #
# Calculate NDI - Powell-Wiley #
# ---------------------------- #
# Source: https://www.tandfonline.com/doi/full/10.1080/17445647.2020.1750066
#         https://www.sciencedirect.com/science/article/pii/S235234092200213X?via%3Dihub
#         https://cran.r-project.org/web/packages/ndi/vignettes/vignette.html
NDIpw <- data.frame()
for (yr in 2017:2021){
  tmp <- powell_wiley(geo = "county", year = yr, state = "SC")
  tmp <- tmp$ndi
  tmp <- tmp[,c("county","NDI","NDIQuint")]
  tmp$county <- gsub(" County","",tmp$county)
  names(tmp) <- c("County","NDI_pw","NDIQuint_pw")
  tmp$Year <- yr
  NDIpw <- rbind(NDIpw,tmp)
}
df <- merge(df,NDIpw,by=c('County','Year'))
rm(NDIpw,yr)


# ------------- #
# Calculate HDI #
# ------------- #
# Source: https://ourworldindata.org/human-development-index
#         https://data.census.gov/table?q=Income&g=010XX00US$0500000
#         https://hdr.undp.org/data-center/human-development-index#/indicies/HDI
#         min and max of UNDP is used for life, edu but not income. Income is from ACS
#         min income 2017: 11680, 2018: 12812, 2019: 12441, 2020: 12283, 2021: 12856
#         max income 2017: 129588, 2018: 136268, 2019: 142299, 2020: 147111, 2021: 156821

# 1. Calculate the dimension indices
LifeExpectancyIndex <- (df$LifeExpectancy - 20) / (85 - 20)
ExpectedSchoolingIndex <- (df$SchoolingExpectedYears - 0) / (18 - 0)
MeanSchoolingIndex <- (df$SchoolingMeanYears - 0) / (15 - 0)
income_min <- 11680
income_max <- 156821
IncomeIndex <- (df$IncomeMedian - income_min) / (income_max - income_min)

# 2. Aggregating the dimension indices to get the HDI
EducationIndex <- (ExpectedSchoolingIndex + MeanSchoolingIndex) / 2
# HDI (geometric mean of the three indices: Life Expectancy, Education, and Income)
df$HDI <- (LifeExpectancyIndex * EducationIndex * IncomeIndex)^(1/3)



# Write final data
fwrite(df,paste0(path,"SocioEconomicCleaned.csv"))
