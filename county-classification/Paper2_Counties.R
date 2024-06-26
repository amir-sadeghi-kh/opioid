# ==============================================================================
# Purpose: Analyzing characteristics of counties with high overdose mortality
# Ref paper: Characteristics of US Counties With High Opioid Overdose Mortality
#            and Low Capacity to Deliver Medications for Opioid Use Disorder
# Author: Amirreza Sahebi
# Last modified: 08-17-2023
# Input: SCRIPT processed / MOUD_SCzips
# Output: 
# ==============================================================================




# ==============================================================================
# ============================  Required libraries =============================
# ==============================================================================
library(data.table)
library(dplyr)
library(bit64)
library(stringr)
library(tidyr)
library(ggplot2)
library(ggrepel)
library(ggpubr)
library(scales)
library(ggpattern)

# library(plyr)
# library(hrbrthemes)
# library(viridis)
# library(plotly)
# library(heatmaply)
# library(catmaply)
# library(multcompView)
# library(lubridate)
# library(zoo)
# library(scales)
# library(gridExtra)
# library(grid)
# library(png)
# 


# Function factory for secondary axis transforms
train_sec <- function(primary, secondary, na.rm = TRUE) {
  # Thanks Henry Holm for including the na.rm argument!
  from <- range(secondary, na.rm = na.rm)
  to   <- range(primary, na.rm = na.rm)
  # Forward transform for the data
  forward <- function(x) {
    rescale(x, from = from, to = to)
  }
  # Reverse transform for the secondary axis
  reverse <- function(x) {
    rescale(x, from = to, to = from)
  }
  list(fwd = forward, rev = reverse)
}

# ==============================================================================
# ============================== Read data & Clean =============================
# ==============================================================================
# General path of data
pathRead <- "/home/asahebi/Data/"
pathWrite <- "/home/asahebi/Opioid 2/Result/"


#===========#
# MOUD Data #
#===========#
MOUDzip <- fread(paste0(pathRead,"MOUDCount_SCzips_Haffajee.csv")) 
MOUDcounty <- MOUDzip %>% group_by(County) %>% summarise(SOTP = sum(SOTP),
                                                         NU = sum(NU),
                                                         BU = sum(BU))
# MOUDcounty$MOUD <- 10*MOUDcounty$SOTP + MOUDcounty$NU + MOUDcounty$BU
MOUDcounty$MOUD <- MOUDcounty$SOTP + MOUDcounty$NU + MOUDcounty$BU


#===================#
# Prescription Data #
#===================#
df <- fread(paste0(pathRead,"SCRIPTS_Processed.csv"))
df <- subset(df, !(df$Patient_State != "SC" & df$dispenser_state != "SC" & df$prescriber_state != "SC"))
df <- subset(df, Patient_State == "SC")

df$class <- str_pad(df$class, 2, pad = "0")
df$date <- as.Date(df$written_at, format = "%Y-%m-%d")
df <- df %>% drop_na(date)
# data is filtered to be compatible with overdose data
df <- subset(df, df$date < as.Date("2021-12-01") & df$date >= as.Date("2017-01-01"))
df <- as.data.frame(df)

# Add new classes
df$class2 <- "PaI_PrI_DiI"
df[which(df$Patient_State == "SC" & df$Prescriber_State == "SC" & df$Dispenser_State == "SC"),"class2"] <- "PaI_PrI_DiI"
df[which(df$Patient_State != "SC" & df$Prescriber_State == "SC" & df$Dispenser_State == "SC"),"class2"] <- "PaO_PrI_DiI"
df[which(df$Patient_State == "SC" & df$Prescriber_State != "SC" & df$Dispenser_State == "SC"),"class2"] <- "PaI_PrO_DiI"
df[which(df$Patient_State == "SC" & df$Prescriber_State == "SC" & df$Dispenser_State != "SC"),"class2"] <- "PaI_PrI_DiO"
df[which(df$Patient_State != "SC" & df$Prescriber_State != "SC" & df$Dispenser_State == "SC"),"class2"] <- "PaO_PrO_DiI"
df[which(df$Patient_State != "SC" & df$Prescriber_State == "SC" & df$Dispenser_State != "SC"),"class2"] <- "PaO_PrI_DiO"
df[which(df$Patient_State == "SC" & df$Prescriber_State != "SC" & df$Dispenser_State != "SC"),"class2"] <- "PaI_PrO_DiO"
df[which(df$Patient_State != "SC" & df$Prescriber_State != "SC" & df$Dispenser_State != "SC"),"class2"] <- "PaO_PrO_DiO"


df$DrugClass <- "Benzos"
df$DrugClass[df$drug_class != "Benzos"] <- "Opioid"
# https://www.cdc.gov/opioids/data/analysis-resources.html
df <- df %>% mutate(DrugProdoction = case_when(drug_class %in% c("Hydrocodone","Hydromorphone",
                                                                 "Oxycodone","Oxymorphone") ~ "SemiSyntheic",
                                               drug_class == "Morphine" ~ "Natural",
                                               drug_class %in% c("Fentanyl","Methadone",
                                                                 "Tapentadol") ~ "Syntheic",
                                               drug_class == "Benzos" ~ "Benzos"))

#===============#
# Overdose Data #
#===============#
dfOverdose <- fread(paste0(pathRead,"VSRR_Provisional_Drug_Overdose_Death_Counts.csv"))
dfOverdose <- subset(dfOverdose, State == "SC" & ((Year == 2021 & Month != "December") | (Year <= 2020)))

dfOverdoseCounty <- fread(paste0(pathRead,"VSRR_Provisional_County-Level_Drug_Overdose_Death_Counts_SC_DHEC.csv"))
dfOverdoseCounty[, 3:10] <- lapply(dfOverdoseCounty[, 3:10], as.integer)
dfOverdoseCounty <- dfOverdoseCounty[!is.na(dfOverdoseCounty$Year),]

#=================#
# Population Data #
#=================#
dfPop <- fread(paste0(pathRead,"sc-rfa-popestbycountyprojection.csv")) 
names(dfPop) <- as.character(dfPop[3,])
dfPop <- dfPop[-c(1:3),]

# Reshape the data to long format
dfPopTotal <- dfPop %>%
  pivot_longer(cols = starts_with("T"), 
               names_to = "Year", 
               values_to = "Total") %>%
  select(COUNTY,`MEDICAID AGE GROUP`,Year,Total) %>%
  mutate(Year = str_remove(Year, "^[A-Z]"))

dfPopMale <- dfPop %>%  
  pivot_longer(cols = grep("^M\\d{4}", names(dfPop), value = TRUE),
               names_to = "Year", 
               values_to = "Male") %>%
  select(COUNTY,`MEDICAID AGE GROUP`,Year,Male) %>%
  mutate(Year = str_remove(Year, "^[A-Z]"))

dfPopFemale <- dfPop %>%
  pivot_longer(cols = starts_with("T"), 
               names_to = "Year", 
               values_to = "Female") %>%
  select(COUNTY,`MEDICAID AGE GROUP`,Year,Female) %>%
  mutate(Year = str_remove(Year, "^[A-Z]"))

dfPop <- merge(dfPopTotal, merge(dfPopMale, dfPopFemale, by = c("COUNTY","MEDICAID AGE GROUP","Year")), by = c("COUNTY","MEDICAID AGE GROUP","Year")) %>%
  mutate(Male = as.numeric(gsub(",", "", Male)),
         Female = as.numeric(gsub(",", "", Female)),
         Total = as.numeric(gsub(",", "", Total)),
         Year = as.integer(Year))
rm(dfPopFemale)
rm(dfPopMale)
rm(dfPopTotal)

names(dfPop) <- c("County","AgeGroup","Year","T","M","F")
dfPop$County <- str_to_title(dfPop$County)
dfPop$AgeGroup <- str_replace_all(dfPop$AgeGroup, " TO ", "-")
dfPop$AgeGroup <- str_replace_all(dfPop$AgeGroup, "ALL AGES", "")

# Pivot the data to wide format
dfPop <- dfPop %>%
  pivot_wider(
    id_cols = c("County", "Year"),
    names_from = "AgeGroup",
    values_from = c("T", "M", "F"),
    names_prefix = "",
    names_sep = ""
  )

# Update name of Mccormick county
dfPop$County[dfPop$County == "Mccormick"] <- "McCormick"

#====================================#
# AHRF Data - County Characteristics #
#====================================#
AHRF <- readRDS(paste0(pathRead,"Cleaned AHRF 06-05-23 DIB.RDS"))
AHRFnames <- c(
  "f00002", # FIPS Code
  "f04437", # County + State abbreviation
  "f12424", # State name abbreviation
  "f00010", # County name
  "f09721", # Land area 
  "f00020", # Rurality code
  "f14067", # CBSA Indicator Code / 0 = Not, 1 = Metro, 2 = Micro
  "f14587", # % employment in manufacturing
  "f09787", # HPSA code
  "f04530", # 2010 Census population
  "f08921", # Hospital Beds
  "f13214", # home health agencies
  "f13220", # Hospices
  "f11984", # population estimate
  "f15549", # total medicare enrollment, later years
  "f13254", # total medicare enrollment, early years
  "f13906", # Total males
  "f13907", # Total females
  "f13920", # Total hispanic males
  "f13921", # Total hispanic females
  "f13915", # Asian females
  "f13914", # Asian males
  "f13910", # Black males
  "f13911", # Black females
  "f13326", # Census asian pop
  "f04536", # Census hisp pop
  "f04532", # Census black pop
  "f13483", # Median age
  "f06712", # Pop male 20-24
  "f06713", # Pop female 20-24
  "f06714", # Pop male 25-29
  "f06715", # Pop female 25-29
  "f06716", # Pop male 30-34
  "f06717", # Pop female 30-34
  "f06718", # Pop male 35-44
  "f06719", # Pop female 35-44
  "f06720", # Pop male 45-54
  "f06721", # Pop female 45-54
  "f06722", # Pop male 55-59
  "f06723", # Pop female 55-59
  "f06724", # Pop male 60-64
  "f06725", # Pop female 60-64
  "f06726", # Pop male 65-74
  "f06727", # Pop female 65-74
  "f11640", # Pop male 75-84
  "f11641", # Pop female 75-84
  "f11642", # Pop male >84
  "f11643", # Pop female >84
  "f14642", # NPs with NPI
  "f14482", # % aged 25+ w/ 4+ years college
  "f14450", # % aged 25+ w.o. high school diploma
  "f14451", # % aged 25+ w. high school diploma
  "f09781", # per capita income
  "f13321", # % in poverty
  "f06795", # % unemployed
  "f11396", # Veteran population
  "f12558", # Total deaths
  "f14751", # % <65 w/o health insurance, pre-2014
  "f15474", # % <65 w/o health insurance, 2014+
  "f13226", # Median household income
  "f15297", # Actual per capita Medicare cost
  "f13874", # Total area in square miles
  "f12424", # State name abbreviation
  "f04538", # % Black
  "f04542", # % Hispanic
  "f14206", # Dual eligible
  "f09787", #  HPSA Code - Primary Care | shortage in 1=Whole, 2=Part County        
  "f12492", #  HPSA Code - Mental Health 
  "f04904", # Total MDs <35
  "f04905", # Total MDs 35-44
  "f04906", # Total MDs 45-54
  "f04907", # Total MDs 55-64
  "f12016", # Total MDs 65-74
  "f12017", # Total MDs 75+
  "f04916", # Total Specs <35
  "f04917", # Total Specs 35-44
  "f04918", # Total Specs 45-54
  "f04919", # Total Specs 55-64
  "f12034", # Total Specs 65-74
  "f12035", # Total Specs 75+
  "f04820", # Total MDs male
  "f04821", # Total MDs female
  "f10498", # Dentists <35
  "f11318", # Dentists 35-44
  "f11319", # Dentists 45-54
  "f13176", # Dentists 55-64
  "f10505", # Dentists 65+
  "f08922", # Short-term general hospital beds
  "f08923", # Long-term non-general hospital beds
  "f08924", # Long-term hospital beds
  "f14045", # Nursing home beds
  "f13191", # Eligible for Medicare
  "f14196", # Eligible for Medicaid
  "f13908", # White males
  "f13909", # White females
  "f09545", # Total inpatient days (all hospitals + nursing homes)
  "f09566", # OP visits ST gen hosps
  "f09567", # OP visits ST non-gen hosps
  "f09568", # OP visits LT hosps
  "f09571"  # OP visits VA hosps
)

# Define the labels for each column
AHRFlables <- c(
  "FIPS Code",
  "County + State abbreviation",
  "State name abbreviation",
  "County name",
  "Land area",
  "Rurality code",
  "CBSA indicator code",
  "Percentage employment in manufacturing",
  "HPSA code",
  "2010 Census population",
  "Hospital Beds",
  "Home health agencies",
  "Hospices",
  "Population estimate",
  "Total Medicare enrollment (later years)",
  "Total Medicare enrollment (early years)",
  "Total males",
  "Total females",
  "Total Hispanic males",
  "Total Hispanic females",
  "Asian females",
  "Asian males",
  "Black males",
  "Black females",
  "Census Asian population",
  "Census Hispanic population",
  "Census Black population",
  "Median age",
  "Population male 20-24",
  "Population female 20-24",
  "Population male 25-29",
  "Population female 25-29",
  "Population male 30-34",
  "Population female 30-34",
  "Population male 35-44",
  "Population female 35-44",
  "Population male 45-54",
  "Population female 45-54",
  "Population male 55-59",
  "Population female 55-59",
  "Population male 60-64",
  "Population female 60-64",
  "Population male 65-74",
  "Population female 65-74",
  "Population male 75-84",
  "Population female 75-84",
  "Population male >84",
  "Population female >84",
  "NPs with NPI",
  "Percentage aged 25+ with 4+ years college",
  "Percentage aged 25+ w.o. high school diploma",
  "Percentage aged 25+ w. high school diploma",
  "Per capita income",
  "Percentage in poverty",
  "Percentage unemployed",
  "Veteran population",
  "Total deaths",
  "Percentage <65 without health insurance (pre-2014)",
  "Percentage <65 without health insurance (2014+)",
  "Median household income",
  "Actual per capita Medicare cost",
  "Total area in square miles",
  "State name abbreviation",
  "Percentage Black",
  "Percentage Hispanic",
  "Dual eligible",
  "HPSA Code - Primary Care",
  "HPSA Code - Mental Health",
  "Total MDs <35",
  "Total MDs 35-44",
  "Total MDs 45-54",
  "Total MDs 55-64",
  "Total MDs 65-74",
  "Total MDs 75+",
  "Total Specs <35",
  "Total Specs 35-44",
  "Total Specs 45-54",
  "Total Specs 55-64",
  "Total Specs 65-74",
  "Total Specs 75+",
  "Total MDs male",
  "Total MDs female",
  "Dentists <35",
  "Dentists 35-44",
  "Dentists 45-54",
  "Dentists 55-64",
  "Dentists 65+",
  "Short-term general hospital beds",
  "Long-term non-general hospital beds",
  "Long-term hospital beds",
  "Nursing home beds",
  "Eligible for Medicare",
  "Eligible for Medicaid",
  "White males",
  "White females",
  "Total inpatient days (all hospitals + nursing homes)",
  "OP visits ST gen hosps",
  "OP visits ST non-gen hosps",
  "OP visits LT hosps",
  "OP visits VA hosps"
)

AHRFdict <- setNames(AHRFlables, AHRFnames)





# ==============================================================================
# ================================== Data Prep =================================
# ==============================================================================


#====================#
# COunty Risk Status #
#====================#

CountyRisk <- merge(MOUDcounty,dfPop[dfPop$Year == 2023,c("County","T")],by = "County", all.x = T)
CountyRisk[,2:5] <- round(CountyRisk[,2:5] / CountyRisk[,6] * 1E5,3)

tmp <- dfOverdoseCounty
tmp <- merge(tmp, dfPop[,c(1,2,7)], by.x = c("County","Year"), by.y = c("County","Year"), all.x = T)
divide_by_divisor <- function(column, divisor) {
  return(column / divisor*1e5)
}
tmp[,3:10] <- as.data.frame(lapply(tmp[, 3:10], divide_by_divisor, divisor = tmp$T))
tmp <- tmp %>% group_by(County) %>% summarise(across(3:10, \(x) sum(x, na.rm = TRUE)))
CountyRisk <- merge(CountyRisk,tmp[,c("County","Opioids","Fentanyl")],by = "County")
rm(tmp)

column_averages <- colMeans(CountyRisk[, c("SOTP", "NU", "BU", "MOUD", "Opioids","Fentanyl")], na.rm = T)



# County Risk Stats
round(apply(CountyRisk[,c(2:8)], 2, mean, na.rm = T),2)
# SOTP        NU        BU      MOUD         T   Opioids  Fentanyl 
# 0.35      0.60      9.35     10.30 114612.20     87.22     61.84
round(apply(CountyRisk[,c(2:7)], 2, sd, na.rm = T),2)
# SOTP       NU       BU      MOUD         T   Opioids 
# 0.64      1.00      7.22      7.54 134505.30     43.16


# Create a new column with "High" or "Low" values based on deviation from averages for each metric
CountyRisk$Metric <- paste(
  ifelse(CountyRisk$SOTP > column_averages["SOTP"], "H", "L"),
  ifelse(CountyRisk$NU > column_averages["NU"], "H", "L"),
  ifelse(CountyRisk$BU > column_averages["BU"], "H", "L"),
  ifelse(CountyRisk$Opioids > column_averages["Opioids"], "H", "L"),
  sep = ""
)

# Create a new column based on conditions from the "Metric" column for all MOUDs
CountyRisk$MetricMOUD <- paste0(ifelse((substr(CountyRisk$Metric, 1, 3) == "HHH" | 
                                          substr(CountyRisk$Metric, 1, 3) == "HLH" | 
                                          substr(CountyRisk$Metric, 1, 3) == "LHH" | 
                                          substr(CountyRisk$Metric, 1, 3) == "HHL"), "H", "L"),
                                substr(CountyRisk$Metric,4,4))

# MOUD
CountyRisk$ColorMOUD <- "lightblue"
CountyRisk$ColorMOUD <- ifelse(substr(CountyRisk$MetricMOUD, 1, 1) == "L" & 
                                 substr(CountyRisk$MetricMOUD, 2, 2) == "L", "darkgreen",
                               CountyRisk$ColorMOUD)
CountyRisk$ColorMOUD <- ifelse(substr(CountyRisk$MetricMOUD, 1, 1) == "H" & 
                                 substr(CountyRisk$MetricMOUD, 2, 2) == "H", "yellow",
                               CountyRisk$ColorMOUD)
CountyRisk$ColorMOUD <- ifelse(substr(CountyRisk$MetricMOUD, 1, 1) == "L" & 
                                 substr(CountyRisk$MetricMOUD, 2, 2) == "H", "red",
                               CountyRisk$ColorMOUD)


# SOTP
CountyRisk$ColorSOTP <- "lightblue"
CountyRisk$ColorSOTP <- ifelse(substr(CountyRisk$Metric, 1, 1) == "L" & 
                                 substr(CountyRisk$Metric, 4, 4) == "L", "darkgreen",
                               CountyRisk$ColorSOTP)
CountyRisk$ColorSOTP <- ifelse(substr(CountyRisk$Metric, 1, 1) == "H" & 
                                 substr(CountyRisk$Metric, 4, 4) == "H", "yellow",
                               CountyRisk$ColorSOTP)
CountyRisk$ColorSOTP <- ifelse(substr(CountyRisk$Metric, 1, 1) == "L" & 
                                 substr(CountyRisk$Metric, 4, 4) == "H", "red",
                               CountyRisk$ColorSOTP)
# NU
CountyRisk$ColorNU <- "lightblue"
CountyRisk$ColorNU <- ifelse(substr(CountyRisk$Metric, 2, 2) == "L" & 
                               substr(CountyRisk$Metric, 4, 4) == "L", "darkgreen",
                             CountyRisk$ColorNU)
CountyRisk$ColorNU <- ifelse(substr(CountyRisk$Metric, 2, 2) == "H" & 
                               substr(CountyRisk$Metric, 4, 4) == "H", "yellow",
                             CountyRisk$ColorNU)
CountyRisk$ColorNU <- ifelse(substr(CountyRisk$Metric, 2, 2) == "L" & 
                               substr(CountyRisk$Metric, 4, 4) == "H", "red",
                             CountyRisk$ColorNU)

# BU
CountyRisk$ColorBU <- "lightblue"
CountyRisk$ColorBU <- ifelse(substr(CountyRisk$Metric, 3, 3) == "L" & 
                               substr(CountyRisk$Metric, 4, 4) == "L", "darkgreen",
                             CountyRisk$ColorBU)
CountyRisk$ColorBU <- ifelse(substr(CountyRisk$Metric, 3, 3) == "H" & 
                               substr(CountyRisk$Metric, 4, 4) == "H", "yellow",
                             CountyRisk$ColorBU)
CountyRisk$ColorBU <- ifelse(substr(CountyRisk$Metric, 3, 3) == "L" & 
                               substr(CountyRisk$Metric, 4, 4) == "H", "red",
                             CountyRisk$ColorBU)


# merge county data with map data
map_data <- map_data("county", "south carolina")
map_data$subregion <- str_to_title(map_data$subregion)
map_data$subregion[map_data$subregion == "Mccormick"] <- "McCormick"
CountyRisk <- left_join(map_data, CountyRisk, by = c("subregion" = "County"))
textdata <- map_data %>% group_by(subregion) %>% summarise(lat = mean(lat), long = mean(long))


# create maps
mapall <- ggplot(CountyRisk) + 
  geom_polygon(aes(x = long, y = lat, group = group, fill = ColorMOUD, color = "black")) + 
  geom_label(aes(x = long, y = lat, label = subregion), data = textdata, size = 3, color = "black", vjust = 0.5, hjust = 0.5, show.legend = F) +
  scale_fill_manual(name = "Risk Status",
                    values = c("lightblue" = "#84DFFF", "darkgreen" = "#C2F784", "yellow" = "#FBFF00", "red" = "#DF2E38"),
                    labels = c("lightblue" = "High MOUD / Low Overdose", "darkgreen" = "Low MOUD / Low Overdose", "yellow" = "High MOUD / High Overdose", "red" = "Low MOUD / High Overdose"),
                    breaks = c("lightblue", "darkgreen", "yellow", "red"),
                    na.value = "gray") +
  scale_color_manual(values = c("black" = "black")) +
  guides(color = FALSE) +
  theme_void() + 
  # ggtitle("Per capita MOUD") + 
  theme(legend.title = element_text(family = "mono",size = 12, face = "bold"),
        legend.position="right",
        legend.text = element_text(family = "mono",size = 12),
        plot.title = element_text(family = "mono",size = 12,face = "bold", hjust = 0.5))
ggsave(paste0(pathWrite,"MOUD_County.jpg"), plot = mapall, width = 14, height = 8, bg="white")

mapall <- ggplot(CountyRisk) + 
  geom_polygon(aes(x = long, y = lat, group = group, fill = ColorSOTP, color = "black")) + 
  geom_label(aes(x = long, y = lat, label = subregion), data = textdata, size = 3, color = "black", vjust = 0.5, hjust = 0.5) +
  scale_fill_manual(name = "Risk Status",
                    values = c("lightblue" = "#84DFFF", "darkgreen" = "#C2F784", "yellow" = "#FBFF00", "red" = "#DF2E38"),
                    labels = c("lightblue" = "High OTP / Low Overdose", "darkgreen" = "Low OTP / Low Overdose", "yellow" = "High OTP / High Overdose", "red" = "Low OTP / High Overdose"),
                    breaks = c("lightblue", "darkgreen", "yellow", "red"),
                    na.value = "gray") +
  scale_color_manual(values = c("black" = "black")) +
  guides(color = FALSE) +
  theme_void() + 
  theme(legend.title = element_text(family = "mono",size = 12, face = "bold"),
        legend.position="right",
        legend.text = element_text(family = "mono",size = 12),
        plot.title = element_text(family = "mono",size = 12,face = "bold", hjust = 0.5))
ggsave(paste0(pathWrite,"SOTP_County.jpg"), plot = mapall, width = 14, height = 8, bg="white")


mapall <- ggplot(CountyRisk) + 
  geom_polygon(aes(x = long, y = lat, group = group, fill = ColorBU, color = "black")) + 
  geom_label(aes(x = long, y = lat, label = subregion), data = textdata, size = 3, color = "black", vjust = 0.5, hjust = 0.5) +
  scale_fill_manual(name = "Risk Status",
                    values = c("lightblue" = "#84DFFF", "darkgreen" = "#C2F784", "yellow" = "#FBFF00", "red" = "#DF2E38"),
                    labels = c("lightblue" = "High BU / Low Overdose", "darkgreen" = "Low BU / Low Overdose", "yellow" = "High BU / High Overdose", "red" = "Low BU / High Overdose"),
                    breaks = c("lightblue", "darkgreen", "yellow", "red"),
                    na.value = "gray") +
  scale_color_manual(values = c("black" = "black")) +
  guides(color = FALSE) +
  theme_void() + 
  theme(legend.title = element_text(family = "mono",size = 12, face = "bold"),
        legend.position="right",
        legend.text = element_text(family = "mono",size = 12),
        plot.title = element_text(family = "mono",size = 12,face = "bold", hjust = 0.5))
ggsave(paste0(pathWrite,"BU_County.jpg"), plot = mapall, width = 14, height = 8, bg="white")


mapall <- ggplot(CountyRisk) + 
  geom_polygon(aes(x = long, y = lat, group = group, fill = ColorNU, color = "black")) + 
  geom_label(aes(x = long, y = lat, label = subregion), data = textdata, size = 3, color = "black", vjust = 0.5, hjust = 0.5) +
  scale_fill_manual(name = "Risk Status",
                    values = c("lightblue" = "#84DFFF", "darkgreen" = "#C2F784", "yellow" = "#FBFF00", "red" = "#DF2E38"),
                    labels = c("lightblue" = "High NU / Low Overdose", "darkgreen" = "Low NU / Low Overdose", "yellow" = "High NU / High Overdose", "red" = "Low NU / High Overdose"),
                    breaks = c("lightblue", "darkgreen", "yellow", "red"),
                    na.value = "gray") +
  scale_color_manual(values = c("black" = "black")) +
  guides(color = FALSE) +
  theme_void() + 
  theme(legend.title = element_text(family = "mono",size = 12, face = "bold"),
        legend.position="right",
        legend.text = element_text(family = "mono",size = 12),
        plot.title = element_text(family = "mono",size = 12,face = "bold", hjust = 0.5))
ggsave(paste0(pathWrite,"NU_County.jpg"), plot = mapall, width = 14, height = 8, bg="white")



# Assign high/low risk counties
CountyRisk$highlow <- ifelse(CountyRisk$MetricMOUD == "LH","HighRisk","LowRisk")
HighRiskCounties <- unique(CountyRisk$subregion[CountyRisk$highlow == "HighRisk"])


#=================#
# Doctor Shopping #
#=================#
dfDoctorShopping <- df %>% drop_na(consolidation_identifier) %>% group_by(consolidation_identifier,DrugClass, year(date)) %>% 
  summarize(PrescriberCount = n_distinct(prescriber_dea_number),
            DispenserCount = n_distinct(dispenser_dea_number)) 
dfDoctorShopping$patientIDyeardrug <-  paste0(dfDoctorShopping$consolidation_identifier," - ",dfDoctorShopping$`year(date)`, " - ", dfDoctorShopping$DrugClass)
DcotorShoppingList <- list(unique(dfDoctorShopping$patientIDyeardrug[which(dfDoctorShopping$PrescriberCount >= 5 & dfDoctorShopping$DispenserCount >= 5)]))
df$DoctorShopping <- 0
df$patientIDyeardrug <- paste0(df$consolidation_identifier," - ",year(df$date), " - ", df$DrugClass)
df$DoctorShopping[df$patientIDyeardrug %in% DcotorShoppingList[[1]]] <- 1


#=================================#
# Merge SCRIPTS with Risk and Pop #
#=================================#
df$CountyRisk <- "Low Risk"
df$CountyRisk[df$Patient_County %in% HighRiskCounties] <- "High Risk"
df$CountyRisk <- factor(df$CountyRisk, levels = c("Low Risk","High Risk"))
df$Covid <- "Pre-Covid"
# Firs covid month in SC
df$Covid[df$date >= as.Date("2020-03-01")] <- "Post-Covid"
df$Year <- year(df$date)
df$Month <- month(df$date)
df$Month_Year <- format(df$date, "%B %Y")
df$Month_Year  <- as.Date(paste("01", df$Month_Year, sep = "-"), format = "%d-%B %Y")

df <- merge(df, dfPop[,c(1,2,7)], by.x = c("Patient_County","Year"), by.y = c("County","Year"), all.x = T)
divide_by_divisor <- function(column, divisor) {
  return(column / divisor*1e5)
}
df <- df %>% mutate(MMEperCapita = divide_by_divisor(total_prescription_mme,T),
                    QuantityperCapita = divide_by_divisor(quantity,T))
df$dispenser_drug_class <- paste0(df$dispenser_class, " - ",df$drug_class)


#================#
# Rename Columns #
#================#
rename_dict <- list(
  # General fields
  "id" = "ID",
  "date" = "Date",
  "V1" = "V1",
  "consolidation_identifier" = "Patient_ID",
  "class" = "Class_Distance_Player",
  "class2" = "Class_In_Out_State",
  "DoctorShopping" = "Doctor_Shopping",
  "patientIDyeardrug" = "Patient_ID_Year_Drug",
  "CountyRisk" = "County_Risk",
  "Covid" = "COVID",
  "Month" = "Month",
  "Month_Year" = "Month_Year",
  "T" = "T",
  "MMEperCapita" = "MME_Per_Capita",
  "QuantityperCapita" = "Quantity_Per_Capita",
  
  # Drug related fields
  "ndc_code" = "NDC_Code",
  "drug_name" = "Drug_Name",
  "drug_family" = "Drug_Family",
  "drug_class" = "Drug_Subclass",
  "DrugClass" = "Drug_Category",
  "DrugProdoction" = "Drug_Production_Type",
  
  # Prescription info
  "filled_at" = "Filled_At",
  "quantity" = "Quantity",
  "days_supply" = "Days_Supply",
  "new_or_refill" = "New_Or_Refill",
  "prescription_number" = "Prescription_Number",
  "diagnosis_code" = "Diagnosis_Code",
  "payment_type" = "Payment_Type",
  "authorized_refill_count" = "Authorized_Refill_Count",
  "total_prescription_mme" = "Total_Prescription_MME",
  
  # Prescriber fields
  "prescriber_last_name" = "Prescriber_Last_Name",
  "prescriber_first_name" = "Prescriber_First_Name",
  "prescriber_address_one" = "Prescriber_Address_One",
  "prescriber_address_two" = "Prescriber_Address_Two",
  "prescriber_city" = "Prescriber_City",
  "prescriber_state" = "Prescriber_State",
  "prescriber_zip" = "Prescriber_Zip",
  "prescriber_dea_number" = "Prescriber_DEA_Number",
  "written_at" = "Written_At",
  "prescriber_national_provider_id" = "Prescriber_NPI",
  "prescriber_LatLong" = "Prescriber_LatLong",
  "prescriber_Lat" = "Prescriber_Lat",
  "prescriber_Long" = "Prescriber_Long",
  "prescriber_type" = "Prescriber_Type",
  "prescriber_taxonomy" = "Prescriber_Taxonomy",
  "prescriber_group" = "Prescriber_Group",
  "prescriber_class" = "Prescriber_Class",
  "prescriber_specialty" = "Prescriber_Specialty",
  "prescriber_section" = "Prescriber_Section",
  "prescriber_national_provider_id_int" = "Prescriber_NPI_Int",
  
  # Dispenser fields
  "dispenser_dea_number" = "Dispenser_DEA_Number",
  "dispenser_name" = "Dispenser_Name",
  "dispenser_address_one" = "Dispenser_Address_One",
  "dispenser_address_two" = "Dispenser_Address_Two",
  "dispenser_city" = "Dispenser_City",
  "dispenser_state" = "Dispenser_State",
  "dispenser_postal_code" = "Dispenser_Postal_Code",
  "dispenser_LatLong" = "Dispenser_LatLong",
  "dispenser_Lat" = "Dispenser_Lat",
  "dispenser_Long" = "Dispenser_Long",
  "dispenser_type" = "Dispenser_Type",
  "dispenser_class" = "Dispenser_Class",
  "dispenser_family" = "Dispenser_Family",
  "dispenser_drug_class" = "Dispenser_Drug_Class",
  
  # Patient fields
  "patient_zip_code" = "Patient_Zip_Code",
  "patient_birth_year" = "Patient_Birth_Year",
  "patient_LatLong" = "Patient_LatLong",
  "patient_Lat" = "Patient_Lat",
  "patient_Long" = "Patient_Long",
  "patient_prescriber_distance" = "Patient_Prescriber_Distance",
  "patient_dispenser_distance" = "Patient_Dispenser_Distance",
  
  # Distance and angles
  "dispenser_prescriber_distance" = "Dispenser_Prescriber_Distance",
  "patient_angle" = "Patient_Angle",
  "dispenser_angle" = "Dispenser_Angle",
  "prescriber_angle" = "Prescriber_Angle",
  "distance_class" = "Distance_Class",
  "player_class" = "Player_Class",
  
  # Policy and regulations
  "policy_dispenser_mandate" = "Policy_Dispenser_Mandate",
  "policy_prescriber_mandate" = "Policy_Prescriber_Mandate",
  "policy_prescriber_limit" = "Policy_Prescriber_Limit",
  
  # Other fields
  "PatientMMEZip" = "Patient_MME_Zip",
  "PatientRecordZip" = "Patient_Record_Zip",
  "DispenserMMEZip" = "Dispenser_MME_Zip",
  "DispenserRecordZip" = "Dispenser_Record_Zip",
  "PrescriberMMEZip" = "Prescriber_MME_Zip",
  "PrescriberRecordZip" = "Prescriber_Record_Zip",
  "MME/Day" = "MME_Per_Day",
  
  # Location related
  "Patient_County" = "Patient_County",
  "Year" = "Year",
  "Prescriber_County" = "Prescriber_County",
  "Dispenser_County" = "Dispenser_County",
  # "Prescriber_State" = "Prescriber_State",
  # "Dispenser_State" = "Dispenser_State"
  "Patient_State" = "Patient_State"
)


rename_dict <- modifyList(rename_dict, rename_dict_drug_info)
reversed_dict <- setNames(names(rename_dict), rename_dict)
df <- df %>% select(-Dispenser_State, -Prescriber_State) %>% rename(!!!reversed_dict)


# ==============================================================================
# ============================= Statistical Analysis ===========================
# ==============================================================================

# ======= #
# Overall #
# ======= #

#--------------------------------------------------#
# Summary info. on opioids vs. benzos prescription #
#--------------------------------------------------#
df %>% group_by(Year,Drug_Category) %>%
  summarise(Count = n() / sum(unique(T), na.rm = T)*1e5,
            MME = sum(Total_Prescription_MME,na.rm = T)/ sum(unique(T), na.rm = T)*1e5,
            Quantity = sum(Quantity,na.rm = T)/ sum(unique(T))*1e5)
# # A tibble: 10 × 5
# # Groups:   Year [5]
# Year DrugClass  Count       MME Quantity
# <dbl> <chr>      <dbl>     <dbl>    <dbl>
#   1  2017 Benzos    12856.  1546870.  848713.
# 2  2017 Opioid    64224. 64749401. 4473908.
# 3  2018 Benzos    10351.  1232801.  688638.
# 4  2018 Opioid    54779. 52979664. 3694682.
# 5  2019 Benzos     8987.   988453.  551697.
# 6  2019 Opioid    50514. 45462580. 3244296.
# 7  2020 Benzos     8367.   903822.  506761.
# 8  2020 Opioid    47687. 42033091. 3025912.
# 9  2021 Benzos     6214.   643654.  364013.
# 10  2021 Opioid    41214. 34672870. 2529018.

print(paste0("Total number of prescriptions between 2017 to 2021 in SC is: ", nrow(subset(df, Year >= 2017))))
# "Total number of prescriptions between 2017 to 2021 in SC is: 15591685"
print(paste0("Total number of opioid prescription between 2017 to 2021 in SC is: ", round(nrow(df[df$Drug_Category == "Opioid" & df$Year >= 2017,]))))
# "Total number of opioid prescription is: 13203802"


#----------------------------------------------------#
# Summary info. on opioid prescription by drug class #
#----------------------------------------------------#

prescriptionbydrug <- df %>% group_by(Year,Drug_Subclass) %>%
  summarise(Count = round(n()/sum(unique(T))*1e5),2) %>%
  pivot_wider(
    id_cols = c("Drug_Subclass"),
    names_from = "Year",
    values_from = c("Count"),
    names_prefix = "",
    names_sep = "")
prescriptionbydrug$Change <- round((prescriptionbydrug$`2021` - prescriptionbydrug$`2017`)/prescriptionbydrug$`2017`*100,2)
fwrite(prescriptionbydrug,paste0(pathWrite,"PrescriptionRateDrug.csv"))


# ============================================= #
# Overdose Characteristic of High Risk Counties #
# ============================================= #
drug_columns <- c("Opioids", "Fentanyl", "Psychostimulants", "Methadone", "Heroin",  "Cocaine")
tmp <- dfOverdoseCounty %>% mutate(Covid = ifelse(Year > 2019, "Post-Covid", "Pre-Covid"))
tmp <- tmp %>% select(c("Year","County","Covid",drug_columns))
tmp <- merge(tmp, dfPop[,c(1,2,7)], by.x = c("County","Year"), by.y = c("County","Year"), all.x = T)
divide_by_divisor <- function(column, divisor) {
  return(column / divisor*1e5)
}
tmp[,4:9] <- as.data.frame(lapply(tmp[, 4:9], divide_by_divisor, divisor = tmp$T))
# Reshape the dataframe to long format for easier plotting
tmp <- pivot_longer(tmp, cols = drug_columns, names_to = "Drug", values_to = "Count")
tmp$RiskStatus <- "Low Risk"
tmp$RiskStatus[tmp$County %in% HighRiskCounties] <- "High Risk"
tmp$Drug <- factor(tmp$Drug, levels = drug_columns)
tmp$Covid <- factor(tmp$Covid, levels = c("Pre-Covid","Post-Covid"))

# Create the line chart
fig <- ggplot(tmp, aes(x = Year, y = Count, color = RiskStatus)) +
  geom_line(stat = "summary", fun = "mean", linewidth = 1) +
  geom_point(stat = "summary", fun = "mean", shape = 21, size = 3, fill = "white") +
  facet_wrap(~ Drug, scales = "free") +
  scale_color_manual(values = c("High Risk" = "red", "Low Risk" = "blue")) +
  labs(x = "Year", y = "Per Capita Overdose", fill = "Risk Status") +
  theme_bw() +
  ggtitle("Per Capita Opioid Overdose in SC Counties (2017-2021)") +
  theme(strip.text.x = element_text(size = 8, color = "black", face = "bold",family = "mono"),
        strip.text.y = element_text(size = 8, color = "black", face = "bold",family = "mono"),
        legend.title = element_text(family = "mono",size = 10),
        legend.position="bottom",
        legend.text = element_text(family = "mono",size = 10),
        axis.title.y  = element_text(family = "mono",size = 12, face = "bold"),
        axis.title.y.right = element_text(family = "mono",size = 12, face = "bold"),
        axis.title.x  = element_text(family = "mono",size = 12, face = "bold", hjust = 0.5),
        axis.text.x   = element_text(family = "mono",size = 10, face = "bold"),
        axis.text.y   = element_text(family = "mono",size = 10, face = "bold"),
        plot.title = element_text(family = "mono",size = 12,face = "bold", hjust = 0.5))
ggsave(paste0(pathWrite,"OverdoseCountyRiskYearly.png"), plot = fig, width = 14, height = 8, bg="white")

# Create the bar chart
fig <- ggplot(tmp, aes(x = factor(Drug, level = drug_columns), y = Count, fill = RiskStatus)) +
  geom_boxplot() +
  facet_wrap(~ Covid) +
  labs(x = "Underlying Death Reason", y = "Per Capita Overdose", fill = "RiskStatus") +
  scale_fill_manual(values = c("High Risk" = "red", "Low Risk" = "blue")) +
  theme_bw() +
  theme(strip.text.x = element_text(size = 8, color = "black", face = "bold",family = "mono"),
        strip.text.y = element_text(size = 8, color = "black", face = "bold",family = "mono"),
        legend.title = element_text(family = "mono",size = 10),
        legend.position="bottom",
        legend.text = element_text(family = "mono",size = 10),
        axis.title.y  = element_text(family = "mono",size = 12, face = "bold"),
        axis.title.y.right = element_text(family = "mono",size = 12, face = "bold"),
        axis.title.x  = element_text(family = "mono",size = 12, face = "bold", hjust = 0.5),
        axis.text.x   = element_text(family = "mono",size = 10, face = "bold"),
        axis.text.y   = element_text(family = "mono",size = 10, face = "bold"),
        plot.title = element_text(family = "mono",size = 12,face = "bold", hjust = 0.5))
ggsave(paste0(pathWrite,"CovidvsOverdosevsvsCountyRisk.png"), plot = fig, width = 14, height = 8, bg="white")
rm(tmp)

# ------------------------ #
# Covid impact on overdose #
# ------------------------ #
tmp <- dfOverdoseCounty %>% mutate(Covid = ifelse(Year > 2019, "Post-Covid", "Pre-Covid"))
tmp <- merge(tmp, dfPop[,c(1,2,7)], by.x = c("County","Year"), by.y = c("County","Year"), all.x = T)
divide_by_divisor <- function(column, divisor) {
  return(column / divisor*1e5)
}
tmp[,3:10] <- as.data.frame(lapply(tmp[, 3:10], divide_by_divisor, divisor = tmp$T))
tmp <- tmp %>% group_by(County,Covid) %>% summarise(across(3:9, ~sum(., na.rm = TRUE)), .groups = 'drop')
columns_to_divide <- c("Prescription Drugs", "Opioids", "Psychostimulants", "Fentanyl",
                       "Heroin", "Methadone", "Cocaine" )
for (col in columns_to_divide) {
  tmp[[paste0(col)]] <- ifelse(tmp$Covid == "Post-Covid", tmp[[col]] / 2, tmp[[col]] / 3)
}
tmp$CountyRisk <- "Low Risk"
tmp$CountyRisk[tmp$County %in% HighRiskCounties] <- "High Risk"
tmp$Covid <- factor(tmp$Covid, levels = c("Pre-Covid","Post-Covid"))
tmp$CountyRisk <- factor(tmp$CountyRisk, levels = c("Low Risk","High Risk"))

t_test_result <- t.test(tmp$Opioids[tmp$Covid == "Post-Covid"], tmp$Opioids[tmp$Covid == "Pre-Covid"], paired = TRUE)
# Paired t-test
# 
# data:  tmp$Opioids[tmp$Covid == "Post-Covid"] and tmp$Opioids[tmp$Covid == "Pre-Covid"]
# t = 11.36, df = 45, p-value = 8.246e-15
# alternative hypothesis: true mean difference is not equal to 0
# 95 percent confidence interval:
#   12.21560 17.48043
# sample estimates:
#   mean difference 
# 14.84802 



# Plot the boxplot
fig <- ggplot(tmp, aes(x = as.character(Covid), y = Opioids)) +
  geom_boxplot(fill = "gray") +
  facet_wrap(~ CountyRisk) +
  xlab("Period") +
  ylab("Overdose") +
  theme_bw() +
  ggtitle("Per Capita Opioid Overdose in SC Counties (2017-2021)") +
  theme(strip.text.x = element_text(size = 8, color = "black", face = "bold",family = "mono"),
        strip.text.y = element_text(size = 8, color = "black", face = "bold",family = "mono"),
        legend.title = element_text(family = "mono",size = 10),
        legend.position="bottom",
        legend.text = element_text(family = "mono",size = 10),
        axis.title.y  = element_text(family = "mono",size = 12, face = "bold"),
        axis.title.y.right = element_text(family = "mono",size = 12, face = "bold"),
        axis.title.x  = element_text(family = "mono",size = 12, face = "bold", hjust = 0.5),
        axis.text.x   = element_text(family = "mono",size = 10, face = "bold"),
        axis.text.y   = element_text(family = "mono",size = 10, face = "bold"),
        plot.title = element_text(family = "mono",size = 12,face = "bold", hjust = 0.5))
ggsave(paste0(pathWrite,"CovidvsOverdosevsRisk.png"), plot = fig, width = 12, height = 8, bg="white")


# Calculate the differences in overdose rates from pre-COVID to post-COVID for high-risk counties
diff_high <- tmp[which(tmp$CountyRisk == "High Risk" & tmp$Covid == "Post-Covid"), "Opioids"] - 
  tmp[which(tmp$CountyRisk == "High Risk" & tmp$Covid == "Pre-Covid"), "Opioids"]

# Calculate the differences in overdose rates from pre-COVID to post-COVID for low-risk counties
diff_low <- tmp[which(tmp$CountyRisk == "Low Risk" & tmp$Covid == "Post-Covid"), "Opioids"] - 
  tmp[which(tmp$CountyRisk == "Low Risk" & tmp$Covid == "Pre-Covid"), "Opioids"]

# Perform a t-test to compare the means of the differences
t_test_result <- t.test(diff_high, diff_low, alternative = "greater", paired = FALSE)
# Welch Two Sample t-test
# 
# data:  diff_high and diff_low
# t = 2.9409, df = 13.152, p-value = 0.005676
# alternative hypothesis: true difference in means is greater than 0
# 95 percent confidence interval:
#   3.643405      Inf
# sample estimates:
#   mean of x mean of y 
# 22.00599  12.85969 

# ======================================================== #
# Prescription Supply Characteristic of High Risk Counties #
# ======================================================== #


# ----------------------------------------------- #
# High Risk vs. Low Risk per capita prescription  #
# ----------------------------------------------- #
subdf <- df[df[,paste0(lens,"_State")] == "SC",] %>% 
  mutate(Risk = ifelse(.data[[paste0(lens,"_County")]] %in% HighRiskCounties,"High Risk","Low Risk")) %>%
  group_by(.data[[paste0(lens,"_County")]],Risk,.data[[time[1]]],.data[[time[2]]]) %>% 
  mutate(Date = as.Date(paste(.data[[time[1]]],.data[[time[2]]], "01", sep = "-"), format = "%Y-%m-%d")) %>%
  summarise(N = n(),
            Quantity = sum(Quantity, na.rm = T),
            MME = sum(Total_Prescription_MME, na.rm = T),
            T = unique(T)) %>% ungroup()
subdf <- subdf %>%   group_by(.data[[paste0(lens,"_County")]],.data[[time[1]]],.data[[time[2]]]) %>% 
  mutate(Date = as.Date(paste(.data[[time[1]]],.data[[time[2]]], "01", sep = "-"), format = "%Y-%m-%d"))
subdf$PrescriptionperCapita <- subdf$N / subdf$T*1e5
subdf$MMEperCapita <- subdf$MME / subdf$T*1e5
subdf$QuantityperCapita <- subdf$Quantity / subdf$T*1e5
avg_subdf <- subdf %>%
  group_by(Risk, Date) %>%
  summarize(AvgPrescription = mean(PrescriptionperCapita), .groups = 'drop')
subdf <- subdf %>%
  left_join(avg_subdf, by = c("Risk", "Date"))
subdf <- as.data.frame(subdf)

# plot
fig <- ggplot(subdf, aes(x = Date)) +
  geom_line(aes(y = PrescriptionperCapita, group = Patient_County), color = "gray",alpha = 0.5) +
  geom_line(aes(y = AvgPrescription), color = "black", linewidth = 1) +
  facet_wrap(~Risk) +
  labs(x = "Date", y = "Prescription per Capita",
       title = "Monthly Prescription per Capita by Risk Status") +
  theme_bw() +
  ggtitle(paste0("Monthly Prescription per Capita by Risk Status")) + 
  theme(strip.text.x = element_text(size = 10, color = "black", face = "bold",family = "mono"),
        strip.text.y = element_text(size = 8, color = "black", face = "bold",family = "mono"),
        legend.title = element_blank(),
        legend.position="bottom",
        legend.text = element_blank(),
        axis.title.y  = element_text(family = "mono",size = 12, face = "bold"),
        axis.title.y.right = element_text(family = "mono",size = 12, face = "bold"),
        axis.title.x  = element_text(family = "mono",size = 12, face = "bold", hjust = 0.5),
        axis.text.x   = element_text(family = "mono",size = 10, face = "bold"),
        axis.text.y   = element_text(family = "mono",size = 10, face = "bold"),
        plot.title = element_text(family = "mono",size = 12,face = "bold", hjust = 0.5))
ggsave(paste0(pathWrite,"PerCapitaPrescription_Monthly.png"), plot = fig, width = 14, height = 8, bg="white")



# --------------------------------------------------------------------------------------------- #
# High Risk vs. Low Risk per capita prescription according to  Location, Drug and Pharmacy type #
# --------------------------------------------------------------------------------------------- #
## Location of players - specifically in-state vs out-of-state ##
## Drug type - Specifically Fentanyl, Morphine, Hydrocodone and Oxycodone ##
## Pharmacy type - Specifically Retail, Chain and Mail Order ##

# Choose from Location, Drug, Pharmacy, PharmacyDrug
grouplabel = "PharmacyDrug"
if (grouplabel == "Location"){
  group = "Class_In_Out_State"
  GroupLevels = c("PaI_PrI_DiI","PaI_PrO_DiI","PaI_PrI_DiO","PaI_PrO_DiO")
  GroupLabels = c("PaI_PrI_DiI" = "Patients, Prescribers and Dispensers In State",
                  "PaI_PrO_DiI" = "Only Prescribers Out of State",
                  "PaI_PrI_DiO" = "Only Dispensers Out of State",
                  "PaI_PrO_DiO" = "Prescribers and Dispensers Out of State")
} else if (grouplabel == "Drug"){
  group = "Drug_Subclass"
  GroupLevels = c("Hydrocodone","Oxycodone","Fentanyl")
  GroupLabels = c("Hydrocodone" = "Hydrocodone", "Oxycodone" = "Oxycodone",
                  "Fentanyl" = "Fentanyl")
} else if (grouplabel == "Pharmacy"){
  group = "Dispenser_Class"
  GroupLevels = c("Chain", "Retail")
  GroupLabels = c("Chain" = "Chain", "Retail" = "Retail")
} else if (grouplabel == "PharmacyDrug"){
  group = "Dispenser_Drug_Class"
  GroupLevels = c("Chain - Hydrocodone", "Chain - Oxycodone",
                  "Chain - Fentanyl",
                  "Retail - Hydrocodone", "Retail - Oxycodone",
                  "Retail - Fentanyl")
  GroupLabels = c("Chain - Hydrocodone" = "Chain - Hydrocodone",
                  "Chain - Oxycodone" = "Chain - Oxycodone",
                  "Chain - Fentanyl" = "Chain - Fentanyl",
                  "Retail - Hydrocodone" = "Retail - Hydrocodone",
                  "Retail - Oxycodone" = "Retail - Oxycodone",
                  "Retail - Fentanyl" = "Retail - Fentanyl")
}
# if (grouplabel == "PharmacyDrug"){
#   lensgroups = c("Dispenser")
# } else{
#   lensgroups = c("Patient","Prescriber","Dispenser")
# }
lensgroups = c("Patient")
time = c("Year","Month")
depvar = "PrescriptionperCapita"


ResMerge <- data.frame(Year = numeric(),
                       Lens = character(),
                       Gorup = character(),
                       Pvalue = numeric(),
                       ChangePct = numeric(),
                       PvalueOverall = numeric(),
                       ChangePctOverall = numeric(),
                       stringsAsFactors = FALSE)
for (lens in lensgroups){
  subdf <- df[df[,paste0(lens,"_State")] == "SC",] %>% 
    mutate(Risk = ifelse(.data[[paste0(lens,"_County")]] %in% HighRiskCounties,"High Risk","Low Risk")) %>%
    group_by(.data[[paste0(lens,"_County")]],Risk,.data[[time[1]]],.data[[time[2]]],.data[[group]]) %>% 
    summarise(N = n(),
              Quantity = sum(Quantity, na.rm = T),
              MME = sum(Total_Prescription_MME, na.rm = T),
              T = unique(T)) %>% ungroup()
  subdf$PrescriptionperCapita <- subdf$N / subdf$T*1e5
  subdf$MMEperCapita <- subdf$MME / subdf$T*1e5
  subdf$QuantityperCapita <- subdf$Quantity / subdf$T*1e5
  subdf <- as.data.frame(subdf)
  subdf <- subdf[subdf[,group] %in% GroupLevels,]
  
  # Perform statistical tests
  Res <- data.frame(Year = numeric(),
                    Lens = character(),
                    Gorup = character(),
                    Pvalue = numeric(),
                    ChangePct = numeric(),
                    stringsAsFactors = FALSE)
  for (t in unique(subdf[,time[1]])) {
    for (g in unique(subdf[,group])) {
      high_risk_data <- subdf %>% filter(Risk == "High Risk" & .data[[time[1]]] == t & .data[[group]] == g)
      low_risk_data <- subdf %>% filter(Risk == "Low Risk" & .data[[time[1]]] == t & .data[[group]] == g)
      Pvalue <- t.test(high_risk_data[,depvar], unlist(low_risk_data[,depvar]))$p.value
      ChangePct <- (mean(high_risk_data[,depvar]) - mean(low_risk_data[,depvar])) / mean(low_risk_data[,depvar])
      Res <- rbind(Res, data.frame(Year = t,
                                   Lens = lens,
                                   Group = g,
                                   Pvalue = Pvalue,
                                   ChangePct = ChangePct,
                                   stringsAsFactors = FALSE))
    }
  }
  ResOverall <- data.frame(Gorup = character(),
                           Lens = character(),
                           PvalueOverall = numeric(),
                           ChangePctOverall = numeric(),
                           stringsAsFactors = FALSE)
  for (g in unique(subdf[,group])) {
    high_risk_data <- subdf %>% filter(Risk == "High Risk" & .data[[group]] == g)
    low_risk_data <- subdf %>% filter(Risk == "Low Risk" & .data[[group]] == g)
    Pvalue <- t.test(high_risk_data[,depvar], unlist(low_risk_data[,depvar]))$p.value
    ChangePct <- (mean(high_risk_data[,depvar]) - mean(low_risk_data[,depvar])) / mean(low_risk_data[,depvar])
    ResOverall <- rbind(ResOverall, data.frame(Group = g,
                                               Lens = lens,
                                               PvalueOverall = Pvalue,
                                               ChangePctOverall = ChangePct,
                                               stringsAsFactors = FALSE))
  }
  Res <- merge(Res, ResOverall, by = c("Group","Lens"), all.x = T)
  ResMerge <- rbind(ResMerge,Res)
  Res$Sig <- "Non-Significant"
  Res$Sig[which(Res$ChangePct < 0 & Res$Pvalue <= 0.05)] <- "Negative-Significant"
  Res$Sig[which(Res$ChangePct > 0 & Res$Pvalue <= 0.05)] <- "Positive-Significant"
  Res$SigOverall <- "Non-Significant"
  Res$SigOverall[which(Res$ChangePctOverall < 0 & Res$PvalueOverall <= 0.05)] <- "Negative-Significant"
  Res$SigOverall[which(Res$ChangePctOverall > 0 & Res$PvalueOverall <= 0.05)] <- "Positive-Significant"
  Res$Group <- factor(Res$Group, levels = GroupLevels)

  # Plot
  ncolval = ifelse(grouplabel == "PharmacyDrug", 2, ceiling(sqrt(length(unique(Res$Group)))))
  fig <- ggplot(Res[Res$Group %in% GroupLevels,], aes(x = Year, y = ChangePct, color = Sig, fill = Sig)) +
    geom_point(shape = 21, size  = 2.5) +
    geom_line(aes(color = "black", fill = NULL)) + 
    geom_label(aes(label = paste0("Mean = ",round(ChangePctOverall*100,2),
                                  " %",
                                  ifelse(PvalueOverall <= 0.001, "***",
                                         ifelse(PvalueOverall <= 0.01, "**",
                                                ifelse(PvalueOverall <= 0.05, "*",""))))),
               nudge_x = -Inf, nudge_y = Inf,color='black', size= 4, hjust=-1, vjust = 2, fill = "white") +
    facet_wrap(~ Group, scales = "free", ncol = ncolval,
               labeller = labeller(Group = as_labeller(GroupLabels))) +
    scale_fill_manual(values = c("Negative-Significant" = "red", "Positive-Significant" = "blue", "Non-Significant" = "white")) +
    scale_color_manual(values = c("Negative-Significant" = "red", "Positive-Significant" = "blue", "Non-Significant" = "black")) +
    scale_y_continuous(labels = percent) +
    labs(x = "Year", y = "Percentage of Changes in Prescription Per Capita") +
    guides(fill = guide_legend(title = "Significance"), 
           color = guide_legend(title = "Significance")) +
    theme_bw() +
    ggtitle(paste0(lens,"s in High Risk Counties vs. Low Risk Counties")) + 
    theme(strip.text.x = element_text(size = 10, color = "black", face = "bold",family = "mono"),
          strip.text.y = element_text(size = 8, color = "black", face = "bold",family = "mono"),
          legend.title = element_text(family = "mono",size = 10),
          legend.position="bottom",
          legend.text = element_text(family = "mono",size = 10),
          axis.title.y  = element_text(family = "mono",size = 12, face = "bold"),
          axis.title.y.right = element_text(family = "mono",size = 12, face = "bold"),
          axis.title.x  = element_text(family = "mono",size = 12, face = "bold", hjust = 0.5),
          axis.text.x   = element_text(family = "mono",size = 10, face = "bold"),
          axis.text.y   = element_text(family = "mono",size = 10, face = "bold"),
          plot.title = element_text(family = "mono",size = 12,face = "bold", hjust = 0.5))
  ggsave(paste0(pathWrite,"YearlyChangePerCapitaPrescription_",lens,"Lens_",grouplabel,".png"), plot = fig, width = 14, height = 8, bg="white")
}

ResMerge$Sig <- "Non-Significant"
ResMerge$Sig[which(ResMerge$ChangePct < 0 & ResMerge$Pvalue <= 0.05)] <- "Negative-Significant"
ResMerge$Sig[which(ResMerge$ChangePct > 0 & ResMerge$Pvalue <= 0.05)] <- "Positive-Significant"
ResMerge$SigOverall <- "Non-Significant"
ResMerge$SigOverall[which(ResMerge$ChangePctOverall < 0 & ResMerge$PvalueOverall <= 0.05)] <- "Negative-Significant"
ResMerge$SigOverall[which(ResMerge$ChangePctOverall > 0 & ResMerge$PvalueOverall <= 0.05)] <- "Positive-Significant"
fwrite(ResMerge,paste0(pathWrite,"YearlyChangePerCapitaPrescription_",grouplabel,".csv"))




# ------------------------------------------------------------------------- #
# Patient Lens - All groups mentioned in previous section with interactions #
# ------------------------------------------------------------------------- #
lens = "Patient"
subdf <- df[df[,paste0(lens,"_State")] == "SC",] %>%
  mutate(Risk = ifelse(.data[[paste0(lens,"_County")]] %in% HighRiskCounties,"High Risk","Low Risk")) %>%
  mutate(PrescriberLocation = ifelse(Prescriber_State == "SC","In State","Out State")) %>%
  mutate(DispenserLocation = ifelse(Dispenser_State == "SC","In State","Out State")) %>%
  group_by(.data[[paste0(lens,"_County")]],Risk,Year,Month,Drug_Subclass,PrescriberLocation,DispenserLocation,Dispenser_Class) %>%
  mutate(Date = as.Date(paste(Year,Month, "01", sep = "-"), format = "%Y-%m-%d")) %>%
  summarise(N = n(),
            Quantity = sum(Quantity, na.rm = T),
            MME = sum(Total_Prescription_MME, na.rm = T),
            T = unique(T)) %>% ungroup()
subdf <- subdf %>%   mutate(Date = as.Date(paste(Year,Month, "01", sep = "-"), format = "%Y-%m-%d"))
subdf$PrescriptionperCapita <- subdf$N / subdf$T*1e5
subdf$MMEperCapita <- subdf$MME / subdf$T*1e5
subdf$QuantityperCapita <- subdf$Quantity / subdf$T*1e5
subdf <- as.data.frame(subdf)

Res <- data.frame(Lens = character(),
                  Metric = character(),
                  Drug = character(),
                  PrescriberLocation = character(),
                  PharmacyLocation = character(),
                  PharmacyType = character(),
                  HighRisk = numeric(),
                  LowRisk = numeric(),
                  Difference = numeric(),
                  ChangePct = numeric(),
                  Pvalue = numeric(),
                  stringsAsFactors = FALSE)
depvar = "PrescriptionperCapita"
for (drug in unique(subdf$Drug_Subclass)){
  for (locp in unique(subdf$PrescriberLocation)){
    for (locd in unique(subdf$DispenserLocation)){
      for (type in unique(subdf$Dispenser_Class)){
        high_risk_data <- subdf %>% filter(Risk == "High Risk" & Drug_Subclass == drug & PrescriberLocation == locp & DispenserLocation == locd & Dispenser_Class == type)
        low_risk_data <- subdf %>% filter(Risk == "Low Risk"  & Drug_Subclass == drug & PrescriberLocation == locp & DispenserLocation == locd & Dispenser_Class == type)
        if (length(high_risk_data[, depvar]) < 2 || length(low_risk_data[, depvar]) < 2) {
          Pvalue <- NaN
        } else {
          Pvalue <- t.test(high_risk_data[,depvar], low_risk_data[,depvar])$p.value
        }

        ChangePct <- (mean(high_risk_data[,depvar], na.rm = T) - mean(low_risk_data[,depvar], na.rm = T)) / mean(low_risk_data[,depvar], na.rm = T)
        Res <- rbind(Res, data.frame(Lens = "Patient",
                                                   Metric = depvar,
                                                   Drug = drug,
                                                   PrescriberLocation = locp,
                                                   PharmacyLocation = locd,
                                                   PharmacyType = type,
                                                   HighRisk = mean(high_risk_data[,depvar], na.rm = T),
                                                   LowRisk = mean(low_risk_data[,depvar], na.rm = T),
                                                   Difference = (mean(high_risk_data[,depvar], na.rm = T) - mean(low_risk_data[,depvar], na.rm = T)),
                                                   ChangePct = ChangePct,
                                                   Pvalue = Pvalue,
                                                   stringsAsFactors = FALSE))
      }
    }
  }
}
groups <- c("Drug_Subclass","PrescriberLocation","DispenserLocation","Dispenser_Class")
all_combinations <- unlist(lapply(seq_along(groups),
                                  function(x) combn(groups, x, simplify = FALSE)),
                           recursive = FALSE)
for (g in all_combinations){
  if (length(g) == 1){
    tmp <- subdf %>%
      group_by(.data[[paste0(lens,"_County")]],Risk,Year,Month,.data[[g]]) %>%
      summarise(PrescriptionperCapita = sum(PrescriptionperCapita, na.rm = T),
                MMEperCapita = sum(MMEperCapita, na.rm = T),
                QuantityperCapita = sum(QuantityperCapita, na.rm = T)) %>% ungroup()
    tmp <- as.data.frame(tmp)
    for (glevel in unique(tmp[,g])){
      high_risk_data <- tmp %>% filter(Risk == "High Risk" & .data[[g]] == glevel)
      low_risk_data <- tmp %>% filter(Risk == "Low Risk"  & .data[[g]] == glevel)
      if (length(high_risk_data[, depvar]) < 2 || length(low_risk_data[, depvar]) < 2) {
        Pvalue <- NaN
      } else {
        Pvalue <- t.test(high_risk_data[,depvar], low_risk_data[,depvar])$p.value
      }
      ChangePct <- (mean(high_risk_data[,depvar], na.rm = T) - mean(low_risk_data[,depvar], na.rm = T)) / mean(low_risk_data[,depvar], na.rm = T)
      Res <- rbind(Res, data.frame(Lens = "Patient",
                                   Metric = depvar,
                                   Drug = ifelse(g == "drug_class", glevel, "All"),
                                   PrescriberLocation = ifelse(g == "PrescriberLocation", glevel, "All"),
                                   PharmacyLocation = ifelse(g == "DispenserLocation", glevel, "All"),
                                   PharmacyType = ifelse(g == "Dispenser_Class", glevel, "All"),
                                   HighRisk = mean(high_risk_data[,depvar], na.rm = T),
                                   LowRisk = mean(low_risk_data[,depvar], na.rm = T),
                                   Difference = (mean(high_risk_data[,depvar], na.rm = T) - mean(low_risk_data[,depvar], na.rm = T)),
                                   ChangePct = ChangePct,
                                   Pvalue = Pvalue,
                                   stringsAsFactors = FALSE))
    }
  } else if (length(g) == 2){
    tmp <- subdf %>%
      group_by(.data[[paste0(lens,"_County")]],Risk,Year,Month,.data[[g[1]]],.data[[g[2]]]) %>%
      summarise(PrescriptionperCapita = sum(PrescriptionperCapita, na.rm = T),
                MMEperCapita = sum(MMEperCapita, na.rm = T),
                QuantityperCapita = sum(QuantityperCapita, na.rm = T)) %>% ungroup()
    tmp <- as.data.frame(tmp)
    for (glevel1 in unique(tmp[,g[1]])){
      for (glevel2 in unique(tmp[,g[2]])){
        high_risk_data <- tmp %>% filter(Risk == "High Risk" & .data[[g[1]]] == glevel1 & .data[[g[2]]] == glevel2)
        low_risk_data <- tmp %>% filter(Risk == "Low Risk"  & .data[[g[1]]] == glevel1 & .data[[g[2]]] == glevel2)
        if (length(high_risk_data[, depvar]) < 2 || length(low_risk_data[, depvar]) < 2) {
          Pvalue <- NaN
        } else {
          Pvalue <- t.test(high_risk_data[,depvar], low_risk_data[,depvar])$p.value
        }
        ChangePct <- (mean(high_risk_data[,depvar], na.rm = T) - mean(low_risk_data[,depvar], na.rm = T)) / mean(low_risk_data[,depvar], na.rm = T)
        Res <- rbind(Res, data.frame(Lens = "Patient",
                                     Metric = depvar,
                                     Drug = ifelse(g[1] == "Drug_Subclass", glevel1,
                                                   ifelse(g[2] == "Drug_Subclass",glevel2,"All")),
                                     PrescriberLocation = ifelse(g[1] == "PrescriberLocation", glevel1,
                                                                 ifelse(g[2] == "PrescriberLocation",glevel2,"All")),
                                     PharmacyLocation = ifelse(g[1] == "DispenserLocation", glevel1,
                                                               ifelse(g[2] == "DispenserLocation",glevel2,"All")),
                                     PharmacyType = ifelse(g[1] == "Dispenser_Class", glevel1,
                                                           ifelse(g[2] == "Dispenser_Class",glevel2,"All")),
                                     HighRisk = mean(high_risk_data[,depvar], na.rm = T),
                                     LowRisk = mean(low_risk_data[,depvar], na.rm = T),
                                     Difference = (mean(high_risk_data[,depvar], na.rm = T) - mean(low_risk_data[,depvar], na.rm = T)),
                                     ChangePct = ChangePct,
                                     Pvalue = Pvalue,
                                     stringsAsFactors = FALSE))
      }
    }
  } else if (length(g) == 3){
      tmp <- subdf %>%
        group_by(.data[[paste0(lens,"_County")]],Risk,Year,Month,.data[[g[1]]],.data[[g[2]]],.data[[g[3]]]) %>%
        summarise(PrescriptionperCapita = sum(PrescriptionperCapita, na.rm = T),
                  MMEperCapita = sum(MMEperCapita, na.rm = T),
                  QuantityperCapita = sum(QuantityperCapita, na.rm = T)) %>% ungroup()
      tmp <- as.data.frame(tmp)
      for (glevel1 in unique(tmp[,g[1]])){
        for (glevel2 in unique(tmp[,g[2]])){
          for (glevel3 in unique(tmp[,g[3]])){
            high_risk_data <- tmp %>% filter(Risk == "High Risk" & .data[[g[1]]] == glevel1 & .data[[g[2]]] == glevel2 & .data[[g[3]]] == glevel3)
            low_risk_data <- tmp %>% filter(Risk == "Low Risk"  & .data[[g[1]]] == glevel1 & .data[[g[2]]] == glevel2 & .data[[g[3]]] == glevel3)
            if (length(high_risk_data[, depvar]) < 2 || length(low_risk_data[, depvar]) < 2) {
              Pvalue <- NaN
            } else {
              Pvalue <- t.test(high_risk_data[,depvar], low_risk_data[,depvar])$p.value
            }
            ChangePct <- (mean(high_risk_data[,depvar], na.rm = T) - mean(low_risk_data[,depvar], na.rm = T)) / mean(low_risk_data[,depvar], na.rm = T)
            Res <- rbind(Res, data.frame(Lens = "Patient",
                                         Metric = depvar,
                                         Drug = ifelse(g[1] == "Drug_Subclass", glevel1,
                                                       ifelse(g[2] == "Drug_Subclass",glevel2,
                                                              ifelse(g[3] == "Drug_Subclass",glevel3,"All"))),
                                         PrescriberLocation = ifelse(g[1] == "PrescriberLocation", glevel1,
                                                                     ifelse(g[2] == "PrescriberLocation",glevel2,
                                                                            ifelse(g[3] == "PrescriberLocation",glevel3,"All"))),
                                         PharmacyLocation = ifelse(g[1] == "DispenserLocation", glevel1,
                                                                   ifelse(g[2] == "DispenserLocation",glevel2,
                                                                          ifelse(g[3] == "DispenserLocation",glevel3,"All"))),
                                         PharmacyType = ifelse(g[1] == "Dispenser_Class", glevel1,
                                                               ifelse(g[2] == "Dispenser_Class",glevel2,
                                                                      ifelse(g[3] == "Dispenser_Class",glevel3,"All"))),
                                         HighRisk = mean(high_risk_data[,depvar], na.rm = T),
                                         LowRisk = mean(low_risk_data[,depvar], na.rm = T),
                                         Difference = (mean(high_risk_data[,depvar], na.rm = T) - mean(low_risk_data[,depvar], na.rm = T)),
                                         ChangePct = ChangePct,
                                         Pvalue = Pvalue,
                                         stringsAsFactors = FALSE))
        }
      }
    }
  }
}
fwrite(Res,paste0(pathWrite,"OveralChangePerCapitaPrescription_PatientLens.csv"))


# Not able to run the plot on linux
# Res$ChangePct[is.na(Res$ChangePct)] <- 0
# Res$Pvalue[is.na(Res$Pvalue)] <- 1
# Res$Significant <- ifelse(Res$Pvalue <= 0.05, "Significant","Non-Significant")
# Res$Sign <- ifelse(Res$ChangePct <= 0, "Negative","Positive")
# 
# DrugLevels = c("Hydrocodone","Oxycodone","Fentanyl","Methadone","All")
# PharmacyLevels = c("Chain", "Retail","All")
# subdf <- Res %>% rowwise() %>%
#   filter(sum(c_across(Drug:PharmacyType) == "All") == 3) %>%
#   filter(Drug %in% DrugLevels) %>% 
#   filter(PharmacyType %in% PharmacyLevels) %>%
#   pivot_longer(cols = c(Drug, PrescriberLocation, PharmacyLocation, PharmacyType),
#                names_to = "Group",
#                values_to = "GroupValue")  %>%
#   filter(GroupValue != "All")
# subdf$Group[subdf$Group == "PrescriberLocation"] <- "Prescriber Location"
# subdf$Group[subdf$Group == "PharmacyLocation"] <- "Pharmacy Location"
# subdf$Group[subdf$Group == "PharmacyType"] <- "Pharmacy Type"
# subdf$Group <- factor(subdf$Group, levels = c("Drug","Prescriber Location","Pharmacy Type","Pharmacy Location"))
# subdf$y_max = subdf$ChangePct*1.2
# 
# fig <- ggplot(subdf, aes(x = GroupValue, y = ChangePct, fill = Sign, pattern = Significant)) +
#   geom_bar_pattern(position = "dodge",
#                    stat = "identity",width = 0.5,alpha=0.2,
#                    color = "black", 
#                    pattern_fill = "black",
#                    pattern_angle = 45,
#                    pattern_density = 0.1,
#                    pattern_spacing = 0.025,
#                    pattern_key_scale_factor = 0.6, na.rm = T) + 
#   geom_label(aes(label = paste0(round(ChangePct*100,2),
#                                 " %",
#                                 ifelse(Pvalue <= 0.001, "***",
#                                        ifelse(Pvalue <= 0.01, "**",
#                                               ifelse(Pvalue <= 0.05, "*",""))))),
#              position=position_dodge(width=0.5),size=4,color='black',angle = 0,fill = "white",
#              vjust = ifelse(subdf$ChangePct >= 0, 0, 1), na.rm = T) +
#   geom_hline(yintercept = 0, linetype = "dashed", linewidth = 1, color = "gray") +
#   facet_wrap(~Group, scales = "free", ncol = 2) +
#   scale_pattern_manual(values = c("Significant" = "stripe", "Non-Significant" = "none")) +
#   scale_fill_manual(values = c("Negative" = "red", "Positive" = "blue")) +
#   scale_y_continuous(labels = percent, expand = expansion(mult = c(0.1, 0.2))) +
#   labs(x = "Drug", y = "Percentage of Changes in Prescription Per Capita") +
#   guides(fill = guide_legend(title = "Sign"), 
#          pattern = guide_legend(title = "Significance")) +
#   theme_bw() +
#   ggtitle(paste0("High Risk Counties vs. Low Risk Counties")) + 
#   theme(strip.text.x = element_text(size = 10, color = "black", face = "bold",family = "mono"),
#         strip.text.y = element_text(size = 10, color = "black", face = "bold",family = "mono"),
#         legend.title = element_text(family = "mono",size = 10),
#         legend.position="bottom",
#         legend.text = element_text(family = "mono",size = 10),
#         axis.title.y  = element_text(family = "mono",size = 12, face = "bold"),
#         axis.title.y.right = element_text(family = "mono",size = 12, face = "bold"),
#         axis.title.x  = element_text(family = "mono",size = 12, face = "bold", hjust = 0.5),
#         axis.text.x   = element_text(family = "mono",size = 10, face = "bold"),
#         axis.text.y   = element_text(family = "mono",size = 10, face = "bold"),
#         plot.title = element_text(family = "mono",size = 12,face = "bold", hjust = 0.5),
#         panel.background = element_rect(fill = "white"),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank()) 
# 
# ggsave(paste0(pathWrite,"OveralChangePerCapitaPrescription_PatientLens.png"), plot = fig, width = 14, height = 8, bg="white")


# # -------------------------------------------------------------------- #
# # Patient Lens - Prescription Characteristics - Four types of counties #
# # -------------------------------------------------------------------- #
# df <- merge(df, CountyRisk[,c("subregion","MetricMOUD")], by.x = c("Patient_County"), by.y = c("subregion"), all.x = T)
# subdf <- df %>% group_by(Year, MetricMOUD, drug_class) %>%
#   summarise(MMEperCapita = mean(MMEperCapita, na.rm = TRUE),
#             QuantityperCapita = mean(QuantityperCapita, na.rm = TRUE),
#             PrescriptionPerCapita = n()/T,
#             .groups = 'drop')
# subdf <- subdf[subdf$drug_class %in% c("Fentanyl","Oxycodone","Hydrocodone"),]
# 
# 
# fig <- ggplot(subdf, aes(x = Year, y = PrescriptionPerCapita, color = MetricMOUD)) +
#   geom_line(linewidth = 1) +
#   geom_point(alpha = 0.5) +
#   facet_wrap(~drug_class) +
#   scale_color_manual(values = c("LL" = "#C2F784", "HH" = "#FBFF00", "LH" = "#DF2E38", "HL" = "#84DFFF")) +
#   labs(x = "Year", y = "Average Prescription per Capita", color = "Metric MOUD")
# 
# ggplot(subdf, aes(x = Date)) +
#   geom_line(aes(y = PrescriptionperCapita, group = Patient_County), color = "gray",alpha = 0.5) +
#   geom_line(aes(y = AvgPrescription), color = "black", linewidth = 1) +
#   facet_wrap(~Risk) +
#   labs(x = "Date", y = "Prescription per Capita",
#        title = "Monthly Prescription per Capita by Risk Status") +
#   theme_bw() +
#   ggtitle(paste0("Monthly Prescription per Capita by Risk Status")) + 
#   theme(strip.text.x = element_text(size = 10, color = "black", face = "bold",family = "mono"),
#         strip.text.y = element_text(size = 8, color = "black", face = "bold",family = "mono"),
#         legend.title = element_blank(),
#         legend.position="bottom",
#         legend.text = element_blank(),
#         axis.title.y  = element_text(family = "mono",size = 12, face = "bold"),
#         axis.title.y.right = element_text(family = "mono",size = 12, face = "bold"),
#         axis.title.x  = element_text(family = "mono",size = 12, face = "bold", hjust = 0.5),
#         axis.text.x   = element_text(family = "mono",size = 10, face = "bold"),
#         axis.text.y   = element_text(family = "mono",size = 10, face = "bold"),
#         plot.title = element_text(family = "mono",size = 12,face = "bold", hjust = 0.5))



# # -------------------------------------------------------------------- #
# # High Risk vs. Low Risk Per capita prescription yearly change by drug #
# # -------------------------------------------------------------------- #
# subdf <- df %>% group_by(Year,Patient_County,County_Risk,Drug_Subclass,T) %>%
#   summarise(N = n(),
#             Doctor_Shopping = sum(Doctor_Shopping, na.rm = T),
#             Quantity = sum(Quantity, na.rm = T),
#             MME = sum(Total_Prescription_MME, na.rm = T)) %>% ungroup()
# subdf$DoctorShoppingperCapita <- subdf$Doctor_Shopping / subdf$T*1e5
# subdf$PrescriptionperCapita <- subdf$N / subdf$T*1e5
# subdf$MMEperCapita <- subdf$MME / subdf$T*1e5
# subdf$QuantityperCapita <- subdf$Quantity / subdf$T*1e5
# subdf <- as.data.frame(subdf)
# 
# # Create the line chart
# fig <- ggplot(subdf, aes(x = Year, y = PrescriptionperCapita, color = County_Risk)) +
#   geom_line(stat = "summary", fun.y = "mean", size = 1) +
#   geom_point(stat = "summary", fun.y = "mean", shape = 21, size = 3, fill = "white") +
#   facet_wrap(~ Drug_Subclass, scales = "free") +
#   scale_color_manual(values = c("Low Risk" = "blue", "High Risk" = "red")) +
#   labs(x = "Year", y = "Per Capita Prescription", fill = "Risk Status") +
#   scale_fill_manual(values = c("HighRisk" = "red", "NonHighRisk" = "blue")) +
#   theme_bw() +
#   ggtitle("Per Capita Opioid Prescription in SC Counties (2017-2021)") +
#   theme(strip.text.x = element_text(size = 8, color = "black", face = "bold",family = "mono"),
#         strip.text.y = element_text(size = 8, color = "black", face = "bold",family = "mono"),
#         legend.title = element_text(family = "mono",size = 10),
#         legend.position="bottom",
#         legend.text = element_text(family = "mono",size = 10),
#         axis.title.y  = element_text(family = "mono",size = 12, face = "bold"),
#         axis.title.y.right = element_text(family = "mono",size = 12, face = "bold"),
#         axis.title.x  = element_text(family = "mono",size = 12, face = "bold", hjust = 0.5),
#         axis.text.x   = element_text(family = "mono",size = 10, face = "bold"),
#         axis.text.y   = element_text(family = "mono",size = 10, face = "bold"),
#         plot.title = element_text(family = "mono",size = 12,face = "bold", hjust = 0.5))
# ggsave(paste0(pathWrite,"PrescriptionCountyRiskYearlyDrug.png"), plot = fig, width = 14, height = 8, bg="white")
# 
# 
# # ----------------------------------------------------------------------- #
# # High Risk vs. Low Risk Per capita doctor shopping yearly change by drug #
# # ----------------------------------------------------------------------- #
# fig <- ggplot(subdf, aes(x = Year, y = DoctorShoppingperCapita, color = County_Risk)) +
#   geom_line(stat = "summary", fun.y = "mean", size = 1) +
#   geom_point(stat = "summary", fun.y = "mean", shape = 21, size = 3, fill = "white") +
#   # facet_wrap(~ Drug_Subclass, scales = "free") +
#   scale_color_manual(values = c("Low Risk" = "blue", "High Risk" = "red")) +
#   labs(x = "Year", y = "Per Capita Prescription", fill = "Risk Status") +
#   scale_fill_manual(values = c("HighRisk" = "red", "NonHighRisk" = "blue")) +
#   theme_bw() +
#   ggtitle("Per Capita Opioid Prescription in SC Counties (2017-2021)") +
#   theme(strip.text.x = element_text(size = 8, color = "black", face = "bold",family = "mono"),
#         strip.text.y = element_text(size = 8, color = "black", face = "bold",family = "mono"),
#         legend.title = element_text(family = "mono",size = 10),
#         legend.position="bottom",
#         legend.text = element_text(family = "mono",size = 10),
#         axis.title.y  = element_text(family = "mono",size = 12, face = "bold"),
#         axis.title.y.right = element_text(family = "mono",size = 12, face = "bold"),
#         axis.title.x  = element_text(family = "mono",size = 12, face = "bold", hjust = 0.5),
#         axis.text.x   = element_text(family = "mono",size = 10, face = "bold"),
#         axis.text.y   = element_text(family = "mono",size = 10, face = "bold"),
#         plot.title = element_text(family = "mono",size = 12,face = "bold", hjust = 0.5))
# ggsave(paste0(pathWrite,"PrescriptionCountyRiskYearlyDoctorShopping.png"), plot = fig, width = 14, height = 8, bg="white")


# ========================================= #
# AHRF Characteristic of High Risk Counties #
# ========================================= #
AHRF_SC <- AHRF[AHRF[2] == "SC",]
AHRF_SC$Risk <- "Low Risk"
AHRF_SC$Risk[AHRF_SC[,3] %in% HighRiskCounties] <- "High Risk"
AHRF_SC <- AHRF_SC[AHRF_SC$yr %in% 2017:2021,]
tmp <- AHRF_SC %>% group_by(.data[[names(AHRFdict[AHRFdict == "County name"])]],Risk,yr) %>%
  summarise(UempRate = mean(.data[[names(AHRFdict[AHRFdict == "Percentage unemployed"])]], na.rm = T),
            PovertyPct = mean(.data[[names(AHRFdict[AHRFdict == "Percentage in poverty"])]], na.rm = T),
            PerCapitaIncome = mean(.data[[names(AHRFdict[AHRFdict == "Per capita income"])]], na.rm = T),
            MedicareElg = mean(.data[[names(AHRFdict[AHRFdict == "Eligible for Medicare" ])]] / 
                                 .data[[names(AHRFdict[AHRFdict == "Population estimate"])]] *100, na.rm = T),
            MedicareEnr = mean(.data[[names(AHRFdict[AHRFdict == "Total Medicare enrollment (later years)" ])]] / 
                                       .data[[names(AHRFdict[AHRFdict == "Population estimate"])]] *100, na.rm = T),
            MedicaidElg = mean(.data[[names(AHRFdict[AHRFdict == "Eligible for Medicaid" ])]] / 
                                 .data[[names(AHRFdict[AHRFdict == "Population estimate"])]] *100, na.rm = T),
            MedicareCost = mean(.data[[names(AHRFdict[AHRFdict == "Actual per capita Medicare cost"])]], na.rm = T),
            UninsuredPct = mean(.data[[names(AHRFdict[AHRFdict == "Percentage <65 without health insurance (2014+)"])]], na.rm = T),
            
            CollegeEdu = mean(.data[[names(AHRFdict[AHRFdict == "Percentage aged 25+ with 4+ years college"])]], na.rm = T),
            NoHighSchoolEdu = mean(.data[[names(AHRFdict[AHRFdict == "Percentage aged 25+ w.o. high school diploma"])]], na.rm = T),
            
            PopDensity = mean(.data[[names(AHRFdict[AHRFdict == "Population estimate"])]] / 
                              .data[[names(AHRFdict[AHRFdict == "Total area in square miles"])]], na.rm = T),  
            
            MalePct = mean(.data[[names(AHRFdict[AHRFdict == "Total males"])]] / 
                             .data[[names(AHRFdict[AHRFdict == "Population estimate"])]]*100, na.rm = T),
            
            MDdensity = mean((.data[[names(AHRFdict[AHRFdict == "Total MDs male" ])]] +  
                              .data[[names(AHRFdict[AHRFdict == "Total MDs female" ])]])/ 
                               .data[[names(AHRFdict[AHRFdict == "Population estimate"])]] *1E5, na.rm = T),
            NPdensity = mean(.data[[names(AHRFdict[AHRFdict == "NPs with NPI" ])]] / 
                             .data[[names(AHRFdict[AHRFdict == "Population estimate"])]] *1E5, na.rm = T),
            Hspcdensity = mean(.data[[names(AHRFdict[AHRFdict == "Hospices" ])]] / 
                               .data[[names(AHRFdict[AHRFdict == "Population estimate"])]] *1E5, na.rm = T),
            SPdensity = mean((.data[[names(AHRFdict[AHRFdict == "Total Specs <35" ])]] +  
                              .data[[names(AHRFdict[AHRFdict == "Total Specs 35-44" ])]] +
                              .data[[names(AHRFdict[AHRFdict == "Total Specs 45-54" ])]] +
                              .data[[names(AHRFdict[AHRFdict == "Total Specs 55-64" ])]] +
                              .data[[names(AHRFdict[AHRFdict == "Total Specs 65-74" ])]] +
                              .data[[names(AHRFdict[AHRFdict == "Total Specs 75+" ])]])/ 
                               .data[[names(AHRFdict[AHRFdict == "Population estimate"])]] *1E5, na.rm = T), 
            
            AgeUnder25Pct = mean((.data[[names(AHRFdict[AHRFdict == "Population estimate"])]] -
                               (.data[[names(AHRFdict[AHRFdict == "Population male 25-29"])]] + 
                                  .data[[names(AHRFdict[AHRFdict == "Population female 25-29"])]] +
                                  .data[[names(AHRFdict[AHRFdict == "Population male 30-34"])]] + 
                                  .data[[names(AHRFdict[AHRFdict == "Population female 30-34"])]] +
                                  .data[[names(AHRFdict[AHRFdict == "Population male 35-44"])]] + 
                                  .data[[names(AHRFdict[AHRFdict == "Population female 35-44"])]] +
                                  .data[[names(AHRFdict[AHRFdict == "Population male 45-54"])]] + 
                                  .data[[names(AHRFdict[AHRFdict == "Population female 45-54"])]] +
                                  .data[[names(AHRFdict[AHRFdict == "Population male 55-59"])]] + 
                                  .data[[names(AHRFdict[AHRFdict == "Population female 55-59"])]] + 
                                  .data[[names(AHRFdict[AHRFdict == "Population male 60-64"])]] + 
                                  .data[[names(AHRFdict[AHRFdict == "Population female 60-64"])]] +
                                  .data[[names(AHRFdict[AHRFdict == "Population male 65-74"])]] + 
                                  .data[[names(AHRFdict[AHRFdict == "Population female 65-74"])]] + 
                                  .data[[names(AHRFdict[AHRFdict == "Population male 75-84"])]] +
                                  .data[[names(AHRFdict[AHRFdict == "Population female 75-84"])]] +
                                  .data[[names(AHRFdict[AHRFdict == "Population male >84"])]] +
                                  .data[[names(AHRFdict[AHRFdict == "Population female >84"])]])                
                               ) / 
                               .data[[names(AHRFdict[AHRFdict == "Population estimate"])]]*100, na.rm = T),
            
            Age2565Pct = mean((.data[[names(AHRFdict[AHRFdict == "Population male 25-29"])]] + 
                            .data[[names(AHRFdict[AHRFdict == "Population female 25-29"])]] +
                            .data[[names(AHRFdict[AHRFdict == "Population male 30-34"])]] + 
                            .data[[names(AHRFdict[AHRFdict == "Population female 30-34"])]] +
                            .data[[names(AHRFdict[AHRFdict == "Population male 35-44"])]] + 
                            .data[[names(AHRFdict[AHRFdict == "Population female 35-44"])]] +
                            .data[[names(AHRFdict[AHRFdict == "Population male 45-54"])]] + 
                            .data[[names(AHRFdict[AHRFdict == "Population female 45-54"])]] +
                            .data[[names(AHRFdict[AHRFdict == "Population male 55-59"])]] + 
                            .data[[names(AHRFdict[AHRFdict == "Population female 55-59"])]] + 
                            .data[[names(AHRFdict[AHRFdict == "Population male 60-64"])]] + 
                            .data[[names(AHRFdict[AHRFdict == "Population female 60-64"])]]   
                            ) / 
                             .data[[names(AHRFdict[AHRFdict == "Population estimate"])]]*100, na.rm = T),
            AgeAbove65Pct = mean((.data[[names(AHRFdict[AHRFdict == "Population male 65-74"])]] + 
                            .data[[names(AHRFdict[AHRFdict == "Population female 65-74"])]] + 
                            .data[[names(AHRFdict[AHRFdict == "Population male 75-84"])]] +
                            .data[[names(AHRFdict[AHRFdict == "Population female 75-84"])]] + 
                            .data[[names(AHRFdict[AHRFdict == "Population male >84"])]] +
                            .data[[names(AHRFdict[AHRFdict == "Population female >84"])]]
                            ) / 
                             .data[[names(AHRFdict[AHRFdict == "Population estimate"])]]*100, na.rm = T),
            
            WhitePct = mean((.data[[names(AHRFdict[AHRFdict == "White males"])]] + 
                               .data[[names(AHRFdict[AHRFdict == "White females"])]]
            ) / 
              .data[[names(AHRFdict[AHRFdict == "Population estimate"])]]*100, na.rm = T),
            BlackPct = mean((.data[[names(AHRFdict[AHRFdict == "Black males"])]] + 
                               .data[[names(AHRFdict[AHRFdict == "Black females"])]]
            ) / 
              .data[[names(AHRFdict[AHRFdict == "Population estimate"])]]*100, na.rm = T),
            HispanicPCT = mean((.data[[names(AHRFdict[AHRFdict == "Total Hispanic males"])]] + 
                                  .data[[names(AHRFdict[AHRFdict == "Total Hispanic females"])]]
            ) / 
              .data[[names(AHRFdict[AHRFdict == "Population estimate"])]]*100, na.rm = T),

            Urbanicity = names(sort(table(.data[[names(AHRFdict[AHRFdict == "CBSA indicator code"])]]), decreasing = TRUE)[1]),
            Rural = unique(.data[[ "rural"]]),
            Metro = unique(.data[[ "metro"]]),
            Nonmetro = unique(.data[[ "nonmetro"]]),
            PCShortage = names(sort(table(.data[[names(AHRFdict[AHRFdict == "HPSA Code - Primary Care"])]]), decreasing = TRUE)[1]),
            MHShortage = names(sort(table(.data[[names(AHRFdict[AHRFdict == "HPSA Code - Mental Health"])]]), decreasing = TRUE)[1])
            )
tmp <- as.data.frame(tmp)




Res <- data.frame(Characteristic = character(),
                  MeanAll = numeric(),
                  SDAll = numeric(),
                  MeanHighRisk = numeric(),
                  SDHighRisk = numeric(),
                  MeanLowRisk = numeric(),
                  SDLowRisk = numeric(),
                  Pvalue = numeric(),
                  stringsAsFactors = FALSE)

# Separate numeric and categorical columns
num_columns <- names(tmp)[4:(length(tmp)-6)]
num_columns <- setdiff(num_columns, c("Metro", "Nonmetro")) # Remove Metro and Nonmetro

cat_columns <- c("Rural", "Urbanicity", "PCShortage", "MHShortage")

# T-test for numeric columns
for (depvar in num_columns) {
  high_risk_data <- tmp %>% filter(Risk == "High Risk")
  low_risk_data <- tmp %>% filter(Risk == "Low Risk")
  
  Pvalue <- round(t.test(high_risk_data[, depvar], unlist(low_risk_data[, depvar]))$p.value, 4)
  MeanAll <- round(mean(tmp[, depvar], na.rm = T), 2)
  SDAll <- round(sd(tmp[, depvar], na.rm = T), 2)
  MeanHighRisk <- round(mean(high_risk_data[, depvar], na.rm = T), 2)
  SDHighRisk <- round(sd(high_risk_data[, depvar], na.rm = T), 2)
  MeanLowRisk <- round(mean(low_risk_data[, depvar], na.rm = T), 2)
  SDLowRisk <- round(sd(low_risk_data[, depvar], na.rm = T), 2)
  
  Res <- rbind(Res, data.frame(Characteristic = depvar,
                               MeanAll = MeanAll,
                               SDAll = SDAll,
                               MeanHighRisk = MeanHighRisk,
                               SDHighRisk = SDHighRisk,
                               MeanLowRisk = MeanLowRisk,
                               SDLowRisk = SDLowRisk,
                               Pvalue = Pvalue,
                               stringsAsFactors = FALSE))
}

# Process categorical columns
for (depvar in cat_columns) {
  unique_vals <- unique(tmp[[depvar]])
  
  for (val in unique_vals) {
    if (depvar == "Rural" & val == 1) {
      MeanAll <- sum(tmp[[depvar]] == val, na.rm = TRUE) / 5
      MeanHighRisk <- sum(tmp$Risk == "High Risk" & tmp[[depvar]] == val, na.rm = TRUE) / 5
      MeanLowRisk <- sum(tmp$Risk == "Low Risk" & tmp[[depvar]] == val, na.rm = TRUE) / 5
    } else {
      MeanAll <- sum(tmp[[depvar]] == val, na.rm = TRUE) / 5
      MeanHighRisk <- sum(tmp$Risk == "High Risk" & tmp[[depvar]] == val, na.rm = TRUE) / 5
      MeanLowRisk <- sum(tmp$Risk == "Low Risk" & tmp[[depvar]] == val, na.rm = TRUE) / 5
    }
    
    # Compute p-value using chisq.test
    tbl <- table(tmp$Risk, tmp[[depvar]])
    Pvalue <- round(chisq.test(tbl)$p.value, 4)
    
    Res <- rbind(Res, data.frame(Characteristic = paste(depvar, val),
                                 MeanAll = MeanAll,
                                 SDAll = NA,
                                 MeanHighRisk = MeanHighRisk,
                                 SDHighRisk = NA,
                                 MeanLowRisk = MeanLowRisk,
                                 SDLowRisk = NA,
                                 Pvalue = Pvalue,
                                 stringsAsFactors = FALSE))
  }
}

fwrite(Res,paste0(pathWrite,"CountyCharacteresticComparison.csv"))