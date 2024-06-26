# ==============================================================================
# Purpose: Comapring different classification systems on counties
# Author: Amirreza Sahebi
# Last modified: 09-21-2023
# Input: MOUD_SCzips
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
library(sf)
library(purrr)
library(units)

# ==============================================================================
# ============================== Read data & Clean =============================
# ==============================================================================
# General path of data
pathRead <- "G:/My Drive/North Carolina State University/Project - Opioid 2/Data/"
pathWrite <- "G:/My Drive/North Carolina State University/Project - Opioid 2/Result/"


#===========#
# MOUD Data #
#===========#
MOUDzip <- fread(paste0(pathRead,"MOUD/MOUDCount_SCzips_Haffajee.csv")) 
MOUDzip$Zipcode <- as.character(MOUDzip$Zipcode)
MOUDcounty <- MOUDzip %>% group_by(County) %>% summarise(SOTP = sum(SOTP),
                                                         NU = sum(NU),
                                                         BU = sum(BU))
# MOUDcounty$MOUD <- 10*MOUDcounty$SOTP + MOUDcounty$NU + MOUDcounty$BU
MOUDcounty$MOUD <- MOUDcounty$SOTP + MOUDcounty$NU + MOUDcounty$BU



#===============#
# Overdose Data #
#===============#
dfOverdose <- fread(paste0(pathRead,"Overdose/VSRR_Provisional_Drug_Overdose_Death_Counts.csv"))
dfOverdose <- subset(dfOverdose, State == "SC" & ((Year == 2021 & Month != "December") | (Year <= 2020)))

dfOverdoseCounty <- fread(paste0(pathRead,"Overdose/VSRR_Provisional_County-Level_Drug_Overdose_Death_Counts_SC_DHEC.csv"))
dfOverdoseCounty[, 3:10] <- lapply(dfOverdoseCounty[, 3:10], as.integer)
dfOverdoseCounty <- dfOverdoseCounty[!is.na(dfOverdoseCounty$Year),]

#=================#
# Population Data #
#=================#
dfPop <- fread(paste0(pathRead,"Characterstics/sc-rfa-popestbycountyprojection.csv")) 
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
dfPop2021 <- dfPop %>%
  filter(Year == 2021) %>%
  select(County, T)


dfPopZip <- fread(paste0(pathRead,"Characterstics/ACS-SCPopZip.csv"))

#================#
# Shapefile Data #
#================#
dfShapeZip <- st_read(paste0(pathRead,"Map/tl_2019_us_zcta510.shp"))
dfShapeZip <- st_transform(dfShapeZip, 2263)

dfShapeCounty <- st_read(paste0(pathRead,"Map/tl_2019_us_county.shp"))
dfShapeCounty <- st_transform(dfShapeCounty, 2263)
county_centroids <- st_centroid(dfShapeCounty)





# ==============================================================================
# ================================== Data Prep =================================
# ==============================================================================


#====================#
# County Risk Status #
#====================#


#-------------------------#
# Classification System 1 #
#-------------------------#
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





# Zip level map
county_data <- map_data("county", "south carolina")
tmp <- merge(dfOverdoseCounty[dfOverdoseCounty$Year == 2021,c("County","Year","Opioids")],dfPop[,c("County","Year","T")], by = c("County","Year"))
tmp$PerCapitaOpioids <- tmp$Opioids / tmp$T
tmp <- tmp %>% 
  group_by(County) %>% 
  summarise(PerCapitaOpioids = sum(PerCapitaOpioids, na.rm = TRUE)*1e5)
county_data$subregion <- str_to_title(county_data$subregion)
county_data$subregion[county_data$subregion == "Mccormick"] = "McCormick"
county_data <- left_join(county_data, tmp, by = c("subregion" = "County"))
sc_shape <- st_read(paste0(pathRead,"/Map/tl_2019_us_zcta510.shp"))
MOUDzip$Zipcode <- as.character(MOUDzip$Zipcode)
sc_shape <- filter(sc_shape, ZCTA5CE10 %in% MOUDzip$Zipcode)
sc_shape$ZCTA5CE10 <- as.character(sc_shape$ZCTA5CE10)
sc_shape <- left_join(sc_shape, MOUDzip, by = c("ZCTA5CE10" = "Zipcode"))
sc_shape <- merge(sc_shape,dfPop[dfPop$Year == 2021,c("County","T")], by = "County")
sc_shape$MOUD <- sc_shape$NU + sc_shape$SOTP + sc_shape$BU
sc_shape <- sc_shape %>%
  mutate(across(c(SOTP, BU, NU, MOUD), ~ . / T * 1e5))
sc_shape$geometry <- st_centroid(sc_shape$geometry)




# Create the plot
p <- ggplot() +
  geom_polygon(data = county_data, aes(x = long, y = lat, group = group, fill = PerCapitaOpioids), color = "black") +
  scale_fill_gradient(name = "Per Capita Overdose", low = "red", high = "yellow", trans = "reverse") +
  labs(title = "Provider and Overdose Data in South Carolina",
       x = "Longitude",
       y = "Latitude") +
  theme_void() + 
  theme(legend.title = element_text(family = "mono",size = 12, face = "bold"),
        legend.position="right",
        legend.text = element_text(family = "mono",size = 12),
        plot.title = element_text(family = "mono",size = 12,face = "bold", hjust = 0.5))


# Plot SOTP
p1 <- p + 
  geom_sf(data = subset(sc_shape, SOTP > 0), aes(size = SOTP), color = "black", alpha = 0.5) +
  ggtitle("Opioid Treatment Programs") +
  scale_size_continuous(range = c(2, 8), name = "OTP count")+
  theme_void() + 
  theme(legend.title = element_text(family = "mono",size = 12, face = "bold"),
        legend.position="right",
        legend.text = element_text(family = "mono",size = 12),
        plot.title = element_text(family = "mono",size = 14,face = "bold", hjust = 0.5))

# Plot BU
p2 <- p + 
  geom_sf(data = subset(sc_shape, BU > 0), aes(size = BU), color = "green", alpha = 0.5) +
  ggtitle("Buprenorphine Providers") +
  scale_size_continuous(range = c(2, 8), name = "Buprenorphine count") +
  theme_void() + 
  theme(legend.title = element_text(family = "mono",size = 12, face = "bold"),
        legend.position="right",
        legend.text = element_text(family = "mono",size = 12),
        plot.title = element_text(family = "mono",size = 14,face = "bold", hjust = 0.5))

# Plot NU
p3 <- p + 
  geom_sf(data = subset(sc_shape, NU > 0), aes(size = NU), color = "blue", alpha = 0.5) +
  ggtitle("Naltrexone Providers") +
  scale_size_continuous(range = c(2, 8), name = "Naltrexone count") +
  theme_void() + 
  theme(legend.title = element_text(family = "mono",size = 12, face = "bold"),
        legend.position="right",
        legend.text = element_text(family = "mono",size = 12),
        plot.title = element_text(family = "mono",size = 14,face = "bold", hjust = 0.5))


p4 <- p + 
  geom_sf(data = subset(sc_shape, MOUD > 0), aes(size = MOUD), color = "purple", alpha = 0.5) +
  ggtitle("Any MOUD Providers") +
  scale_size_continuous(range = c(2, 8), name = "Any MOUD Count") +
  theme_void() + 
  theme(legend.title = element_text(family = "mono",size = 12, face = "bold"),
        legend.position="right",
        legend.text = element_text(family = "mono",size = 12),
        plot.title = element_text(family = "mono",size = 14,face = "bold", hjust = 0.5))

# Display plots
fig <- gridExtra::grid.arrange(p1, p2, p3, p4, nrow = 2)
ggsave(paste0(pathWrite,"MOUDavailabilty_OverdoseRate.jpg"),plot = fig, width = 24, height = 12, bg="white")  

