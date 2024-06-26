# ==============================================================================
# Purpose: Plotting outputs of Paper2_Counties
# Author: Amirreza Sahebi
# Last modified: 06-14-2023
# Input: Outputs of Paper2_Counties
# Output: 
# ==============================================================================




# ================ Required libraries ================
library(dplyr)
library(data.table)
library(ggplot2)
library(ggpattern)
library(maps)
library(scales)
library(tidyverse)
library(sf)
library(ggpattern)



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
# ================================== Read data =================================
# ==============================================================================
# General path of data
pathRead <- "G:/My Drive/North Carolina State University/Project - Opioid 2/Result/"
pathWrite <- "G:/My Drive/North Carolina State University/Project - Opioid 2/Result/"



#============================ #
# Risk Data - Overdose & MOUD #
#============================ #
pathRead <- "G:/My Drive/North Carolina State University/Project - Opioid 2/Data/"
pathWrite <- "G:/My Drive/North Carolina State University/Project - Opioid 2/Result/"

dfOverdose <- fread(paste0(pathRead,"Overdose/VSRR_Provisional_Drug_Overdose_Death_Counts.csv"))
dfOverdose <- subset(dfOverdose, State == "SC" & ((Year == 2021 & Month != "December") | (Year <= 2020)))

dfOverdoseCounty <- fread(paste0(pathRead,"Overdose/VSRR_Provisional_County-Level_Drug_Overdose_Death_Counts_SC_DHEC.csv"))
dfOverdoseCounty[, 3:10] <- lapply(dfOverdoseCounty[, 3:10], as.integer)
dfOverdoseCounty <- dfOverdoseCounty[!is.na(dfOverdoseCounty$Year),]



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


MOUDzip <- fread(paste0(pathRead,"MOUD/MOUDCount_SCzips_Haffajee.csv")) 
MOUDzip <- as.data.frame(MOUDzip)
MOUDcounty <- MOUDzip %>% group_by(County) %>% summarise(SOTP = sum(SOTP),
                                                         NU = sum(NU),
                                                         BU = sum(BU))
# MOUDcounty$MOUD <- 10*MOUDcounty$SOTP + MOUDcounty$NU + MOUDcounty$BU
MOUDcounty$MOUD <- MOUDcounty$SOTP + MOUDcounty$NU + MOUDcounty$BU



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

# --------- #
# Risk Maps #
# --------- #

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


# ----------------------------- #
# Assign high/low risk counties #
# ----------------------------- #
CountyRisk$highlow <- ifelse(CountyRisk$MetricMOUD == "LH","HighRisk","LowRisk")
HighRiskCounties <- unique(CountyRisk$subregion[CountyRisk$highlow == "HighRisk"])



# ------------- #
# Zip level map #
# ------------- #
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
county_data$HighRisk <- county_data$subregion %in% HighRiskCounties




# Create the plot
p <- ggplot() +
  # geom_pattern_polygon(data = county_data, aes(x = long, y = lat, group = group, fill = PerCapitaOpioids, pattern = HighRisk), pattern = 'stripe', color = "black") +
  # geom_polygon(data = county_data, aes(x = long, y = lat, group = group, fill = PerCapitaOpioids), color = "black") +
  geom_polygon(data = county_data, 
               aes(x = long, y = lat, group = group, fill = PerCapitaOpioids, 
                   color = ifelse(HighRisk, "blue", "black")), 
               size = ifelse(county_data$HighRisk, 2, 0.5)) +
  scale_fill_gradient(name = "Per Capita Overdose", low = "red", high = "yellow", trans = "reverse") +
  scale_color_identity() + 
  labs(x = "Longitude",
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
        plot.title = element_text(family = "mono",size = 16,face = "bold", hjust = 0.5))

# Plot BU
p2 <- p + 
  geom_sf(data = subset(sc_shape, BU > 0), aes(size = BU), color = "green", alpha = 0.5) +
  ggtitle("Buprenorphine Providers") +
  scale_size_continuous(range = c(2, 8), name = "Buprenorphine count") +
  theme_void() + 
  theme(legend.title = element_text(family = "mono",size = 12, face = "bold"),
        legend.position="right",
        legend.text = element_text(family = "mono",size = 12),
        plot.title = element_text(family = "mono",size = 16,face = "bold", hjust = 0.5))

# Plot NU
p3 <- p + 
  geom_sf(data = subset(sc_shape, NU > 0), aes(size = NU), color = "blue", alpha = 0.5) +
  ggtitle("Naltrexone Providers") +
  scale_size_continuous(range = c(2, 8), name = "Naltrexone count") +
  theme_void() + 
  theme(legend.title = element_text(family = "mono",size = 12, face = "bold"),
        legend.position="right",
        legend.text = element_text(family = "mono",size = 12),
        plot.title = element_text(family = "mono",size = 16,face = "bold", hjust = 0.5))


p4 <- p + 
  geom_sf(data = subset(sc_shape, MOUD > 0), aes(size = MOUD), color = "purple", alpha = 0.5) +
  ggtitle("Any MOUD Providers") +
  scale_size_continuous(range = c(2, 8), name = "Any MOUD Count") +
  theme_void() + 
  theme(legend.title = element_text(family = "mono",size = 12, face = "bold"),
        legend.position="right",
        legend.text = element_text(family = "mono",size = 12),
        plot.title = element_text(family = "mono",size = 16,face = "bold", hjust = 0.5))

# Display plots
fig <- gridExtra::grid.arrange(p1, p2, p3, p4, nrow = 2)
ggsave(paste0(pathWrite,"MOUDavailabilty_OverdoseRate.jpg"),plot = fig, width = 24, height = 12, bg="white")  





# ------------------------------------ #
# Overdose for high vs. low over years #
# ------------------------------------ #


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
fig <- ggplot(tmp, aes(x = Year, y = Count, fill = RiskStatus, color = 'black')) +
  geom_bar(stat = "summary", position = "dodge") + # Use "identity" if Count is already summarized
  # geom_line(stat = "summary", fun = "mean", linewidth = 1) +
  # geom_point(stat = "summary", fun = "mean", shape = 21, size = 3, fill = "white") +
  facet_wrap(~ Drug, scales = "free") +
  scale_color_manual(values = c("High Risk" = "red", "Low Risk" = "blue")) +
  scale_fill_manual(values = c("High Risk" = "red", "Low Risk" = "blue")) +
  labs(x = "Year", y = "Per Capita Overdose", fill = "Risk Status") +
  theme_bw() +
  # ggtitle("Per Capita Opioid Overdose in SC Counties (2017-2021)") +
  theme(strip.text.x = element_text(size = 14, color = "black", face = "bold",family = "mono"),
        strip.text.y = element_text(size = 14, color = "black", face = "bold",family = "mono"),
        legend.title = element_text(family = "mono",size = 14),
        legend.position="bottom",
        legend.text = element_text(family = "mono",size = 12),
        axis.title.y  = element_text(family = "mono",size = 16, face = "bold"),
        axis.title.y.right = element_text(family = "mono",size = 16, face = "bold"),
        axis.title.x  = element_text(family = "mono",size = 16, face = "bold", hjust = 0.5),
        axis.text.x   = element_text(family = "mono",size = 14, face = "bold"),
        axis.text.y   = element_text(family = "mono",size = 14, face = "bold"),
        plot.title = element_text(family = "mono",size = 12,face = "bold", hjust = 0.5))
ggsave(paste0(pathWrite,"OverdoseCountyRiskYearly.png"), plot = fig, width = 14, height = 8, bg="white")



#================== #
# Prescription Data #
#================== #
pathRead <- "G:/My Drive/North Carolina State University/Project - Opioid 2/Result/"
pathWrite <- "G:/My Drive/North Carolina State University/Project - Opioid 2/Result/"

# Only patient lens, but with interactions

df <- fread(paste0(pathRead,"MonthlyChangePerCapitaPrescription.csv"))
subdf <- df[,-c(3,4,5,6,9)] %>% 
  subset(GroupLabel %in% c("Drug Type","Dispenser Type","Prescriber Location","Dispenser Location")) %>%
  distinct()
subdf$GroupLabel <- factor(subdf$GroupLabel, levels = c("Drug Type","Prescriber Location","Dispenser Type","Dispenser Location"))


for (g in unique(subdf$GroupLabel)){
  tmp <- subdf[subdf$GroupLabel == g,]
  fig <- ggplot(tmp, aes(x = Group, y = ChangePctOverall, fill = SignOverall, pattern = SigOverall)) +
    geom_bar_pattern(position = "dodge",
                     stat = "identity",width = 0.5,alpha=0.2,
                     color = "black", 
                     pattern_fill = "black",
                     pattern_angle = 45,
                     pattern_density = 0.1,
                     pattern_spacing = 0.025,
                     pattern_key_scale_factor = 0.6, na.rm = T) +
    geom_label(aes(label = paste0(round(ChangePctOverall*100,2),
                                  " %",
                                  ifelse(PvalueOverall <= 0.001, "***",
                                         ifelse(PvalueOverall <= 0.01, "**",
                                                ifelse(PvalueOverall <= 0.05, "*",""))))),
               position=position_dodge(width=0.5),size=6,color='black',angle = 0,fill = "white",
               vjust = ifelse(tmp$ChangePctOverall >= 0, 0, 1), na.rm = T) +
    geom_hline(yintercept = 0, linetype = "dashed", linewidth = 1, color = "gray") +
    # facet_wrap(~GroupLabel, scales = "free", ncol = 2) +
    scale_pattern_manual(values = c("Positive-Significant" = "stripe", "Negative-Significant" = "stripe", "Non-Significant" = "none")) +
    scale_fill_manual(values = c("Negative" = "red", "Positive" = "blue")) +
    scale_y_continuous(labels = percent, expand = expansion(mult = c(0.1, 0.2))) +
    labs(x = g, y = "Percentage of Changes in Prescription Per Capita") +
    guides(fill = guide_legend(title = "Sign"), 
           pattern = guide_legend(title = "Significance")) +
    theme_bw() +
    # ggtitle(paste0("High Risk Counties vs. Low Risk Counties")) + 
    theme(strip.text.x = element_text(size = 10, color = "black", face = "bold",family = "mono"),
          strip.text.y = element_text(size = 10, color = "black", face = "bold",family = "mono"),
          legend.title = element_text(family = "mono",size = 10),
          legend.position="bottom",
          legend.text = element_text(family = "mono",size = 10),
          axis.title.y  = element_text(family = "mono",size = 14, face = "bold"),
          axis.title.y.right = element_text(family = "mono",size = 16, face = "bold"),
          axis.title.x  = element_text(family = "mono",size = 16, face = "bold", hjust = 0.5),
          axis.text.x   = element_text(family = "mono",size = 14, face = "bold"),
          axis.text.y   = element_text(family = "mono",size = 14, face = "bold"),
          plot.title = element_text(family = "mono",size = 12,face = "bold", hjust = 0.5),
          panel.background = element_rect(fill = "white"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()) 
  ggsave(paste0(pathWrite,"OveralChangePerCapitaPrescription_",gsub(" ","",g),".png"), plot = fig, width = 14, height = 8, bg="white")
}



fig <- ggplot(subdf, aes(x = Group, y = ChangePctOverall, fill = SignOverall, pattern = SigOverall)) +
  geom_bar_pattern(position = "dodge",
                   stat = "identity",width = 0.5,alpha=0.2,
                   color = "black", 
                   pattern_fill = "black",
                   pattern_angle = 45,
                   pattern_density = 0.1,
                   pattern_spacing = 0.025,
                   pattern_key_scale_factor = 0.6, na.rm = T) +
  geom_label(aes(label = paste0(round(ChangePctOverall*100,2),
                                " %",
                                ifelse(PvalueOverall <= 0.001, "***",
                                       ifelse(PvalueOverall <= 0.01, "**",
                                              ifelse(PvalueOverall <= 0.05, "*",""))))),
             position=position_dodge(width=0.5),size=4,color='black',angle = 0,fill = "white",
             vjust = ifelse(subdf$ChangePctOverall >= 0, 0, 1), na.rm = T) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 1, color = "gray") +
  facet_wrap(~GroupLabel, scales = "free", ncol = 2) +
  scale_pattern_manual(values = c("Positive-Significant" = "stripe", "Negative-Significant" = "stripe", "Non-Significant" = "none")) +
  scale_fill_manual(values = c("Negative" = "red", "Positive" = "blue")) +
  scale_y_continuous(labels = percent, expand = expansion(mult = c(0.1, 0.2))) +
  labs(x = g, y = "Percentage of Changes in Prescription Per Capita") +
  guides(fill = guide_legend(title = "Sign"), 
         pattern = guide_legend(title = "Significance")) +
  theme_bw() +
  # ggtitle(paste0("High Risk Counties vs. Low Risk Counties")) + 
  theme(strip.text.x = element_text(size = 10, color = "black", face = "bold",family = "mono"),
        strip.text.y = element_text(size = 10, color = "black", face = "bold",family = "mono"),
        legend.title = element_text(family = "mono",size = 10),
        legend.position="bottom",
        legend.text = element_text(family = "mono",size = 10),
        axis.title.y  = element_text(family = "mono",size = 12, face = "bold"),
        axis.title.y.right = element_text(family = "mono",size = 14, face = "bold"),
        axis.title.x  = element_text(family = "mono",size = 14, face = "bold", hjust = 0.5),
        axis.text.x   = element_text(family = "mono",size = 12, face = "bold"),
        axis.text.y   = element_text(family = "mono",size = 12, face = "bold"),
        plot.title = element_text(family = "mono",size = 12,face = "bold", hjust = 0.5),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) 
ggsave(paste0(pathWrite,"OveralChangePerCapitaPrescription.png"), plot = fig, width = 14, height = 8, bg="white")


# df <- fread(paste0(pathRead,"OveralChangePerCapitaPrescription_PatientLens.csv"))
# df$ChangePct[is.na(df$ChangePct)] <- 0
# df$Pvalue[is.na(df$Pvalue)] <- 1
# df$Significant <- ifelse(df$Pvalue <= 0.05, "Significant","Non-Significant")
# df$Sign <- ifelse(df$ChangePct <= 0, "Negative","Positive")
# 
# DrugLevels = c("Hydrocodone","Oxycodone","Fentanyl","Methadone","All")
# PharmacyLevels = c("Chain", "Retail","All")
# subdf <- df %>% rowwise() %>%
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
# 



# # All players lens, but no interactions
# grouplabel = "Drug"
# df <- fread(paste0(pathRead,"YearlyChangePerCapitaPrescription_",grouplabel,".csv"))
# 
# 
# 
# lensgroups = c("Patient","Prescriber","Dispenser")
# time = c("Year","Month")
# depvar = "PrescriptionperCapita"
# if (grouplabel == "Location"){
#   group = "class2"
#   GroupLevels = c("PaI_PrI_DiI","PaI_PrO_DiI","PaI_PrI_DiO","PaI_PrO_DiO")
#   GroupLabels = c("PaI_PrI_DiI" = "Patients, Prescribers and Dispensers In State",
#                   "PaI_PrO_DiI" = "Only Prescribers Out of State",
#                   "PaI_PrI_DiO" = "Only Dispensers Out of State",
#                   "PaI_PrO_DiO" = "Prescribers and Dispensers Out of State")
# } else if (grouplabel == "Drug"){
#   group = "drug_class"
#   GroupLevels = c("Hydrocodone","Oxycodone","Fentanyl","Methadone")
#   GroupLabels = c("Hydrocodone" = "Hydrocodone", "Oxycodone" = "Oxycodone",
#                   "Fentanyl" = "Fentanyl","Methadone" = "Methadone")
# } else if (grouplabel == "Pharmacy"){
#   group = "dispenser_class"
#   GroupLevels = c("Chain", "Retail", "Mail Order")
#   GroupLabels = c("Chain" = "Chain", "Retail" = "Retail","Mail Order" = "Mail Order")
# } else if (grouplabel == "PharmacyDrug"){
#   group = "dispenser_drug_class"
#   GroupLevels = c("Chain - Hydrocodone", "Chain - Oxycodone",
#                   "Chain - Fentanyl", "Chain - Methadone",
#                   "Retail - Hydrocodone", "Retail - Oxycodone",
#                   "Retail - Fentanyl", "Retail - Methadone",
#                   "Mail Order - Hydrocodone", "Mail Order - Oxycodone",
#                   "Mail Order - Fentanyl", "Mail Order - Methadone")
#   GroupLabels = c("Chain - Hydrocodone" = "Chain - Hydrocodone",
#                   "Chain - Oxycodone" = "Chain - Oxycodone",
#                   "Chain - Fentanyl" = "Chain - Fentanyl",
#                   "Chain - Methadone" = "Chain - Methadone",
#                   "Retail - Hydrocodone" = "Retail - Hydrocodone",
#                   "Retail - Oxycodone" = "Retail - Oxycodone",
#                   "Retail - Fentanyl" = "Retail - Fentanyl",
#                   "Retail - Methadone" = "Retail - Methadone",
#                   "Mail Order - Hydrocodone" = "Mail Order - Hydrocodone",
#                   "Mail Order - Oxycodone" = "Mail Order - Oxycodone",
#                   "Mail Order - Fentanyl" = "Mail Order - Fentanyl",
#                   "Mail Order - Methadone" = "Mail Order - Methadone")
# }
# subdf <- df[df$Group %in% GroupLevels,]
# subdf$Group <- factor(subdf$Group, levels = GroupLevels)
# subdf$Lens <- factor(subdf$Lens, levels = c("Patient","Prescriber","Dispenser"))
# 
# 
# fig <- ggplot(subdf, aes(x = Group, y = ChangePctOverall, group = Lens, fill = Lens, pattern = SigOverall)) +
#   # geom_bar(position = "dodge", stat = "identity",width = 0.5,alpha=0.2,color = "black") +
#   geom_bar_pattern(position = "dodge",
#                    # position = position_dodge(preserve = "single"),
#                    stat = "identity",width = 0.5,alpha=0.2,color = "black",
#                    color = "black", 
#                    pattern_fill = "black",
#                    pattern_angle = 45,
#                    pattern_density = 0.1,
#                    pattern_spacing = 0.025,
#                    pattern_key_scale_factor = 0.6) + 
#   geom_label(aes(label = paste0(round(ChangePctOverall*100,2),
#                                 " %",
#                                 ifelse(PvalueOverall <= 0.001, "***",
#                                        ifelse(PvalueOverall <= 0.01, "**",
#                                               ifelse(PvalueOverall <= 0.05, "*",""))))),
#              position=position_dodge(width=0.5),size=3,color='black',angle = 0,fill = "white",
#              vjust = ifelse(subdf$ChangePctOverall >= 0, 0, 1)) +
#   geom_hline(yintercept = 0, linetype = "dashed", linewidth = 1, color = "gray") +
#   scale_pattern_manual(values = c("Negative-Significant" = "stripe", "Positive-Significant" = "stripe", "Non-Significant" = "none")) +
#   scale_y_continuous(labels = percent) +
#   scale_x_discrete(labels = GroupLabels)+
#   labs(x = grouplabel, y = "Percentage of Changes in Prescription Per Capita") +
#   guides(fill = guide_legend(title = "Player"), 
#          # color = guide_legend(title = "Player"),
#          pattern = guide_legend(title = "Sig.")) +
#   theme_bw() +
#   ggtitle(paste0("High Risk Counties vs. Low Risk Counties")) + 
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
#         plot.title = element_text(family = "mono",size = 12,face = "bold", hjust = 0.5),
#         panel.background = element_rect(fill = "white"),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank())
# ggsave(paste0(pathWrite,"YearlyChangePerCapitaPrescription_",grouplabel,".png"), plot = fig, width = 16, height = 8, bg="white")

