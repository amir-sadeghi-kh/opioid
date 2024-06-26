# ==============================================================================
# Warning:  This file is not clean. PLease use Paper2_County file
# ==============================================================================

# ==============================================================================
# Purpose: Correlation between mortality and prescription volume
# Author: Amirreza Sahebi
# Last modified: 03.29.23
# Input: SCRIPT processed / 
# Output: Stats
# ==============================================================================



# ==============================================================================
#=============================  Required libraries =============================
# ==============================================================================
# library(plyr)
library(ggplot2)
library(data.table)
library(hrbrthemes)
library(viridis)
# library(plotly)
library(heatmaply)
library(catmaply)
library(multcompView)
library(stringr)
library(lubridate)
library(tidyr)
library(zoo)
library(scales)
library(gridExtra)
library(grid)
library(png)
library(dplyr)
library(foreach)
library(doParallel)

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

# Plot function
plot_noclass <- function(df,DoctorShopingType,path,text,label){
  fig_list <- list()
  counter = 1
  for (ii in 0:1){
    if (DoctorShopingType == 0){
      subdf <- df[which(df$DoctorShopping0 == ii),]
    } else if(DoctorShopingType == 1){
      subdf <- df[which(df$DoctorShopping1 == ii),]
    } else if(DoctorShopingType == 2){
      subdf <- df[which(df$DoctorShopping2 == ii),]
    }
    sec <- with(subdf, train_sec(OverdosePredcited, MMEperDay))
    color1 <- "red"
    color2 <- "blue"
    res <- cor.test(subdf$OverdosePredcited,subdf$MMEperDay)
    if (res$p.value > 0.1){
      res <- paste0(round(res$estimate,2))
    } else if (res$p.value > 0.05){
      res <- paste0(round(res$estimate,2),"(.)")
    } else if (res$p.value > 0.01){
      res <- paste0(round(res$estimate,2),"(*)")
    } else if (res$p.value > 0.001){
      res <- paste0(round(res$estimate,2),"(**)")
    } else{
      res <- paste0(round(res$estimate,2),"(***)")
    }
    textdata <- as.data.frame(subdf %>% summarize(ylevel = quantile(OverdosePredcited,0.90), yvalue = res))
    fig <- ggplot(subdf, aes(x = Date)) +
      geom_point(aes(y = OverdosePredcited), color = color1, shape = "square", size = 1) +
      geom_line(aes(y = OverdosePredcited), color = color1, size=0.7, linetype = "dashed") +
      geom_point(aes(y = sec$fwd(MMEperDay)), color = color2, shape = "triangle", size = 1) +
      geom_line(aes(y = sec$fwd(MMEperDay)), color = color2, size=0.7, linetype = "dashed") +
      scale_y_continuous(name = "Overdose Count", sec.axis = sec_axis(~sec$rev(.), name = "Daily MME Avg.")) +
      theme_ipsum() +
      scale_fill_manual()+
      labs(x = "Month") +
      geom_label(data = textdata, aes(label = paste0("cor = ",yvalue),
                                      y = ylevel, x = as.yearmon("201901","%Y %m")),
                 parse = FALSE, hjust = 0, size = 4, fontface = "bold",
                 family = 'mono', color = 'brown3') +
      theme(strip.text.x = element_text(size = 8, color = "black", face = "bold",family = "mono"),
            strip.text.y = element_text(size = 8, color = "black", face = "bold",family = "mono"),
            legend.title = element_text(family = "mono",size = 10),
            legend.position="bottom",
            legend.text = element_text(family = "mono",size = 10),
            axis.line.x = element_line(color="black", size = 0.8),
            axis.line.y = element_line(color="black", size = 0.8),
            axis.title.y  = element_text(family = "mono",size = 12, face = "bold", color = color1),
            axis.title.y.right = element_text(family = "mono",size = 12, face = "bold", color = color2),
            axis.title.x  = element_text(family = "mono",size = 12, face = "bold", hjust = 0.5),
            axis.text.x   = element_text(family = "mono",size = 10, face = "bold"),
            axis.text.y   = element_text(family = "mono",size = 10, face = "bold"),
            plot.title = element_text(family = "mono",size = 12,face = "bold", hjust = 0.5)) +
      ggtitle(ifelse(ii == 0, "No Shopping","Doctor Shopping"))
    fig_list[[counter]] <- fig
    counter = counter + 1
  }
  fig <- grid.arrange(arrangeGrob(fig_list[[1]]+ theme(legend.position="none"),
                                  fig_list[[2]]+ theme(legend.position="none"),
                                  nrow = 1), ncol = 1,
                      top =textGrob(text,gp=gpar(fontsize=14, font=4)))
  ggsave(paste0(path,label), plot = fig, width = 14, height = 8, bg="white")
}
# ==============================================================================
# ================================== Read data =================================
# ==============================================================================
# General path of data
pathRead <- "/home/asahebi/Data/"
pathWrite <- "/home/asahebi/Opioid 2/Result/"

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
df[which(df$Patient_State == "SC" & df$Dispenser_State == "SC" & df$Prescriber_State == "SC"),"class2"] <- "PaI_PrI_DiI"
df[which(df$Patient_State != "SC" & df$Dispenser_State == "SC" & df$Prescriber_State == "SC"),"class2"] <- "PaO_PrI_DiI"
df[which(df$Patient_State == "SC" & df$Dispenser_State != "SC" & df$Prescriber_State == "SC"),"class2"] <- "PaI_PrO_DiI"
df[which(df$Patient_State == "SC" & df$Dispenser_State == "SC" & df$Prescriber_State != "SC"),"class2"] <- "PaI_PrI_DiO"
df[which(df$Patient_State != "SC" & df$Dispenser_State != "SC" & df$Prescriber_State == "SC"),"class2"] <- "PaO_PrO_DiI"
df[which(df$Patient_State != "SC" & df$Dispenser_State == "SC" & df$Prescriber_State != "SC"),"class2"] <- "PaO_PrI_DiO"
df[which(df$Patient_State == "SC" & df$Dispenser_State != "SC" & df$Prescriber_State != "SC"),"class2"] <- "PaI_PrO_DiO"
df[which(df$Patient_State != "SC" & df$Dispenser_State != "SC" & df$Prescriber_State != "SC"),"class2"] <- "PaO_PrO_DiO"


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
dfPop$County[dfPop$County == "Mccormick"] <- "McCormick"


# ==============================================================================
# ================================== Data Prep =================================
# ==============================================================================

#=================#
# Doctor Shopping #
#=================#
# Defined as >= 4 prescriber and >= 4 dispensers in a year for a drug (opioid)

dfDoctorShopping <- df %>% drop_na(consolidation_identifier) %>% group_by(consolidation_identifier,DrugClass, year(date)) %>% 
  summarize(PrescriberCount = n_distinct(prescriber_dea_number),
            DispenserCount = n_distinct(dispenser_dea_number)) 
dfDoctorShopping$patientIDyeardrugclass <-  paste0(dfDoctorShopping$consolidation_identifier," - ",dfDoctorShopping$`year(date)`, " - ", dfDoctorShopping$DrugClass)
DcotorShoppingList0 <- list(unique(dfDoctorShopping$patientIDyeardrugclass[which(dfDoctorShopping$PrescriberCount >= 5 & dfDoctorShopping$DispenserCount >= 5)]))

dfDoctorShopping <- df %>% drop_na(consolidation_identifier) %>% group_by(consolidation_identifier,drug_class, year(date)) %>% 
  summarize(PrescriberCount = n_distinct(prescriber_dea_number),
            DispenserCount = n_distinct(dispenser_dea_number)) 
dfDoctorShopping$patientIDyeardrug <-  paste0(dfDoctorShopping$consolidation_identifier," - ",dfDoctorShopping$`year(date)`, " - ", dfDoctorShopping$drug_class)
DcotorShoppingList1 <- list(unique(dfDoctorShopping$patientIDyeardrug[which(dfDoctorShopping$PrescriberCount >= 5 & dfDoctorShopping$DispenserCount >= 5)]))

df$DoctorShopping0 <- 0
df$patientIDyeardrugclass <- paste0(df$consolidation_identifier," - ",year(df$date), " - ", df$DrugClass)
df$DoctorShopping0[df$patientIDyeardrugclass %in% DcotorShoppingList0[[1]]] <- 1
df$DoctorShopping1 <- 0
df$patientIDyeardrug <-  paste0(df$consolidation_identifier," - ",year(df$date), " - ", df$drug_class)
df$DoctorShopping1[df$patientIDyeardrug %in% DcotorShoppingList1[[1]]] <- 1


# More precised way of defining doctor shopping! Check it later...
# # Create a function that for each row, looks ahead for 365 days and counts unique doctors and pharmacies
# count_ahead <- function(id, date, class, prescriber, pharmacy) {
#   subset <- df %>%
#     filter(consolidation_identifier == id & DrugClass == class, date >= date & date <= (date + days(365)))
#   unique_docs <- length(unique(subset$prescriber_dea_number))
#   unique_pharmacies <- length(unique(subset$dispenser_dea_number))
#   return(list(unique_docs, unique_pharmacies))
# }
# 
# # Apply the function
# shopping_list <- mapply(count_ahead, 
#                         df$consolidation_identifier, 
#                         df$date, 
#                         df$DrugClass,
#                         df$prescriber_dea_number,
#                         df$dispenser_dea_number)
# 
# shopping_df <- data.frame(do.call(rbind, shopping_list))
# colnames(shopping_df) <- c("unique_docs", "unique_pharmacies")


#================#
# Merge with Pop #
#================#
df$Year <- year(df$date)
df <- merge(df,dfPop,by.x = c("Patient_County","Year"), by.y = c("County","Year"), all.x = T)


#===============================================================================
#================================ Data Analysis ================================
#===============================================================================


#===================================#
# Pharmacy Summary - Opioid 0 paper #
#===================================#
# This part is for paper opioid 0 (SC opioid)
tmp <- df %>% group_by(class) %>% mutate(MME = sum(total_prescription_mme)) %>% ungroup() 
tmp <- as.data.frame(tmp)
tmp <- tmp %>%
  group_by(class,dispenser_class) %>% 
  summarize(Share = round(sum(total_prescription_mme)/unique(MME)*100,2)) %>%
  arrange(class, desc(Share))

disp_class <- c("Chain", "Retail", "Mail Order")
tmp <- subset(tmp, dispenser_class %in% disp_class)
tmp$class <- factor(tmp$class, levels = c("00","01","02","03","10","11","12","13",
                                          "20","21","22","23","30","31","32","33"), ordered = TRUE)
# tmp <- tmp[order(tmp$class, -tmp$Share),]
tmp$dispenser_class <- factor(tmp$dispenser_class, disp_class, ordered = TRUE)
p <- ggplot(subset(tmp, dispenser_class %in% disp_class), aes(x = class, y = Share, fill = dispenser_class)) + 
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("#609EA2", "#C92C6D", "#332C39","#FDD36A")) +
  labs(x = "Class", y = "Share") +
  theme_bw() +
  coord_flip() +
  theme(strip.text.x = element_text(size = 12, color = "black", face = "bold",family = "mono"),
        strip.text.y = element_text(size = 12, color = "black", face = "bold",family = "mono"),
        legend.title = element_text(family = "mono",size = 14, face = "bold"),
        legend.text = element_text(family = "mono",size = 14, face = "bold"),
        legend.position="right",
        axis.title.y  = element_text(family = "mono",size = 14, face = "bold"),
        axis.title.x  = element_text(family = "mono",size = 14, face = "bold"),
        axis.text.x   = element_text(family = "mono",size = 12, face = "bold"),
        axis.text.y   = element_text(family = "mono",size = 12, face = "bold"),
        plot.title = element_text(family = "mono",size = 12,face = "bold", hjust = 0.5))
ggsave(paste0(pathWrite,"PharmacPrescription.jpg"),p,width = 12,height = 8)



#============================#
# Prescription Data Analysis #
#============================#

#-------------------------------------#
# Summary info. on opioids vs. benzos #
#-------------------------------------#
  
df %>% group_by(Year,DrugClass) %>%
  summarise(Count = n() / sum(unique(T), na.rm = T)*1e5,
            MME = sum(total_prescription_mme,na.rm = T)/ sum(unique(T), na.rm = T)*1e5,
            Quantity = sum(quantity,na.rm = T)/ sum(unique(T))*1e5)
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
print(paste0("Total number of opioid prescription between 2017 to 2021 in SC is: ", round(nrow(df[df$DrugClass == "Opioid" & df$Year >= 2017,]))))
# "Total number of opioid prescription is: 13203802"

#--------------------------------------------------------#
# Summary info. on opioids by drug class and patient age #
#--------------------------------------------------------#
df$patient_age <- df$Year - df$patient_birth_year
df$patient_age <- ifelse(df$patient_age <= 4, '0-4',
                         ifelse(df$patient_age <= 17,'5-17',
                                ifelse(df$patient_age <= 64,'18-64','65+')))
table(df$patient_age) / nrow(df)*100
#     0-4      18-64       5-17        65+ 
# 0.0546381 65.1074980  1.1786218 33.6592421 

prescriptionbydrug <- df %>% group_by(Year,drug_class) %>%
  summarise(Count = n())

tmp1 <- dfPop[dfPop$County == "South Carolina",c(1:2,7)]
prescriptionbydrug <- merge(prescriptionbydrug,tmp1, by = "Year", all.x = T)

prescriptionbydrug <- prescriptionbydrug %>% group_by(Year,drug_class) %>%
  summarise(Count = round(Count/T*100000,2))

prescriptionbydrug <- prescriptionbydrug %>%
  pivot_wider(
  id_cols = c("drug_class"),
  names_from = "Year",
  values_from = c("Count"),
  names_prefix = "",
  names_sep = "")
prescriptionbydrug$Change <- round((prescriptionbydrug$`2021` - prescriptionbydrug$`2017`)/prescriptionbydrug$`2017`*100,2)
fwrite(prescriptionbydrug,paste0(pathWrite,"PrescriptionRateDrug.csv"))

#=============================================#
# Mean daily dosage in MMEs by type of opioid #
#=============================================#
dailydosagebydrug <- df[!is.na(df$`MME/Day`),] %>% group_by(Year,drug_class) %>%
  summarize(MMEperDay = mean(`MME/Day`, na.rm = T),
            Above100 = sum(`MME/Day` >= 100),
            N = n())
dailydosagebydrug <- dailydosagebydrug %>% mutate_if(is.numeric, round, 2)
tmp <- dailydosagebydrug %>% group_by(Year) %>%
  summarise(Above100_sum = sum(Above100), N_sum = sum(N)) %>%
  mutate(drug_class = "Above100") %>%
  mutate(Percentage = round(Above100_sum / N_sum * 100, 2)) %>%
  select(Year, drug_class, Percentage)
dailydosagebydrug <- dailydosagebydrug[,1:3]
names(dailydosagebydrug)[3] <- "value"
names(tmp)[3] <- "value"
dailydosagebydrug <- rbind(tmp, dailydosagebydrug) %>%
  arrange(Year, row_number()) %>%
  pivot_wider(
    id_cols = c("drug_class"),
    names_from = "Year",
    values_from = c("value"),
    names_prefix = "",
    names_sep = "")
dailydosagebydrug$Change <- round((dailydosagebydrug$`2021` - dailydosagebydrug$`2015`)/dailydosagebydrug$`2015`*100,2)
fwrite(dailydosagebydrug,paste0(pathWrite,"DailyDosageRateAgeDrug.csv"))

df %>% group_by(Year) %>% summarise(MMEperDay = mean(`MME/Day`,na.rm = T))



#========================================#
# Overdose-Prescription Volume by County #
#========================================#
# load necessary packages
library(ggplot2)
library(dplyr)
library(maps)
library(ggrepel)

# County overdose and volume table
dfCounty <- dfOverdoseCounty[,c("Year", "County", "Opioids", "Fentanyl")]
names(dfCounty) <- c("Year","County","OverdoseOpioid","OverdoseFentanyl")

# Group by sth to be used as a feature for counties with high overdose
# tested and not good: dispenser state, prescriber state, doctor shopping
# teste good: dispenser_class
tmp <- df %>% group_by(Year,Patient_County,dispenser_class,drug_class) %>%  
  mutate(dispenser_class = ifelse(dispenser_class == "Mail Order","Mail","NonMail")) %>% 
  summarise(Count = n(),
            MME = sum(total_prescription_mme,na.rm = T),
            MMEperDay = mean(`MME/Day`,na.rm = T),
            Quantity = sum(quantity, na.rm = T)) %>%
  pivot_wider(id_cols = c("Year", "drug_class", "Patient_County"),
              names_from = dispenser_class,
              values_from = c("Count", "MME", "MMEperDay", "Quantity"),
              names_prefix = "",
              names_sep = "_",
              values_fill = 0,
              names_repair = "universal") %>%
  mutate(Count_Tot = Count_Mail + Count_NonMail,
         MME_Tot = MME_Mail + MME_NonMail,
         MMEperDay_Tot = MMEperDay_Mail + MMEperDay_NonMail,
         Quantity_Tot = Quantity_Mail + Quantity_NonMail)

dfCounty <- merge(dfCounty,subset(tmp, drug_class == "Fentanyl"), 
                  by.x = c("Year","County"), by.y = c("Year","Patient_County"), all.x =T)
dfCounty <- dfCounty %>% select(-drug_class)
dfCounty <- dfCounty %>%
  rename_with(~gsub("^Count", "Records", .x), starts_with("Count_")) %>%
  rename_with(~gsub("_", "", .x), contains("_")) %>%
  rename_with(~paste0("Fentanyl", .x), ends_with("Mail") | ends_with("NonMail") | ends_with("Tot"))

tmp <- subset(tmp, drug_class != "Benzos") %>% group_by(Year,Patient_County) %>%
  summarise(across(starts_with("Count"), sum, na.rm =T),
            across(starts_with("MME"), sum, na.rm =T),
            across(starts_with("Quantity"), sum, na.rm =T),
            across(starts_with("MMEperDay"), mean, na.rm =T)) %>%
  rename_with(~gsub("_", "", .x), contains("_")) %>%
  rename_with(~gsub("^Count", "Records", .x), starts_with("Count")) %>%
  rename_with(~paste0("Opioid", .x), ends_with("Mail") | ends_with("NonMail") | ends_with("Tot"))

dfCounty <- merge(dfCounty,tmp, 
                  by.x = c("Year","County"), by.y = c("Year","PatientCounty"), all.x =T)

dfCounty <- merge(dfCounty,dfPop[,c(1,2,7)], 
                  by = c("Year","County"), all.x =T)
dfCounty <- dfCounty %>% mutate(across(names(.)[grep("^Overdose|MME[^p]|Records|Quantity", names(.))], ~ . / T*10000))
dfCounty <- dfCounty %>% select(-T)
dfCounty <- dfCounty %>% 
  group_by(County) %>% 
  summarise(across(names(.)[grep("^Overdose|MME[^p]|Records|Quantity", names(.))], sum, na.rm = T),
            across(names(.)[grep("^MMEperDay", names(.))], mean, na.rm = T))


# Calculate correlation coefficients and p-values
correlations <- apply(dfCounty[, 4:21], 2, function(x) cor.test(dfCounty$OverdoseOpioid, x)$estimate)
p_values <- apply(dfCounty[, 4:21], 2, function(x) cor.test(dfCounty$OverdoseOpioid, x)$p.value)
result <- cbind(correlations, p_values)
colnames(result) <- c("Correlation", "P-value")
rownames(result) <- colnames(dfCounty[, 4:21])
print(result)



dfall <- dfOverdoseCounty
dfall <- merge(dfall, dfPop[,c(1,2,7)], by.x = c("County","Year"), by.y = c("County","Year"), all.x = T)
divide_by_divisor <- function(column, divisor) {
  return(column / divisor*10000)
}
dfall[,3:10] <- as.data.frame(lapply(dfall[, 3:10], divide_by_divisor, divisor = dfall$T))
dfall <- dfall %>% group_by(County) %>% summarise(across(3:10, sum, na.rm = TRUE))

# merge county data with map data
map_data <- map_data("county", "south carolina")
map_data$subregion <- str_to_title(map_data$subregion)
dfall <- left_join(map_data, dfall, by = c("subregion" = "County"))

# create map for each year
textdata <- map_data %>% group_by(subregion) %>% summarise(lat = mean(lat), long = mean(long))
mapall <- ggplot(dfall) + 
  geom_polygon(aes(x = long, y = lat, group = group, fill = Opioids)) + 
  geom_label(aes(x = long, y = lat, label = subregion), data = textdata, size = 3, color = "black", vjust = 0.5, hjust = 0.5) +
  scale_fill_gradient(low = "white", high = "red", name = "Opioids") +
  theme_void() + 
  ggtitle("Per capita opioid overdose death") + 
  theme(legend.title = element_text(family = "mono",size = 12),
        legend.position="right",
        legend.text = element_text(family = "mono",size = 12),
        plot.title = element_text(family = "mono",size = 12,face = "bold", hjust = 0.5))
ggsave(paste0(pathWrite,"Overdose_County.jpg"), plot = mapall, width = 12, height = 8, bg="white")


# State average
tmp <- dfOverdoseCounty %>% group_by(Year) %>% summarise(across(3:9, sum, na.rm = T))
tmp <- merge(tmp, dfPop[dfPop$County == "South Carolina",c(2,7)], by = "Year", all.x = T)
tmp[,2:8] <- tmp[,2:8] / tmp$T*10000
tmp <- subset(tmp, select=-c(Year,T))
tmp <- colSums(tmp, na.rm = T)
# Overdose deaths per 10000 residents
# Prescription Drugs            Opioids   Psychostimulants           Fentanyl             Heroin          Methadone            Cocaine 
# 11.4701750         10.8645637          4.0929906          7.6939878          1.7346322          0.4786646          2.9596718 



# Prescription volume
dfall <- df %>% group_by(Patient_County, Year) %>% 
  summarise(Count = n(),
            MME = sum(total_prescription_mme, na.rm = T),
            MMEperDay = mean(`MME/Day`,na.rm=T)) 
dfall <- merge(dfall, dfPop[,c(1,2,7)], by.x = c("Patient_County", "Year"), by.y = c("County","Year"), all.x = T)
dfall[,c(3:5)] <- dfall[,c(3:4)] / dfall$T
dfall <- subset(dfall,select = -c(Year,T))
dfall <- dfall %>% group_by(Patient_County) %>% summarise(across(1:3,sum,na.rm=T))
map_data <- map_data("county", "south carolina")
map_data$subregion <- str_to_title(map_data$subregion)
dfall <- left_join(map_data, dfall, by = c("subregion" = "Patient_County"))
textdata <- map_data %>% group_by(subregion) %>% summarise(lat = mean(lat), long = mean(long))
mapall <- ggplot(dfall) + 
  geom_polygon(aes(x = long, y = lat, group = group, fill = MMEperDay)) + 
  geom_label(aes(x = long, y = lat, label = subregion), data = textdata, size = 3, color = "black", vjust = 0.5, hjust = 0.5) +
  scale_fill_gradient(low = "white", high = "red", name = "MMEperDay") +
  theme_void() + 
  ggtitle("Per 10000 residents opioid prescription rate") + 
  theme(legend.title = element_text(family = "mono",size = 12),
        legend.position="right",
        legend.text = element_text(family = "mono",size = 12),
        plot.title = element_text(family = "mono",size = 12,face = "bold", hjust = 0.5))
# ggsave(paste0(pathWrite,"Overdose_County.jpg"), plot = mapall, width = 12, height = 8, bg="white")













#==============================================================================#
#============================= Overdose Data Analysis =========================#
#==============================================================================#
# Monthly - Overdose data
subdf <- subset(dfOverdose, State == "SC" & !(Indicator %in% c("Number of Deaths","Percent with drugs specified")))
subdf$Month <- match(subdf$Month,month.name)
subdf <- subdf[,c("Year","Month","Indicator","Data Value","Predicted Value")]
colnames(subdf) <- c("Year","Month","Indicator","OverdoseReported","OverdosePredcited")
subdf$Date <- as.yearmon(paste(subdf$Year, subdf$Month), "%Y %m")
subdf[,c("OverdosePredcited","OverdoseReported")] <- lapply(subdf[,c("OverdosePredcited","OverdoseReported")],function(x){as.numeric(gsub(",", "", x))})
IndicatorList <- c("Opioids (T40.0-T40.4,T40.6)",
                   "Natural & semi-synthetic opioids (T40.2)",
                   "Synthetic opioids, excl. methadone (T40.4)",
                   "Methadone (T40.3)")
subdf <- subdf %>% filter(Indicator %in% IndicatorList)
subdf <- subdf %>% mutate(Indicator = factor(Indicator, levels = IndicatorList))
wrapit <- function(text) {
  wtext <- paste(strwrap(text,width=30),collapse=" \n ")
  return(wtext)
}

subdf$Indicator <- llply(subdf$Indicator, wrapit)
subdf$Indicator <- unlist(subdf$Indicator)

fig <- ggplot(subset(subdf, Indicator == "Opioids (T40.0-T40.4,T40.6)"), aes(x = Date)) +
  # geom_point(aes(y = OverdosePredcited, color = Indicator), shape = "square", size = 1) +
  geom_line(aes(y = OverdosePredcited), size=0.7) +
  # facet_wrap(~Indicator, scales = "free_y") +
  theme_ipsum() +
  scale_fill_manual()+
  labs(x = "Month") +
  theme(strip.text.x = element_text(size = 8, color = "black", face = "bold",family = "mono"),
        strip.text.y = element_text(size = 8, color = "black", face = "bold",family = "mono"),
        legend.title = element_text(family = "mono",size = 10),
        legend.position="bottom",
        legend.text = element_text(family = "mono",size = 10),
        axis.line.x = element_line(color="black", size = 0.8),
        axis.line.y = element_line(color="black", size = 0.8),
        axis.title.y  = element_text(family = "mono",size = 12, face = "bold", color = "black"),
        axis.title.x  = element_text(family = "mono",size = 12, face = "bold", hjust = 0.5),
        axis.text.x   = element_text(family = "mono",size = 8, face = "bold",angle = 90),
        axis.text.y   = element_text(family = "mono",size = 10, face = "bold"),
        plot.title = element_text(family = "mono",size = 12,face = "bold", hjust = 0.5)) +
  ggtitle("12 Month-ending Provisional Number of Opioid Overdose Deaths: South Carolina")
ggsave(paste0(pathWrite,"Overdose_Monthly_AllOpioids.jpg"), plot = fig, width = 12, height = 8, bg="white")

fig <- ggplot(subdf, aes(x = Date)) +
  # geom_point(aes(y = OverdosePredcited, color = Indicator), shape = "square", size = 1) +
  geom_line(aes(y = OverdosePredcited), size=0.7) +
  facet_wrap(~Indicator, scales = "free_y") +
  theme_ipsum() +
  scale_fill_manual()+
  labs(x = "Month") +
  theme(strip.text.x = element_text(size = 8, color = "black", face = "bold",family = "mono"),
        strip.text.y = element_text(size = 8, color = "black", face = "bold",family = "mono"),
        legend.title = element_text(family = "mono",size = 10),
        legend.position="bottom",
        legend.text = element_text(family = "mono",size = 10),
        axis.line.x = element_line(color="black", size = 0.8),
        axis.line.y = element_line(color="black", size = 0.8),
        axis.title.y  = element_text(family = "mono",size = 12, face = "bold", color = "black"),
        axis.title.x  = element_text(family = "mono",size = 12, face = "bold", hjust = 0.5),
        axis.text.x   = element_text(family = "mono",size = 8, face = "bold",angle = 90),
        axis.text.y   = element_text(family = "mono",size = 10, face = "bold"),
        plot.title = element_text(family = "mono",size = 12,face = "bold", hjust = 0.5)) +
  ggtitle("12 Month-ending Provisional Number of Opioid Overdose Deaths: South Carolina - Root Cause")
ggsave(paste0(pathWrite,"Overdose_Monthly.jpg"), plot = fig, width = 12, height = 8, bg="white")




#==============================================================================#
#==================== Overdose-Prescription Volume Summary ====================#
#==============================================================================#
tmp0 <- subset(df, DrugClass == "Opioid") %>% 
  group_by(year(date), month(date),DrugProdoction, drug_class, DrugClass) %>% 
  summarise(MMEperDay = round(mean(`MME/Day`, na.rm = T),2),
            MME = round(mean(total_prescription_mme, na.rm = T),2),
            Quantity = round(mean(quantity, na.rm = T),2),
            N = n())
names(tmp0) <- c("Year","Month","DrugProdoction","drug_class","DrugClass",
                 "MMEperDay","MME","Quantity","N")


tmp1 <- dfOverdose
tmp1 <- tmp1[,c(2:3,5,12)]
names(tmp1) <- c("Year","Month","Indicator","Overdose")
IndicatorList <- c("Opioids (T40.0-T40.4,T40.6)",
                   "Natural & semi-synthetic opioids (T40.2)",
                   "Synthetic opioids, excl. methadone (T40.4)",
                   "Methadone (T40.3)")
tmp1 <- subset(tmp1, Indicator %in% IndicatorList)
tmp1 <- tmp1 %>% mutate(Indicator = factor(Indicator, levels = IndicatorList))
tmp1[,c("Overdose")] <- lapply(tmp1[,c("Overdose")],function(x){as.numeric(gsub(",", "", x))})
tmp1$Month<- as.numeric(match(tmp1$Month, month.name))
tmp1$Year <- as.numeric(tmp1$Year)

tmp2 <- subset(tmp0, DrugProdoction %in% c("SemiSyntheic", "Natural")) %>% 
  group_by(Year,Month) %>%
  summarise(MMEperDay = mean(MMEperDay, na.rm = T),
            MME = mean(MME, na.rm = T),
            Quantity = mean(Quantity, na.rm = T),
            N = mean(N,na.rm = T))
tmp2$Indicator <- "Natural & semi-synthetic opioids (T40.2)"
tmp1 <- merge(tmp1, tmp2, by = c("Indicator","Year","Month"), all.x = T)

tmp2 <- subset(tmp0, DrugClass == "Opioid") %>% 
  group_by(Year,Month) %>%
  summarise(MMEperDay = mean(MMEperDay, na.rm = T),
            MME = mean(MME, na.rm = T),
            Quantity = mean(Quantity, na.rm = T),
            N = mean(N,na.rm = T))
tmp2$Indicator <- "Opioids (T40.0-T40.4,T40.6)"
tmp1 <- merge(tmp1, tmp2, by = c("Indicator","Year","Month"), all.x = T)
tmp1$MMEperDay <- coalesce(tmp1$MMEperDay.x,0) + coalesce(tmp1$MMEperDay.y,0)
tmp1 <- tmp1[,-c("MMEperDay.x","MMEperDay.y")]
tmp1$MME <- coalesce(tmp1$MME.x,0) + coalesce(tmp1$MME.y,0)
tmp1 <- tmp1[,-c("MME.x","MME.y")]
tmp1$Quantity <- coalesce(tmp1$Quantity.x,0) + coalesce(tmp1$Quantity.y,0)
tmp1 <- tmp1[,-c("Quantity.x","Quantity.y")]
tmp1$N <- coalesce(tmp1$N.x,0) + coalesce(tmp1$N.y,0)
tmp1 <- tmp1[,-c("N.x","N.y")]

tmp2 <- subset(tmp0, drug_class == "Methadone") %>% 
  group_by(Year,Month) %>%
  summarise(MMEperDay = mean(MMEperDay, na.rm = T),
            MME = mean(MME, na.rm = T),
            Quantity = mean(Quantity, na.rm = T),
            N = mean(N,na.rm = T))
tmp2$Indicator <-  "Methadone (T40.3)"
tmp1 <- merge(tmp1, tmp2, by = c("Indicator","Year","Month"), all.x = T)
tmp1$MMEperDay <- coalesce(tmp1$MMEperDay.x,0) + coalesce(tmp1$MMEperDay.y,0)
tmp1 <- tmp1[,-c("MMEperDay.x","MMEperDay.y")]
tmp1$MME <- coalesce(tmp1$MME.x,0) + coalesce(tmp1$MME.y,0)
tmp1 <- tmp1[,-c("MME.x","MME.y")]
tmp1$Quantity <- coalesce(tmp1$Quantity.x,0) + coalesce(tmp1$Quantity.y,0)
tmp1 <- tmp1[,-c("Quantity.x","Quantity.y")]
tmp1$N <- coalesce(tmp1$N.x,0) + coalesce(tmp1$N.y,0)
tmp1 <- tmp1[,-c("N.x","N.y")]


tmp2 <- subset(tmp0, DrugProdoction == "Syntheic" & drug_class != "Methadone") %>% 
  group_by(Year,Month) %>%
  summarise(MMEperDay = mean(MMEperDay, na.rm = T),
            MME = mean(MME, na.rm = T),
            Quantity = mean(Quantity, na.rm = T),
            N = mean(N,na.rm = T))
tmp2$Indicator <-  "Synthetic opioids, excl. methadone (T40.4)"
tmp1 <- merge(tmp1, tmp2, by = c("Indicator","Year","Month"), all.x = T)
tmp1$MMEperDay <- coalesce(tmp1$MMEperDay.x,0) + coalesce(tmp1$MMEperDay.y,0)
tmp1 <- tmp1[,-c("MMEperDay.x","MMEperDay.y")]
tmp1$MME <- coalesce(tmp1$MME.x,0) + coalesce(tmp1$MME.y,0)
tmp1 <- tmp1[,-c("MME.x","MME.y")]
tmp1$Quantity <- coalesce(tmp1$Quantity.x,0) + coalesce(tmp1$Quantity.y,0)
tmp1 <- tmp1[,-c("Quantity.x","Quantity.y")]
tmp1$N <- coalesce(tmp1$N.x,0) + coalesce(tmp1$N.y,0)
tmp1 <- tmp1[,-c("N.x","N.y")]
rm(tmp2)
rm(tmp0)
tmp1$Date <- as.yearmon(as.Date(paste0(tmp1$Year,"-",tmp1$Month,"-01")))


# Plot overdose vs MMEperDay
fig_list <- list()
counter = 1
for (ii in IndicatorList){
  subdf <- tmp1 %>% filter(Indicator == ii & Date <= as.yearmon("202111","%Y %m"))
  sec <- with(subdf, train_sec(Overdose, MMEperDay))
  color1 <- "red"
  color2 <- "blue"
  res <- cor.test(subdf$Overdose,subdf$MMEperDay)
  if (res$p.value > 0.1){
    res <- paste0(round(res$estimate,2))
  } else if (res$p.value > 0.05){
    res <- paste0(round(res$estimate,2),"(.)")
  } else if (res$p.value > 0.01){
    res <- paste0(round(res$estimate,2),"(*)")
  } else if (res$p.value > 0.001){
    res <- paste0(round(res$estimate,2),"(**)")
  } else{
    res <- paste0(round(res$estimate,2),"(***)")
  }
  textdata <- as.data.frame(subdf %>% summarize(ylevel = quantile(Overdose,0.90), yvalue = res))
  fig <- ggplot(subdf, aes(x = Date)) +
    geom_point(aes(y = Overdose), color = color1, shape = "square", size = 1) +
    geom_line(aes(y = Overdose), color = color1, linewidth=0.7, linetype = "dashed") +
    geom_point(aes(y = sec$fwd(MMEperDay)), color = color2, shape = "triangle", size = 1) +
    geom_line(aes(y = sec$fwd(MMEperDay)), color = color2, size=0.7, linetype = "dashed") +
    scale_y_continuous(name = "Overdose Count", sec.axis = sec_axis(~sec$rev(.), name = "MME Avg.")) +
    theme_ipsum() +
    scale_fill_manual()+
    labs(x = "Month") +
    geom_label(data = textdata, aes(label = paste0("cor = ",yvalue),
                                    y = ylevel, x = as.yearmon("201901","%Y %m")),
               parse = FALSE, hjust = 0, size = 4, fontface = "bold",
               family = 'mono', color = 'brown3') +
    theme(strip.text.x = element_text(size = 8, color = "black", face = "bold",family = "mono"),
          strip.text.y = element_text(size = 8, color = "black", face = "bold",family = "mono"),
          legend.title = element_text(family = "mono",size = 10),
          legend.position="bottom",
          legend.text = element_text(family = "mono",size = 10),
          axis.line.x = element_line(color="black", size = 0.8),
          axis.line.y = element_line(color="black", size = 0.8),
          axis.title.y  = element_text(family = "mono",size = 12, face = "bold", color = color1),
          axis.title.y.right = element_text(family = "mono",size = 12, face = "bold", color = color2),
          axis.title.x  = element_text(family = "mono",size = 12, face = "bold", hjust = 0.5),
          axis.text.x   = element_text(family = "mono",size = 10, face = "bold"),
          axis.text.y   = element_text(family = "mono",size = 10, face = "bold"),
          plot.title = element_text(family = "mono",size = 12,face = "bold", hjust = 0.5)) +
    ggtitle(ii)
  # fig_list[[counter]] <- fig
  # save(fig, file = paste0(pathWrite,"fig",counter,".RData"))
  ggsave(paste0(pathWrite,"fig",counter,".PNG"), plot = fig, width = 14, height = 8, bg="white")
  counter = counter + 1
}
# load(paste0(pathWrite,"fig1.RData"))
loaded_images <- list()
# Loop through each file, read the image, and store it in the list
for (i in 1:4) {
  loaded_images[[i]] <- readPNG(paste0(pathWrite,"fig",i,".PNG"))
}
fig <- grid.arrange(grobs = lapply(loaded_images, rasterGrob), ncol = 2)
ggsave(paste0(pathWrite,"MMEperDay_Overdose_Monthly.jpg"), plot = fig, width = 14, height = 8, bg="white")




# Overdose vs MMEperDay yearly
tmp <- tmp1 %>% group_by(Year,Indicator) %>% summarise(Overdose = paste0(round(sum(Overdose,na.rm = T),2)),
                                                       MMEperDay = paste0(round(mean(MMEperDay,na.rm = T),2),
                                                                          " [",
                                                                          round(quantile(MMEperDay,0.025,na.rm = T),2),
                                                                          ", ",
                                                                          round(quantile(MMEperDay,0.975,na.rm = T),2),
                                                                          "]"))


tmp3 <- reshape2::dcast(tmp, Indicator ~ Year, value.var = "Overdose")
tmp3$Measurement <- "Overdose"
tmp4 <- reshape2::dcast(tmp, Indicator ~ Year, value.var = "MMEperDay")
tmp4$Measurement <- "MMEperDay"
tmp <- rbind(tmp3,tmp4)
rm(tmp3)
rm(tmp4)
fwrite(tmp,paste0(pathWrite,"OverdoseRate_PrescriptionVolume_Yearly_DrugClass.csv"))




#==============================================================================#
#=========================== Doctor Shopping Analysis =========================#
#==============================================================================#
# Overall
subdf <- df %>% 
  group_by(DoctorShopping1) %>% 
  summarise(MMEperDay = round(mean(`MME/Day`, na.rm = T),2),
            quantity = round(mean(quantity, na.rm = T),2),
            days_supply = round(mean(days_supply, na.rm = T),2),
            MME = round(mean(total_prescription_mme, na.rm = T),2),
            distance = mean(patient_prescriber_distance + patient_dispenser_distance + dispenser_prescriber_distance, na.rm = T),
            Total_MME = sum(total_prescription_mme, na.rm = T)) %>%  
  mutate(Share = round(Total_MME / sum(Total_MME, na.rm = T)*100,2))
# Shoppers have only 2% share but have more MMEperDay, distance
# Try to test it statistically using t.test

# Drug
subdf <- df %>% 
  group_by(DoctorShopping1, drug_class) %>% 
  summarise(MMEperDay = round(mean(`MME/Day`, na.rm = T),2),
            quantity = round(mean(quantity, na.rm = T),2),
            days_supply = round(mean(days_supply, na.rm = T),2),
            MME = round(mean(total_prescription_mme, na.rm = T),2),
            distance = mean(patient_prescriber_distance + patient_dispenser_distance + dispenser_prescriber_distance, na.rm = T),
            Total_MME = sum(total_prescription_mme, na.rm = T)) %>%  
  group_by(DoctorShopping1) %>%
  mutate(Share = round(Total_MME / sum(Total_MME, na.rm = T)*100,2))
# Other than the fact that MMEperDay is more in shoppers drugs,
# there is no specific different between the portions of drugs 
# among shoppers and non-shoppers. Oxycodone is always the top
# in both groups, but in shoppers the share is much higher.

# Prescriber State
subdf <- df %>% 
  group_by(Prescriber_State,DoctorShopping1) %>% 
  summarise(MMEperDay = round(mean(`MME/Day`, na.rm = T),2),
            quantity = round(mean(quantity, na.rm = T),2),
            days_supply = round(mean(days_supply, na.rm = T),2),
            MME = round(mean(total_prescription_mme, na.rm = T),2),
            distance = mean(patient_prescriber_distance + patient_dispenser_distance + dispenser_prescriber_distance, na.rm = T),
            Total_MME = sum(total_prescription_mme, na.rm = T),
            N = n()) %>%  
  group_by(DoctorShopping1) %>%
  mutate(Share = round(Total_MME / sum(Total_MME, na.rm = T)*100,2)) %>%
  arrange(Prescriber_State,desc(Share))
# The only state with above 10% shoppers is AL with unsual MMEperDay


# Prescriber State
subdf <- df %>%
  group_by(DoctorShopping1, Prescriber_State) %>% 
  summarise(MMEperDay = round(mean(`MME/Day`, na.rm = T),2),
            quantity = round(mean(quantity, na.rm = T),2),
            days_supply = round(mean(days_supply, na.rm = T),2),
            MME = round(mean(total_prescription_mme, na.rm = T),2),
            distance = mean(patient_prescriber_distance + patient_dispenser_distance + dispenser_prescriber_distance, na.rm = T),
            Total_MME = sum(total_prescription_mme, na.rm = T),
            N = length(unique(consolidation_identifier))) %>%  
  group_by(DoctorShopping1) %>%
  mutate(Share = round(Total_MME / sum(Total_MME, na.rm = T)*100,2)) %>%
  arrange(DoctorShopping1,desc(Share)) %>% top_n(5)
# Top states in non-shoppers are SC,NC,GA,FL,TX
# Top states in shoppers are SC,NC,GA,AL,FL,TX
# AL doctors have high MMEperDay!


# Prescriber State is AL
subdf <- subset(df,Prescriber_State == "AL") %>% 
  group_by(DoctorShopping1,dispenser_class) %>% 
  summarise(MMEperDay = round(mean(`MME/Day`, na.rm = T),2),
            quantity = round(mean(quantity, na.rm = T),2),
            days_supply = round(mean(days_supply, na.rm = T),2),
            MME = round(mean(total_prescription_mme, na.rm = T),2),
            distance = mean(patient_prescriber_distance + patient_dispenser_distance + dispenser_prescriber_distance, na.rm = T),
            Total_MME = sum(total_prescription_mme, na.rm = T),
            N = length(unique(consolidation_identifier))) %>%  
  group_by(DoctorShopping1) %>%
  mutate(Share = round(Total_MME / sum(Total_MME, na.rm = T)*100,2))
# Among the shoppers from AL doctors, majority of them filled the prescriptions 
# in retail pharmacies. This is while the dominant type of pharmacies among
# non-shoppers is chain.



# Dispenser Class
subdf <- df %>% 
  group_by(DoctorShopping1,dispenser_class) %>% 
  summarise(MMEperDay = round(mean(`MME/Day`, na.rm = T),2),
            quantity = round(mean(quantity, na.rm = T),2),
            days_supply = round(mean(days_supply, na.rm = T),2),
            MME = round(mean(total_prescription_mme, na.rm = T),2),
            distance = mean(patient_prescriber_distance + patient_dispenser_distance + dispenser_prescriber_distance, na.rm = T),
            Total_MME = sum(total_prescription_mme, na.rm = T)) %>%  
  group_by(DoctorShopping1) %>%
  mutate(Share = round(Total_MME / sum(Total_MME, na.rm = T)*100,2))
# Again MMEperDay among shoppers is higher.
# In terms of the share of different pharmacies, there is no obvious difference
# Shoppers Mail orders have very high MMEperDay.

# Dispenser Class
subdf <- df %>% 
  group_by(DoctorShopping1,dispenser_class,dispenser_state) %>% 
  summarise(MMEperDay = round(mean(`MME/Day`, na.rm = T),2),
            quantity = round(mean(quantity, na.rm = T),2),
            days_supply = round(mean(days_supply, na.rm = T),2),
            MME = round(mean(total_prescription_mme, na.rm = T),2),
            distance = mean(patient_prescriber_distance + patient_dispenser_distance + dispenser_prescriber_distance, na.rm = T),
            Total_MME = sum(total_prescription_mme, na.rm = T),
            N = length(unique(consolidation_identifier))) %>%  
  group_by(DoctorShopping1,dispenser_class) %>%
  mutate(Share = round(Total_MME / sum(Total_MME, na.rm = T)*100,2))
# Top non-shopper mail orders: AZ, MO, PA, TN
# Top non-shopper chain, retail: always SC with other states having less than 1%
# Top shopper mail-order: TN (78%!) then PA and MO each with 10%
# Top shopper chain: SC (99%) / Top shopper retails: SC (94%), PA (4%)
# PA pharmacies are the only one with above 1% share in retail shoppers


# Dispenser Class
subdf <- df %>% 
  group_by(DoctorShopping1,dispenser_class,drug_class) %>% 
  summarise(MMEperDay = round(mean(`MME/Day`, na.rm = T),2),
            quantity = round(mean(quantity, na.rm = T),2),
            days_supply = round(mean(days_supply, na.rm = T),2),
            MME = round(mean(total_prescription_mme, na.rm = T),2),
            distance = mean(patient_prescriber_distance + patient_dispenser_distance + dispenser_prescriber_distance, na.rm = T),
            Total_MME = sum(total_prescription_mme, na.rm = T)) %>%  
  group_by(DoctorShopping1,dispenser_class) %>%
  mutate(Share = round(Total_MME / sum(Total_MME, na.rm = T)*100,2))
# Non-shopper chain: Oxycodone (40%), Hydrocodone (30%), Fentanyl and Morphine (each 8%)
# Non-shopper retail: Oxycodone (45%), Hydrocodone (20%), Fentanyl and Morphine (each 10%)
# Non-shopper mail-order: Oxycodone (30%), Fentanyl (25%), Morphine and Hydrocodone (each 13%)
# Shopper chain: Oxycodone (60%), Hydrocodone (20%), Morphine (6%), Fentanyl (3%)
# Shopper retail: Oxycodone (65%), Hydrocodone (15%), Morphine (8%), Fentanyl (4%)
# Shopper mail-order: Oxycodone (50%), Morphine (20%), Fentanyl (12%), Hydrocodone (5%)

# Mail order dispensers
subdf <- subset(df, dispenser_class == "Mail Order" & dispenser_state == "TN") %>% 
  group_by(DoctorShopping1,drug_class) %>% 
  summarise(MMEperDay = round(mean(`MME/Day`, na.rm = T),2),
            quantity = round(mean(quantity, na.rm = T),2),
            days_supply = round(mean(days_supply, na.rm = T),2),
            MME = round(mean(total_prescription_mme, na.rm = T),2),
            distance = mean(patient_prescriber_distance + patient_dispenser_distance + dispenser_prescriber_distance, na.rm = T),
            Total_MME = sum(total_prescription_mme, na.rm = T)) %>%  
  group_by(DoctorShopping1) %>%
  mutate(Share = round(Total_MME / sum(Total_MME, na.rm = T)*100,2)) %>%
  arrange(DoctorShopping1, desc(Share))


# Looking at doctor shoppers
subdf <- subset(df, consolidation_identifier %in% unique(df$consolidation_identifier[df$DoctorShopping0 == 1]))
subdf$dispenser_class[subdf$dispenser_class %in% c("Not Found in ARCOS","Practitioner","Hospital/Clinic","Central")] <- "Other"
res <- subdf %>%
  group_by(DoctorShopping0,drug_class) %>%
  summarise(MMEperDay = round(mean(`MME/Day`, na.rm = T),2),
            quantity = round(mean(quantity, na.rm = T),2),
            days_supply = round(mean(days_supply, na.rm = T),2),
            MME = round(mean(total_prescription_mme, na.rm = T),2),
            distance = mean(patient_prescriber_distance + patient_dispenser_distance + dispenser_prescriber_distance, na.rm = T),
            Total_MME = sum(total_prescription_mme, na.rm = T),
            N = n()) %>%
  mutate(Share = round(Total_MME / sum(Total_MME, na.rm = T)*100,2)) %>%
  arrange(drug_class,DoctorShopping0)

subdf <- df[which(df$drug_class == "Hydromorphone" & df$dispenser_class == "Retail"),] %>% 
  group_by(DoctorShopping0,Patient_County) %>% 
  summarise(MMEperDay = round(mean(`MME/Day`, na.rm = T),2),
            quantity = round(mean(quantity, na.rm = T),2),
            days_supply = round(mean(days_supply, na.rm = T),2),
            MME = round(mean(total_prescription_mme, na.rm = T),2),
            distance = mean(patient_prescriber_distance + patient_dispenser_distance + dispenser_prescriber_distance, na.rm = T),
            Total_MME = sum(total_prescription_mme, na.rm = T),
            N = length(unique(consolidation_identifier))) %>%  
  group_by(DoctorShopping0) %>%
  mutate(Share = round(Total_MME / sum(Total_MME, na.rm = T)*100,2)) %>%
  arrange(Patient_County,DoctorShopping0)



shoppers <- subset(df, DoctorShopping0 == 1)
drug_table <- table(shoppers$consolidation_identifier, shoppers$drug_class)
drug_table[drug_table != 0] <- 1
drug_matrix <- as.matrix(drug_table > 0)
co_occurrence_matrix <- t(drug_matrix) %*% drug_matrix
shoppersco_occurrence_df <- as.data.frame(co_occurrence_matrix)
diag(shoppersco_occurrence_df) <- 0
# shoppersco_occurrence_df <- shoppersco_occurrence_df / apply(co_occurrence_matrix, 1, function(x) min(x[x != 0]))
shoppersco_occurrence_df <- round(shoppersco_occurrence_df / rowSums(shoppersco_occurrence_df)*100,2)
fwrite(shoppersco_occurrence_df,file= paste0(pathWrite,"shoppersco_occurrence_df.csv"))


nonshoppers <- subset(df, consolidation_identifier %in% unique(df$consolidation_identifier[df$DoctorShopping1 == 0]))
drug_table <- table(nonshoppers$consolidation_identifier, nonshoppers$drug_class)
drug_table <- drug_table / rowSums(drug_table)
drug_matrix <- as.matrix(drug_table)
co_occurrence_matrix <- t(drug_matrix) %*% drug_matrix
nonshoppers_co_occurrence_df <- as.data.frame(co_occurrence_matrix)
nonshoppers_co_occurrence_df <- round(nonshoppers_co_occurrence_df / rowSums(nonshoppers_co_occurrence_df)*100,2)
fwrite(nonshoppers_co_occurrence_df,file= paste0(pathWrite,"nonshoppers_co_occurrence_df.csv"))


#==============================================================================#
#================================ Covid Analysis ==============================#
#==============================================================================#
CovidStat <- subset(df, new_or_refill == "new") %>% group_by(as.yearmon(date),DrugClass) %>%
  summarise(Npatients = length(unique(consolidation_identifier)),
            Nprescribers = length(unique(prescriber_dea_number)),
            Ndispensers = length(unique(dispenser_dea_number)),
            N = n())
names(CovidStat)[1] <- "Month"
fig <- ggplot(CovidStat, aes(x = Month)) +
  geom_point(aes(y = N), color = "red", shape = "square", size = 1) +
  geom_line(aes(y = N), color = "red", size=0.7, linetype = "dashed") +
  theme_ipsum() +
  facet_wrap(~DrugClass, scales = "free_y")+
  scale_fill_manual()+
  labs(x = "Date") +
  theme(strip.text.x = element_text(size = 8, color = "black", face = "bold",family = "mono"),
        strip.text.y = element_text(size = 8, color = "black", face = "bold",family = "mono"),
        legend.title = element_text(family = "mono",size = 10),
        legend.position="bottom",
        legend.text = element_text(family = "mono",size = 10),
        axis.line.x = element_line(color="black", size = 0.8),
        axis.line.y = element_line(color="black", size = 0.8),
        axis.title.y  = element_text(family = "mono",size = 12, face = "bold", color = color1),
        axis.title.y.right = element_text(family = "mono",size = 12, face = "bold", color = color2),
        axis.title.x  = element_text(family = "mono",size = 12, face = "bold", hjust = 0.5),
        axis.text.x   = element_text(family = "mono",size = 10, face = "bold"),
        axis.text.y   = element_text(family = "mono",size = 10, face = "bold"),
        plot.title = element_text(family = "mono",size = 12,face = "bold", hjust = 0.5))
  # ggtitle(ifelse(ii == 0, "No Shopping","Doctor Shopping"))


  
  
#==============================================================================#
#============================= Demographic Analysis ===========================#
#==============================================================================#  


  
  










# ==============================================================================
# Monthly - Synthetic opioids excl. methadone - No Class
subdf <- subset(df, Patient_State == "SC" & date >= as.Date("2015-01-01") & 
                  date < as.Date("2022-01-01") & DrugClass == "Opioid" &
                  DrugProdoction == "Syntheic" & drug_class != "Methadone")
# dfDoctorShopping <- subdf %>% drop_na(consolidation_identifier) %>% group_by(consolidation_identifier,drug_class, year(date)) %>% 
#   summarize(PrescriberCount = n_distinct(prescriber_dea_number),
#             DispenserCount = n_distinct(dispenser_dea_number))
# DS <- list(unique(dfDoctorShopping$consolidation_identifier[which(dfDoctorShopping$PrescriberCount >= 4 & dfDoctorShopping$DispenserCount >= 4)]))
# subdf$DoctorShopping1 <- 0
# subdf$DoctorShopping1[subdf$consolidation_identifier %in% DS[[1]]] <- 1
subdf <- subdf %>% 
  group_by(year(date), month(date), DoctorShopping1) %>% 
  summarize(MME = sum(total_prescription_mme),
            Quantity = sum(quantity),
            MMEperDay = mean(`MME/Day`, na.rm = T),
            DaysSupply = mean(days_supply, na.rm = T))
colnames(subdf)[1:2] <- c("Year","Month")

tmp <- subset(dfOverdose, Indicator == "Synthetic opioids, excl. methadone (T40.4)")
tmp$Month <- match(tmp$Month,month.name)
tmp <- tmp[,c("Year","Month","Data Value","Predicted Value")]
colnames(tmp) <- c("Year","Month","OverdoseReported","OverdosePredcited")

subdf <- merge(subdf, tmp, by = c("Year","Month"), all.x=T)
subdf$Date <- as.yearmon(paste(subdf$Year, subdf$Month), "%Y %m")
subdf[,c("OverdosePredcited","OverdoseReported")] <- lapply(subdf[,c("OverdosePredcited","OverdoseReported")],function(x){as.numeric(gsub(",", "", x))})

text = "Correlation between overdose and prescription volume (synthetic excl. methadone)"
label = "MMEperDay_Overdose_Monthly_SyntheticOpioids_NoClass_DoctorShopping1.jpg"
plot_noclass(subdf,1,pathWrite,text,label)
# ==============================================================================
# Monthly - Methadone (T40.3) - No Class
subdf <- subset(df, Patient_State == "SC" & date >= as.Date("2015-01-01") & 
                  date < as.Date("2022-01-01") & drug_class == "Methadone")
dfDoctorShopping <- subdf %>% drop_na(consolidation_identifier) %>% group_by(consolidation_identifier,drug_class, year(date)) %>% 
  summarize(PrescriberCount = n_distinct(prescriber_dea_number),
            DispenserCount = n_distinct(dispenser_dea_number))
DS <- list(unique(dfDoctorShopping$consolidation_identifier[which(dfDoctorShopping$PrescriberCount >= 4 & dfDoctorShopping$DispenserCount >= 4)]))
subdf$DoctorShopping1 <- 0
subdf$DoctorShopping1[subdf$consolidation_identifier %in% DS[[1]]] <- 1

subdf <- subdf %>%  
  group_by(year(date), month(date),DoctorShopping1) %>% 
  summarize(MME = sum(total_prescription_mme),
            Quantity = sum(quantity),
            MMEperDay = mean(`MME/Day`, na.rm = T),
            DaysSupply = mean(days_supply, na.rm = T))
colnames(subdf)[1:2] <- c("Year","Month")

tmp <- subset(dfOverdose, Indicator == "Methadone (T40.3)")
tmp$Month <- match(tmp$Month,month.name)
tmp <- tmp[,c("Year","Month","Data Value","Predicted Value")]
colnames(tmp) <- c("Year","Month","OverdoseReported","OverdosePredcited")

subdf <- merge(subdf, tmp, by = c("Year","Month"), all.x=T)
subdf$Date <- as.yearmon(paste(subdf$Year, subdf$Month), "%Y %m")
subdf[,c("OverdosePredcited","OverdoseReported")] <- lapply(subdf[,c("OverdosePredcited","OverdoseReported")],function(x){as.numeric(gsub(",", "", x))})

text = "Correlation between overdose and prescription volume (Methadone)"
label = "MMEperDay_Overdose_Monthly_Methadone_NoClass_DoctorShopping1.jpg"
plot_noclass(subdf,1,pathWrite,text,label)
# ==============================================================================
# Monthly - Natural & semi-synthetic opioids (T40.2) - No Class
subdf <- subset(df, Patient_State == "SC" & date >= as.Date("2015-01-01") & 
                  date < as.Date("2022-01-01") & 
                  DrugProdoction %in% c("SemiSyntheic","Natural"))
dfDoctorShopping <- subdf %>% drop_na(consolidation_identifier) %>% group_by(consolidation_identifier,drug_class, year(date)) %>% 
  summarize(PrescriberCount = n_distinct(prescriber_dea_number),
            DispenserCount = n_distinct(dispenser_dea_number))
DS <- list(unique(dfDoctorShopping$consolidation_identifier[which(dfDoctorShopping$PrescriberCount >= 4 & dfDoctorShopping$DispenserCount >= 4)]))
subdf$DoctorShopping1 <- 0
subdf$DoctorShopping1[subdf$consolidation_identifier %in% DS[[1]]] <- 1

subdf <- subdf %>%
  group_by(year(date), month(date),DoctorShopping1) %>% 
  summarize(MME = sum(total_prescription_mme),
            Quantity = sum(quantity),
            MMEperDay = mean(`MME/Day`, na.rm = T),
            DaysSupply = mean(days_supply, na.rm = T))
colnames(subdf)[1:2] <- c("Year","Month")

tmp <- subset(dfOverdose, Indicator == "Natural & semi-synthetic opioids (T40.2)")
tmp$Month <- match(tmp$Month,month.name)
tmp <- tmp[,c("Year","Month","Data Value","Predicted Value")]
colnames(tmp) <- c("Year","Month","OverdoseReported","OverdosePredcited")

subdf <- merge(subdf, tmp, by = c("Year","Month"), all.x=T)
subdf$Date <- as.yearmon(paste(subdf$Year, subdf$Month), "%Y %m")
subdf[,c("OverdosePredcited","OverdoseReported")] <- lapply(subdf[,c("OverdosePredcited","OverdoseReported")],function(x){as.numeric(gsub(",", "", x))})


text = "Correlation between overdose and prescription volume (Natural & semi-synthetic opioids)"
label = "MMEperDay_Overdose_Monthly_NaturalSemiSynthetic_NoClass_DoctorShopping1.jpg"
plot_noclass(subdf,1,pathWrite,text,label)

# ==============================================================================
# Monthly - In state vs Out state doctors
subdf <- subset(df, Patient_State == "SC" & date >= as.Date("2015-01-01") & 
                  date < as.Date("2022-01-01") & DrugClass == "Opioid") %>%
  mutate(InStatePrescriber = ifelse(Prescriber_State == "SC",1,0)) %>%
  group_by(year(date), month(date), DoctorShopping1, InStatePrescriber) %>% 
  summarize(MME = sum(total_prescription_mme),
            Quantity = sum(quantity),
            MMEperDay = mean(`MME/Day`, na.rm = T),
            DaysSupply = mean(days_supply, na.rm = T))
colnames(subdf)[1:3] <- c("Year","Month","DoctorShopping","InStatePrescriber")

tmp <- subset(dfOverdose, Indicator == "Opioids (T40.0-T40.4,T40.6)")
tmp$Month <- match(tmp$Month,month.name)
tmp <- tmp[,c("Year","Month","Data Value","Predicted Value")]
colnames(tmp) <- c("Year","Month","OverdoseReported","OverdosePredcited")

subdf <- merge(subdf, tmp, by = c("Year","Month"), all.x=T)
subdf$Date <- as.yearmon(paste(subdf$Year, subdf$Month), "%Y %m")

subdf[,c("OverdosePredcited")] <- as.numeric(subdf$OverdosePredcited)


# objectiveColor <- "#69b3a2"
# timeColor <- rgb(0.2, 0.6, 0.9, 1)
color1 <- "red"
color2 <- "blue"
counter = 1
fig_list <- list()
for (ii in c(0,1)){
  for (jj in c(0,1)){
    tmp <- subset(subdf, InStatePrescriber == ii & DoctorShopping == jj)
    sec <- with(tmp, train_sec(OverdosePredcited, MMEperDay))
    fig <- ggplot(tmp, aes(x = Date)) +
      geom_line(aes(y = OverdosePredcited), color = color1, size=0.5, linetype = "dashed") +
      geom_line(aes(y = sec$fwd(MMEperDay)), color = color2, size=0.5, linetype = "dashed") +
      scale_y_continuous(name = "Overdose Count", sec.axis = sec_axis(~sec$rev(.), name = "MME per Day Avg.")) +
      theme_ipsum() +
      scale_fill_manual()+
      # facet_grid(class_player ~ class_distance, scale = "free_y") +
      labs(x = "Month") +
      theme(strip.text.x = element_text(size = 8, color = "black", face = "bold",family = "mono"),
            strip.text.y = element_text(size = 8, color = "black", face = "bold",family = "mono"),
            legend.title = element_text(family = "mono",size = 10),
            legend.position="bottom",
            legend.text = element_text(family = "mono",size = 10),
            axis.line.x = element_line(color="black", size = 0.8),
            axis.line.y = element_line(color="black", size = 0.8),
            axis.title.y  = element_text(family = "mono",size = 8, face = "bold", color = color1),
            axis.title.y.right = element_text(family = "mono",size = 8, face = "bold", color = color2),
            axis.title.x  = element_text(family = "mono",size = 8, face = "bold", hjust = 0.5),
            axis.text.x   = element_text(family = "mono",size = 7, face = "bold", angle = 90),
            axis.text.y   = element_text(family = "mono",size = 8, face = "bold"),
            plot.title = element_text(family = "mono",size = 12,face = "bold", hjust = 0.5)) +
      ggtitle(paste0("DoctorShopping: ", jj, "Prescriber in SC: ", ii))
    fig_list[[counter]] <- fig
    counter = counter + 1
  }
}
fig <- grid.arrange(arrangeGrob(fig_list[[1]]+ theme(legend.position="none"),
                                fig_list[[2]]+ theme(legend.position="none"),
                                fig_list[[3]]+ theme(legend.position="none"),
                                fig_list[[4]]+ theme(legend.position="none"),
                                nrow = 2), ncol = 1,
                    top =textGrob(paste0("Correlation between overdose and prescription volume"),gp=gpar(fontsize=14, font=4)))
ggsave(paste0(pathWrite,"MMEperDay_Overdose_AllOpioids_Monthly_Class.jpg"), plot = fig, width = 12, height = 12)


#==============================================================================#
#=============================== County Analysis ==============================#
#==============================================================================#
subdf <- df %>% group_by(Patient_County,year(date)) %>% 
  summarise(MMEperDay = mean(`MME/Day`,na.rm = T),
            Quantity = mean(quantity, na.rm = T),
            MME = mean(total_prescription_mme, na.rm = T),
            N = n()) %>% 
  group_by(`year(date)`) %>%
  mutate(Share = round(N / sum(N)*100,2)) %>%
  filter(Share > 0.15) %>%
  group_by(Patient_County) %>%
  mutate(Count = n()) %>%
  filter(unique(Count) == 7)
  
colnames(subdf)[1:2] <- c("County","Year")
subdf <- subdf %>% filter(County != "" & Year >= 2017)
subdf <- merge(subdf,dfOverdoseCounty,by = c("Year","County"), all.x = T)

correlation_with_significance <- function(Volume,Overdose) {
  if (sum(is.na(Overdose)) > 2) {
    return("NaN")
  } else{
    res <- cor.test(Volume, Overdose)
    if (res$p.value > 0.1){
      res <- paste0(round(res$estimate,2))
    } else if (res$p.value > 0.05){
      res <- paste0(round(res$estimate,2),"(.)")
    } else if (res$p.value > 0.01){
      res <- paste0(round(res$estimate,2),"(*)")
    } else if (res$p.value > 0.001){
      res <- paste0(round(res$estimate,2),"(**)")
    } else{
      res <- paste0(round(res$estimate,2),"(***)")
    }
    return(res)
  }
}


subdf <- subdf %>% 
  group_by(County) %>% 
  mutate(DailyMMECor = correlation_with_significance(MMEperDay, Opioids),
         MMECor = correlation_with_significance(MME,Opioids),
         QuantityCor = correlation_with_significance(Quantity,Opioids)) %>% 
  arrange(County, Year)

tmp <- subdf %>% group_by(County) %>% summarise(DailyMMECor = unique(DailyMMECor),
                                                MMECor = unique(MMECor),
                                                QuantityCor = unique(QuantityCor),
                                                Share = mean(Share))




subdf <- subset(df, Patient_State == "SC") %>% 
  mutate(InStatePrescriber = ifelse(Prescriber_State == "SC",1,0)) %>%
  group_by(Patient_County,DoctorShopping1,InStatePrescriber) %>% 
  summarise(MMEperDay = mean(`MME/Day`,na.rm = T),
            Quantity = mean(quantity, na.rm = T),
            MME = mean(total_prescription_mme, na.rm = T),
            N = n()) %>% 
  group_by(Patient_County,DoctorShopping1) %>%
  mutate(Share = round(N / sum(N)*100,2))
names(subdf)[1:2] <- c("County","DoctorShopping")

tmp <- dfOverdoseCounty %>% 
  group_by(County) %>% 
  summarise(OverdoseDrug = sum(Drug, na.rm = T),
            OverdoseOpioids = sum(Opioids, na.rm = T),
            OverdoseFentanyl = sum(Fentanyl, na.rm = T))

subdf <- merge(subdf,tmp,by = "County", all.x = T)
subdf <- subdf %>% arrange(desc(OverdoseOpioids))



subdf <- subset(df, Patient_State == "SC") %>% 
  group_by(Patient_County,dispenser_class,DoctorShopping1) %>% 
  summarise(MMEperDay = mean(`MME/Day`,na.rm = T),
            Quantity = mean(quantity, na.rm = T),
            MME = sum(total_prescription_mme, na.rm = T),
            N = n()) %>% 
  group_by(Patient_County,DoctorShopping1) %>%
  mutate(Share = round(MME / sum(MME)*100,2))
names(subdf)[1:2] <- c("County","Pharmacy")

tmp <- dfOverdoseCounty %>% 
  group_by(County) %>% 
  summarise(OverdoseDrug = sum(Drug, na.rm = T),
            OverdoseOpioids = sum(Opioids, na.rm = T),
            OverdoseFentanyl = sum(Fentanyl, na.rm = T))

subdf <- merge(subdf,tmp,by = "County", all.x = T)
subdf <- subdf %>% arrange(desc(OverdoseOpioids),Pharmacy)

