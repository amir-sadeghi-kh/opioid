setwd("G:/.shortcut-targets-by-id/1iHpLOWfFozx8SgSm4MvDaqRCiaVdu3b8/Project - Opioid 3/Code/MME Per Population TN, GA, AL")
library(readxl)
library(ggplot2)
data <- read_excel("MMEState Population by Date and Buyer State.xlsx", sheet = "MMEState Population by Date and")
attach(data)

data = data[BuyerState %in% c("AL", "AR","GA","IL", "MS","NC","OH", "SC","VA"),]



p2 <- ggplot(data, aes(x=Policy, y=MME, fill=BuyerState)) + 
  geom_boxplot() +
  facet_wrap(~Policy, scale="free")
p2

