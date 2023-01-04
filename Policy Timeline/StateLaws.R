setwd("G:/.shortcut-targets-by-id/1iHpLOWfFozx8SgSm4MvDaqRCiaVdu3b8/Project - Opioid 3/Power BI")
library(readxl)
library(vistime)



data <- read_excel("StateLawsExcel.xlsx", sheet = "T_Laws")
attach(data)

data <- data[order(StartDate),]





vistime(data, events = "Policy", groups = "state", 
        start = "StartDate", end = "EndDate", col.color = "Color",
        show_labels = F,
        optimize_y = F,
        linewidth = 8,
        title = "Timeline of state policies")



plot_legend <- vistime(data = data,
                       col.start = "StartDate",
                          col.group = "state",
                          col.event = "Policy",
                          show_labels = TRUE,
                          linewidth = 20,
                          title = "Legend")
plot_legend
