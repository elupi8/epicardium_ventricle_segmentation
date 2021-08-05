rm(list=ls())
{
  library(data.table)
  library(dplyr)
  library(ggplot2)
  library(xlsx)
  library(stringr)
  library(ggpubr)
  library(stringi)
  library(tidyr)
  library(plotly) 
  library(EnhancedVolcano)        #all inizio
  
  #library(openxlsx)
}


setwd(".") # set a new working directory   --> ".." means one folder above. "." means the current folder. or use function file.path() to browse to the folder you want
getwd()

input.Directory = getwd()


input.file.pm = file.path(input.Directory, "Manual_Automatic_esp19_10_2t.csv")

dt_epi_group_all = fread(input.file.pm, header = T)

shapiro.test(dt_epi_group_all$Manual)
shapiro.test(dt_epi_group_all$Automatic)   #if p value > 0.05 the data pass the normality test

coor<-cor.test(dt_epi_group_all$Manual, dt_epi_group_all$Automatic, method=c("spearman"), exact=FALSE)




g<-ggscatter(dt_epi_group_all, x = "Manual", y = "Automatic", 
              cor.coef = TRUE, cor.method = "spearman", color="black", size= 0.9, alpha = 0.5) +
               geom_smooth(method = "lm", se = FALSE, col = "deeppink", size=1)

g




ggsave("correlation_Manual_Epi_esp19_10.pdf", g, width = 4, height = 3)


