#boxplot and statistic manual vs automatic quantification



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


input.file.pm = file.path(input.Directory, "boxplot_AM_of_esp_19.csv")

dt_epi_group_all = fread(input.file.pm, header = T)
dt_epi_group_all = dt_epi_group_all[, uniqueID := paste(Drug_code, method, sep = "_")]

dt_epi_group_all$Drug_code <- factor(dt_epi_group_all$Drug_code, levels = c("TSA", "OXL", "SPT", "NSPT", "CTPB","DMSO")) 

stat = compare_means(c(epi_cells,
                                   #, erkCNratio_LQBgCorr_robustZscore
                                   #, aktCNratio_LQBgCorr_robustZscore
)
~ method #concentration
, data = dt_epi_group_all
,group.by = "Drug_code"
, method = "wilcox.test" #  default is "wilcox.test", but it accepts "t.test", "anova", "kruskal.test"
, paired = TRUE
# , group.by = "unique_Treatment_Stats"
 # a character string specifying the reference group. If specified, for a given grouping variable, each of the group levels will be compared to the reference group (i.e. control group). ref.group can be also ".all.".
, p.adjust.method = "BH" # "holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none"

)

g<- ggboxplot(dt_epi_group_all, x= "Drug_code", y= "epi_cells", color="method")+
  geom_boxplot(aes( colour = method),lwd = 0.5, position=position_dodge(0.8))+
  
  
  geom_jitter(aes(colour = method), size=0.7, alpha=0.6,position=position_jitterdodge(0.2)) +
  scale_color_manual(values=c(rgb(151,7,115, max=255), rgb(161,163,163, max=255)))+
  
  
  theme(plot.title = element_text(size=5) ) +
  xlab("") +
  ylim(0,150) 
g 


stat<-as.data.table(stat)
stat = stat[ , new_dt.adj:= "ns"  ]
stat = stat[p.adj<=0.05,  new_dt.adj:= "*"  ]
stat = stat[p.adj<=0.01,  new_dt.adj:= "**"  ]
stat = stat[p.adj<=0.001,  new_dt.adj:= "***"  ]
stat = stat[p.adj<=0.0001,  new_dt.adj:= "****"  ]


g
ggsave("boxplot_esp19_manual_auto_Wilcoxon.pdf", g, width = 3, height = 3)

setwd(".") # set a new working directory   --> ".." means one folder above. "." means the current folder. or use function file.path() to browse to the folder you want
getwd()

input.Directory = getwd()
output.AM = paste0("auto_manual_19_wilcoxon",".csv")
fwrite(stat, file = file.path(input.Directory, output.AM), row.names=FALSE)
