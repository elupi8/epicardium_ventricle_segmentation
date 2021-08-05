setwd("D:/D7HB/") #choose your fowlder

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


getwd()

name<-list.files(path = ".", pattern = NULL, all.files = FALSE,
                 full.names = FALSE, recursive = FALSE,
                 ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)

dir(path = ".", pattern = NULL, all.files = FALSE,
    full.names = FALSE, recursive = FALSE,
    ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)

list.dirs(path = ".", full.names = TRUE, recursive = TRUE)


s.metadata.loc2<-  str_extract(name, "(?<=-D006--PO01--LO001--CO5--S[:graph:]).*(?=.tif)")
s.meta_ch2<- str_extract(s.metadata.loc2, "(?<=[:graph:])T2.*(?=--WE[:graph:])" )
s.meta_frame2<- str_extract(s.meta_ch2, "(?<=T)[:digit:]{10}" )


s.meta_frame2 <- s.meta_frame2[!is.na(s.meta_frame2)]
print(s.meta_frame2)
pre_frame2<-as.data.table(s.meta_frame2)


frame_diff2 <- diff(as.numeric(pre_frame2$s.meta_frame2, difference= 1))
frame_diff2<-as.data.table(frame_diff2)

frame_mean2<- mean(as.numeric(frame_diff2$frame_diff2, difference= 1))
frame_mean2<-as.data.table(frame_mean2)

frame_millisec2 <- as.numeric((frame_mean2$frame_mean2 *300))
frame_millisec2<-as.data.table(frame_millisec2)


frame_min2<- as.numeric((frame_millisec2$frame_millisec2 /60000))
frame_min2<-as.data.table(frame_min2)


frame_per_min2<- as.numeric((1/frame_min2$frame_min2))
frame_per_min2<-as.data.table(frame_per_min2)




#change directory----

setwd(".") # set a new working directory   --> ".." means one folder above. "." means the current folder. or use function file.path() to browse to the folder you want
getwd()

input.Directory = getwd()


input.file.pm = file.path(input.Directory, "table", "heart_beat.csv")

dt_heart_all = fread(input.file.pm, header = T)

dt_heart_all = dt_heart_all[!(Well %in% c("", NA) ),]



input.file.pm = file.path(input.Directory, "platemap", "platemap.csv")

dt_platemap = fread(input.file.pm, header = T)

dt_platemap = dt_platemap[!(Well %in% c("", NA) ),]


## padd well number 
dt_platemap$Well = toupper(dt_platemap$Well)
dt_platemap$Well = paste0( str_extract(dt_platemap$Well, "[:alpha:]") , sprintf( '%03d', as.numeric( str_extract(dt_platemap$Well, '[0-9]+') ) ) )


# merge platemap with main data table

dt_hb = merge(dt_heart_all,
              dt_platemap,
              by.x = "Well",
              by.y = "Well")

dt_hb$heartbeat<-ceiling(dt_hb$heartbeat)


dt_hb[,2]<-dt_hb[,2]*frame_per_min2$frame_per_min2



output.p_values = "D7_heartbeat_rate.csv"
fwrite(dt_hb, file = file.path(input.Directory,  output.p_values ), row.names = TRUE, quote = FALSE )











stat = compare_means(c(heartbeat,
                       #, erkCNratio_LQBgCorr_robustZscore
                       #, aktCNratio_LQBgCorr_robustZscore
)
~ Concentration #concentration
, data = dt_hb

, method = "kruskal.test" #  default is "wilcox.test", but it accepts "t.test", "anova", "kruskal.test"
, paired = FALSE
# , group.by = "unique_Treatment_Stats"
# a character string specifying the reference group. If specified, for a given grouping variable, each of the group levels will be compared to the reference group (i.e. control group). ref.group can be also ".all.".
, p.adjust.method = "BH" # "holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none"

)



input.Directory = getwd()
output.epi = paste0("heartbeat_A1_Kruskal",".csv")
fwrite(stat, file = file.path(input.Directory, output.epi), row.names=FALSE)


library(FSA)
stat_dunn <- dunnTest(heartbeat ~ Concentration,
                     data=dt_hb,
                     method="sidak", res)
A<-print(stat_dunn, dunn.test.results = TRUE)

Dunn<- as.data.table(A)

stat_w<-as.data.table(stat_w)
stat_w = stat_w[ , new_dt.adj:= "ns"  ]
stat_w = stat_w[p.adj<=0.05,  new_dt.adj:= "*"  ]
stat_w = stat_w[p.adj<=0.01,  new_dt.adj:= "**"  ]
stat_w = stat_w[p.adj<=0.001,  new_dt.adj:= "***"  ]
stat_w = stat_w[p.adj<=0.0001,  new_dt.adj:= "****"  ]

input.Directory = getwd()
output.epi = paste0("heartbeat_A1_Wilcoxon",".csv")
fwrite(stat_w, file = file.path(input.Directory, output.epi), row.names=FALSE)



grp <- group_by(dt_hb, Concentration)
dt_sd<-summarise(grp, mean=mean(heartbeat), sd=sd(heartbeat))

dt_hb$Concentration <- factor(dt_hb$Concentration, levels = c("0uM", "125nM", "250nM", "500nM", "1uM","2uM")) 

g<- ggplot(dt_hb, aes(x= Concentration, y= heartbeat))+
  geom_boxplot(lwd = 0.5, position=position_dodge(0.8))+ 
  
  
  geom_jitter(aes(colour = "black"), size=0.5, alpha=0.6,position=position_jitterdodge(0.2)) +
  scale_color_manual(values=c(rgb(151,7,115, max=255), rgb(161,163,163, max=255)))+
  
  
  theme(plot.title = element_text(size=5) ) +
  xlab("")  
graph<-g + theme_classic() + theme(aspect.ratio=4/2,axis.text.x = element_text(angle = 30, hjust=0.5, vjust=0.8,size=5),
                                   axis.text.y = element_text(vjust = 0.5, hjust=1, size=5)) + ylim(0,210)

graph

ggsave("boxplot_heartbeat_A1_Wilcoxon2.pdf", graph, width = 2.5, height = 3)



