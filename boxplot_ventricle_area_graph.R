
#create boxplot of the ventricle areas, in the imported file was added an additional column indicating the number of the repetion and incross or outcross
rm(list=ls()) #clean space
setwd(".") # set a new working directory   --> ".." means one folder above. "." means the current folder. or use function file.path() to browse to the folder you want
getwd()

input.Directory = getwd()

##------------- if R was closed and no library are loaded please load the ggplot library

input.file.pm = file.path(input.Directory, "heart_for_plot.csv")

dt_heart_all = fread(input.file.pm, header = T)

dt_heart_all = dt_heart_all[!(Well %in% c("", NA) ),]

dt_heart_all = dt_heart_all[, uniqueMerge := paste(Drug_code, Concentration,esp, sep = "_")] #create unique Drug IDs by pasting the columns Drug and Concentration

### for control
aggregationColumn <- dt_heart_all[, uniqueMerge]


dt_heart_all$class_Drug <- factor(dt_heart_all$class_Drug, levels = c("HDAC_inhibitor", "SIRT_activator", "SIRT_inhibitor", "HAT_inhibitor","others")) 





#library(viridis)
c<-ggplot(dt_heart_all[dt_heart_all$Drug_code != "DMSO", ], aes(x = uniqueMerge, y = norm_Area, color=new_dt.adj.Area, alpha=9/10)) + 
  geom_boxplot(lwd = 0.3, width=0.5,
               position=position_dodge(0.5), outlier.size = 0.5)  + 
  
  theme(plot.title = element_text(size=5) ) +
  xlab("") +
  ylim(0,2) +
  facet_grid(. ~ class_Drug, scales = "free_x", space="free")+
  scale_x_discrete(expand = c(0, 0.5))
d<- c+theme_classic2()


e <- d+ theme(axis.text.x = element_text(angle = 90, hjust=1, vjust=0,size=7),
              axis.text.y = element_text(vjust = 0.5, hjust=1, size=7),
              
              panel.border=element_blank(),
              legend.position = "top",
              legend.text=element_text(size=8),  
              legend.title = element_text(size=9),
              axis.title = element_text(size=9),
              axis.line = element_line(size = 0.2),
              axis.ticks = element_line(size = 0.2))




f <- e + geom_hline(yintercept = 1,
                    linetype ="dotted",
                    color = "black",
                    size = 0.5) 
#CC33CC"


e<-f+scale_color_manual(values=c("#000033", "#660099", "#CC33CC", "#FF3399", "#999999"))

e #to visualize the graph




setwd(".") # set a new working directory   --> ".." means one folder above. "." means the current folder. or use function file.path() to browse to the folder you want
getwd()
output.Directory <- file.path(input.Directory)
pdf( file.path(output.Directory, paste0("heart_Area_boxplot_plot_paper",".pdf") ), width = 10, height = 5) # height of 6 for each plot, width of 5.5 for each plot
e
dev.off()







