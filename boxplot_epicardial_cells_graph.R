#generate boxplots of epicardial cells, before the in the merge file was mnually added a column corresponding to number of test and incross or outcross


rm(list=ls())
setwd(".") # set a new working directory   --> ".." means one folder above. "." means the current folder. or use function file.path() to browse to the folder you want
getwd()

input.Directory = getwd()



input.file.pm = file.path(input.Directory, "new_epi_cells_for_plot.csv")

dt_epicard = fread(input.file.pm, header = T)

dt_epicard = dt_epicard[!(Well %in% c("", NA) ),]

dt_epicard = dt_epicard[, uniqueMerge := paste(Drug_code, Concentration, esp, sep = "_")] #create unique Drug IDs by pasting the columns Drug and Concentration

### for control
aggregationColumn <- dt_epicard[, uniqueMerge]

dt_epicard$class_Drug <- factor(dt_epicard$class_Drug, levels = c("HDAC_inhibitor", "SIRT_activator", "SIRT_inhibitor", "HAT_inhibitor","others")) 


c<-ggplot(dt_epicard[dt_epicard$Drug_code != "DMSO", ], aes(x = uniqueMerge, y = cell.epicardium_norm, color=new_dt.adj_cell.epicardium, alpha=9/10)) + 
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





g<-f+scale_color_manual(values=c("#000033", "#660099", "#CC33CC", "#FF3399", "#999999"))

g




setwd(".") # set a new working directory   --> ".." means one folder above. "." means the current folder. or use function file.path() to browse to the folder you want
getwd()
pdf( file.path(input.Directory, paste0("EPI_boxplot_plot",".pdf") ), width = 10, height = 5) # height of 6 for each plot, width of 5.5 for each plot
g
dev.off()
