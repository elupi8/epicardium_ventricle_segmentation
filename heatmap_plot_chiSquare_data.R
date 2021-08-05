#heatmap plot of the chi square data

rm(list=ls())
library(data.table)
library(ggplot2)

dt = fread(file.path(getwd(), "heat_map_chi_sq_PLOT_HMinverted.csv"), header = T)  #TRA LE PARENTESI QUADRE INDICA CHE VOGLIO TOGLIERE LE COLONNE DEL DATATABLE DOPO IL 9

dt[is.na(dt)]<-0
dt$merge<- paste(dt$p_signif_PE, dt$binary, sep = "_" )

dt$merge<-replace(dt$merge, dt$merge=="ns_1", "ns")
dt$merge<-replace(dt$merge, dt$merge=="ns_0", "ns")


dt$heart <- factor(dt$heart, levels = c("PE_1", "PE_2", "Epi_1", "Epi_2", "HM_1", "HM_2")) 

dt$ID<-factor(dt$ID, levels = c("AA","BL3","CTPB",	"GRC",	"APC",	"B210",	"B281",	"CI_994",	"FSAHA",	"M344",	"MC",	"NCH_51",	"NSC",	"OXL",	"PBANa",	"SAHA",	"SBHA","SPT","NSPT",	"TSA",	"VPA",	"VPAH",	"ARVS", "B278", "PCT",	"RSV", 	"TARV",	"AGK2", "B2",	"B266", "EX",	"NAM", "SLM", "SPL","SRM", "SRN",  "AZA", "BIXT","INAM", "ITSA","PDA","TNCH",	"ZBL"))
g<- ggplot(dt, aes(x=heart, y=ID, fill= merge)) + geom_tile (colour = "white")+ scale_fill_manual(values = c("#FFFFFF", "#CCCCCC", "#660066", "#FFCC00"))+
  theme_classic() +  coord_fixed(ratio = 2/6) +theme(axis.title.x=element_blank(), axis.title.y = element_blank(), axis.text.y= element_text(size=7), axis.text.x= element_text(size=7), legend.text = element_text(size=7), legend.title = element_blank())
  
g

setwd(".") # set a new working directory   --> ".." means one folder above. "." means the current folder. or use function file.path() to browse to the folder you want
getwd()
output.Directory <- file.path(getwd())
pdf( file.path(output.Directory, paste0("heat_map_chi_sq_INVERTED_hm",".pdf") ), width = 10, height = 5) # height of 6 for each plot, width of 5.5 for each plot
g
dev.off()


