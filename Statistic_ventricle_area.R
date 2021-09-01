# clean environment
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

## function
readCSV = function(fileIn) {
  
  
  print(fileIn)
  
  
  dt_loc = fread(fileIn, header = T)  #dt_loc stands for local data table (inside the function) , header = TRUE means that the first row is read as a header (column names)
  head(dt_loc) # head gives you the first rows
  
  names(dt_loc)  #names of the columns 
  
  
  
  #change column name
  setnames(dt_loc, old = c("Drug code", "class Drug"), new= c("Drug_code", "class_Drug"), skip_absent = TRUE)
  
  
  # extract metadata from the file name
  s.metadata.loc = str_extract(fileIn, "esp[:digit:]{1,2}([:graph:].*|)(?=.csv)") # {3} means look for exactly 3 elements. "+" means "how many elements are available, how many you can find that corresponds"
  s.exp.loc=str_extract(s.metadata.loc,"esp[:digit:]{1,2}")
  s.cross.loc=str_extract(s.metadata.loc, "(?<=esp[:digit:]{1,2}(_Stardist_)).*(?=.automatic)") 
 
  
  dt_loc = dt_loc[, "Experiment" := s.exp.loc] #define a new column.. if the column already exists and we want to update its value by reference to the same column, use (Gene):= fun(Gene)
  
  
  
  dt_loc = dt_loc[, "Cross" := s.cross.loc] #define a new column.. if the column already exists and we want to update its value by reference to the same column, use (Gene):= fun(Gene)
  
  
  
  return(dt_loc)
}


# define directory with merged output
setwd(".") # set a new working directory   --> ".." means one folder above. "." means the current folder. or use function file.path() to browse to the folder you want
getwd()

input.Directory = getwd() # ".." means one folder above. "." means the current folder

s.heart = "heart" # Green
s.dir.out.mer = paste0("all_exp_merged_",s.heart,"_output")


output.Directory <- file.path(input.Directory, s.dir.out.mer, s.heart)
ifelse( !dir.exists(output.Directory), dir.create(output.Directory, recursive = T), FALSE)



# define the filename that you want to read or a string contained in the filenames
s.files.core = paste0("Merged_", s.heart,"_of_esp") #input file name


# Import and prepare an object.table that contains all the plates from both cell lines--------

# search subdirectories for csv files with Nuclei data


s.files = list.files(
  path = file.path(input.Directory),
  pattern = s.files.core,
  recursive = TRUE, # to look not only in the specified folder, but also in subfolders
  full.names = TRUE
)

sort(s.files ) # check that it contains the correct paths to the csv files

s.files.test=s.files[1]
## combine all the csv of the ventricle paramenters together from different experiments
dt_segmentation = rbindlist(lapply(s.files, readCSV), use.names=TRUE, fill=FALSE, idcol=NULL) #rbindlist (l, ...) is not faster (less than 3% faster) than do.call(rbind, l) as stated in R documentation; # dt.img = do.call(rbind, lapply(s.files.img, myFreadImg))


output.filename = paste0("Segmentation","_of_heart","_all_exp",".csv")
fwrite(dt_segmentation, file = file.path(output.Directory, output.filename), row.names=FALSE)





#-----------------------------------------------------------

names(dt_segmentation)
dt_stats = dt_segmentation[, .(mean.Area = mean(Area), #mean
                               se.Area = sd(Area)/ sqrt(length(Area)),  #standard error
                               median.Area = as.numeric(median(Area)), #median
                               Q1.Area = quantile(Area, 1/4)[[1]], #1st quartile
                               Q3.Area = quantile(Area, 3/4)[[1]], #3rd quartile
                               IQR.Area = IQR(Area), #interquartile range
                               mad.Area = mad(Area), #median absolute deviation
                               sd.Area = sd(Area) # standard deviation
                               
                               
),
by = c("Drug", "Concentration", "Experiment", "Cross") #by = .(Image_Metadata_WellNum, Image_Metadata_PlateName)
]

### normalize data against control

s.experiments = sort(unique(dt_segmentation$Experiment)) 
features_toNorm = c("Area")

s.control.toNorm = "dmso" #or "all_library_withoutCtrl_butWithXWNeg9"

s.cols.combination = c("Experiment", "Cross")
dt_combination = unique(dt_segmentation[, ..s.cols.combination])


for (i in 1:length(features_toNorm)) {
  
  # i = 1
  ###
  
  y_plot = features_toNorm[i]
  
  previous_temp_experiment = "A"
  
  for (j in 1:dim(dt_combination)[1] ) {
    # j=1
    
    temp_experiment = dt_combination[j, Experiment]
    temp_cross = dt_combination [j, Cross]
    
    
    if ( !(temp_experiment == previous_temp_experiment) == TRUE) {
      
      col.median = names(dt_stats)[grepl(paste0("median.", y_plot), names(dt_stats))]   
      
      col.mean = names(dt_stats)[grepl(paste0("mean.", y_plot), names(dt_stats))] 
      
      
      
      median_to_norm= dt_stats[(Experiment == temp_experiment) &
                                 (Drug == s.control.toNorm) &
                                 (Cross == temp_cross)] [, get(noquote(col.median)) ] 
      
      
      mean_to_norm= dt_stats[(Experiment == temp_experiment) &
                                 (Drug == s.control.toNorm) &
                                 (Cross == temp_cross)] [, get(noquote(col.mean)) ] 
      
      col.mad = names(dt_stats)[grepl(paste0("mad.", y_plot), names(dt_stats))]   #mad.= robust standard deviation (MAD)
      col.sd = names(dt_stats)[grepl(paste0("sd.", y_plot), names(dt_stats))]
      
      
      mad_to_norm = dt_stats[(Experiment == temp_experiment) &
                               (Drug == s.control.toNorm) &
                               (Cross == temp_cross)] [, get(noquote(col.mad)) ] 
      
      sd_to_norm = dt_stats[(Experiment == temp_experiment) &
                               (Drug == s.control.toNorm) &
                               (Cross == temp_cross)] [, get(noquote(col.sd)) ]
      
    }
    
    ## create table with normalized data
    y_plot_norm = paste0(y_plot,"_norm") #create string name for the normalized variable
    y_plot_robustZscore = paste0(y_plot,"_robustZscore")
    y_plot_Zscore = paste0(y_plot,"_Zscore")
    
    # normalize against control
    dt_segmentation[(Experiment == temp_experiment) &
                      (Cross== temp_cross), noquote(y_plot_norm) := get(noquote(y_plot)) / median_to_norm] #area di ogni well diviso mediana dell area del controllo
    
    
    dt_segmentation[(Experiment == temp_experiment) &
                      (Cross== temp_cross), noquote(y_plot_robustZscore) := (get(noquote(y_plot)) - median_to_norm) / mad_to_norm ]
    dt_segmentation[(Experiment == temp_experiment) &
                      (Cross== temp_cross), noquote(y_plot_Zscore) := (get(noquote(y_plot)) - mean_to_norm) / sd_to_norm ]
    
  }
}


head(dt_segmentation[, c("Area_robustZscore", "Area_norm")])

names(dt_segmentation)



### save table of normilized data by the median of the control and the calculation of zscore and robust zscore

output.norm_red = paste0("norm_heart_robZscore_all_exp",".csv")
fwrite(dt_segmentation, file = file.path(output.Directory, output.norm_red), row.names=FALSE)


# p_values statistics ----------------------------------------------------------------

dt_segmentation = dt_segmentation[, uniqueID := paste(Drug, Concentration, sep = "_")] #create unique Drug IDs by pasting the columns Drug and Concentration
sort( unique(dt_segmentation$uniqueID) )
s.uniqueIDs = sort( unique(dt_segmentation$uniqueID) )

s.grepIDs = s.uniqueIDs[ grep("(DMSO|dmso)", s.uniqueIDs) ]


old.names = c(s.grepIDs)
new.names = c("DMSO", "DMSO")


dt_changenames = data.table(old_name = old.names,
                            new_name = new.names)



dt_segmentation$uniqueID = stri_replace_all_fixed(dt_segmentation$uniqueID,
                                                  pattern = dt_changenames$old_name,
                                                  replacement = dt_changenames$new_name,
                                                  vectorize_all = F)




# activate only if you have to calculate again the p values
###  p-values
l.pvalues = vector(mode = "list", length = dim(dt_combination)[1])

unique(dt_segmentation$Experiment)


for (i in 1: dim(dt_combination)[1]){
  # i = 1
  temp_exp = as.character(dt_combination[i,1])
  temp_cross = as.character(dt_combination[i,2])
  
  
  
  dt_p_values_temp = compare_means(c(Area,
                                     #, erkCNratio_LQBgCorr_robustZscore
                                     #, aktCNratio_LQBgCorr_robustZscore
  )
  ~ uniqueID #concentration
  , data = dt_segmentation[(Experiment == temp_exp) &
                             (Cross == temp_cross)] #exp cross drug
  , method = "kruskal.test" #  default is "wilcox.test", but it accepts "t.test", "anova", "kruskal.test"
  
  # , group.by = "unique_Treatment_Stats"
  # a character string specifying the reference group. If specified, for a given grouping variable, each of the group levels will be compared to the reference group (i.e. control group). ref.group can be also ".all.".
  , p.adjust.method = "BH" # "holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none"
  , symnum.args = list(cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, 1),
                       symbols = c("****", "***", "**", "*", "ns")) #In other words, we use the following convention for symbols indicating statistical significance: 1) ns: p > 0.05 / 2) *: p <= 0.05 / 3) **: p <= 0.01 / 4) ***: p <= 0.001 / 5) ****: p <= 0.0001
  )
  
  class(dt_p_values_temp)
  
  dt_p_values_temp = as.data.table(dt_p_values_temp)
  
  
  l.pvalues[[i]] = dt_p_values_temp
  names(l.pvalues)[i] = paste(temp_exp, temp_cross, sep = "_") # name of the i-th element of the list
}

l.pvalues[1]
l.pvalues.backup = copy(l.pvalues)


dt.pvalues = rbindlist(l.pvalues, use.names = T, fill = T, idcol = "unique_plate")

setnames(dt.pvalues, old = c(".y."), new = c( "feature"), skip_absent = T)


dt.pvalues = separate(data = dt.pvalues, col = "unique_plate", into = c("Experiment","Cross"), sep = "_")



# make dt_pvalues wide format
names(dt.pvalues)

dt.pvalues.wide = pivot_wider(dt.pvalues#[,c("ncells", "nFOV") := NULL]
                              , id_cols = c("Experiment","Cross","method" ), #cols not to pivot
                              names_from = "feature", #to pivot
                              values_from = c("p", "p.adj", "p.format", "p.signif"))

output.p_values = "2_kruskal_pValues_heart_Wide.csv"
fwrite(dt.pvalues.wide, file = file.path(input.Directory, s.dir.out.mer, s.heart, output.p_values ), row.names = TRUE, quote = FALSE )



for (i in 1: dim(dt_combination)[1]){
  #i = 3
  temp_exp = as.character(dt_combination[i,1])
  temp_cross = as.character(dt_combination[i,2])
  
  
  
  dt_p_values_temp = compare_means(c(Area,
                                    
                                     
  )
  ~ uniqueID #concentration
  , data = dt_segmentation[(Experiment == temp_exp) &
                             (Cross == temp_cross)] #exp cross drug
  , method = "wilcox.test" #  default is "wilcox.test", but it accepts "t.test", "anova", "kruskal.test"
  , paired = FALSE
  # , group.by = "unique_Treatment_Stats"
  , ref.group = "DMSO" 
  , p.adjust.method = "BH" # "holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none"
  , symnum.args = list(cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, 1),
                       symbols = c("****", "***", "**", "*", "ns"))
  )
  
  class(dt_p_values_temp)
  
  dt_p_values_temp = as.data.table(dt_p_values_temp)
  
  
  l.pvalues[[i]] = dt_p_values_temp
  names(l.pvalues)[i] = paste(temp_exp, temp_cross, sep = "_") # name of the i-th element of the list
}

l.pvalues[1]
l.pvalues.backup = copy(l.pvalues)


dt.pvalues = rbindlist(l.pvalues, use.names = T, fill = T, idcol = "unique_plate")

setnames(dt.pvalues, old = c("group1","group2", ".y."), new = c("ctrl_treatment","treatment", "feature"), skip_absent = T)


dt.pvalues = separate(data = dt.pvalues, col = "unique_plate", into = c("Experiment","Cross"), sep = "_")


dt.pvalues = dt.pvalues[ , new_dt.adj:= "ns"  ]
dt.pvalues = dt.pvalues[p.adj<=0.05,  new_dt.adj:= "*"  ]
dt.pvalues = dt.pvalues[p.adj<=0.01,  new_dt.adj:= "**"  ]
dt.pvalues = dt.pvalues[p.adj<=0.001,  new_dt.adj:= "***"  ]
dt.pvalues = dt.pvalues[p.adj<=0.0001,  new_dt.adj:= "****"  ]





# make dt_pvalues wide format
names(dt.pvalues)

dt.pvalues.wide = pivot_wider(dt.pvalues
                              , id_cols = c("Experiment","Cross","ctrl_treatment","treatment","method" ), #cols not to pivot
                              names_from = "feature", #to pivot
                              values_from = c("p", "p.adj", "p.format", "p.signif", "new_dt.adj"))



##merge

dt_segmentation_pvalues = merge(dt_segmentation,
                                dt.pvalues.wide,
                                by.y = c("treatment","Experiment", "Cross"),
                                by.x =  c("uniqueID","Experiment", "Cross"),
                                all = T)


#change all the commas with underscores

dt_segmentation_pvalues$Drug = gsub(",", replacement = "_", dt_segmentation_pvalues$Drug )
dt_segmentation_pvalues$uniqueID = gsub(",", replacement = "_", dt_segmentation_pvalues$uniqueID )





output.p_values = "2_wilcoxtest_pValues_heart_Wide.csv" #final csv
fwrite(dt_segmentation_pvalues, file = file.path(input.Directory, s.dir.out.mer, s.heart, output.p_values ), row.names = TRUE, quote = FALSE )



