# clean environment
rm(list=ls())

{
  library(data.table)
  library(dplyr)
  library(ggplot2)
  library(xlsx)
  library(stringr)
  #library(openxlsx)
}

# read.xlsx <- xlsx::read.xlsx
# write.xlsx <- xlsx::write.xlsx



## function
readCSV = function(fileIn) {
  
  ###
  #fileIn = s.files[1]
  ###
  print(fileIn)
  
  
  dt_loc = fread(fileIn, header = T)  #dt_loc stands for local data table (inside the function) 
  head(dt_loc)
  
  names(dt_loc)  #names of the columns 
  
  # delete first column
  dt_loc = dt_loc[,] # or dt_loc[, V1 := NULL]
  
  # extract name of the well form the filename
  
  s.well.loc = str_extract(fileIn, "[:alpha:][:digit:]{3}")
  s.well.loc = toupper(s.well.loc)
  s.well = paste0( str_extract(s.well.loc, "[:alpha:]") , sprintf( '%03d', as.numeric( str_extract(s.well.loc, '[0-9]+') ) ) )
  
  
  dt_loc = dt_loc[, "Well" := s.well] #define a new column.. if the column already exists and we want to update its value by reference to the same column, use (Gene):= fun(Gene)
  
  
  return(dt_loc)
}


s.exp = "exp7_i" #name folder


# define directory with merged output
setwd(".") # set a new working directory   --> ".." means one folder above. "." means the current folder.

input.Directory = getwd() #give the name of the current directory

s.dir.out.mer = "max_epicardium" #output directory


output.Directory <- file.path(input.Directory, s.dir.out.mer)
ifelse( !dir.exists(output.Directory), dir.create(output.Directory, recursive = T), FALSE) #if the folder already exists R is rewritnig on that folder



s.files.core = 'deconvolved_epic_cells.csv' #part of the nae of the input file


# search subdirectories for csv files with Nuclei data

s.files = list.files(
  path = file.path(input.Directory,"tables"),
  pattern = s.files.core,
  recursive = TRUE, # to look not only in the specified folder, but also in subfolders
  full.names = TRUE
)

s.files  # list of files finded


dt_segmentation = rbindlist(lapply(s.files, readCSV), use.names=TRUE, fill=FALSE, idcol=NULL) #rbindlist (l, ...) is not faster (less than 3% faster) than do.call(rbind, l) as stated in R documentation; # dt.img = do.call(rbind, lapply(s.files.img, myFreadImg))
dt_epicardium = dt_segmentation[,.(cell.epicardium= max(V1)), by =Well] ##get the max value of the column without header (indicating the number of the epicardial cells)

output.filename = paste0("cells","_of_","_epicardium_", s.exp,".csv") #name of the output files
fwrite(dt_epicardium, file = file.path(output.Directory, output.filename), row.names=FALSE) 



##merge the csv file with the platemap files

## read platemap

input.file.pm = file.path(input.Directory, "platemap", "platemap.csv") #platemap is a file containing name of the drugs, drug code, concentration etc...

dt_platemap = fread(input.file.pm, header = T)

dt_platemap = dt_platemap[!(Well %in% c("", NA) ),]


## padd well number 
dt_platemap$Well = toupper(dt_platemap$Well) #capitle letters
dt_platemap$Well = paste0( str_extract(dt_platemap$Well, "[:alpha:]") , sprintf( '%03d', as.numeric( str_extract(dt_platemap$Well, '[0-9]+') ) ) )


# merge platemap with main data table

dt_epicardium = merge(dt_epicardium,
                        dt_platemap,
                        by.x = "Well",
                        by.y = "Well")


# Save as -----------------------------------------------------------------

output.filename = paste0("Merged","_epicardial","_cells","_of_", s.exp,".csv") #name of the output files
fwrite(dt_epicardium, file = file.path(output.Directory, output.filename), row.names=FALSE)



