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


## function
readCSV = function(fileIn) {
  
  print(fileIn)
  
  
  dt_loc = fread(fileIn, header = T)  #dt_loc stands for local data table (inside the function) 
  head(dt_loc)
  
  names(dt_loc)  #names of the columns 
  
  # delete first column
  dt_loc = dt_loc[, V1 := NULL] # or dt_loc[, V1 := NULL]
  
  # extract name of the well from the filename
  
  s.well.loc = str_extract(fileIn, "[:alpha:][:digit:]{3}")
  s.well.loc = toupper(s.well.loc)
  s.well = paste0( str_extract(s.well.loc, "[:alpha:]") , sprintf( '%03d', as.numeric( str_extract(s.well.loc, '[0-9]+') ) ) )
  
  
  dt_loc = dt_loc[, "Well" := s.well] #define a new column.. 
  
  return(dt_loc)
}


s.exp = "esp25"


# define directory with merged output
setwd(".") # set a new working directory   --> ".." means one folder above. "." means the current folder. or use function file.path() to browse to the folder you want

input.Directory = getwd() # ".." means one folder above. "." means the current folder

s.dir.out.mer = "heart"


output.Directory <- file.path(input.Directory, s.dir.out.mer)
ifelse( !dir.exists(output.Directory), dir.create(output.Directory, recursive = T), FALSE)




# define the filename that you want to read or a string contained in the filenames
s.files.core = 'deconvolved_heart.csv' #input file name


s.files = list.files(
  path = file.path(input.Directory,"tables"),
  pattern = s.files.core,
  recursive = TRUE, # to look not only in the specified folder, but also in subfolders
  full.names = TRUE
)

s.files  # check that it contains the correct paths to the csv files

## Load csv files by applying the  custom made function readCSV to all the elements whose paths are specified in s.files
dt_segmentation = rbindlist(lapply(s.files, readCSV), use.names=TRUE, fill=TRUE, idcol=NULL) #rbindlist (l, ...) is not faster (less than 3% faster) than do.call(rbind, l) as stated in R documentation; # dt.img = do.call(rbind, lapply(s.files.img, myFreadImg))
#dt_epicardium = dt_segmentation[,.(cell.epicardium= max(V1)), by =Well]

output.filename = paste0("heart", s.exp,".csv") #otput file name
fwrite(dt_segmentation, file = file.path(output.Directory, output.filename), row.names=FALSE)


#merge file with the platemap containing: name of the drugs, concentration etc..

input.file.pm = file.path(input.Directory, "platemap", "platemap.csv") #import platempa

dt_platemap = fread(input.file.pm, header = T)

dt_platemap = dt_platemap[!(Well %in% c("", NA) ),]


## padd well number 
dt_platemap$Well = toupper(dt_platemap$Well) #capital letter
dt_platemap$Well = paste0( str_extract(dt_platemap$Well, "[:alpha:]") , sprintf( '%03d', as.numeric( str_extract(dt_platemap$Well, '[0-9]+') ) ) )


# merge platemap with main data table according to the column Well

dt_epicardium = merge(dt_segmentation,
                      dt_platemap,
                      by.x = "Well",
                      by.y = "Well")


# Save as -----------------------------------------------------------------

output.filename = paste0("Merged","_heart","_of_", s.exp,".csv") #output file name
fwrite(dt_epicardium, file = file.path(output.Directory, output.filename), row.names=FALSE)

