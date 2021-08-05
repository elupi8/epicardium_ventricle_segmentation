#merge ventricle area with platemap file
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
  
  
  dt_loc = fread(fileIn, header = T)  #dt_loc stands for local data table (inside the function) , header = TRUE means that the first row is read as a header (column names)
  head(dt_loc) # head gives you the first rows
  
  names(dt_loc)  #names of the columns 
  
  # delete first column
  dt_loc = dt_loc[, -c(1)] # or dt_loc[, V1 := NULL]
  
  # extract name of the well form the filename
  
  s.well.loc = str_extract(fileIn, "[:alpha:][:digit:]{3}") # {3} means look for exactly 3 elements. "+" means "how many elements are available, how many you can find that corresponds"
  s.well.loc = toupper(s.well.loc) # make first letter capital
  s.well = paste0( str_extract(s.well.loc, "[:alpha:]") , sprintf( '%03d', as.numeric( str_extract(s.well.loc, '[0-9]+') ) ) ) #padd zer
  
  
  dt_loc = dt_loc[, "Well" := s.well] #define a new column.. if the column already exists and we want to update its value by reference to the same column, use (Gene):= fun(Gene)
  
  
  return(dt_loc)
}



s.exp = "exp13"

# define directory with merged output
setwd("..") # set a new working directory   --> ".." means one folder above. "." means the current folder. or use function file.path() to browse to the folder you want
getwd()

input.Directory = getwd() # ".." means one folder above. "." means the current folder

s.dir.out.mer = "merged_red_output_2"


output.Directory <- file.path(input.Directory, s.dir.out.mer)
ifelse( !dir.exists(output.Directory), dir.create(output.Directory, recursive = T), FALSE)



# define the filename that you want to read or a string contained in the filenames
s.files.core = 'deconvolved_heart(|_seg).csv'



# Import and prepare an object.table that contains all the plates from both cell lines--------

# search subdirectories for csv files with Nuclei data


s.files = list.files(
  path = file.path(input.Directory, "tables"),
  pattern = s.files.core,
  recursive = TRUE, # to look not only in the specified folder, but also in subfolders
  full.names = TRUE
)



s.files  # check that it contains the correct paths to the csv files


## Load csv files by applying the  custom made function readCSV to all the elements whose paths are specified in s.files
dt_segmentation = rbindlist(lapply(s.files, readCSV), use.names=TRUE, fill=FALSE, idcol=NULL) #rbindlist (l, ...) is not faster (less than 3% faster) than do.call(rbind, l) as stated in R documentation; # dt.img = do.call(rbind, lapply(s.files.img, myFreadImg))


output.filename = paste0("Segmentation","_of_", s.exp,".csv")
fwrite(dt_segmentation, file = file.path(output.Directory, output.filename), row.names=FALSE)





# Data cleaning -----------------------------------------------------------



## read platemap

input.file.pm = file.path(input.Directory, "platemap", "platemap.csv")

dt_platemap = fread(input.file.pm, header = T)

dt_platemap = dt_platemap[!(Well %in% c("", NA) ),]


## padd well number 
dt_platemap$Well = toupper(dt_platemap$Well)
dt_platemap$Well = paste0( str_extract(dt_platemap$Well, "[:alpha:]") , sprintf( '%03d', as.numeric( str_extract(dt_platemap$Well, '[0-9]+') ) ) )


# merge platemap with main data table

dt_segmentation = merge(dt_segmentation,
                        dt_platemap,
                        by.x = "Well",
                        by.y = "Well")


# save csv ----------------------------------------------------------------

output.filename = paste0("Merged","_Red","_Segmentation","_of_", s.exp,".csv")
fwrite(dt_segmentation, file = file.path(output.Directory, output.filename), row.names=FALSE)

