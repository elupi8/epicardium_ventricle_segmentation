#merge file from automatic and manual quantification with platemap files
setwd(".") # set a new working directory   --> ".." means one folder above. "." means the current folder. or use function file.path() to browse to the folder you want
getwd()

input.Directory = getwd() # ".." means one folder above. "." means the current folder

s.dir.out.mer = "merged_esp10_output"


output.Directory <- file.path(input.Directory, s.dir.out.mer)
ifelse( !dir.exists(output.Directory), dir.create(output.Directory, recursive = T), FALSE)




input.file.pm = file.path(input.Directory, "platemap.csv")


dt_platemap = fread(input.file.pm, header = T)

dt_platemap = dt_platemap[!(Well %in% c("", NA) ),]

input.file.pm = file.path (input.Directory, "manual_automatic_esp10.csv")
dt_am= fread(input.file.pm, header = T)
dt_am = dt_am[!(Well %in% c("", NA) ),]
## padd well number 
dt_platemap$Well = toupper(dt_platemap$Well)
dt_platemap$Well = paste0( str_extract(dt_platemap$Well, "[:alpha:]") , sprintf( '%03d', as.numeric( str_extract(dt_platemap$Well, '[0-9]+') ) ) )

dt_am$Well = toupper(dt_am$Well)
dt_am$Well = paste0( str_extract(dt_am$Well, "[:alpha:]") , sprintf( '%03d', as.numeric( str_extract(dt_am$Well, '[0-9]+') ) ) )

# merge platemap with main data table

dt_am = merge(dt_am,
              dt_platemap,
              by.x = "Well",
              by.y = "Well")


# save csv ----------------------------------------------------------------

output.filename = paste0("Merged","_AM","_of_","esp_10",".csv")
fwrite(dt_am, file = file.path(output.Directory, output.filename), row.names=FALSE)

