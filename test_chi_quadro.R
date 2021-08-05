#test chi quadro for heart malformation

# clean environment
rm(list=ls())
library(data.table)

dt = fread(file.path(getwd(), "test_chi_quadro.csv"), header = T) [,1:10] #TRA LE PARENTESI QUADRE INDICA CHE VOGLIO TOGLIERE LE COLONNE DEL DATATABLE DOPO IL 9



dt_control= dt[drug == "DMSO"]
dt_treatments= dt [!(drug== "DMSO")]



# n_ID = sort( unique( dt$ID ) )
# 
# names(dt)
#cols_OI = c("HM","no_HM")

# pre allocate list to hold results
l_fraction = vector(mode = "list", length = nrow(dt_treatments))

#chamge HM with PE or Epi to calculate the chi square for PE or epicardium

for(i in 1:nrow(dt_treatments)){
  i = 1
  temp_exp = dt_treatments[i, exp]
  temp_drug = dt_treatments[i, drug]
  
  temp_dt_exp = dt_treatments[i, ]
  temp_dt_ctrl = dt_control[exp == temp_exp,]
  
  temp_dt_exp$fraction= (temp_dt_exp$HM/temp_dt_exp$TOT_embryos)
  temp_dt_ctrl$fraction= (temp_dt_ctrl$HM/temp_dt_ctrl$TOT_embryos)
 
  temp_dt = rbindlist(list(temp_dt_exp, temp_dt_ctrl), use.names = T)
  temp_dt$value= sum(temp_dt_exp$fraction,-temp_dt_ctrl$fraction)
  
  
  
  temp_dt= temp_dt[, binary:= "0"  ] #if epi or PE write 1
  temp_dt = temp_dt[value <= 0 , binary:= "1"  ] #if epi or PE write 0
  
 
  
  ## save the output dt of the loop in the list
  l_fraction[[i]] = temp_dt
  ## give name to the element of the list (choose in a conveniente way to retain valuable identifiers)
  names(l_fraction)[i] = paste0(temp_drug, "_", temp_exp )
  
}

## create dt out of the list
dt_temp = rbindlist(l_fraction, idcol =  T)
dt_temp= dt_temp[!(drug == "DMSO")]


#dt_chi$ID = as.numeric(dt_chi$ID

# sort columns the way you want

dt_temp = dt_temp[with(dt_temp, order(.id)), ]
dt_temp = dt_temp[with(dt_temp, order(class)), ] # order() accept multiple columns and the minus sign "-" to reverse the order

fwrite(dt_temp, file = "chi_sq_HM_incross_binary_INVERTED.csv", quote = FALSE )
















