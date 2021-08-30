# epigenetic_screening_code
scripts for epicardial and ventricle area segmentation


R script for epicardial cells quantification 

1) Name of the script: “find_max_cell_epicardium”
This script is needed to obtain the exact number of epicardial cells from the csv file obtained from Fiji after analyze particles.
In those csv files indeed, each epicardial cells of one heart is listed one by one with their relative parameters. the first column of the csvs (without header) indicates the number of the cells (1,2,3…,65..), the last number (the biggest) indicates the total number of the epicardial cells in this heart. With this script you can obtain the last value (the max) indicating the number of epicardial cells per each csv.
With this script we also combine the platemap file containing: name of drugs, drug code, concentration..., with the number of cells and the well, name of the file: Merged_epicardial_cells_of_"name of the experiment"


2) Name of the script: 


