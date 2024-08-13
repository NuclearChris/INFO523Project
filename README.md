This project explores the National Solar Radiation Database hosted by the National Renewable Energies Laboratory (NREL) to find patterns in solar and wind energy at locations in Arizona. This project involves creating an R-programming script to interact with the NSRDB, to process and organize the data, and to generate plots and summary statistics. This project mainly explores Tucson, AZ and has a minor exploration of Phoenix, AZ, but the scripts provided can explore the entire United States or more. The README file included in this package provides instructions on modifying the POINTS list in the code to explore new areas of interest.

For more information about the NSRDB by NREL, go here:
https://nsrdb.nrel.gov/about/what-is-the-nsrdb

For more information about this project, see the powerpoint presentation here:
https://drive.google.com/drive/folders/1FpCvhm5oyu_E892sVgDHbJI9ptkpxrSZ?usp=drive_link

#########
IMPORTANT
#########

To run this code, the following libraries must be installed!

ggplot2
dplyr
scales
readr
glue
utils
ggpmisc
httr
jsonlite

#########
PreProcess.R will gather the data from NSRDB and organize it into dataframes
Dataminer.R will call the PreProcess script and generate statistics and plots
