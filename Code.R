################################################################################
################################################################################
####################                 2019                  #####################
################################################################################
################################################################################
#https://www2.census.gov/programs-surveys/acs/data/pums/2019/1-Year/
#csv_pus.zip
#Note that for the National files, there are multiple files. 
#For PUMS 1-year data there is an “A” and “B” file. For Person-level data the 
#names are “PSAM_PUSA” and “PSAM_PUSB”. The Housing-level files are “PSAM_HUSA” 
#and “PSAM_HUSB”.

if (!require("readr")) install.packages("readr", dependencies = TRUE)
library(readr)

# We append a and b data once
#data_a <- read.csv("/Users/zequi/Desktop/Berkeley/230A/stat230A/csv_pus/psam_pusa.csv")
#data_b <- read.csv("/Users/zequi/Desktop/Berkeley/230A/stat230A/csv_pus/psam_pusb.csv")
#Below are instructions for concatenating the two 1-year “a” and “b” PUMS files 
#to create a single national file. The code is in italics and uses SAS 
#programming code. Concatenate the person-level files using the set statement:
#data population; set psam_pusa psam_pusb; run;
#data <- rbind(data_a, data_b)
#write.csv(data, "/Users/zequi/Desktop/Berkeley/230A/stat230A/psam_pus.csv", row.names = FALSE)
#zip("psam_pus.zip", "psam_pus.csv")
#file.remove("/Users/zequi/Desktop/Berkeley/230A/stat230A/psam_pus.csv")


zip_file_path <- "/Users/zequi/Desktop/Berkeley/230A/stat230A/psam_pus.zip"
csv_file_inside_zip <- "psam_pus.csv"
data_2019 <- read.csv(unzip(zip_file_path, files = csv_file_inside_zip))
data_2019$HISP_dummy <- ifelse(data_2019$HISP == 1, 0, 1)
#White Non-Latino
data_2019$RACWHT_NOT_HISP <- ifelse(data_2019$HISP_dummy == 0 $ data_2019$RACWHT ==1, 0, 1)
#Black Non-Latino
data_2019$RACBLK_NOT_HISP <- ifelse(data_2019$HISP_dummy == 0 $ data_2019$RACBLK ==1, 0, 1)

#Adults non-Americans who are living in the US for more than 1 year
#ATIVITY, CIT, POB
non_americans_2019 <- data_2019[data_2019$NATIVITY==2 & data_2019$CIT!=3 & data_2019$AGEP>=18 & (data_2019$MIG==1 | data_2019$MIG==3),]
#Asian dummy
non_americans_2019$RACASN
#asian <- data_2019[data_2019$RACASN==1,]
#Asian detailed
non_americans_2019$RAC2P
#Hispanic dummy
data_2019$HISP_dummy
#Hispanic detailed
data_2019$HISP


