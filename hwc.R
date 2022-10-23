## Human Wildlife conflict in Chitwan National Park----
### Saroj Kandel, 10-23-2022, sarojkndl@gmail.com----
library(dplyr)
library(readxl)
data1<-read_xlsx("hwc_data.xlsx" )
data1
#making data frame----
data2<-data.frame(data1)
data2
str(data2)

#changing character column into factor column----
data3<-as.data.frame(unclass(data2),stringsAsFactors = TRUE)
str(data3)

#for categorical variables use Table to find statistics----
table(data3$Species)
###finding the proportions or percentage of each species----
table_species<- table(data3$Species)
table_species
percentage_species<- prop.table(table_species)
percentage_species

# for cross tabulation----
#install.packages("gmodels")
library(gmodels)
CrossTable(data3$code_attack, data3$Location)# 1 is death and 0 is injury of humans















