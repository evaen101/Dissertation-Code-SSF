### STEP FOUR
## JOIN SCRIPT

## To run SSF, the annotated datasets for each individual must be joined into one
## This will allow the SSF to be composed of comprehensive annotated datasets 

#In this script, '##' marks variables where IDs should be changed for joining datasets

#Set wd
setwd("H:/DISS FOLDER/Data Transfer")

#install.packages("survival")
library(survival)
#install.packages("dplyr")
library(dplyr)
#Load the annotated datasets
modis<-read.csv('West_Win1516-MODIS,AST,LC.csv') ##
precip<-read.csv('West_Win1516-PRECIP.csv') ##
snow<-read.csv('West_Win1516-SNOWDEP.csv') ##
temp<-read.csv('West_Win1516-TEMP.csv') ##
wind<-read.csv('West_Win1516-WINDANALYSIS.csv') ##
#Load the original data, no annotations
original<-read.csv('West_Win1516ANNOTATE.csv') ##
#Join the annotated data to the original dataset
data <- modis %>%
  inner_join(precip, by = "row_ID") %>%
  inner_join(snow, by = "row_ID") %>%
  inner_join(temp, by = "row_ID") %>%
  inner_join(wind, by = "row_ID") %>%
  inner_join(original, by = "row_ID")
head(data)
#Clean up the dataset to keep only necessary columns
data<-data[c('timestamp.x','location.long.x','location.lat.x','step_id_.x',
             'SEDAC.GPW.V3.and.GRUMP.V1.GRUMP.2000.Population.Density.Adjusted',
             'MODIS.Land.LAI...FPAR.500m.4d.Combined.FPAR',
             'MODIS.Land.LC..05deg.Yearly.Combined.Majority.Type.1..IGBP.',
             'MODIS.Land.LC.500m.Yearly.Combined.Type.1..IGBP.',
             'MODIS.Land.LAI...FPAR.500m.8d.Combined.FPAR',
             'MODIS.Land.Vegetation.Indices.500m.16d.Aqua.NDVI',
             'SEDAC.GPW.V3.and.GRUMP.V1.GPW.2000.Population.Density.Adjusted',
             'ASTER.Elevation','MODIS.Land.Vegetation.Indices..05deg.16d.Terra.NDVI',
             'MODIS.Land.Vegetation.Indices.500m.16d.Terra.NDVI',
             'MODIS.Land.Vegetation.Indices..05deg.16d.Aqua.NDVI',
             'ECMWF.ERA5.Land.Total.Precipitation',
             'ECMWF.ERA5.Land.Snow.Depth',
             'ECMWF.ERA5.Land.Temperature..2.m.above.Ground.',
             'support','cross')]

head(data)
str(data)
#Rename columns for clarity
colnames(data)[1]<-"timestamp"
colnames(data)[2]<-"longitude"
colnames(data)[3]<-"latitude"
colnames(data)[4]<-"step_id"
colnames(data)[5]<-"pop_den_coarse"
colnames(data)[6]<-"fpar_500m_4d"
colnames(data)[7]<-"land_cover_.05"
colnames(data)[8]<-"land_cover_500m"
colnames(data)[9]<-"fpar_500m_8d"
colnames(data)[10]<-"ndvi_500m_16d_aqua"
colnames(data)[11]<-"pop_den_fine"
colnames(data)[12]<-"elevation"
colnames(data)[13]<-"ndvi_.05_16d_terra"
colnames(data)[14]<-"ndvi_500m_16d_terra"
colnames(data)[15]<-"ndvi_.05_16d_aqua"
colnames(data)[16]<-"precip"
colnames(data)[17]<-"snow"
colnames(data)[18]<-"temp"
colnames(data)[19]<-"wind_support"
colnames(data)[20]<-"crosswind"

head(data)
#Reassign the case column values as in original step creation
# Real steps are 1, Fake steps are 0
data$case<- "na"
data$case[1]<-1
data$case[2:11]<-0
rows<-nrow(data)
for(i in seq(12,rows,by=11)){
  data$case[i]<-1
  data$case[(i+1):(i+10)]<-0
}
head(data)
colnames(data)[21]<-"case"
head(data)
#View(data)

#Add ID column
data$ID<- "West_Win1516" ##
head(data)

#Save this new data 
write.csv(data,'West_Win1516-FINAL.csv') ##

