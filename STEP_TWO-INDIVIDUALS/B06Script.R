## B06 DATA

#Set wd
setwd("C:/Users/evaen/Dissertation")
#Run the packages
library(adehabitatLT) #for analysing trajectories
library(lubridate) #for processing time
library(sf) #for dealing with spatial data
library(ggplot2) #for making nice figures
library(grid) #for creating grids in figures
library(gridExtra) #for creating grids in figures
library(circular) #for circular statistics of TAs and heading
library(move) #another movement package
library(adehabitatHR) #for space use
library(scales) #make polygons transparent in plotting
library(terra) #for manipulating raster data
library(amt) #for creating steps
library(tidyverse) #for help with data challenges
#Load data
data<-read.csv('B06.csv')
head(data)

##BLANK OUT IF RUNNING FOR ENV ANNOTATION
##Preparing temporal attributes
#Split into components
data$tstamp<-strptime(as.character(data$timestamp),"%Y-%m-%d %H:%M:%S")
data$date<-as.Date(data$tstamp)
data$year<-year(data$tstamp)
data$month<-month(data$tstamp)
data$day<-day(data$tstamp)
data$hour<-hour(data$tstamp)
data$min<-minute(data$tstamp)
data$sec<-second(data$tstamp)
head(data)

#Time period covered by the data?
years<-unique(data$year)
years # 2013 to 2019
#Create birdyear id sp that trajectories for differnt years can be separated
data$idyear<-paste0(data$individual.local.identifier,year(data$date))
#Get individuals by year
indivYear<-unique(data$idyear)
indivYear
#Add in time in POSIX format
data$POSIX<-as.POSIXct(data$timestamp,format="%Y-%m-%d %H:%M:%S")
head(data)
#Duplicated times already removed in data cleaning 1A

##Preparing geographic coordinates
#Convert buzzards df into sf object + specify coord system collected
dataSF<-st_as_sf(data,coords=c('location.long','location.lat'),crs=4326)
dataSF
#Use projection suitable for GPS data from north Russia to southwest extent
lcc_crs<-"+proj=lcc +lat_1=46 +lat_2=64 +lon_0=40.85 +ellps=WGS84" #Lambert Conformal Conic 
#Transform data into new coordinate system
dataSF_proj<-st_transform(dataSF,crs=lcc_crs)
head(dataSF_proj)

#Separate X&Y into easting & northing for adehabitatLT use
dataSF_proj$X<-st_coordinates(dataSF_proj)[,1]
dataSF_proj$Y<-st_coordinates(dataSF_proj)[,2]
dataSF_proj<-st_drop_geometry(dataSF_proj) #Drop geometry column
head(dataSF_proj)

#VIEW MOVEMENT CHARACTERISTICS
#Prep data
data_ltraj<-as.ltraj(xy=dataSF_proj[,c('X','Y')], #spatial coords
                     date=dataSF_proj$POSIX, #timestamp
                     id=dataSF_proj$tag.local.identifier) #indiv ID
#Rename this formatted data
head(data_ltraj[[1]])
data<-data_ltraj[[1]]
head(data)

tail(data) #Check for end points of data - cannot do this on test data
#To remove final row when it comes
nrows<-nrow(data)
data<-data[-nrows,]
tail(data) #Check final row removed

#Regularise the data using the 3600 sec median
#Set this to an hour but allow a 2 minute +/- buffer
graphics.off()
dev.new()
ggplot(data,aes(x=dt))+geom_boxplot()
upper<-data[data$dt<=3720,] #Remove values 62 mins
ggplot(upper,aes(x=dt))+geom_boxplot()
regular<-upper[upper$dt>=3480,] #Remove values under 58 mins
ggplot(regular,aes(x=dt))+geom_boxplot()
#Dataset 'regular' now represents the data with regularised fix rate


#Graph distance
ggplot(regular,aes(x=dist))+geom_boxplot()
ggplot(regular,aes(x=dist))+geom_histogram()

#Graph speed
#Calculate speed in m/s
regular$speedMS<-regular$dist/regular$dt
#Convert to km/hr
regular$speedKmH<-regular$speedMS*3.6
#Convert to km per day (SEE Pokrovsky et al 2023 for reference)
regular$speedKmD<-regular$speedKmH*24
#Plot speed in km per day
ggplot(regular,aes(x=speedKmD))+geom_boxplot()
ggplot(regular,aes(x=speedKmD))+geom_histogram(bins=100)

##REMOVE STOPOVER SITES
#stopover<-which(regular$speedKmD<=2,)
#migration<-regular[-stopover,]

##SPLIT BY MIGRATION PHASES
#head(migration)
#See how speed per day changes over time/date
ggplot(regular,aes(x=date,y=speedKmD))+geom_line()

y1<-regular[regular$date>="2015-01-01"&regular$date<"2016-01-01",] #2015
y2<-regular[regular$date>="2016-01-01"&regular$date<"2017-01-01",] #2016
y3<-regular[regular$date>="2017-01-01"&regular$date<"2018-01-01",] #2017
y4<-regular[regular$date>="2018-01-01"&regular$date<"2019-01-01",] #2018
y5<-regular[regular$date>="2019-01-01",] #2019
ggplot(y1,aes(x=date,y=speedKmD))+geom_line()
ggplot(y2,aes(x=date,y=speedKmD))+geom_line()
ggplot(y3,aes(x=date,y=speedKmD))+geom_line()
ggplot(y4,aes(x=date,y=speedKmD))+geom_line()
ggplot(y5,aes(x=date,y=speedKmD))+geom_line()

autumn15<-regular[regular$date<"2015-11-01"& regular$date>"2015-07-01",]
winter1516<-regular[regular$date>="2015-11-01"&regular$date<"2016-04-26",]
spring16<-regular[regular$date>="2016-04-27"&regular$date<"2016-07-01",]
autumn16<-regular[regular$date<"2016-11-01"& regular$date>"2016-07-01",]
winter1617<-regular[regular$date>="2016-11-01"&regular$date<"2017-04-26",]
spring17<-regular[regular$date>="2017-04-27"&regular$date<"2017-07-01",]
autumn17<-regular[regular$date<"2017-11-01"& regular$date>"2017-07-01",]
winter1718<-regular[regular$date>="2017-11-01"&regular$date<"2018-04-26",]
spring18<-regular[regular$date>="2018-04-27"&regular$date<"2018-07-01",]
autumn18<-regular[regular$date<"2018-11-01"& regular$date>"2018-07-01",]
winter1819<-regular[regular$date>="2018-11-01"&regular$date<"2019-04-26",]
spring19<-regular[regular$date>="2019-04-27"&regular$date<"2019-07-01",]


#Export as csv files
write.csv(autumn15,'B06_Autumn15.csv')
write.csv(winter1516,'B06_Winter1516.csv')
write.csv(spring16,'B06_Spring16.csv')
write.csv(autumn16,'B06_Autumn16.csv')
write.csv(winter1617,'B06_Winter1617.csv')
write.csv(spring17,'B06_Spring17.csv')
write.csv(autumn17,'B06_Autumn17.csv')
write.csv(winter1718,'B06_Winter1718.csv')
write.csv(spring18,'B06_Spring18.csv')
write.csv(autumn18,'B06_Autumn18.csv')
write.csv(winter1819,'B06_Winter1819.csv')
write.csv(spring19,'B06_Spring19.csv')