### SCRIPT 1
## DATA EXPLORATION/FILTERING SCRIPT

#Set wd
setwd("C:/Users/evaen/Dissertation")
#Install required packages
install.packages('adehabitatLT')
install.packages('lubridate')
install.packages('sf')
install.packages('ggplot2')
install.packages('grid')
install.packages('gridExtra')
install.packages('circular')
install.packages('move')
install.packages('adehabitatHR')
install.packages('scales')
install.packages('terra')
install.packages('sp')

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
library(sp)


#Read data
buzzards<-read.csv('LifeTrack Rough-legged buzzards_part-gps.csv')
head(buzzards)
indivs<-unique(buzzards$individual.local.identifier)
indivs

#Remove rows with NA values in coordinates
NAPoints<-which(is.na(buzzards$location.lat)|is.na(buzzards$location.long))
buzzards<-buzzards[-NAPoints,]

#Remove rows with NA values in height above ellipsoid
NAHeight<-which(is.na(buzzards$height.above.ellipsoid))
buzzards<-buzzards[-NAHeight,]

#Remove data prior to migration
#Remove data from breeding areas
#Use 64 degrees latitude based on Curk et al., 2022 prospecting paper
breeding<-which(buzzards$location.lat>=64)
buzzards<-buzzards[-breeding,]
head(buzzards)
min_lat<-min(buzzards$location.lat)
min_lat #min latitude = 46.80052


#DOP/Data Quality?? 
dop<-unique(buzzards$gps.dop) 
dop #NA and 10

##Preparing temporal attributes
#Split into components
buzzards$tstamp<-strptime(as.character(buzzards$timestamp),"%Y-%m-%d %H:%M:%S")
buzzards$date<-as.Date(buzzards$tstamp)
buzzards$year<-year(buzzards$tstamp)
buzzards$month<-month(buzzards$tstamp)
buzzards$day<-day(buzzards$tstamp)
buzzards$hour<-hour(buzzards$tstamp)
buzzards$min<-minute(buzzards$tstamp)
buzzards$sec<-second(buzzards$tstamp)
head(buzzards)

#Time period covered by the data?
years<-unique(buzzards$year)
years # 2013 to 2019
#Create birdyear id sp that trajectories for differnt years can be separated
buzzards$idyear<-paste0(buzzards$individual.local.identifier,year(buzzards$date))
#Get individuals by year
indivYear<-unique(buzzards$idyear)
indivYear
#Zima removed from data

#Add in time in POSIX format
buzzards$POSIX<-as.POSIXct(buzzards$timestamp,format="%Y-%m-%d %H:%M:%S")
head(buzzards)
#Remove any duplicated times in record  for same individual
duplics<-which(duplicated(buzzards$POSIX)==TRUE)
buzzards<-buzzards[-duplics,]
indivYear


##Preparing geographic coordinates
#Convert buzzards df into sf object + specify coord system collected
buzzardsSF<-st_as_sf(buzzards,coords=c('location.long','location.lat'),crs=4326)
buzzardsSF
#Transform data into new coordinate system
#Use projection suitable for GPS data from north Russia to southwest extent
lcc_crs<-"+proj=lcc +lat_1=46 +lat_2=64 +lon_0=40.85 +ellps=WGS84"
buzzardsSF_proj<-st_transform(buzzardsSF,crs=lcc_crs)
head(buzzardsSF_proj)



#Separate X&Y into easting & northing for adehabitatLT use
buzzardsSF_proj$X<-st_coordinates(buzzardsSF_proj)[,1]
buzzardsSF_proj$Y<-st_coordinates(buzzardsSF_proj)[,2]
buzzardsSF_proj<-st_drop_geometry(buzzardsSF_proj) #Drop geometry column
head(buzzardsSF_proj)

#Create trajectories
buzzards_ltraj<-as.ltraj(xy=buzzardsSF_proj[,c('X','Y')],
                         date=buzzardsSF_proj$POSIX,
                         id=buzzardsSF_proj$individual.local.identifier)
#Plot trajectories
plot(buzzards_ltraj)

#Remove final row of data from each individual to remove NAs
#1
nrows<-nrow(buzzards_ltraj[[1]])
buzzards_ltraj[[1]]<-buzzards_ltraj[[1]][-nrows,]
#2
nrows<-nrow(buzzards_ltraj[[2]])
buzzards_ltraj[[2]]<-buzzards_ltraj[[2]][-nrows,]
#3
nrows<-nrow(buzzards_ltraj[[3]])
buzzards_ltraj[[3]]<-buzzards_ltraj[[3]][-nrows,]
#4
nrows<-nrow(buzzards_ltraj[[4]])
buzzards_ltraj[[4]]<-buzzards_ltraj[[4]][-nrows,]
#5
nrows<-nrow(buzzards_ltraj[[5]])
buzzards_ltraj[[5]]<-buzzards_ltraj[[5]][-nrows,]
#6
nrows<-nrow(buzzards_ltraj[[6]])
buzzards_ltraj[[6]]<-buzzards_ltraj[[6]][-nrows,]
#7
nrows<-nrow(buzzards_ltraj[[7]])
buzzards_ltraj[[7]]<-buzzards_ltraj[[7]][-nrows,]
#8
nrows<-nrow(buzzards_ltraj[[8]])
buzzards_ltraj[[8]]<-buzzards_ltraj[[8]][-nrows,]
#9
nrows<-nrow(buzzards_ltraj[[9]])
buzzards_ltraj[[9]]<-buzzards_ltraj[[9]][-nrows,]
#10
nrows<-nrow(buzzards_ltraj[[10]])
buzzards_ltraj[[10]]<-buzzards_ltraj[[10]][-nrows,]
#11
nrows<-nrow(buzzards_ltraj[[11]])
buzzards_ltraj[[11]]<-buzzards_ltraj[[11]][-nrows,]
#12
nrows<-nrow(buzzards_ltraj[[12]])
buzzards_ltraj[[12]]<-buzzards_ltraj[[12]][-nrows,]
#13
nrows<-nrow(buzzards_ltraj[[13]])
buzzards_ltraj[[13]]<-buzzards_ltraj[[13]][-nrows,]
#14
nrows<-nrow(buzzards_ltraj[[14]])
buzzards_ltraj[[14]]<-buzzards_ltraj[[14]][-nrows,]


#Rename trajectory objects to each bird's name
east<-buzzards_ltraj[[1]]
lermontov<-buzzards_ltraj[[2]]
leto<-buzzards_ltraj[[3]]
north<-buzzards_ltraj[[4]]
osen<-buzzards_ltraj[[5]]
pushkin<-buzzards_ltraj[[6]]
smena<-buzzards_ltraj[[7]]
vesna<-buzzards_ltraj[[8]]
b02<-buzzards_ltraj[[9]]
b05<-buzzards_ltraj[[10]]
b06<-buzzards_ltraj[[11]]
b07<-buzzards_ltraj[[12]]
b08<-buzzards_ltraj[[13]]
west<-buzzards_ltraj[[14]]

##Graph time difference
graphics.off()
dev.new()
grid.newpage()
plot1<-ggplot(east,aes(x=dt))+geom_boxplot()
plot2<-ggplot(lermontov,aes(x=dt))+geom_boxplot()
plot3<-ggplot(leto,aes(x=dt))+geom_boxplot()
plot4<-ggplot(north,aes(x=dt))+geom_boxplot()
plot5<-ggplot(osen,aes(x=dt))+geom_boxplot()
plot6<-ggplot(pushkin,aes(x=dt))+geom_boxplot()
plot7<-ggplot(smena,aes(x=dt))+geom_boxplot()
plot8<-ggplot(vesna,aes(x=dt))+geom_boxplot()
plot9<-ggplot(b02,aes(x=dt))+geom_boxplot()
plot10<-ggplot(b05,aes(x=dt))+geom_boxplot()
plot11<-ggplot(b06,aes(x=dt))+geom_boxplot()
plot12<-ggplot(b07,aes(x=dt))+geom_boxplot()
plot13<-ggplot(b08,aes(x=dt))+geom_boxplot()
plot14<-ggplot(west,aes(x=dt))+geom_boxplot()
#Display side by side
grid.arrange(plot1,plot2,plot3,plot4,plot5,plot6,plot7,plot8,plot9,plot10,plot11,plot12,plot13,plot14,ncol=4)

#Remove massive outliers
for(i in 1:length(buzzards_ltraj)){
  data<-buzzards_ltraj[[i]] #Subset df in each object
  data<-data[data$dt<=5000,] #Remove values over 83 mins apart
  buzzards_ltraj[[i]]<-data #Assign back to df
}

#Update to subsets
east<-buzzards_ltraj[[1]]
lermontov<-buzzards_ltraj[[2]]
leto<-buzzards_ltraj[[3]]
north<-buzzards_ltraj[[4]]
osen<-buzzards_ltraj[[5]]
pushkin<-buzzards_ltraj[[6]]
smena<-buzzards_ltraj[[7]]
vesna<-buzzards_ltraj[[8]]
b02<-buzzards_ltraj[[9]]
b05<-buzzards_ltraj[[10]]
b06<-buzzards_ltraj[[11]]
b07<-buzzards_ltraj[[12]]
b08<-buzzards_ltraj[[13]]
west<-buzzards_ltraj[[14]]
#Update plots
plot1<-ggplot(east,aes(x=dt))+geom_boxplot()
plot2<-ggplot(lermontov,aes(x=dt))+geom_boxplot()
plot3<-ggplot(leto,aes(x=dt))+geom_boxplot()
plot4<-ggplot(north,aes(x=dt))+geom_boxplot()
plot5<-ggplot(osen,aes(x=dt))+geom_boxplot()
plot6<-ggplot(pushkin,aes(x=dt))+geom_boxplot()
plot7<-ggplot(smena,aes(x=dt))+geom_boxplot()
plot8<-ggplot(vesna,aes(x=dt))+geom_boxplot()
plot9<-ggplot(b02,aes(x=dt))+geom_boxplot()
plot10<-ggplot(b05,aes(x=dt))+geom_boxplot()
plot11<-ggplot(b06,aes(x=dt))+geom_boxplot()
plot12<-ggplot(b07,aes(x=dt))+geom_boxplot()
plot13<-ggplot(b08,aes(x=dt))+geom_boxplot()
plot14<-ggplot(west,aes(x=dt))+geom_boxplot()
#View
grid.arrange(plot1,plot2,plot3,plot4,plot5,plot6,plot7,plot8,plot9,plot10,plot11,plot12,plot13,plot14,ncol=4)

#b02 is left with only 3 observations at this point so remove from data

buzzards_ltraj<-buzzards_ltraj[-9]

#Update names
b05<-buzzards_ltraj[[9]]
b06<-buzzards_ltraj[[10]]
b07<-buzzards_ltraj[[11]]
b08<-buzzards_ltraj[[12]]
west<-buzzards_ltraj[[13]]

#Check for removing high frequency sampling intervals
for(i in 1:length(buzzards_ltraj)){
data<-buzzards_ltraj[[i]] #Subset df in each object
data<-data[data$dt>=1500,] #Remove values less than 25 mins apart
buzzards_ltraj[[i]]<-data #Assign back to df
}
#Update to subsets
east<-buzzards_ltraj[[1]]
lermontov<-buzzards_ltraj[[2]]
leto<-buzzards_ltraj[[3]]
north<-buzzards_ltraj[[4]]
osen<-buzzards_ltraj[[5]]
pushkin<-buzzards_ltraj[[6]]
smena<-buzzards_ltraj[[7]]
vesna<-buzzards_ltraj[[8]]
b05<-buzzards_ltraj[[9]]
b06<-buzzards_ltraj[[10]]
b07<-buzzards_ltraj[[11]]
b08<-buzzards_ltraj[[12]]
west<-buzzards_ltraj[[13]]
#Update plots
plot1<-ggplot(east,aes(x=dt))+geom_boxplot()
plot2<-ggplot(lermontov,aes(x=dt))+geom_boxplot()
plot3<-ggplot(leto,aes(x=dt))+geom_boxplot()
plot4<-ggplot(north,aes(x=dt))+geom_boxplot()
plot5<-ggplot(osen,aes(x=dt))+geom_boxplot()
plot6<-ggplot(pushkin,aes(x=dt))+geom_boxplot()
plot7<-ggplot(smena,aes(x=dt))+geom_boxplot()
plot8<-ggplot(vesna,aes(x=dt))+geom_boxplot()
plot9<-ggplot(b05,aes(x=dt))+geom_boxplot()
plot10<-ggplot(b06,aes(x=dt))+geom_boxplot()
plot11<-ggplot(b07,aes(x=dt))+geom_boxplot()
plot12<-ggplot(b08,aes(x=dt))+geom_boxplot()
plot13<-ggplot(west,aes(x=dt))+geom_boxplot()
#View
grid.arrange(plot1,plot2,plot3,plot4,plot5,plot6,plot7,plot8,plot9,plot10,plot11,plot12,plot13,ncol=3)

#Regularise the data using the 3600 sec median
#Set this to an hour but allow a 2 minute +/- buffer
for(i in 1:length(buzzards_ltraj)){
  data<-buzzards_ltraj[[i]] #Subset df in each object
  data<-data[data$dt>=3480,] #Remove values over 83 mins apart
  data<-data[data$dt<=3720,]
  buzzards_ltraj[[i]]<-data #Assign back to df
}
#Vesna has too little data so remove
#Leto has too little data so remove
buzzards_ltraj<-buzzards_ltraj[-8]
#Update to subsets
east<-buzzards_ltraj[[1]]
lermontov<-buzzards_ltraj[[2]]
north<-buzzards_ltraj[[4]]
osen<-buzzards_ltraj[[5]]
pushkin<-buzzards_ltraj[[6]]
smena<-buzzards_ltraj[[7]]
b05<-buzzards_ltraj[[8]]
b06<-buzzards_ltraj[[9]]
b07<-buzzards_ltraj[[10]]
b08<-buzzards_ltraj[[11]]
west<-buzzards_ltraj[[12]]
#Update plots
plot1<-ggplot(east,aes(x=dt))+geom_boxplot()
plot2<-ggplot(lermontov,aes(x=dt))+geom_boxplot()
plot3<-ggplot(leto,aes(x=dt))+geom_boxplot()
plot4<-ggplot(north,aes(x=dt))+geom_boxplot()
plot5<-ggplot(osen,aes(x=dt))+geom_boxplot()
plot6<-ggplot(pushkin,aes(x=dt))+geom_boxplot()
plot7<-ggplot(smena,aes(x=dt))+geom_boxplot()
plot8<-ggplot(b05,aes(x=dt))+geom_boxplot()
plot9<-ggplot(b06,aes(x=dt))+geom_boxplot()
plot10<-ggplot(b07,aes(x=dt))+geom_boxplot()
plot11<-ggplot(b08,aes(x=dt))+geom_boxplot()
plot12<-ggplot(west,aes(x=dt))+geom_boxplot()
#View
grid.arrange(plot1,plot2,plot3,plot4,plot5,plot6,plot7,plot8,plot9,plot10,plot11,plot12,ncol=3)

#Now have informed boundaries to use for regular fix rate (for SSF analysis)


