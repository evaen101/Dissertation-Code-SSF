### SCRIPT 3
## DATA STEPS TRANSFORMATION SCRIPT
## For this script, substitute each of the individual datasets (indiv_yr_ssn) into 'data'
## Save the output by changing the file name in final write.csv to match input ID

#install.packages('amt')
#install.packages('tidyr')

#####
library(lubridate)
library(amt)
library(dplyr)
library(tidyr)

#Read in migration dataset
data<-read.csv('B05_Autumn15.csv')
head(data)
lcc_crs<-"+proj=lcc +lat_1=46 +lat_2=64 +lon_0=40.85 +ellps=WGS84"
data1<-amt::make_track(data,.x=x,.y=y,dt=dt,crs=lcc_crs)
head(data1)
as_steps<-steps(data1)
head(as_steps)
all_steps<-random_steps(as_steps,n_control=10)
head(all_steps,20)
tail(all_steps)
add_ID<-cbind(all_steps,row_ID=1:nrow(all_steps))
head(add_ID)

# Create column with coords & date
tstamp<-data[c('x','y','date')]
head(tstamp)

# Join all_steps and tstamp to reassign date to step endpoints (x2,y2)
time_added <- merge(add_ID, tstamp, by.x = c("x2_", "y2_"), by.y = c("x", "y"),all.x=TRUE)
head(time_added)

## Assign time only to TRUE cases to correct for time annotation to fake steps
#Number of values in date that aren't NA
non_na_count <- length(which(!is.na(time_added$date)))
print(non_na_count) 
#Assign NA to date in all rows with fake steps (where case is FALSE)
time_added$date<-ifelse(time_added$case_,time_added$date,NA)
#Check
non_na_count <- length(which(!is.na(time_added$date)))
# Print the result
print(non_na_count)
head(time_added)
#Rearrange by row_id
time_ordered<-time_added[order(time_added$row_ID),]
head(time_ordered)

##Now match timestamp of real steps to respective fake steps using step_ID
time_filled<-group_by(time_ordered,step_id_)
time_filled<-fill(time_filled,date,.direction="down")
time_filled<-ungroup(time_filled)
head(time_filled)


#Trim data
trimmed<-time_filled[c('row_ID','date','x2_','y2_','step_id_')]
head(trimmed)
#Rename column headers
colnames(trimmed)[2]<-"timestamp"
colnames(trimmed)[3]<-"location-long"
colnames(trimmed)[4]<-"location-lat"

##Reproject coordinates into latitude and longitude/3D
#NAPoints<-which(is.na(trimmed$`location-long`)|is.na(trimmed$`location-lat`)) #For if 'missing coordinates'
#trimmed<-trimmed[-NAPoints,] #Remove 'missing coordinates'
as_sf<-st_as_sf(trimmed,coords=c("location-long","location-lat"),crs=lcc_crs)
head(as_sf)
as_wgs84<-st_transform(as_sf,crs=4326)
head(as_wgs84)
projected<-st_coordinates(as_wgs84)
head(projected)
#Bind back to trimmed df
rejoin<-cbind(trimmed,projected)
head(rejoin)


#Reformat for ENV-Data annotation
final<-rejoin[c('row_ID','timestamp','X','Y','step_id_')]
head(final)
colnames(final)[3]<-"location-long"
colnames(final)[4]<-"location-lat"
#Fix .000 to the seconds for ENV-data
final$timestamp <- paste0(final$timestamp, ".000")
head(final$timestamp)
head(final,12)
tail(final,12)


## WRITE CSV
write.csv(final,'B05_Aut15ANNOTATE.csv')








