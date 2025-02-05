## WIND SCRIPT

#U is wind speed in E-W direction
#V is wind speed in the N-S direction

setwd("C:/Users/evaen/Dissertation/Annotated Data")
data<-read.csv('West_Win1516-WIND.csv')
head(data)
str(data)
#Rename u & v components
data$u<-data$ECMWF.ERA5.Land.Wind..10.m.above.Ground.U.Component.
data$v<-data$ECMWF.ERA5.Land.Wind..10.m.above.Ground.V.Component.
str(data)
#Function for degrees to radians
#deg2rad<-function(degrees){
#  return(degrees * pi/180)
#}
# Function for radians to degrees
#rad2deg <- function(radians) {
#  return(radians * 180/pi)
#}

################## 14/03

#Loop for all wind analysis
for(i in 1:(nrow(data)-1)){
  nxt<-i+1 #Name i+1 to use
  #Vector of bird movement
  #Distance between x2,y2 and y1,y2 
  a<-c(data$location.long[nxt]-data$location.long[i],data$location.lat[nxt]-data$location.lat[i])
  #Vector of wind
  b<-c(data$u[i],data$v[i])
  #Magnitude a
  a_mag<-sqrt((data$location.long[nxt]-data$location.long[i])**2+(data$location.lat[nxt]-data$location.lat[i])**2)
  #Vector of wind
  b_mag<-sqrt(data$u[i]**2+data$v[i]**2)
  #Calculate the dot product of a.b  
  a_dot_b<-drop(a %*% b)  
  
  #Put components into scalar product formula
  cos<-a_dot_b/(a_mag*b_mag)
  theta<-acos(cos)
  
  #Calculate wind components using calculated angle
  support<-b_mag*sin(theta)
  cross<-b_mag*cos(theta)
  #Calculate airspeed
  airspeed<-sqrt((a_mag-support)**2 + (cross)**2)
  
  #Save results to dataframe
  data$a_x[i] <- a[1]
  data$a_y[i] <- a[2]
  data$b_x[i] <- b[1]
  data$b_y[i] <- b[2]
  data$a_mag[i] <- a_mag
  data$b_mag[i] <- b_mag
  data$a_dot_b[i] <- a_dot_b
  data$cos[i] <- cos
  data$theta[i] <- theta
  data$support[i] <- support
  data$cross[i] <- cross
  data$airspeed[i] <- airspeed
}

#View(data)
#warnings()

setwd("C:/Users/evaen/Dissertation/Annotated Data")
#Save
write.csv(data,'West_Win1516-WINDANALYSIS.csv')
