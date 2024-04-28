### STEP FIVE
## SSF FUNCTION of all variables

#Set wd
setwd("H:/DISS FOLDER/Joined Data")

#install.packages("foreach")
#install.packages("doParallel")
#install.packages("survival")
#install.packages("amt")

library(foreach) # to parallelise code
library(doParallel) # to parallelise code
library(survival) # to run ssf (has wrapper of clogit from amt package)
library(amt)

#Load all of the individual joined datasets
d1<-read.csv('B05_Aut15-FINAL.csv') 
d2<-read.csv('B06_Aut15-FINAL.csv')
d3<-read.csv('B06_Aut16-FINAL.csv')
d4<-read.csv('B06_Aut17-FINAL.csv')
d5<-read.csv('B06_Aut18-FINAL.csv')
d6<-read.csv('B06_Spr16-FINAL.csv')
d7<-read.csv('B06_Spr17-FINAL.csv')
d8<-read.csv('B06_Spr18-FINAL.csv')
d9<-read.csv('B06_Spr19-FINAL.csv')  #Insufficient data 
d10<-read.csv('B06_Win1516-FINAL.csv')
d11<-read.csv('B06_Win1617-FINAL.csv')
d12<-read.csv('B06_Win1718-FINAL.csv')
d13<-read.csv('B06_Win1819-FINAL.csv')
d14<-read.csv('B07_Aut15-FINAL.csv')
d15<-read.csv('B07_Aut16-FINAL.csv')
d16<-read.csv('B07_Aut17-FINAL.csv')
d17<-read.csv('B07_Aut18-FINAL.csv')
d18<-read.csv('B07_Spr16-FINAL.csv')
d19<-read.csv('B07_Spr17-FINAL.csv')
d20<-read.csv('B07_Spr18-FINAL.csv')
d21<-read.csv('B07_Win1617-FINAL.csv')
d22<-read.csv('B07_Win1718-FINAL.csv')
d23<-read.csv('B07_Win1819-FINAL.csv')
d24<-read.csv('B08_Aut15-FINAL.csv')
d25<-read.csv('B08_Aut16-FINAL.csv')
d26<-read.csv('B08_Win1516-FINAL.csv')
d27<-read.csv('East_Aut14-FINAL.csv')
d28<-read.csv('East_Aut15-FINAL.csv')
d29<-read.csv('East_Spr15-FINAL.csv')
d30<-read.csv('East_Spr16-FINAL.csv')
d31<-read.csv('East_Win1415-FINAL.csv')
d32<-read.csv('East_Win1516-FINAL.csv')
d33<-read.csv('Lermontov_Aut15-FINAL.csv')
d34<-read.csv('Lermontov_Spr16-FINAL.csv')
d35<-read.csv('Lermontov_Win1516-FINAL.csv')
d36<-read.csv('North_Aut14-FINAL.csv')
d37<-read.csv('North_Aut15-FINAL.csv')
d38<-read.csv('North_Aut16-FINAL.csv')
d39<-read.csv('North_Aut17-FINAL.csv')
d40<-read.csv('North_Spr15-FINAL.csv')
d41<-read.csv('North_Spr16-FINAL.csv')
d42<-read.csv('North_Win1415-FINAL.csv')
d43<-read.csv('North_Win1516-FINAL.csv')
d44<-read.csv('North_Win1617-FINAL.csv')
d45<-read.csv('North_Win1718-FINAL.csv')
d46<-read.csv('Osen_Aut14-FINAL.csv')
d47<-read.csv('Osen_Aut15-FINAL.csv')
d48<-read.csv('Osen_Aut16-FINAL.csv')  #Insufficient data
d49<-read.csv('Osen_Aut17-FINAL.csv')
d50<-read.csv('Osen_Aut18-FINAL.csv')  #Insufficient data
d51<-read.csv('Osen_Spr14-FINAL.csv')
d52<-read.csv('Osen_Spr15-FINAL.csv')
d53<-read.csv('Osen_Spr16-FINAL.csv')
d54<-read.csv('Osen_Spr17-FINAL.csv')
d55<-read.csv('Osen_Spr18-FINAL.csv')
d56<-read.csv('Osen_Spr19-FINAL.csv')
d57<-read.csv('Osen_Win14-FINAL.csv')
d58<-read.csv('Osen_Win1415-FINAL.csv')
d59<-read.csv('Osen_Win1516-FINAL.csv')
d60<-read.csv('Osen_Win1617-FINAL.csv')
d61<-read.csv('Osen_Win1718-FINAL.csv')
d62<-read.csv('Osen_Win1819-FINAL.csv')
d63<-read.csv('Pushkin_Aut16-FINAL.csv')
d64<-read.csv('Pushkin_Aut17-FINAL.csv')
d65<-read.csv('Pushkin_Aut18-FINAL.csv')
d66<-read.csv('Pushkin_Spr17-FINAL.csv')
d67<-read.csv('Pushkin_Spr18-FINAL.csv')
d68<-read.csv('Pushkin_Win1617-FINAL.csv')
d69<-read.csv('Pushkin_Win1718-FINAL.csv')
d70<-read.csv('Pushkin_Win1819-FINAL.csv')  #Insufficient data
d71<-read.csv('Smena_Aut15-FINAL.csv')
d72<-read.csv('Smena_Spr14-FINAL.csv')
d73<-read.csv('Smena_Spr15-FINAL.csv')
d74<-read.csv('Smena_Win1415-FINAL.csv')
d75<-read.csv('West_Aut14-FINAL.csv')
d76<-read.csv('West_Aut15-FINAL.csv')
d77<-read.csv('West_Spr15-FINAL.csv')
d78<-read.csv('West_Win1415-FINAL.csv')
d79<-read.csv('West_Win1516-FINAL.csv')


#Join all the individuals into one singular dataset
#Datasets with insufficient data excluded
data<-rbind(d1,d2,d3,d4,d5,d6,d7,d8,d10,d11,d12,d13,d14,d15,d16,d17,d18,d19,d20,
            d21,d22,d23,d24,d25,d26,d27,d28,d29,d30,d31,d32,d33,d34,d35,d36,d37,d38,
            d39,d40,d41,d42,d43,d44,d45,d46,d47,d49,d51,d52,d53,d54,d55,d56,
            d57,d58,d59,d60,d61,d62,d63,d64,d65,d66,d67,d68,d69,d71,d72,d73,d74,
            d75,d76,d77,d78,d79)
#View(data) 

#Detect cores 
ncores <- detectCores() - 1L #Leave one for normal processes
ncores # 23 cores to run data on
#Register cores
registerDoParallel(ncores)

#Set up inputs for parallel processing
buzzards<-unique(data$ID)
nBuzzards<-length(buzzards)

#Make an empty list for the SSF model results
summary_results<-list()

#Have R recognise the values of land cover as categorical - make factor levels
data$land_cover_.05<-factor(data$land_cover_.05,levels=1:18)
data$land_cover_500m<-factor(data$land_cover_500m,levels=1:18)


#Use parallel foreach loop so each buzzard sent to a different core
ssf<- foreach (i=1:nBuzzards, .packages=c("survival"), .export=c("clogit","strata")) %dopar% {
  buzzardIndices<-which(data$ID == buzzards[i])
  oneBuzzard<-data[buzzardIndices,]
  sink(paste("MEGA_SSF_",i,"_.txt",sep=""))
  indivID<-oneBuzzard$ID[[i]]
  model<-clogit(case~elevation+snow+land_cover_.05+land_cover_500m +fpar_500m_8d
                +fpar_500m_4d+ndvi_500m_16d_aqua
                +ndvi_500m_16d_terra+ndvi_.05_16d_terra
                +ndvi_.05_16d_aqua+temp+precip+wind_support+crosswind+airspeed 
                +pop_den_coarse+pop_den_fine+ strata(step_id),
             data=oneBuzzard)
  OR.CI<-cbind("OR"=exp(coef(model)), exp(confint(model)))
  summary_results[[i]]<-list(model=summary(model),ORm1=OR.CI)
  sink(paste("MEGA_SSF_",indivID,"_.txt",sep=""))
  print(indivID)
  print(summary_results[[i]])
  sink()
}

stopImplicitCluster()