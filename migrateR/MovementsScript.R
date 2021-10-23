###Movement Trajectories tutorial
##more tutorials on movement within the adehabitat package is found in the
##Manual of Applied Spatial Ecology:
##https://ecosystems.psu.edu/research/labs/walter-lab/manual

library(adehabitatLT)
library(chron)
library(spatstat)#for "duplicate" function

#this is mule deer GPS collar data
muleys <-read.csv("DCmuleysedited.csv", header=T)
str(muleys)

#Check for duplicate locations in dataset
#should show FALSE for all locations, if not, use code below
summary(duplicated(muleys))

#Sort data to address error in code if needed
#muleys <- muleys[order(muleys$id),]

### Conversion of the date to the format POSIX
#Date <- as.character(muleys$GPSFixTime)
#Date <- as.POSIXct(strptime(as.character(muleys$GPSFixTime),"%Y.%m.%d %H:%M:%S"))
#muleys$Date <- Date


######################################################
##
## Example of a trajectory of type II (time recorded)
### Conversion of the date to the format POSIX
#Needs to be done to get proper digits of date into R then POSIXct
#Need to confirm correct time zone, otherwise use tz= in function

#for BUOW PTT data, need to figure out what the date/time structure will be 
#if I use a location that was not visually confirmed...for example if an owl
#disperses so far that I can't check on it as regularly as the others?

da <- as.POSIXct(strptime(muleys$GPSFixTime,format="%Y.%m.%d %H:%M:%S"))
da
muleys$da <- da
str(muleys)

#create time lag between successive locations to censor data if needed
#diff function is for date and POSIX formats to calculate the differences
#this calculates the difference in minutes, then multiplies by 60 
#to get in seconds
timediff <- diff(muleys$da)*60

#removed the first row, not sure why
muleys <-muleys[-1,]

#abs(x) calculates the absolute value
muleys$timediff <-as.numeric(abs(timediff)) 
str(muleys)#check to see timediff column was added to muleys

#number of locations by animal id
##they all have the same number of locations!!
summary(muleys$id)

#remove outliers and known locations that were collected too far apart in time
#can do this with Lat/Long columns
#seems like this is done based on home range size or something, not sure if this
#will apply to me
newmuleys <-subset(muleys, muleys$X > 599000 & muleys$X < 705000 & 
                     muleys$Y > 4167000 & muleys$timediff < 14401)
muleys <- newmuleys
str(muleys)

##create a spatial points dataframe in UTM zone 12 
#name a subset that includes only the UTMs
data.xy = muleys[c("X","Y")]

#Creates class Spatial Points for all locations
xysp <- SpatialPoints(data.xy)
#if there is an error with creating this class, use below code to set/retreive
#projection attributes, this is important so the projection matches for analysis
#proj4string(xysp) <- CRS("+proj=utm +zone=13 +ellps=WGS84")

#Creates a Spatial Data Frame from class spatial points
sppt<-data.frame(xysp)

#Creates a spatial data frame of ID
idsp<-data.frame(muleys[2])

#Creates a spatial data frame of time difference
dtsp<-data.frame(muleys[24])

#Creates a spatial data frame of Burst (time location was collected)
busp<-data.frame(muleys[23])

#Merges ID, Date and time diff into the same spatial data frame
merge<-data.frame(idsp,dtsp,busp)

#Adds ID and Date data frame with locations data frame to create the
#spatial points data frame
coordinates(merge)<-sppt
plot(merge)
str(merge)

### Creation of an object of class "ltraj" by animal and display by each 
###animal
#provide data, date, id within function
#ltraj created and separated by individual (7 individuals in this dataset)
#when an ltraj is created, it provides information like distance between locs (dx, dy, dist),
#difference in time (dt), squared distance (R2n), etc. 
#use vignette("adehabitatLT") to get full description of what adehabitat does
ltraj <- as.ltraj(coordinates(merge),merge$da,id=merge$id)

#plots trajectories separated by individual
plot(ltraj)

#Describes and plots the trajectory for first animal
head(ltraj[1])
plot(ltraj[1])

#plots each individual's trajectory separately
plot(ltraj[2])
plot(ltraj[3])
plot(ltraj[4])
plot(ltraj[5])
plot(ltraj[6])
plot(ltraj[7])

##Let’s create a histogram of time lag (i.e., interval) and distance 
##between successive locations for each deer. This is a nice way to inspect 
##the time lag between locations as you don’t want to include a location if 
##too much time has passed since the previous and it also shows why a 
##trajectory is irregular.
hist(ltraj[1], "dt", freq = TRUE)
#looks like there is one point that is more than 30000 secs separated for first
hist(ltraj[1], "dist", freq = TRUE)
hist(ltraj[2], "dt", freq = TRUE)
hist(ltraj[2], "dist", freq = TRUE)
hist(ltraj[3], "dt", freq = TRUE)
hist(ltraj[3], "dist", freq = TRUE)
hist(ltraj[4], "dt", freq = TRUE)
hist(ltraj[4], "dist", freq = TRUE)
hist(ltraj[5], "dt", freq = TRUE)
hist(ltraj[5], "dist", freq = TRUE)
hist(ltraj[6], "dt", freq = TRUE)
hist(ltraj[6], "dist", freq = TRUE)
hist(ltraj[7], "dt", freq = TRUE)
hist(ltraj[7], "dist", freq = TRUE)

###more good stuff on movement that I might need to use, but this is needed
###to create ltraj objects for the migrateR analysis
