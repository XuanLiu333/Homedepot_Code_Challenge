########HOME DEPOT########
##### Simulate Skill sets &  Certification Indicator ######
r <- 1000; c <- 8
Skills <- matrix(sample(0:1,r*c, replace=TRUE),r,c)
colnames(Skills) <- c("Hardwood Flooring", "Carpet Flooring", "Tile Flooring", "Roofing","Painting", 
                      "Lawn & Garden", "Electrical", "Plumbing")
c1 <- sample(0:1, r, replace=TRUE)
c2 <- sample(0:1, r, replace=TRUE)
Certification <- cbind(c1, c2)
colnames(Certification) <- c("ElectricalCertify", "PlumbingCertify")

##### Simulate Work Year ######
WorkYears <- sample(0:30, r, replace=T)

##### Simulate #Complaints, #Cases, Complaint Rate ######
NComplaints <- sample(0:10, size=r, replace=T, prob=dpois(0:10, lambda=3))
NCases <- sample(0:50, size=r, replace=T, prob=dpois(0:50, lambda=30))
RComplaint <- NComplaints/NCases

##### Simulate Rating ######
Rating <- matrix(0,r,c)
colnames(Rating) <- c("RHardwood", "RCarpet", "RTile", "RRoofing","RPainting", "RLawnGarden", 
                      "RElectrical", "RPlumbing")
for (i in 1:r){
  for (j in 1:6){
    if (Skills[i,j]==1) {Rating[i,j]<- WorkYears[i] - 10*RComplaint[i]}
    else{Rating[i,j]<- 0}
  }
  for (j in 7:8){
    if (Skills[i,j]==1) {Rating[i,j]<- WorkYears[i] - 10*RComplaint[i] + 3*Certification[i,j-6]}
    else{Rating[i,j]<- 0}
  }
}


#####simulate address start#####
install.packages("zipcode")
library(zipcode)
data(zipcode)
head(zipcode)
zip.atlanta = zipcode[zipcode$city=="Atlanta"&zipcode$state=="GA",]
head(zip.atlanta)
zip.simu = sample(zip.atlanta$zip, 1000, replace = TRUE)
address.zip =zip.atlanta[sample(1:nrow(zip.atlanta), 1000, replace = TRUE),]
str(address.zip)
head(zip.simu)
str(zipcode)
address.num = sample(1:10000, 1000)

###
MHmakeRandomString = function(n=1, lenght=12)
{
  randomString = c(1:n)                  # initialize vector
  for (i in 1:n)
  {
    randomString[i] = paste(sample(LETTERS, lenght, replace=TRUE), collapse="")
    randomString[i] = paste(randomString[i], "ST.")
  }
  return(randomString)
}
###

address.st = MHmakeRandomString(1000, 6)
address.ns = paste(address.num, address.st)
address.simu = data.frame(cbind(address.ns, address.zip))
head(address.simu)

#####simulate address end#####

############ PRO Table
makeRandomString = function(n=1, lenght=12)
{
  randomString = c(1:n)                  # initialize vector
  for (i in 1:n)
  {
    randomString[i] = paste(sample(LETTERS, lenght, replace=TRUE), collapse="")
    randomString[i] = paste(randomString[i], "CO.")
  }
  return(randomString)
}
Pro.name <- makeRandomString(1000, 5)
WorkingRange <- sample(5:50, 1000, replace=T)
StoreID <- sample(1:3, 1000, replace=T)
Pro.ID <- c(1:1000)
PRO <- data.frame(Pro.ID, Pro.name, address.simu, WorkingRange, NComplaints, StoreID, WorkYears)

setwd("C:/Users/Jing/Documents/HomeDepotCoding")
write.csv(PRO, file="PRO.csv",row.names=FALSE)

############ Capability 
col.names(Rating) <- c("OverallRatingHardwood", "OverallRatingCarpet", "OverallRatingTile", "OverallRatingRoofing",
                       "OverallRatingPainting", "OverallRatingLawnGarden", "OverallRatingElectrical", "OverallRatingPlumbing")

Capability <- data.frame(Pro.ID, Skills, Rating , Certification)
write.csv(Capability, file="Capability.csv", row.names=FALSE)

############ SkillSet Table
SkillSet <- data.frame(Pro.ID, Skills, Rating)
write.csv(SkillSet, file="SkillSet.csv", row.names=FALSE)

############ Schedule Table
Date <- seq(as.Date("2016/02/19"), as.Date("2016/03/03"), "day")
Schedule <- matrix(0, 1000*14, 3)
for (i in 1:length(Pro.ID)){
  for (j in 1:length(Date)){
    Schedule[(i-1)*length(Date)+j,1] <- format(Date[j], "%m/%d/%Y")
    Schedule[(i-1)*length(Date)+j,2] <- sample(0:1,1,replace=T)
    Schedule[(i-1)*length(Date)+j,3] <- Pro.ID[i]
  }
}
Schedule <- data.frame(Schedule)
colnames(Schedule) <- c("Date", "Availability", "Pro.ID")

write.csv(Schedule, file="Schedule.csv", row.names=FALSE)


##################### Store Table
PRO <- read.csv(file="Pro.csv")
StoreID <- PRO$StoreID

#####simulate store address######
#assume 3 stores in Atlanta area
Store.st = MHmakeRandomString(3, 6)
Store.name <- c("HomeDepot 1","HomeDepot 2","HomeDepot 3")
Store.num <- sample(80:150, 3)
Store.ns = paste(Store.num, Store.st)
StoreL1 = apply(PRO[1:330,7:8], 2, mean)
which((PRO[,7]-StoreL1[1]<0.000001)&(PRO[,8]-StoreL1[2]<0.000001))
StoreZ1 <- PRO$zip[215]
StoreL2 = apply(PRO[331:660,7:8], 2, mean)
which((PRO[,7]-StoreL2[1]<0.000001)&(PRO[,8]-StoreL2[2]<0.000001))
StoreZ2 <- PRO$zip[695]
StoreL3 = apply(PRO[661:1000,7:8], 2, mean)
StoreZ3 <- PRO$zip[996]
StoreL <- rbind(StoreL1, StoreL2, StoreL3)
StoreZ <- rbind(StoreZ1, StoreZ2, StoreZ3)
Store.City <- rep("Atlanta",3)
Store.State <- rep("GA",3)

Store <- data.frame(Store.name, Store.ns, Store.City, Store.State, StoreZ, StoreL)
colnames(Store) <- c("StoreName", "StoreStreet", "StoreCity", "StoreState", "StoreZip", "Latitude", "Longitude")

write.csv(Store, file="Store.csv", row.names=FALSE)



