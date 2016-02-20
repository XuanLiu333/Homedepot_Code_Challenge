########HOME DEPOT########

#####simulate address start#####
library(zipcode)
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



#####aimulate store address######
#assume 3 stores in Atlanta area
Store1 = apply(zip.atlanta[1:30,4:5], 2, mean)
Store2 = apply(zip.atlanta[31:60,4:5], 2, mean)
Store3 = apply(zip.atlanta[61:90,4:5], 2, mean)




#####simulate available days and hours#####
availabilities = data.frame(rbind(sample(0:1, 7, replace = TRUE),
                                  sample(0:1, 7, replace = TRUE),
                                  sample(0:1, 7, replace = TRUE),
                                  sample(0:1, 7, replace = TRUE)))
rownames(availabilities) = c("WEEK1.AM", "WEEK1.PM", "WEEK2.AM", "WEEK2.PM")
colnames(availabilities) = c("SUN","MON", "TUE", "WED", "THU", "FRI", "SAT")
