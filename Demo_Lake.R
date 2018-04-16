#flexible grammar of data manipulation
library("dplyr")

#read JSON
#library("jsonlite") # 
library("rjson")

#preperation and work with lists
library("rlist")

#JSON viewer
library("listviewer")

data <- readLines("C:/TempMP/R JSON/test.csv") #remove header. 
data <- substring(data,2,nchar(data)-1) # remove Quotes
json <- lapply(data,fromJSON) # read json in a LIST


# Subset with RS - Rechtschutz
# Subset with GEB - Gebäude
# test <- json["RSLine" %in% names(json[[1]]$PolicyPeriod)]
#Building Insurance
GEBfilter <- list()
j <- 0
for(i in 1:length(json))
{if("GEBLine" %in% names(json[i][[1]]$PolicyPeriod)){j <- j+1; GEBfilter[j] <- json[i]}} 

#home content insurance
HRfilter <- list()
j <- 0
for(i in 1:length(json))
{if("HRLine" %in% names(json[i][[1]]$PolicyPeriod)){j <- j+1; HRfilter[j] <- json[i]}} 

#legal costs insurance
RSfilter <- list()
j <- 0
for(i in 1:length(json))
{if("RSLine" %in% names(json[i][[1]]$PolicyPeriod)){j <- j+1; RSfilter[j] <- json[i]}} 

# read Building Data in DataFrame

# complex version
dBuilding <- data.frame()
j <- 0
for(i in 1:length(GEBfilter))
{ 
  anzBuild <- length(GEBfilter[i][[1]]$PolicyPeriod$GEBLine$GEBRiskLocations)
  for(k in 1:anzBuild)
  {  
    if("Building" %in% names(GEBfilter[i][[1]]$PolicyPeriod$GEBLine$GEBRiskLocations[[1]]$GEBCoverables[[1]]))
    {j <- j+1; dBuilding <- bind_rows(dBuilding,as.data.frame(GEBfilter[i][[1]]$PolicyPeriod$GEBLine$GEBRiskLocations[[k]]$GEBCoverables[[1]]$Building))}
  } 
}

dLocation <- data.frame()
for(i in 1:length(GEBfilter))
{  
  anzLoc<- length(GEBfilter[i][[1]]$PolicyPeriod$GEBLine$GEBRiskLocations)
  for(k in 1:anzLoc)
  {
    if("Location" %in% names(GEBfilter[i][[1]]$PolicyPeriod$GEBLine$GEBRiskLocations[[k]]))
    {dLocation <- bind_rows(dLocation,as.data.frame(GEBfilter[i][[1]]$PolicyPeriod$GEBLine$GEBRiskLocations[[k]]$Location))}
  } 
}
#HR
#Apartmentsize
dApartsize <- data.frame()
for(i in 1:length(HRfilter))
{
    anzApart <- length(HRfilter[i][[1]]$PolicyPeriod$HRLine$HRRiskLocations)
    for(k in 1:anzApart)
    {
      if("Apartment" %in% names(HRfilter[i][[1]]$PolicyPeriod$HRLine$HRRiskLocations[[k]]$HRApartments[[1]]))
        {dApartsize <- bind_rows(dApartsize,as.data.frame(HRfilter[i][[1]]$PolicyPeriod$HRLine$HRRiskLocations[[k]]$HRApartments[[1]]$Apartment))}
    }
}

# Merge dLocation and Building 
dLocBuild <- cbind(dLocation,dBuilding)

#Histogram of Apartment size
hist(dApartsize$ApptmntOrOfficeArea)

#easier part # problem with morw then 1 (building)
dbuilding  <- bind_rows(Filter(Negate(is.null),sapply(json,function(x) x[[1]]$GEBLine$GEBRiskLocations[[1]]$GEBCoverables[[1]]$Building)))
dlocations <- bind_rows(Filter(Negate(is.null),sapply(json,function(x) x[[1]]$GEBLine$GEBRiskLocations[[1]]$Location)))
dataTotal <- cbind(dbuilding,dlocations)

sapply(json, function (x) sapply(x[[1]]$GEBLine$GEBRiskLocations,function(y) y$GEBCoverables[[1]]$Building))



# count about Cities
table(dLocBuild$City)


# search for a Character string in hole List (Eigenschaft)

list.search(GEBfilter[1], grepl('Building',.), 'character')

#How much is the word "bulding" in there
list.count(GEBfilter[1], grepl('Building',.))

#Package "listviewer" display List in a better way - fct ist as a Example jsonedit(filter[1])

library("listviewer")
jsonedit(HRfilter[2])

# RShiny example
#library(shiny)
#runExample("01_Hello")

############################## RS #################################
#untersparte
RSfilter[1][[1]]$PolicyPeriod$RSLine$CoverageParts[[1]]$"@odata.type"

#Selektieren der Unterparten
lRSprivat <- list()
lRSFamily <- list()
lRSimmo <- list()
lRSno <- list()
j<-0; s<-0; t<-0; k<-0
for(i in 1:length(RSfilter))
{
  if(length(list.search(RSfilter[i][[1]], identical(., '#zde.entities.RSPrivatCovPart_ZDE'))))
  {
    j <- j + 1
    lRSprivat[j] <- RSfilter[i] 
  }
  else if(length(list.search(RSfilter[i][[1]], identical(., '#zde.entities.RSFamilyTrafficCovPart_ZDE'))))
  {
    s <- s + 1
    lRSFamily[s] <- RSfilter[i]
  }
  else if(length(list.search(RSfilter[i][[1]], identical(., '#zde.entities.RSRealEstateCovPart_ZDE'))))
  {
    t <- t + 1 
    lRSimmo[t] <- RSfilter[i] 
  }
  else 
  {
    k <- k + 1
    lRSno[k] <- RSfilter[i]
  }  
}

# read out the CoverageParts 'real estate'
dRSimmo <- data.frame()
index <- data.frame()
temp <- data.frame("index"=matrix(ncol = 1, nrow = 1))
for(i in 1:length(lRSimmo)){
  for (j in 1:length(lRSimmo[i][[1]]$PolicyPeriod$RSLine$CoverageParts))
  {
    temp$index <- i
    index <- rbind(index,temp)
    dRSimmo <- bind_rows(dRSimmo,as.data.frame(lRSimmo[i][[1]]$PolicyPeriod$RSLine$CoverageParts[[j]]))
  }
}
dRSimmo <- cbind(index,dRSimmo)

# read out the CoverageParts Private
dRSprivat <- data.frame()
index <- data.frame()
temp <- data.frame("index"=matrix(ncol = 1, nrow = 1))
for(i in 1:length(lRSprivat)){
  for (j in 1:length(lRSprivat[i][[1]]$PolicyPeriod$RSLine$CoverageParts))
  {
    temp$index <- i
    index <- rbind(index,temp)
    dRSprivat <- bind_rows(dRSprivat,as.data.frame(lRSprivat[i][[1]]$PolicyPeriod$RSLine$CoverageParts[[j]]))
  }
}
dRSprivat <- cbind(index,dRSprivat)

# read out the CoverageParts Family
dRSFamily <- data.frame()
index <- data.frame()
temp <- data.frame("index"=matrix(ncol = 1, nrow = 1))
for(i in 1:length(lRSFamily)){
  for (j in 1:length(lRSFamily[i][[1]]$PolicyPeriod$RSLine$CoverageParts))
  {
    temp$index <- i
    index <- rbind(index,temp)
    dRSFamily <- bind_rows(dRSFamily,as.data.frame(lRSFamily[i][[1]]$PolicyPeriod$RSLine$CoverageParts[[j]]))
  }
}
dRSFamily <- cbind(index,dRSFamily)