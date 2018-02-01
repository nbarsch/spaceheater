
print(paste("See developers.google.com/maps/documentation/javascript/get-api-key if you don't have an API key "))
key <- readline(prompt="Enter Google API Key: ")
key <- gsub(' ','',key)


print(paste("Does your data have a column of places like 'Seattle, Washington' or '1700 Pennsylvania Ave?'"))
print(paste("Geographic Input Type: Names"))
print(paste("Latitude Column AND Longitude Column in your dataset?"))
print(paste("Geographic Input Type:GPS"))
print(paste("Manually type each location entry to lookup?"))
print(paste("Geographic Input Type:Each"))
inputType <- readline(prompt="Enter Geographic Input Type: (Names, GPS, or Each): ")
inputType <- tolower(inputType)
inputType <- gsub(' ','',inputType)
inputType <- gsub("[^[:alnum:]]", "", inputType)
inputType <- gsub("list", "", inputType)
inputType <- gsub("of", "", inputType)
inputType <- gsub("input", "", inputType)
inputType <- gsub("location", "", inputType)
replacebug <-"n"








oride <- readline(prompt="Override GADM Download?(ONLY IF ALREADY DOWNLOADED)(y/n): ")
oride <- tolower(oride)
if(inputType=="each") {
  print(paste("Accepts any type of placename (including Addresses)"))
  uinput.ncounty <- readline(prompt="Enter Number of Locations to Lookup and Extract: ")
  uinput.ncounty <- gsub(' ','',uinput.ncounty)
  locations.df <- foreach(a=1:uinput.ncounty, .combine=rbind) %do% {
    uinput.location <- readline(prompt="Enter Location (County, State, country, or Other Place Name): ")
    if(uinput.location=="NewYork,NewYork"){uinput.location <- gsub('NewYork,NewYork','Empire State Building',uinput.location)}
    gway.df <- google_geocode(paste(uinput.location),key=key, simplify=TRUE)
    if(gway.df$status=="ZERO_RESULTS"){
      print(paste0("Could not find entry: ", uinput.location))
      uinput.newname <- readline(prompt="Replace with manual entry: ")
      uinput.newname <- gsub(' ','',uinput.newname)
      dfname$namelook <- gsub(paste(lookVector[a]), paste(uinput.newname), dfname[,"namelook"])
      uinput.location <- uinput.newname
      gway.df <- google_geocode(paste(uinput.newname),key=key, simplify=TRUE)
    }
    gway.df<- unlist(gway.df)
    gway.df<- as.data.frame(gway.df)
    gway.df <- as.data.frame(gway.df[c("results.geometry.location.lat","results.geometry.location.lng","results.formatted_address"), "gway.df"])
    colnames(gway.df)<-"gway"
    gway.df <- t(gway.df)
    gway.df <- data.frame(r1= row.names(gway.df), gway.df, row.names=NULL)
    if(uinput.location=="Empire State Building"){
      uinput.location<-"NewYork,NewYork"
    }
    gway.df$namelook <- uinput.location
    gway.df <- gway.df[,2:ncol(gway.df)]
    colnames(gway.df) <- c("lat", "lon","PlacenameGeocoded", "namelook")
    gway.df
  }
}

if(inputType=="names") {
  print(paste("Accepts any type of placename"))
  print(paste("If your DATASET NAME is 'df.example', type:df.example"))
  uinput.dfname <- readline(prompt="Enter DATASET NAME: ")
  uinput.dfname <- gsub(' ','',uinput.dfname)
  print(paste("Don't use quotes, if your COLUMN NAME for LOCATIONS is locationcol, type:locationcol"))
  uinput.lookcol <- readline(prompt="Enter COLUMN NAME of LOCATIONS to Lookup and Extract: ")
  dfname <- get(uinput.dfname)
  dfname$namelook <- as.character(dfname[,paste0(uinput.lookcol)])
  dfname$namelook <- gsub(' ','',dfname[,"namelook"])
  lookcol <- dfname[,"namelook"]
  lookcol <- as.data.frame(lookcol)
  colnames(lookcol) <- "lookcol"
  lookcol$lookcol <- as.character(lookcol[,"lookcol"])
  lookcol <- lookcol[!(is.na(lookcol[,"lookcol"]) | lookcol[,"lookcol"]==""),]
  lookVector <- unique(lookcol)
  iters.look <- length(lookVector)
  pb = txtProgressBar(min = 0, max = iters.look, initial = 0, style=3)
  print(paste("...Geocoding your locations in dataset, please wait..."))
  locations.df <- foreach(a=1:iters.look, .combine=rbind) %do% {
    setTxtProgressBar(pb,a)
    if(lookVector[a]=="NewYork,NewYork"){
      replacebug <- lookVector[a]
      lookVector[a] <- gsub('NewYork,NewYork','Empire State Building',lookVector[a])
    }
    gway.df <- google_geocode(paste(lookVector[a]),key=key)
    if(gway.df$status=="ZERO_RESULTS"){
      print(paste0("...Could not find entry: ", lookVector[a]))
      uinput.newname <- readline(prompt="Replace with manual entry: ")
      uinput.newname <- gsub(' ','',uinput.newname)
      dfname$namelook <- gsub(paste(lookVector[a]), paste(uinput.newname), dfname[,"namelook"])
      lookVector[a] <- uinput.newname
      gway.df <- google_geocode(paste(lookVector[a]),key=key)
      rm(uinput.newname)
    }
    gway.df<- unlist(gway.df)
    gway.df<- as.data.frame(gway.df)
    gway.df <- as.data.frame(gway.df[c("results.geometry.location.lat","results.geometry.location.lng","results.formatted_address"), "gway.df"])
    colnames(gway.df)<-"gway"
    gway.df <- t(gway.df)
    gway.df <- data.frame(r1= row.names(gway.df), gway.df, row.names=NULL)
    if(replacebug=="NewYork,NewYork"){
      lookVector[a]<-replacebug
    }
    gway.df$namelook <- lookVector[a]
    gway.df <- gway.df[,2:ncol(gway.df)]
    colnames(gway.df) <- c("lat", "lon","PlacenameGeocoded", "namelook")
    replacebug <-"n"
    gway.df
  }
  rm(lookcol, lookVector, uinput.lookcol)
}
a <-1
if(inputType=="gps") {
  uinput.dfname <- readline(prompt="Enter Dataset Name: ")
  uinput.latcol <- readline(prompt="Enter Latitude Column Name: ")
  uinput.loncol <- readline(prompt="Enter Longitude Column Name: ")
  dfname <- get(uinput.dfname)
  dfname$latlon <- paste0(dfname[,uinput.latcol],"-",dfname[,uinput.loncol])
  uni.loc<- subset(dfname, !duplicated(latlon))
  uni.loc <- uni.loc[!(is.na(uni.loc[,"latlon"]) | uni.loc[,"latlon"]=="-"),]
  iters.look <- nrow(uni.loc)
  pb = txtProgressBar(min = 0, max = iters.look, initial = 0, style=3)
  print(paste("...Reverse geocoding your locations in dataset, please wait..."))
  locations.df <- foreach(a=1:iters.look, .combine=rbind) %do% {
    setTxtProgressBar(pb,a)
    gway.df <- google_reverse_geocode(location=c(uni.loc[a,uinput.latcol],
                                                uni.loc[a,uinput.loncol]),
                                                simplify=TRUE,
                                      key=key )
    gway.df<- as.data.frame(gway.df)
    gway.df <- gway.df[1,"results.formatted_address"]
    gway.df <- as.data.frame(gway.df)
    gway.df$lat <- uni.loc[a,uinput.latcol]
    gway.df$lon <- uni.loc[a,uinput.loncol]
    gway.df$PlacenameGeocoded <- gway.df[,"gway.df"]
    gway.df <- gway.df[,2:ncol(gway.df)]
    gway.df
  }
}
locations.df$lat <- as.numeric(as.character(locations.df[,"lat"]))
locations.df$lon <- as.numeric(as.character(locations.df[,"lon"]))
locations.df$PN <- gsub('[0-9]','', as.character(locations.df[,"PlacenameGeocoded"]))
locations.df$PN <- gsub(' ,',',', as.character(locations.df[,"PN"]))
locations.df$country <- sub(".*,\\s*([^,]+)$", "\\1", locations.df$PN)
locations.df$iso3 <- countrycode(locations.df[,"country"], 'country.name', 'iso3c')
uni.loc<- subset(locations.df, !duplicated(iso3))
uni.loc<- subset(uni.loc, !is.na(iso3))
a <-1
clist <- list()
pb = txtProgressBar(min = 0, max = nrow(uni.loc), initial = 0, style=3)
for(i in uni.loc[,"iso3"]){
  if(oride !="y"){
    curl_download(paste0("http://biogeo.ucdavis.edu/data/gadm2.8/shp/", i,"_adm_shp.zip"),
                              destfile=paste0("gadm",i,".zip"))
    unzip(paste0("gadm",i,".zip"), exdir=paste0("gadm",i), overwrite=TRUE)
  }
  listfiles <-list.files(paste0("gadm",i,"/"))
  locations.df$PN <- gsub(' ,',',', as.character(locations.df[,"PN"]))
  listfiles <- gsub("license.txt","aaa.txt", listfiles)
  listfiles <- sort(listfiles)
  smallest.file <- substr(listfiles[length(listfiles)],8,8)
  sptemp <- readOGR(dsn=paste0("gadm",i), layer=paste0(i,"_adm",smallest.file))
  sp.locations <- locations.df[ which(locations.df$iso3==i ), ]
  coordinates(sp.locations) <- ~lon+lat
  proj4string(sp.locations)=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
  over <- over(sp.locations,sptemp)
  sp.locations <- as.data.frame(sp.locations)
  clist[[a]] <- data.frame(sp.locations, over)
  clist[[a]] <- lapply(clist[[a]], as.character)
  setTxtProgressBar(pb,a)
  a <- a+1
}
masterloc <- do.call("bind_rows",clist)
masterloc <- as.data.frame(masterloc)
masterloc$lat <- as.numeric(masterloc[,"lat"])
masterloc$lon <- as.numeric(masterloc[,"lon"])
if(inputType=="names") {
 masterloc$latlong <- paste0(masterloc[,"lat"],"-",masterloc[,"lon"])
 masterout<- left_join(dfname,masterloc, by="namelook")
}
if(inputType=="each") {
  locations.df$latlon <- paste0(locations.df[,"lat"],"-",locations.df[,"lon"])
  masterout<- locations.df
}
if(inputType=="gps") {
  masterout<- left_join(dfname,masterloc, by=c("lat","lon"))
}
rm(a, oride, dfname, gway.df, i, inputType, iters.look,
   listfiles, locations.df, over, pb, smallest.file,
   sp.locations, sptemp, uinput.dfname, uni.loc, clist, replacebug )


