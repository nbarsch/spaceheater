spheatLookup <- function (googleapikey, oride=FALSE, deleteGADM=TRUE)  {
  replacebug <-"n"
  a <- 1
  locations.df <- NULL
  repeat{
    uinput.location <- readline(prompt="Enter Location or DONE (Country, State, Address, or other Place Name): ")
    if(uinput.location=="DONE"){break}
    uinput.newname <- gsub(' ','',uinput.location)
    if(uinput.newname=="NewYork,NewYork"){
      replacebug <- uinput.newname
      uinput.newname <- gsub('NewYork,NewYork','Empire State Building',uinput.newname)
    }
    gway.df <- google_geocode(uinput.newname,key=googleapikey)
    if(gway.df$status=="ZERO_RESULTS"){
      print(paste0("...Could not find entry: ", uinput.location))
      uinput.newname <- readline(prompt="Replace with manual entry: ")
      uinput.newname <- gsub(' ','',uinput.newname)
      gway.df <- google_geocode(paste(uinput.newname),key=googleapikey)
    }
    gway.df<- unlist(gway.df)
    gway.df<- as.data.frame(gway.df)
    gway.df <- as.data.frame(gway.df[c("results.geometry.location.lat","results.geometry.location.lng","results.formatted_address"), "gway.df"])
    colnames(gway.df)<-"gway"
    gway.df <- t(gway.df)
    gway.df <- data.frame(r1= row.names(gway.df), gway.df, row.names=NULL)
    if(replacebug=="NewYork,NewYork"){
      uinput.location<-replacebug
    }
    gway.df$namelook <- uinput.location
    gway.df <- gway.df[,2:ncol(gway.df)]
    colnames(gway.df) <- c("lat", "lon","PlacenameGeocoded", "namelook")
    replacebug <-"n"
    locations.df <- rbind(locations.df, gway.df)
  }
  locations.df$lat <- as.numeric(as.character(locations.df[,"lat"]))
  locations.df$lon <- as.numeric(as.character(locations.df[,"lon"]))
  locations.df$PN <- gsub('[0-9]','', as.character(locations.df[,"PlacenameGeocoded"]))
  locations.df$PN <- gsub(' ,',',', as.character(locations.df[,"PN"]))
  locations.df$country <- sub(".*,\\s*([^,]+)$", "\\1", locations.df$PN)
  locations.df$iso3c <- countrycode(locations.df[,"country"], 'country.name', 'iso3c')
  uni.loc<- subset(locations.df, !duplicated(iso3c))
  uni.loc<- subset(uni.loc, !is.na(iso3c))
  a <-1
  clist <- list()
  pb = txtProgressBar(min = 0, max = nrow(uni.loc), initial = 0, style=3)
  for(i in uni.loc[,"iso3c"]){
    if(oride!=TRUE){
      curl_download(paste0("http://biogeo.ucdavis.edu/data/gadm2.8/shp/", i,"_adm_shp.zip"),
                    destfile=paste0("gadm",i,".zip"))
      unzip(paste0("gadm",i,".zip"), exdir=paste0("gadm",i), overwrite=TRUE)
    }
    listfiles <-list.files(paste0("gadm",i,"/"))
    listfiles <- gsub("license.txt","aaa.txt", listfiles)
    listfiles <- sort(listfiles)
    smallest.file <- substr(listfiles[length(listfiles)],8,8)
    sptemp <- readOGR(dsn=paste0("gadm",i), layer=paste0(i,"_adm",smallest.file))
    sp.locations <- locations.df[ which(locations.df$iso3c==i ), ]
    coordinates(sp.locations) <- ~lon+lat
    proj4string(sp.locations)=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    over <- over(sp.locations,sptemp)
    sp.locations <- as.data.frame(sp.locations)
    clist[[a]] <- data.frame(sp.locations, over)
    setTxtProgressBar(pb,a)
    a <- a+1
    if(deleteGADM!=FALSE){
      file.remove(paste0("gadm",i,".zip"))
      unlink(paste0("gadm",i, "/*"))
      file.remove(paste0("gadm",i,"/"))
    }
  }

  masterloc <- rbind.fill(clist)
  masterloc$lat <- as.numeric(masterloc[,"lat"])
  masterloc$lon <- as.numeric(masterloc[,"lon"])
  masterloc$latlong <- paste0(masterloc[,"lat"],"-",masterloc[,"lon"])
  masterloc <- masterloc[, colSums(is.na(masterloc)) != nrow(masterloc)]
  MasterSPheat <<- masterloc
}

