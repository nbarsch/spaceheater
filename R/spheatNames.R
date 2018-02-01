spheatNames <- function (dataset, colname, googleapikey, oride=FALSE, deleteGADM=TRUE)  {
  replacebug <-"n"
  dfname <- get(dataset)
  dfname$namelook <- as.character(dfname[,paste0(colname)])
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
    gway.df <- google_geocode(paste(lookVector[a]),key=googleapikey)
    if(gway.df$status=="ZERO_RESULTS"){
      print(paste0("...Could not find entry: ", lookVector[a]))
      uinput.newname <- readline(prompt="Replace with manual entry: ")
      uinput.newname <- gsub(' ','',uinput.newname)
      dfname$namelook <- gsub(paste(lookVector[a]), paste(uinput.newname), dfname[,"namelook"])
      lookVector[a] <- uinput.newname
      gway.df <- google_geocode(paste(lookVector[a]),key=googleapikey)
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
    clist[[a]] <- lapply(clist[[a]], as.character)
    setTxtProgressBar(pb,a)
    a <- a+1
    if(deleteGADM==TRUE){
      file.remove(paste0("gadm",i,".zip"))
      unlink(paste0("gadm",i, "/*"))
      file.remove(paste0("gadm",i,"/"))
      }
  }
  masterloc <- do.call("bind_rows",clist)
  masterloc <- as.data.frame(masterloc)
  masterloc$lat <- as.numeric(masterloc[,"lat"])
  masterloc$lon <- as.numeric(masterloc[,"lon"])
  masterloc$latlong <- paste0(masterloc[,"lat"],"-",masterloc[,"lon"])
  masterout<- left_join(dfname,masterloc, by="namelook")
  masterout <- masterout[, colSums(is.na(masterout)) != nrow(masterout)]
  MasterSPheat <<- masterout
}

