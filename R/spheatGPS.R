spheatGPS <- function (dataset, latcol,loncol, googleapikey, oride=FALSE, deleteGADM=TRUE)  {
  dfname <- get(dataset)
  dfname[,latcol] <- as.numeric(as.character(dfname[,latcol]))
  dfname[,loncol] <- as.numeric(as.character(dfname[,loncol]))
  dfname$latlonccc <- paste0(dfname[,latcol],"-",dfname[,loncol])
  uni.loc<- subset(dfname, !duplicated(latlonccc))
  uni.loc <- subset(uni.loc, latlonccc!="NA-NA")
  uni.loc <- uni.loc[!(is.na(uni.loc[,"latlonccc"]) | uni.loc[,"latlonccc"]=="-"),]
  iters.look <- nrow(uni.loc)
  pb = txtProgressBar(min = 0, max = iters.look, initial = 0, style=3)
  print(paste("...Reverse geocoding your locations in dataset, please wait..."))
  locations.df <- foreach(a=1:iters.look, .combine=rbind) %do% {
    setTxtProgressBar(pb,a)
    gway.df <- google_reverse_geocode(location=c(as.numeric(uni.loc[a,latcol]),
                                                 as.numeric(uni.loc[a,loncol])),
                                      simplify=TRUE,
                                      key=googleapikey)
    gway.df<- as.data.frame(gway.df)
    gway.df <- gway.df[1,"results.formatted_address"]
    gway.df <- as.data.frame(gway.df)
    gway.df$lat <- uni.loc[a,latcol]
    gway.df$lon <- uni.loc[a,loncol]
    gway.df$PlacenameGeocoded <- gway.df[,"gway.df"]
    gway.df <- gway.df[,2:ncol(gway.df)]
    gway.df
  }
 dfname<-  dfname[ , -which(names(dfname) %in% c("latlonccc"))]
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
   if(exists("deleteGADM")){
    if(deleteGADM==TRUE){
      file.remove(paste0("gadm",i,".zip"))
      unlink(paste0("gadm",i, "/*"))
      file.remove(paste0("gadm",i,"/"))
     }
   }
  }
 masterloc <- do.call("bind_rows",clist)
 masterloc <- as.data.frame(masterloc)
 masterloc$lat <- as.numeric(masterloc[,"lat"])
 masterloc$lon <- as.numeric(masterloc[,"lon"])
 names(masterloc)[names(masterloc) == "lat"] <- paste(latcol)
 names(masterloc)[names(masterloc) == "lon"] <- paste(loncol)
 masterloc <- masterloc[, colSums(is.na(masterloc)) != nrow(masterloc)]
 dfname <- suppressMessages(left_join(dfname, masterloc))
 MasterSPheat <<- dfname
}
