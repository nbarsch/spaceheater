#' Reverse geocode GPS coordinates in a dataset
#'
#' Reverse geocode latitude and longitude columns in a dataset with
#' administrative layers determined by GADM layers
#'
#'
#' @param dataset (character), the name of the data frame containing a column
#' of place names. e.g. \code{“mydataframe”}
#' @param latcol (character), the name of the column in the data frame
#' containing LATITUDES \code{“mylatcol”}
#' @param loncol (character), the name of the column in the data frame
#' containing LONGITUDES \code{“myloncol”}
#' @param googleapikey (character), a valid Google Maps API key. See
#' https://developers.google.com/maps/documentation/javascript/get-api-key to
#' attain one.
#' @param oride (logical), if you already have the specified shapefile
#' downloads from GADM for each country in your data frame, you may override
#' the GADM downloads included in a-heat using oride=TRUE. ONLY do this if you
#' are sure you have the GADM shapefiles for every country in your set already
#' in your R working directory. Default is oride=FALSE
#' @param deleteGADM (logical), if after geocoding you would like to keep the
#' GADM shapefiles downloaded in your working directory you may use
#' deleteGADM=FALSE. The files can be large, especially if you have many
#' countries in your dataset. Use carefully, could cause many large GADM
#' shapefiles saved to your working directory. The default is deleteGADM=TRUE.
#' @author Neal Thomas Barsch
#' @references GADM DATA are attained through the GADM project website.
#' Commercial use of this function is not allowed without prior permission from
#' GADM.org. \url{http://gadm.org/}.
#' @examples
#'
#'
#' spheatGPS("myDataframe", "myLatColumnName","myLonColumnName", "mygoogleapikey")
#'
#' #Keeping all GADM shapefiles
#' spheatGPS("myDataframe", "myLatColumnName","myLonColumnName", "mygoogleapikey", deleteGADM=FALSE)
#'
#' #You already have the GADM shapefiles and don't want to redownload or delete them
#' spheatGPS("myDataframe", "myLatColumnName","myLonColumnName", "mygoogleapikey", oride=TRUE, deleteGADM=FALSE)
#'
#'
#' @export spheatGPS
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
 MASTER_gps <<- dfname
}
