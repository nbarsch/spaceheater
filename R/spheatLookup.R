#' Geocode a list of places manually typed into the function in a loop
#'
#' Geocode a list of places manually typed into spaceheater in a loop with
#' administrative layers determined by GADM layers. The function will prompt
#' the user to type place names until the user is finished (indicated by typing
#' DONE). The function returns a dataframe of all typed locations with GADM
#' administrative levels and GPS coordinates. This function is only useful if
#' your locations are not already in a data frame, see spheatNames() if you
#' already have locations in data frame format.
#'
#'
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
#' spheatLookup("mygoogleapikey")
#'
#' #Keeping all GADM shapefiles
#' spheatLookup("mygoogleapikey", deleteGADM=FALSE)
#'
#' #You already have the GADM shapefiles and don't want to redownload or delete them
#' spheatLookup("mygoogleapikey", oride=TRUE, deleteGADM=FALSE)
#'
#'
#' @export spheatLookup
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

  masterloc <- do.call("bind_rows",clist)
  masterloc <- as.data.frame(masterloc)
  masterloc$lat <- as.numeric(masterloc[,"lat"])
  masterloc$lon <- as.numeric(masterloc[,"lon"])
  masterloc$latlong <- paste0(masterloc[,"lat"],"-",masterloc[,"lon"])
  masterloc <- masterloc[, colSums(is.na(masterloc)) != nrow(masterloc)]
  MASTER_lookup <<- masterloc
}

