#'
#' Determine GADM.org administrative division of GPS coordinates from a dataset and
#' download geometries of dataset locations for later use
#'
#'
#' @param dataset (character), the name of the data frame containing a column
#' of place names. e.g. \code{“mydataframe”}
#' @param latcol (character), the name of the column in the data frame
#' containing latitude values e.g. \code{“mylatcolname”}
#' @param loncol (character), the name of the column in the data frame
#' containing longitude values e.g. \code{“myloncolname”}
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

#'
#'
#' @author Neal Thomas Barsch
#' @references GADM DATA are attained through the GADM project website.
#' Commercial use of this function is not allowed without prior permission from
#' GADM.org. \url{http://gadm.org/}.
#' @examples
#'
#'
#' spheatGPS("myDataframe", "mylatcol", "myloncol", "mygoogleapikey")
#'
#' #Keeping all GADM shapefiles
#' spheatGPS("myDataframe", "mylatcol", "myloncol", "mygoogleapikey", deleteGADM=FALSE)
#'
#' #You already have the GADM shapefiles and don't want to redownload or delete them
#' spheatGPS("myDataframe", "mylatcol", "myloncol", "mygoogleapikey", oride=TRUE, deleteGADM=FALSE)
#'
#'
#' @export spheatGPS2
spheatGPS2 <- function (dataset, latcol="lat", loncol="lon", googleapikey, gadmlevel="lowest", fill=TRUE, skipMissing=FALSE, orideGADM=FALSE, deleteGADM=TRUE)  {

  ###Get dataset to merge on and column to look up
  ### Replaced dfname <- get(dataset)
  replacebug <-"n"
  dfname <- dataset
  dfname$lon <- dfname[,paste0(loncol)]
  dfname$lat <- dfname[,paste0(latcol)]


  dfname[grep('ID_', names(dfname))] <- lapply(dfname[grep('ID_', names(dfname))], as.character)

  ###Minor housekeeping with the names before fed to google

  ###Housekeeping
  if(is.character(gadmlevel)){
    gadmlevel <- tolower(gadmlevel)
    if(gadmlevel=="village"){gadmlevel <-"lowest" }
    if(gadmlevel=="state"){gadmlevel <-1 }
    if(gadmlevel=="province"){gadmlevel <-1 }
    if(gadmlevel=="low"){gadmlevel <-"lowest" }
    if(gadmlevel=="l"){gadmlevel <-"lowest" }
    if(gadmlevel=="h"){gadmlevel <-0 }
    if(gadmlevel=="high"){gadmlevel <-0 }
    if(gadmlevel=="country"){gadmlevel <-0 }
    if(gadmlevel=="highest"){gadmlevel <- 0}
    if(gadmlevel=="lowestcommon"){gadmlevel <- "lcl"}
    if(gadmlevel=="lowest common"){gadmlevel <- "lcl"}
    if(gadmlevel=="lc"){gadmlevel <- "lcl"}
    if(gadmlevel=="common"){gadmlevel <- "lcl"}



    if(gadmlevel=="lcl" | gadmlevel=="lowest") {
      load("gadm_levels28.RData")
      gadm_levels2 <- gadm_levels2[,c("iso3c", "level")]
      ### deleted gadm_levels2[grep('iso3c', names(gadm_levels2))] <- lapply(gadm_levels2[grep('iso3c', names(gadm_levels2))], as.character)
    }
  }




  ###Taking only unique text lookups and merging later will speed sets with dup locations

  #Replace progress bar

  print(paste("...Geocoding your locations in dataset, please wait..."))
  pb = txtProgressBar(min = 0, max = 100, initial = 0, style=3)
  ###Making defaults of deleting all and skipping all FALSE



  dfname$countryplace <- map.where(database="world", dfname$lon, dfname$lat)
  dfname$CountryName <- sapply(str_split(dfname$countryplace, ":"),"[[",1)
  dfname$CountryName <- standardizeCountry(dfname$CountryName, fuzzyDist = 20)

  locations.df <- dfname[!duplicated(dfname[,c('lon','lat')]),]
  locations.df[grep('^ID_', names(locations.df))] <- lapply(locations.df[grep('^ID_', names(locations.df))], as.character)
  locations.df$iso3c <- suppressWarnings(invisible(countrycode(locations.df[,"CountryName"], 'country.name', 'iso3c')))
  locations.df[grep('iso3c', names(locations.df))] <- lapply(locations.df[grep('iso3c', names(locations.df))], as.character)

  ###little format housekeeping (probably unnecessary with the update, remove in future update)
  locations.df$lat <- as.numeric(as.character(locations.df[,"lat"]))
  locations.df$lon <- as.numeric(as.character(locations.df[,"lon"]))


  ###subset list of countries to work with
  uni.loc<- locations.df[!duplicated(locations.df$iso3c),]
  uni.loc<- uni.loc[!is.na(uni.loc$iso3c),]

  ###Get the lowest gadm level available for each country, from loaded dataframe
  ### TOOK OUT gadmlevel=="lowest" | ###I'm 99 percent sure unnecessary, due to smallest.file
  if(gadmlevel=="lcl"){
    uni.loc <- suppressMessages(left_join(uni.loc, gadm_levels2[, c("iso3c", "level")]))
    mins <- uni.loc[,"level"]
    gadmlevel <- min(mins)
  }

  n.uniloc <- nrow(uni.loc)

  a <-1
  clist <- list()
  gadmorig <- gadmlevel
  globalgeo <- list()
  globalstat <- list()
  ###For loop for each country in dataset
  for(i in uni.loc[,"iso3c"]){
    ###Defaults of errors on WorldPop and GADM to FALSE
    noGADM <- FALSE
    gadmlevel <- gadmorig

    dGADM <- file.exists(paste0("gadm",i))
    ###Download and unzip GADM if exists
    if(!isTRUE(orideGADM) & !isTRUE(dGADM)){
      writeLines(c("",paste0("Currently DOWNLOADING the GADM file for: ", uni.loc[a,"CountryName"] )))
      gadmresp <- tryCatch(GET(
        url = paste0("http://biogeo.ucdavis.edu/data/gadm2.8/shp/", i,"_adm_shp.zip"),
        progress(),
        write_disk(paste0("gadm",i,".zip"), overwrite=TRUE)
      )-> gadmresp,
      error=function(e){noGADM <- TRUE})
      if(!isTRUE(gadmresp)){gadmresp <- FALSE}
      if(!isTRUE(noGADM) & !isTRUE(gadmresp)){unzip(paste0("gadm",i,".zip"), exdir=paste0("gadm",i), overwrite=TRUE)}
    }

    ###Get the lowest GADM layer to overlay. Lowest layers come with higher division codes as well
    dGADM <- file.exists(paste0("gadm",i))
    if(isTRUE(dGADM)){
      writeLines(c("",paste0("SHAPEFILE SUCCESS!: ", uni.loc[a,"CountryName"] )))

      if(is.character(gadmlevel)){
        listfiles <-list.files(paste0("gadm",i,"/"))

        ###Sometimes license.txt gets in the wrong place
        listfiles <- gsub("license.txt","aaa.txt", listfiles)
        listfiles <- sort(listfiles)
        smallest.file <- substr(listfiles[length(listfiles)],8,8)
        gadmlevel <- as.numeric(smallest.file)
      }

      ###Shapefile read (using sf)
      sptemp <- sf::st_read(dsn=paste0("gadm",i), layer=paste0(i,"_adm",gadmlevel), quiet=TRUE)

      flist <- list()
      glist <- list()


      ###Over SF methods for determining dataset locations administrative levels
      sp.locations <- locations.df[ which(locations.df$iso3c==i ), ]
      sf.locations <- st_as_sf(sp.locations, coords=c("lon","lat"), crs= 4326)
      over <- suppressWarnings(suppressMessages(st_intersection(sf.locations,sptemp)))
      st_geometry(over) <- NULL
      over <- suppressMessages(left_join(over, sptemp[,c(paste0("ID_",gadmlevel), "geometry")]))
      sp.locations <- as.data.frame(sp.locations)
      df.loc <- bind_cols(sp.locations[,c("lat", "lon")], over)


      ### Took out df.loc<- suppressMessages(suppressWarnings(left_join(sp.locations, over)))

      df.loc[grep('^ID_', names(df.loc))] <- lapply(df.loc[grep('^ID_', names(df.loc))], as.character)
      df.loc[grep('iso3c', names(df.loc))] <- lapply(df.loc[grep('iso3c', names(df.loc))], as.character)
      ##get area and modify format
      df.loc$area_a <- st_area(df.loc$geometry)
      df.loc$area_a <- as.numeric(df.loc$area_a)
      df.loc$area_a_sqkm <- df.loc[,"area_a"]/1000000
      df.loc[grep('^ID_', names(df.loc))] <- lapply(df.loc[grep('^ID_', names(df.loc))], as.character)

      justid <- df.loc[ ,  grepl( "ID_" , colnames( df.loc ) ) ]

      flist[[1]] <- cbind(justid, df.loc[,c("area_a", "area_a_sqkm", "geometry")])
      glist[[1]] <- cbind(justid, df.loc[,c("countryplace", "lat", "lon",
                                            "CountryName", "iso3c", "area_a", "area_a_sqkm")])

      colnames(flist[[1]])[colnames(flist[[1]])=='geometry'] <- paste0("geometry",gadmlevel)
      colnames(flist[[1]])[colnames(flist[[1]])=='area_a'] <- paste0("area_",gadmlevel,"_Msq")
      colnames(flist[[1]])[colnames(flist[[1]])=='area_a_sqkm'] <- paste0("area_",gadmlevel,"_KMsq")

      colnames(glist[[1]])[colnames(glist[[1]])=='geometry'] <- paste0("geometry",gadmlevel)
      colnames(glist[[1]])[colnames(glist[[1]])=='area_a'] <- paste0("area_",gadmlevel,"_Msq")
      colnames(glist[[1]])[colnames(glist[[1]])=='area_a_sqkm'] <- paste0("area_",gadmlevel,"_KMsq")

      if(isTRUE(fill)){
        rgadmlevel <- gadmlevel
        rr <-2
        repeat{
          rgadmlevel <- rgadmlevel - 1
          if(rgadmlevel<0){break}
          sptemp <- suppressWarnings(sf::st_read(dsn=paste0("gadm",i), layer=paste0(i,"_adm",rgadmlevel), quiet=TRUE))
          over <- suppressWarnings(suppressMessages(st_intersection(sf.locations,sptemp)))
          over <- over[,c(paste0("ID_",rgadmlevel))]
          st_geometry(over) <- NULL
          over <- unique(over)
          over <- suppressMessages(left_join(over, sptemp[,c(paste0("ID_",rgadmlevel), "geometry")]))
          over[grep('^ID_', names(over))] <- lapply(over[grep('^ID_', names(over))], as.character)
          over$area_a <- st_area(over$geometry)
          over$area_a <- as.numeric(over$area_a)
          over$area_a_sqkm <- over[,"area_a"]/1000000
          ### dont think need this: over <- suppressMessages(left_join(over, df.loc[,c(paste0("ID_",rgadmlevel),"countryplace")]))
          colnames(over)[colnames(over)=='geometry'] <- paste0("geometry",rgadmlevel)
          colnames(over)[colnames(over)=='area_a'] <- paste0("area_",rgadmlevel,"_Msq")
          colnames(over)[colnames(over)=='area_a_sqkm'] <- paste0("area_",rgadmlevel,"_KMsq")
          gover <- over[,c(1, 3, 4)]
          flist[[rr]]<- over
          glist[[rr]]<- gover
          rr <- rr+1
        }
      }
    }
    globalgeo[[a]] <- flist
    globalstat[[a]] <- glist
    a <- a+1
    ###Delete GADM shapefile in disk space (wd) if default deleteGADM=TRUE. Used for all extraction already
    if(isTRUE(deleteGADM) & isTRUE(dGADM)){
      file.remove(paste0("gadm",i,".zip"))
      unlink(paste0("gadm",i, "/*"))
      file.remove(paste0("gadm",i,"/"))
    }
  }
  MASTERstat <- list()
  foreach(p=1:length(globalstat))%do%{
    MASTERstat[[p]] <- suppressMessages(Reduce(left_join, globalstat[[p]]))
  }
  MASTERstat <- suppressWarnings(do.call("bind_rows",MASTERstat))

  ###MASTERgeo <<- globalgeo
  foreach(g=1:length(globalgeo))%do%{
    n.lgg <- length(globalgeo[[g]])
    globalgeo[[g]][[n.lgg]] <- suppressMessages(inner_join(globalgeo[[g]][[n.lgg]],
                                                           as.data.frame(unique(MASTERstat[,c("ID_0", "CountryName")]))))
    checkin1 <<- TRUE
  }
  MASTERgeo <<- globalgeo
  colnames(MASTERstat) <- paste("sp", colnames(MASTERstat), sep="_")
  colnames(MASTERstat)[colnames(MASTERstat)=="sp_lat"] <- "lat"
  colnames(MASTERstat)[colnames(MASTERstat)=="sp_lon"] <- "lon"
  MASTERstat <<- MASTERstat
  MASTERout <<- suppressMessages(left_join(dfname, MASTERstat))
  writeLines(c("",green("Written to Global Environment:"),
               "MASTERgeo is your list of geometries. Use it for extraction functions.",
               "MASTERout is your dataset bound with newly attached geocoding.",
               "MASTERstat is your dataset of unique locations in the dataset, unbound."))


}











