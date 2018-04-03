#' Geocode place names in a dataset
#'
#' geocode a vector of names in a dataset with administrative layers determined
#' by GADM layers
#'
#'
#' @param dataset (character), the name of the data frame containing a column
#' of place names. e.g. \code{“mydataframe”}
#' @param colname (character), the name of the column in the data frame
#' containing place names e.g. \code{“mycolname”}
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
#'
#' @author Neal Thomas Barsch
#' @references GADM DATA are attained through the GADM project website.
#' Commercial use of this function is not allowed without prior permission from
#' GADM.org. \url{http://gadm.org/}.
#' @examples
#'
#'
#' spheatNAMES("myDataframe", "myColWithPlaceNames", "mygoogleapikey")
#'
#' #Keeping all GADM shapefiles
#' spheatNAMES("myDataframe", "myColWithPlaceNames", "mygoogleapikey", deleteGADM=FALSE)
#'
#' #You already have the GADM shapefiles and don't want to redownload or delete them
#' spheatNAMES("myDataframe", "myColWithPlaceNames", "mygoogleapikey", oride=TRUE, deleteGADM=FALSE)
#'
#'
#' @export spheatNAMES
spheatNAMES <- function (dataset, colname, googleapikey, gadmlevel="lowest", fill=TRUE, skipMissing=FALSE, orideGADM=FALSE, deleteGADM=TRUE)  {

  ###Get dataset to merge on and column to look up
  ### Replaced dfname <- get(dataset)
  replacebug <-"n"
  dfname <- dataset

  dfname[,sapply(dfname,is.character)] <- sapply(
    dfname[,sapply(dfname,is.character)],
    iconv,"WINDOWS-1252","UTF-8")



  dfname[grep('ID_', names(dfname))] <- lapply(dfname[grep('ID_', names(dfname))], as.character)
  dfname$namelookorig <- dfname[,paste0(colname)]
  dfname$namelook <- tolower(dfname$namelookorig)
  dfname$namelook <- gsub("^ *|(?<= ) | *$", "", dfname$namelook, perl = TRUE)
  dfname$namelook <- gsub(", ", ",", dfname$namelook)

  ###Minor housekeeping with the names before fed to google
  lookcol <- dfname[,"namelook"]
  lookcol <- as.data.frame(lookcol)
  colnames(lookcol) <- "lookcol"
  lookcol$lookcol <- as.character(lookcol[,"lookcol"])
  lookcol <- lookcol[!(is.na(lookcol[,"lookcol"]) | lookcol[,"lookcol"]==""),]


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
  lookVector <- unique(lookcol)
  iters.look <- length(lookVector)

  #Replace progress bar

  print(paste("...Geocoding your locations in dataset, please wait..."))
  pb = txtProgressBar(min = 0, max = 100, initial = 0, style=3)
  ###Making defaults of deleting all and skipping all FALSE
  skipall <- FALSE

  if ("skipall" %in% ls(envir = .GlobalEnv) && missing(skipall)) {
    skipall<- get("skipall", envir = .GlobalEnv)
  }
  if(isTRUE(skipMissing)){skipall <- TRUE}



  ###Looking up and geocoding locations (below)

  testlist <- list()
  ### DELETED .combine=rbind put it back
  locations.df <- foreach(a=1:iters.look, .combine=rbind) %do% {
  replacebug <- "n"
    ###There is an odd bug where if you look up New York, New York, it thinks you mean the hotel in Las Vegas
    ###Below fixes that small bug (by switching the lookup location to Empire State Building)
    if(lookVector[a]=="new york,new york"){
      replacebug <- lookVector[a]
      lookVector[a] <- "empire state building"
    }
    gwayerror <- FALSE

    ###geocode text locations w googleway
    gway.df <- tryCatch(google_geocode(paste0("\"",lookVector[a],"\""),key=googleapikey),
                        error=function(e){gwayerror <- TRUE})

    ###Give user a chance to type a manual entry not found, or delete or ignore the entry
    if(gway.df$status=="ZERO_RESULTS" | isTRUE(gwayerror)){
      if(gway.df$status=="ZERO_RESULTS") {nores <- "no google maps result"}
      if(isTRUE(gwayerror)) {nores <- "internet error, or invalid symbol error"}

    j <- 1
    repeat{
      ###Set default repeat continue to TRUE
      dcont <- TRUE
      ###repeats don't have to go through again after choosing one of the 'all' options
      if(!(isTRUE(skipall))){
        writeLines(c( "",
                      "",
                      paste0("Could not find entry: ", red(lookVector[a])),
                      paste0("Due to ", red(paste0(nores) )),
                      "",
                      underline(blue("OPTIONS:")),
                      paste0("RETRY: " ,blue("Hit enter")),
                      paste0("TYPE CORRECTED PLACE NAME: ", blue("Type the corrected place name for: "),red(lookVector[a])),
                      paste0("SKIP ENTRY: ", blue("Type 's or skip'")),
                      paste0("SKIP ALL NOT FOUND ENTRIES: ", blue("Type 'sa or skip all'"))
        )
        )

        uinput.newname <- readline(prompt="TYPE AN OPTION GIVEN ABOVE: ")

        ###Some housekeeping
        uinput.newname <- gsub("^ *|(?<= ) | *$", "", uinput.newname, perl = TRUE)
        uinput.newname <- tolower(uinput.newname)
        if(uinput.newname=="s"){uinput.newname <- "skip"}
        if(uinput.newname=="sa"){uinput.newname <- "skip all"}
        if(uinput.newname=="skip a"){uinput.newname <- "skip all"}
        if(uinput.newname=="s all"){uinput.newname <- "skip all"}
        if(uinput.newname=="s a"){uinput.newname <- "skip all"}
        if(uinput.newname=="sa"){uinput.newname <- "skip all"}
        if(uinput.newname=="all"){uinput.newname <- "skip all"}
      }

      ###axeem and skipall back in play
      if(isTRUE(skipall)){uinput.newname <- "skip all"}

       ###all processes that SKIP
      if(uinput.newname=="skip" | uinput.newname=="skip all"){
        dfname <- as.data.frame(dfname)
        coltemp <- (which(dfname$namelook==lookVector[a]))
        dfname[c(paste(coltemp), collapse=","),"namelook"] <- "SKIP"
        lookVector[a] <- "SKIP"
        if(uinput.newname=="skip all"){skipall <- TRUE}
        break
      }


      ###Set dcont to continue for another google geocode
      dcont <- TRUE
      gwayerror <- FALSE
      if(isTRUE(dcont)){
        gway.df <- tryCatch(google_geocode(paste(uinput.newname),key=googleapikey),
                            error=function(e){gwayerror <- TRUE})

      }
      ###If new match successful
      if(!(gway.df$status=="ZERO_RESULTS") & !isTRUE(gwayerror)){
        n.gwaydf <- nrow(gway.df$results)
        if(n.gwaydf!=1){
          resy <- as.data.frame(gway.df$results)
          clost <- stringdist::amatch(paste0(lookVector[a]),resy[,"formatted_address"], maxDist=500)
          gway.df$results <- gway.df$results[clost,]
        }
        dfname <- as.data.frame(dfname)
        coltemp <- (which(dfname$namelook==lookVector[a]))
        dfname[c(paste(coltemp), collapse=","),"namelook"] <- uinput.newname
        lookVector[a] <- uinput.newname
        break
      }
      j <- j+1
    }
    }else{
      n.gwaydf <- nrow(gway.df$results)
      if(n.gwaydf!=1){
        resy <- as.data.frame(gway.df$results)
        clost <- stringdist::amatch(paste0(lookVector[a]),resy[,"formatted_address"], maxDist=500)
        gway.df$results <- gway.df$results[clost,]
      }
    }
      ###If not skip this entry
      if(!(is.na(lookVector[a]))){
        gway.df<- unlist(gway.df)
        gway.df<- as.data.frame(gway.df)
        gway.df <- as.data.frame(gway.df[c("results.geometry.location.lat",
                                         "results.geometry.location.lng",
                                         "results.formatted_address"), "gway.df"])
        colnames(gway.df)<-"gway"
        gway.df <- t(gway.df)
        gway.df <- data.frame(r1= row.names(gway.df), gway.df, row.names=NULL)
        ###Weird new york bug see above, putting original back
        if(replacebug=="new york,new york"){
          lookVector[a]<-replacebug
        }
        gway.df$namelook <- lookVector[a]
        gway.df <- gway.df[,2:ncol(gway.df)]
        colnames(gway.df) <- c("lat", "lon","PlacenameGeocoded", "namelook")
        replacebug <-"n"
      }
      ####good to here on a47
      ###If skip this entry enter NA for merge
      if(is.na(lookVector[a])){
        gway.df <- data.frame(
          lat=as.factor(NA),
          lon=as.factor(NA),
          PlacenameGeocoded=as.factor(NA),
          namelook=as.character("SKIP")
          )
      }
    setTxtProgressBar(pb, (a/iters.look)*100)
    return(gway.df)
  }


  ###take out entries commanded to delete
  locations.df <- filter(locations.df, namelook!="SKIP")
  locations.df[grep('^ID_', names(locations.df))] <- lapply(locations.df[grep('^ID_', names(locations.df))], as.character)
  dfname2 <- unique(dfname[,c("namelook", "namelookorig")])
  dfname2[grep('^ID_', names(dfname2))] <- lapply(dfname2[grep('^ID_', names(dfname2))], as.character)
  locations.df <- suppressMessages(left_join(locations.df, dfname2[,c("namelook", "namelookorig")]))

  ###little format housekeeping
  locations.df$lat <- as.numeric(as.character(locations.df[,"lat"]))
  locations.df$lon <- as.numeric(as.character(locations.df[,"lon"]))
  locations.df$PN <- gsub('[0-9]','', as.character(locations.df[,"PlacenameGeocoded"]))
  locations.df$PN <- gsub(' ,',',', as.character(locations.df[,"PN"]))
  locations.df$country <- sub(".*,\\s*([^,]+)$", "\\1", locations.df$PN)

  ###function to get a common country name from country entered
  locations.df$CountryName <- standardizeCountry(locations.df$country, fuzzyDist = 25)

  ###function to get iso3 character code from common country name
  locations.df$iso3c <- suppressWarnings(invisible(countrycode(locations.df[,"CountryName"], 'country.name', 'iso3c')))
  locations.df[grep('iso3c', names(locations.df))] <- lapply(locations.df[grep('iso3c', names(locations.df))], as.character)

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
      glist[[1]] <- cbind(justid, df.loc[,c("namelook", "PlacenameGeocoded", "lat", "lon",
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
          ### dont think need this: over <- suppressMessages(left_join(over, df.loc[,c(paste0("ID_",rgadmlevel),"namelook")]))
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
  colnames(MASTERstat)[colnames(MASTERstat)=="sp_namelook"] <- "namelook"
  MASTERstat <<- MASTERstat
  MASTERout <<- suppressMessages(left_join(dfname, MASTERstat))
  writeLines(c("",green("Written to Global Environment:"),
                "MASTERgeo is your list of geometries. Use it for extraction functions.",
               "MASTERout is your dataset bound with newly attached geocoding.",
               "MASTERstat is your dataset of unique locations in the dataset, unbound."))


}











