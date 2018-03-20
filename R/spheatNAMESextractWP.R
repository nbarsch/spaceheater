#' Extract WorldPop Values for Regions In A Dataset
#'
#' This function does three main tasks in order:
#' 1. Geocodes and assesses the GADM level of every entry in your dataset (via Google and GADM.org)
#' 2. Downloads the desired WorldPop UK raster dataset based on inputs
#' and extracts values for every geocoded entry in dataset
#' 3. Combine extracted stats back onto original dataframe
#' and delete (if desired) all GADM and WorldPop sets from disk (to hold one country of sets on disk at a time)
#'
#'This function has the POTENTIAL TO BE VERY TIME AND COMPUTATION INTENSIVE. Suppose you
#'have 100 countries in your dataset, this function will download 100 gadm files and as many worldpop sets
#'that exist for those 100 countries AND analyze all of them. I have used the most up to date loading and extraction methods available
#'(sf and velox extractions) to speed this up as much as possible. An estimated overall progress is periodically given.
#'A future update will include parallelization capacity.
#'
#' At this time, only one WorldPop Raster at a time can be analyzed (with only one set of options).
#' In a future update, this function will be updated to handle multiple rasters (i.e. multiple options)
#'
#' As a bonus and to make matching WorldPop sets easier, you don't have to be terribly precise with your
#' place names. The function includes correction algorithms before searching through google maps (which is quite flexible).
#' Furthermore you can mix place names without issue (i.e. one cell be "1313 Disneyland Dr., CA, USA" and another be "Nigeria" without
#' problems other than your level comparisons being inconsistent on extraction values)
#'
#' See getWPdatatypes() and getWPoptions() for possible inputs for options and years. If missing from inputs, the program
#' takes the first file available based on the other inputs.
#'
#'
#'
#'
#' @param dataset (character), the name of the data frame containing a column
#' of place names. e.g. \code{“mydataframe”}
#' @param colname (character), the name of the column in the data frame
#' containing place names e.g. \code{“mycolname”}
#' @param datatype (character), the datatype you would like to see what options
#' are available for WorldPop UK data. The available datatypes can be attained
#' with the function getWPdatatypes() Example datatypes: \code{“Population”} or
#' \code{“Stunting”}
#' @param gadmlevel (numeric or character), the gadm level you wish to extract data for.
#' You can type an ingteger level or "lowest" for the lowest level among each country in your dataset,
#' or "lcl" for lowest common level among each country. Use fill=TRUE to extract more than one level.
#' Default is lowest common level (gadmlevel="lcl")
#' @param levellimit (numeric), only usable if fill=TRUE, the gadm level you wish to fill data to.
#' Default is levellimit=0 for the country limitlevel set to country gadm files.
#' @param options (character), the options of the tif dataset you would like to
#' download. The available options can be attained with the function
#' getWPoptions() e.g. \code{“ppp”} or \code{c(“F”, "interdecile")}
#' If options is not entered, default is the options of the first file of the country and datatype availabile at WorldPop UK.
#' @param year (numeric), the year of the dataset you would like to download.
#' Available years are given by the getWPoptions() function.
#' If year is not entered, default is the year of the first file of the country and datatype availabile at WorldPop UK.
#' @param googleapikey (character), a valid Google Maps API key. See
#' https://developers.google.com/maps/documentation/javascript/get-api-key to
#' attain one.
#' @param orideGADM (logical), TRUE or FALSE that the GADM file downloads should be overriden.
#' Use if you already have all the countries GADM files in your working directory.
#' The program auto-looks for already downloaded sets, so don't delete files from working directory if you only have some countries.
#' Default=FALSE.
#' @param orideWP (logical), TRUE or FALSE that the WorldPop file downloads should be overriden.
#' Use if you already have all the countries WorldPop files in your working directory.
#' The program auto-looks for already downloaded sets, so don't delete files from working directory if you only have some countries.
#' Default=FALSE.
#' @param deleteGADM (logical), TRUE or FALSE that the GADM file downloads should be deleted after analyzing is complete.
#' Completely deletes the GADM files from your computer.
#' Pro: Save disk space and running many countries, this function deletes as each file is analyzed in a sequence,
#' thus no more than one country is downloaded at once.
#' Con: To do another analysis the files will have to be downloaded again.
#' Default is deleteGADM=TRUE
#' @param deleteWP (logical), TRUE or FALSE that the WorldPop file downloads should be deleted after analyzing is complete.
#' Completely deletes the WorldPop files from your computer.
#' Pro: Save disk space and running many countries, this function deletes as each file is analyzed in a sequence,
#' thus no more than one country is downloaded at once.
#' Con: To do another analysis the files will have to be downloaded again.
#' Default is deleteWP=TRUE
#' @param skipMissing (logical), TRUE or FALSE that locations that cannot be found and geocoded by google should be ignored.
#' When FALSE, the program prompts for each not found entry. When TRUE, the program automatically skips. If not running interactively,
#' skipMissing or deleteMissing must be TRUE. Default is skipMissing=FALSE.
#' @param skipMissing (logical), TRUE or FALSE that locations that cannot be found and geocoded by google will be ignored and marked for deletion.
#' When FALSE, the program prompts for each not found entry. When TRUE, the program automatically skips and marks entries not found for deletion.
#' If not running interactively, skipMissing or deleteMissing must be TRUE. Default is skipMissing=FALSE.
#'
#'@return A \code{data.frame} named MASTER_NamesExt in your global environment with your original dataset bound with data
#' extracted from WorldPop with the specified inputs. MASTER_NamesExt will include all entries from original dataset,
#' not just ones where a GADM and WorldPop layer was found. Also comes with area in M^2 and KM^2 of the extraction polygon
#' for ease of population density and other density calculations.
#'
#' @author Neal Thomas Barsch
#' @references WorldPop UK data are attained through the WorldPop UK website.
#' These data are licensensed under the Creative Commons Attribution 4.0
#' License. \url{http://www.worldpop.org.uk/}.
#' GADM DATA are attained through the GADM project website.
#' Commercial use of this function is not allowed without prior permission from
#' GADM.org. \url{http://gadm.org/}.
#'
#' @examples
#'###FULL EXAMPLES###
#'###Creating Data###
#' test.vec <- as.data.frame(c(rep("Stade Charles De Gaulle, RN11, Porto-Novo, Benin", 20), rep("Cotonou, Benin", 20),
#'                             rep("ethiopia", 20), rep("University of Denver", 20),
#'                             rep("1313 Disneyland Dr, Anaheim, CA 92802", 20)))
#' colnames(test.vec)<- "namelook"
#'
#' apikey <- "My google api key here"
#'
#'
#'###Many of the arguments below are defaults, but including them for show
#'###Completing the function
#' spheatNAMESextractWP(dataset="test.vec", colname="namelook", datatype="Births",
#'                      options="pp", year=2015, googleapikey=apikey, fill=TRUE, orideGADM=FALSE, orideWP=FALSE,
#'                      deleteGADM=TRUE, deleteWP=TRUE, skipMissing=FALSE, deleteMissing=FALSE)
#'
#'
#' @export spheatNAMESextractWP



spheatNAMESextractWP <- function (dataset, colname="PN", datatype="Births",
                              gadmlevel="lcl", levellimit=0, options, year,
                              fill=TRUE, googleapikey, orideGADM=FALSE,
                              orideWP=FALSE, deleteGADM=TRUE, deleteWP=TRUE,
                              skipMissing=FALSE, deleteMissing=FALSE)
{



###Dig out the variables if they exist in the global environment
###Thought is to make a  future interactive to set the variables in the global
  if ("dataset" %in% ls(envir = .GlobalEnv) && missing(dataset)) {
    dataset <- get("dataset", envir = .GlobalEnv)
  }
  if ("colname" %in% ls(envir = .GlobalEnv) && missing(colname)) {
    colname <- get("colname", envir = .GlobalEnv)
  }
  if ("datatype" %in% ls(envir = .GlobalEnv) && missing(datatype)) {
    method <- get("datatype", envir = .GlobalEnv)
  }
  if ("gadmlevel" %in% ls(envir = .GlobalEnv) && missing(gadmlevel)) {
    gadmlevel <- get("gadmlevel", envir = .GlobalEnv)
  }
  if ("options" %in% ls(envir = .GlobalEnv) && missing(options)) {
    options <- get("options", envir = .GlobalEnv)
  }
  if ("year" %in% ls(envir = .GlobalEnv) && missing(year)) {
    year <- get("year", envir = .GlobalEnv)
  }
  if ("fill" %in% ls(envir = .GlobalEnv) && missing(fill)) {
    fill <- get("fill", envir = .GlobalEnv)
  }
  if ("googleapikey" %in% ls(envir = .GlobalEnv) && missing(googleapikey)) {
    googleapikey <- get("googleapikey", envir = .GlobalEnv)
  }
  if ("orideGADM" %in% ls(envir = .GlobalEnv) && missing(orideGADM)) {
   orideGADM <- get("orideGADM", envir = .GlobalEnv)
  }
  if ("orideWP" %in% ls(envir = .GlobalEnv) && missing(orideWP)) {
    orideWP <- get("orideWP", envir = .GlobalEnv)
  }
  if ("deleteGADM" %in% ls(envir = .GlobalEnv) && missing(deleteGADM)) {
    deleteGADM<- get("deleteGADM", envir = .GlobalEnv)
  }
  if ("deleteWP" %in% ls(envir = .GlobalEnv) && missing(deleteWP)) {
    deleteWP<- get("deleteWP", envir = .GlobalEnv)
  }






###Set METHOD for WP datatype
  if(datatype %in% c("Births", "Population", "Pregnancies", "AgeStructures", "SUM")){
    method <- "SUM"
  }else{
    method <- "MEAN"
  }

###Load lowest geographical level GADM available data frame from rdata
  load("gadm_levels28.RData")
  gadm28 <- get("gadm_levels2")
  gadm_levels2 <- gadm_levels2[,c("iso3c", "level")]
  gadm_levels2[grep('iso3c', names(gadm_levels2))] <- lapply(gadm_levels2[grep('iso3c', names(gadm_levels2))], as.character)

  replacebug <-"n"



  pb = txtProgressBar(min = 0, max = 100, initial = 0, style=3)


###Get dataset to merge on and column to look up
  dfname <- get(dataset)
  dfname$namelookorig <- dfname[,paste0(colname)]
  dfname$namelook <- tolower(dfname$namelookorig)
  dfname$namelook <- gsub("^ *|(?<= ) | *$", "", dfname$namelook, perl = TRUE)
  dfname$namelook <- gsub(", ", ",", dfname$namelook)
  dfname$namelookmerge <- dfname$namelook

###Minor housekeeping with the names before fed to google
  lookcol <- dfname[,"namelook"]
  lookcol <- as.data.frame(lookcol)
  colnames(lookcol) <- "lookcol"
  lookcol$lookcol <- as.character(lookcol[,"lookcol"])
  lookcol <- lookcol[!(is.na(lookcol[,"lookcol"]) | lookcol[,"lookcol"]==""),]

###Taking only unique text lookups and merging later will speed sets with dup locations
  lookVector <- unique(lookcol)
  iters.look <- length(lookVector)

#Replace progress bar

  print(paste("...Geocoding your locations in dataset, please wait..."))

###Making defaults of deleting all and skipping all FALSE
  axeem <- FALSE
  skipall <- FALSE

  if ("skipall" %in% ls(envir = .GlobalEnv) && missing(skipall)) {
    skipall<- get("skipall", envir = .GlobalEnv)
  }
  if(isTRUE(skipMissing)){skipall <- TRUE}

  if ("axeem" %in% ls(envir = .GlobalEnv) && missing(axeem)) {
    axeem <- get("axeem", envir = .GlobalEnv)
  }
  if(isTRUE(deleteMissing)){axeem <- TRUE}


###Looking up and geocoding locations (below)


  locations.df <- foreach(a=1:iters.look, .combine=rbind) %do% {

  ###There is an odd bug where if you look up New York, New York, it thinks you mean the hotel in Las Vegas
  ###Below fixes that small bug (by switching the lookup location to Empire State Building)
    if(lookVector[a]=="new york,new york"){
      replacebug <- lookVector[a]
      lookVector[a] <- "empire state building"
    }
    gwayerror <- FALSE

  ###geocode text locations w googleway
    gway.df <- tryCatch(google_geocode(paste(lookVector[a]),key=googleapikey),
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
        if(!(isTRUE(skipall)) & !(isTRUE(axeem))){
          writeLines(c( "",
                        "",
                        paste0("Could not find entry: ", red(lookVector[a])),
                        paste0("Due to ", red(nores )),
                        "",
                        underline(blue("OPTIONS:")),
                        paste0("RETRY: " ,blue("Hit enter")),
                        paste0("TYPE CORRECTED PLACE NAME: ", blue("Type the corrected place name for: "),red(lookVector[a])),
                        paste0("SKIP ENTRY: ", blue("Type 's or skip'")),
                        paste0("SKIP ALL NOT FOUND ENTRIES: ", blue("Type 'sa or skip all'")),
                        paste0("DELETE ENTRY: ", blue("Type 'd or delete'")),
                        paste0("DELETE ALL NOT FOUND ENTRIES: ", blue("Type 'da or delete all'"))
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
          if(uinput.newname=="d"){uinput.newname <- "delete"}
          if(uinput.newname=="del"){uinput.newname <- "delete"}
          if(uinput.newname=="da"){uinput.newname <- "delete all"}
          if(uinput.newname=="d all"){uinput.newname <- "delete all"}
          if(uinput.newname=="del all"){uinput.newname <- "delete all"}
          if(uinput.newname=="d a"){uinput.newname <- "delete all"}
          if(uinput.newname=="del a"){uinput.newname <- "delete all"}
        }

  ###axeem and skipall back in play
        if(isTRUE(axeem)){uinput.newname <- "axeem"}
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

  ###all processes that DELETE
        if(uinput.newname=="delete" | uinput.newname=="delete all" | uinput.newname=="axeem"){
          if(uinput.newname != "axeem"){
            writeLines(c(paste0("Deleting entries will not affect your original dataset: ", dataset),
                            "However, new cells created in dataset will be NA for deleted entry(ies)"))
            delconfirm <- readline(prompt="HIT ENTER to confirm delete (type any entry to retry): ")
          }
          if(isTRUE(axeem)){delconfirm <- ""}
          if(delconfirm==""){
            if(uinput.newname=="delete all") {axeem <- TRUE}
            dfname <- as.data.frame(dfname)
            coltemp <- (which(dfname$namelook==lookVector[a]))
            dfname[c(paste(coltemp), collapse=","),"namelook"] <- "DELETE"
            lookVector[a] <- "DELETE"
            break
          }
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
          dfname <- as.data.frame(dfname)
          coltemp <- (which(dfname$namelook==lookVector[a]))
          dfname[c(paste(coltemp), collapse=","),"namelook"] <- uinput.newname
          lookVector[a] <- uinput.newname
          break
        }
        j <- j+1
      }
    }
    ###If not skip or delete this entry
    if(!(lookVector[a]=="DELETE") & !(is.na(lookVector[a]))){
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

  ###If delete this entry enter delete into frame for later deletion as to not screw up cbinds
    if(lookVector[a]=="DELETE"){
      gway.df <- data.frame(
        lat=as.factor("DELETE"),
        lon=as.factor("DELETE"),
        PlacenameGeocoded=as.factor("DELETE"),
        namelook=as.character("DELETE")
      )
    }
  ###If skip this entry enter NA for merge
    if(is.na(lookVector[a])){
      gway.df <- data.frame(
        lat=as.factor(NA),
        lon=as.factor(NA),
        PlacenameGeocoded=as.factor(NA),
        namelook=as.character(NA)
      )
    }
    return(gway.df)
  }


###take out entries commanded to delete
  locations.df <- filter(locations.df, namelook!="DELETE")
  locations.df <- filter(locations.df, namelook!="SKIP")
  locations.df[grep('^ID_', names(locations.df))] <- lapply(locations.df[grep('^ID_', names(locations.df))], as.character)


###little format housekeeping
  locations.df$lat <- as.numeric(as.character(locations.df[,"lat"]))
  locations.df$lon <- as.numeric(as.character(locations.df[,"lon"]))
  locations.df$PN <- gsub('[0-9]','', as.character(locations.df[,"PlacenameGeocoded"]))
  locations.df$PN <- gsub(' ,',',', as.character(locations.df[,"PN"]))
  locations.df$country <- sub(".*,\\s*([^,]+)$", "\\1", locations.df$PN)

###function to get a common country name from country entered
  locations.df$CountryName <- standardizeCountry(locations.df$country, fuzzyDist = 25)

###function to get iso3 character code from common country name
  locations.df$iso3c <- invisible(countrycode(locations.df[,"CountryName"], 'country.name', 'iso3c'))
  locations.df[grep('iso3c', names(locations.df))] <- lapply(locations.df[grep('iso3c', names(locations.df))], as.character)

###subset list of countries to work with
  uni.loc<- locations.df[!duplicated(locations.df$iso3c),]
  uni.loc<- uni.loc[!is.na(uni.loc$iso3c),]

###Get the lowest gadm level available for each country, from loaded dataframe
  uni.loc <- suppressMessages(left_join(uni.loc, gadm_levels2[, c("iso3c", "level")]))

  n.uniloc <- nrow(uni.loc)



  ##prep for coountry repeat
  a <-1
  clist <- list()
  gadmorig <- gadmlevel




###For loop for each country in dataset
  for(i in uni.loc[,"iso3c"]){

    if (missing(options)) {
      sink(file="/dev/null")
      getWPoptions(paste0(i), paste0(datatype))
      sink()
      WP.options <- get("WP.options", env= .GlobalEnv)
      justoptioncodes <- WP.options[ ,  grepl( "OptionCode" , colnames( WP.options ) ) ]

      ####Future modify here for multiple rasters at once
      justoptioncodes <- justoptioncodes[1,]
      justoptioncodes[grep('OptionCode', names(justoptioncodes))] <- lapply(justoptioncodes[grep('OptionCode', names(justoptioncodes))], as.character)
      justoptioncodes <- as.character(justoptioncodes)
      options <- justoptioncodes
    }

    if (missing(year)) {
      sink(file="/dev/null")
      getWPoptions(paste0(i), paste0(datatype))
      sink()
      WP.options <- get("WP.options", env= .GlobalEnv)
      justyears <- WP.options[ ,  "years" ]
      justyears <- justyears[1]

    }



###Defaults of errors on WorldPop and GADM to FALSE
    noWP <- FALSE
    noGADM <- FALSE
    gadmlevel <- gadmorig
    dWP <- FALSE

    dGADM <- file.exists(paste0("gadm",i))
###Download and unzip GADM if exists
    if(!isTRUE(orideGADM) & !isTRUE(dGADM)){
      writeLines(c("",paste0("Currently DOWNLOADING the GADM file for: ", uni.loc[a,"CountryName"] )))
      gadmresp <- tryCatch(curl_download(paste0("http://biogeo.ucdavis.edu/data/gadm2.8/shp/", i,"_adm_shp.zip"),
                    destfile=paste0("gadm",i,".zip"),
                    quiet = TRUE),
                    error=function(e){noGADM <- TRUE})
      if(!isTRUE(gadmresp)){gadmresp <- FALSE}
      if(!isTRUE(noGADM) & !isTRUE(gadmresp)){unzip(paste0("gadm",i,".zip"), exdir=paste0("gadm",i), overwrite=TRUE)}
    }

###Get the lowest GADM layer to overlay. Lowest layers come with higher division codes as well
    dGADM <- file.exists(paste0("gadm",i))
    if(isTRUE(dGADM)){
      writeLines(c("",paste0("SHAPEFILE SUCCESS!: ", uni.loc[a,"CountryName"] )))

      listfiles <-list.files(paste0("gadm",i,"/"))

###Sometimes license.txt gets in the wrong place
      listfiles <- gsub("license.txt","aaa.txt", listfiles)
      listfiles <- sort(listfiles)
      smallest.file <- substr(listfiles[length(listfiles)],8,8)


###Load lowest geographical level shapefile
      sptemp <- sf::st_read(dsn=paste0("gadm",i), layer=paste0(i,"_adm",smallest.file), quiet=TRUE)

###Over SF methods (speeding everything up from SP!) for determining dataset locations administrative levels
      sp.locations <- locations.df[ which(locations.df$iso3c==i ), ]
      coordinates(sp.locations) <- ~lon+lat
      proj4string(sp.locations)=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
      sf.locations <- st_as_sf(sp.locations)
      over <- suppressMessages(suppressWarnings(st_intersection(sf.locations,sptemp)))


      sp.locations <- as.data.frame(sp.locations)


###New df.loc starts here (for testing)
      df.loc<- suppressMessages(suppressWarnings(left_join(sp.locations, over)))

      df.loc[grep('^ID_', names(df.loc))] <- lapply(df.loc[grep('^ID_', names(df.loc))], as.character)
      df.loc[grep('iso3c', names(df.loc))] <- lapply(df.loc[grep('iso3c', names(df.loc))], as.character)
      df.loc <- suppressMessages(left_join(df.loc, gadm_levels2))






###Housekeeping
      if(is.character(gadmlevel)){
        gadmlevel <- tolower(gadmlevel)
        if(gadmlevel=="village"){gadmlevel <-"lowest" }
        if(gadmlevel=="state"){gadmlevel <-"1" }
        if(gadmlevel=="province"){gadmlevel <-"1" }
        if(gadmlevel=="low"){gadmlevel <-"lowest" }
        if(gadmlevel=="l"){gadmlevel <-"lowest" }
        if(gadmlevel=="h"){gadmlevel <-"highest" }
        if(gadmlevel=="high"){gadmlevel <-"highest" }
        if(gadmlevel=="country"){gadmlevel <-"highest" }
        if(gadmlevel=="highest"){gadmlevel <- "0"}
        if(gadmlevel=="lowestcommon"){gadmlevel <- "lcl"}
        if(gadmlevel=="lowest common"){gadmlevel <- "lcl"}
        if(gadmlevel=="lc"){gadmlevel <- "lcl"}
        if(gadmlevel=="common"){gadmlevel <- "lcl"}
        if(gadmlevel=="lowest") {
          gadm28 <- filter(gadm28, iso3c==ctry )
          gadmlevel <- as.character(gadm28[1,"level"])
        }
      }
      if(is.integer(gadmlevel)){lcl <- as.character(gadmlevel)}
      if(gadmlevel=="lcl") {
        lcl <- min(uni.loc$level)
      }else{
        if(gadmlevel=="lowest"){
          lcl <- uni.loc[a,"level"]
        }else{
          if(gadmlevel=="highest"){
          lcl <- "0"
          }else{
            lcl <- as.character(gadmlevel)
          }
        }
      }
      lcl <- as.character(lcl)
      gadmlevel <- lcl


      dWP <- file.exists(paste0(uni.loc[a,"CountryName"],"_", datatype,"_", paste(options, collapse="_"),"_",year, ".tif"))


      if(!isTRUE(orideWP) & !isTRUE(dWP)){
###Download WorldPop Raster
        writeLines(c("", paste0("Checking WorldPop file exists for ",
                                uni.loc[a,"CountryName"], " , ",
                                datatype, " , ",
                                year
        )
        )
        )
        writeLines(c("", paste0("GETTING WORLDPOP for ",
                                uni.loc[a,"CountryName"], " , ",
                                datatype, " , ",
                                year
        )
        )
        )
        failwp <-  tryCatch(getWPdownload(country=uni.loc[a,"CountryName"], datatype=datatype, options=options, year=year),
                            error=function(ee){ noWP<- TRUE })
      }
####Skip for if that country's raster does not exist

      dWP<- file.exists(paste0(uni.loc[a,"CountryName"],"_", datatype,"_", paste(options, collapse="_"),"_",year, ".tif"))


###Don't include an isTRUE(noGADM) here because it is taken care of above. noGADM=TRUE can't get here



###Extract
      tempframe <- extlevel(spdataframe=df.loc, country=as.character(df.loc[1,"iso3c"]),
                            gadmlevel=lcl ,method=method,datatype=datatype, options=options, year=year
                            )


      }
###dGADM as FALSE back in play as possibility
###Skip if GADM file not found for country
      if(!isTRUE(dGADM)){
        #removed nolocations <- filter(locations.df, iso3c==uni.loc[a,"iso3c"])
        #removed n.noloc<-nrow(nolocations)
        tempframe <- data.frame(
          ID_0=df.loc[1,"ID_0"],
          ext = NA
          )
        colnames(tempframe)[colnames(tempframe)=='ext'] <- paste0(method,"_", datatype,"_", options, "_", year, "_",lcl)
        tempframe[grep('^ID_', names(tempframe))] <- lapply(tempframe[grep('^ID_', names(tempframe))], as.character)
        tempframe[grep('ext', names(tempframe))] <- lapply(tempframe[grep('ext', names(tempframe))], as.integer)


###test only delete later
      testlist5[[a]] <- "NOGADM"

      }




    if(isTRUE(dGADM)){
      if(isTRUE(fill)){



###Fill all higer geographic divisions for each country raster extract, with default fill=TRUE

        df.loc$level <- as.integer(lcl)
        destroyer <- df.loc
        tlist <- list()
        b <- 1
        repeat{
###Drop a level to extract until 0 last extraction, repeat until no more
          destroyer$level <- destroyer$level -1
          destroyer <- filter(destroyer, level> -1 )

          if(nrow(destroyer)==0){break}
          elev <- max(destroyer$level)
          if(elev==levellimit-1){break}
          destroyer <- destroyer[!duplicated(destroyer[,paste0("ID_",elev)]),]
          tlist[[b]] <- extlevel(spdataframe=destroyer, country=as.character(destroyer[1,"iso3c"]),
                              gadmlevel=destroyer[1,"level"],method=method,datatype=datatype, options=options, year=year
                              )
          b <- b+1
        }
###Compile all the layers together
        justid <- df.loc[ ,  grepl( "ID_" , colnames( df.loc ) ) ]
        tlist[[1]] <- suppressMessages(left_join(tlist[[1]], justid ))
        tempframe <- suppressMessages(left_join(tempframe, justid))
        tlist[[1]] <- suppressMessages(left_join(tlist[[1]], tempframe))
        n.tlist <- length(tlist)
        basemerge <- suppressMessages(left_join(tlist[[1]], tlist[[2]]))
        if(n.tlist>2){
          foreach(r=3:n.tlist)%do%{
            basemerge <- suppressMessages(left_join(basemerge, tlist[[r]]))
          }
        }


        ###final basemerge
        basemerge <- basemerge[, order(names(basemerge))]




      }else{
        justid <- df.loc[ ,  grepl( "ID_" , colnames( df.loc ) ) ]
        justid[grep('^ID_', names(justid))] <- lapply(justid[grep('^ID_', names(justid))], as.character)
        tlist[[1]] <- suppressMessages(left_join(tlist[[1]], justid ))
        tempframe <- suppressMessages(left_join(tempframe, justid))

      ###final basemerge
        basemerge <- tempframe[,order(names(tempframe))]

      }
      localize <- suppressMessages(left_join(df.loc, basemerge))
    }else{localize <-suppressMessages(left_join(df.loc, tempframe)) }


##Compile
    clist[[a]] <- localize


    dGADM <- file.exists(paste0("gadm",i))

###Delete GADM shapefile in disk space (wd) if default deleteGADM=TRUE. Used for all extraction already
    if(isTRUE(deleteGADM) & isTRUE(dGADM)){
      file.remove(paste0("gadm",i,".zip"))
      unlink(paste0("gadm",i, "/*"))
      file.remove(paste0("gadm",i,"/"))
    }
###Delete WorldPop Raster in disk space (wd) if default deleteGADM=TRUE. Used for all extraction already
    if(isTRUE(deleteWP) & isTRUE(dWP)){
      cc <- invisible(countrycode(i,origin="iso3c", destination="country.name" ))
      cc <- standardizeCountry(cc, fuzzyDist = 25)
      file.remove(paste0(cc,"_", datatype,"_", paste(options, collapse="_"),"_",year,".tif"))
    }

    ###Progress bar stuff
    if(a==1){setpb <- 0}
    if(levellimit==0){
      setpb <- setpb + 95/n.uniloc
      writeLines(c("",
                   bold("GUESSTIMATED OVERALL PROGRESS"),
                    "(estimated due to varied file sizes by country):")
                 )
      if(a==nrow(uni.loc)){setpb <- 100}
      setTxtProgressBar(pb, setpb)
    }else{
      writeLines(c("",
                   bold("GUESSTIMATED OVERALL PROGRESS"),
                   "(estimated due to varied file sizes by country):")
      )
      setpb <- setpb + 80/n.uniloc
      if(a==nrow(uni.loc)){setpb <- 100}
      setTxtProgressBar(pb, setpb)
    }
    a <- a+1
  }

###COUNTRY LOOP ENDS HERE

###Merge all the columns together, suppressing warnings for the factors vs character column that didn't get formatted somewhere
  masterloc <- suppressWarnings(do.call("bind_rows",clist))
  colnames(masterloc)[colnames(masterloc)=='level'] <- "max_GADMlevel"


  masterout <- suppressMessages(left_join(dfname, masterloc))
  MasterSPheat <<- masterout
}

