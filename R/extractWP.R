#'
#' Extract WorldPop UK Raster Sets for all locations from a spheated dataset.
#'
#' You must have run an spheat function (spheatNAMES, spheatGPS, or spheatLOOKUP)
#' to exercise this function.
#'
#' @param datatype (character), the datatype you would like to see what options
#' are available for WorldPop UK data. The available datatypes can be attained
#' with the function getWPdatatypes() Example datatypes: \code{“Population”} or
#' \code{“Stunting”}
#' @param options (character), the options of the tif dataset you would like to
#' download. The available options can be attained with the function
#' getWPoptions() e.g. \code{“ppp”} or \code{c(“F”, "interdecile")}
#' @param year (numeric), the year of the dataset you would like to download.
#' Available years are given by the getWPoptions() function for any given country.
#' @param gadmlevel (integer or character), either an integer level gadm level to extract for all countries in MASTERout, or one of
#' \code{gadmlevel="lowest"} for the lowest respective gadm level available
#' or \code{gadmlevel="lcl"} for lowest COMMON level among all countries in MASTERout.
#' Default is \code{gadmlevel="lowest"}
#' @param fill (logical), if extraction values for all higher gadm geographic level values should also be returned.
#' I.e. if gadmlevel=3 and fill=TRUE, extractWP will return the extraction values for gadm levels 0, 1, 2, and 3.
#' If fill=FALSE in this case, extractWP will only return extraction values for gadm level 3.
#' See gadm.org for details about levels (in short level 0 is always country, 1 is state/province/equivalent,
#' 2 is county/municipality/equivalent, 3 is village/equivalent, 4 and more get into blocks and other lower geographic levels).
#' Default is \code{fill=TRUE}
#' @param deleteRAST (logical), logical if the downloaded WorldPop UK Raster sets should be deleted when they are done being analyzed.
#' Reccomended TRUE for datasets containing many countries as TRUE deletes the done analyzed raster
#' before a new one is downloaded, minimizing disk space use.
#' Default is \code{deleteRAST=TRUE}

#'
#' @author Neal Thomas Barsch
#' @references GADM DATA are attained through the GADM project website.
#' Commercial use of this function is not allowed without prior permission from
#' GADM.org. \url{http://gadm.org/}.
#'
#'
#' @references WorldPop UK data are attained through the WorldPop UK website.
#' These data are licensensed under the Creative Commons Attribution 4.0
#' License. \url{http://www.worldpop.org.uk/}.
#'
#' Eternal grattitude to StackOverflow member hrbrmstr who contributed crucially
#' to the code for the downloads from WorldPop.
#'
#'
#' @examples
#'
#' extractWP(datatype="Births", options="pp", year=2015, gadmlevel="lowest", fill=TRUE, deleteRAST=TRUE)
#'
#' extractWP(datatype="Population", options=c("ppp", "adj"), year=2010, gadmlevel="lowest", fill=TRUE, deleteRAST=TRUE)
#'
#' #You dont want to delete the Raster files (warning, could take a lot of storage space if not deleted)
#' extractWP(datatype="Births", options="pp", year=2015,deleteRAST=FALSE)
#'
#'
#' @export extractWP


extractWP <- function(datatype, options, year, gadmlevel="lowest", fill=TRUE,
                     deleteRAST=TRUE, outdata=MASTERout, geolist=MASTERgeo)
{

  ###Objective here is to take extlevel and make it uniform for other raster layers
  ###AND FILL IN ALL COUNTRIES NOT JUST ONE


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

    if(gadmlevel=="lcl"){
      uni.loc <- as.data.frame(unique(outdata[,"sp_iso3c"]))
      colnames(uni.loc)<- "iso3c"
      uni.loc <- suppressWarnings(left_join(uni.loc, gadm_levels2))
      gadmlevel <- min(uni.loc$level)
    }

  }





    ###Set METHOD for WP datatype
  if(datatype %in% c("Births", "Population", "Pregnancies", "AgeStructures", "SUM")){
    method <- "SUM"
  }else{
    method <- "MEAN"
  }

  n.geolist <- length(geolist)

  fmax <- function(dat){
    nm1 <- grep("ID", names(dat), value = TRUE)
    stopifnot(length(nm1) > 0)
    max(as.numeric(gsub("\\D+", "", nm1)))
  }

  if(isTRUE(fill)){n.geolist <- length(geolist)
  }else{n.geolist <-1}
  x2 <- list()
  foreach(v=1:length(geolist))%do%{
    n.lgg <- length(geolist[[v]])
    CountryName <- as.character(geolist[[v]][[n.lgg]][1,"CountryName"])
    loggyras <- FALSE
    ###download raster here
    tempexists <- file.exists(paste0(CountryName,"_", datatype,"_", paste(options, collapse="_"),"_",year,".tif"))
    if(!isTRUE(tempexists)){
      ds.wp <-  tryCatch(getWPdownload(country=CountryName, datatype=datatype, options=options, year=year),
                         error=function(ee){ tempexists<- FALSE })
    }
    tempexists <- file.exists(paste0(CountryName,"_", datatype,"_", paste(options, collapse="_"),"_",year,".tif"))
    if(isTRUE(tempexists)){
      rastemp <- tryCatch(raster(paste0(CountryName,"_", datatype,"_", paste(options, collapse="_"),"_",year,".tif")),
                          error=function(err){loggyras <- TRUE})
      n.ras <- length(rastemp)
      vrastemp <- velox(rastemp)
    }else{loggyras<-TRUE}
    if(!isTRUE(loggyras)){




      nv.geosub <- length(geolist[[v]])-1
      if(is.character(gadmlevel)){
        gadmlevel <- nv.geosub
      }
      skip <- 0
      if(gadmlevel!=nv.geosub){
        skip <- (nv.geosub-gadmlevel)
        start <- skip+1
        glist <- list()
        each <-1
        foreach(s=start:length(geolist[[v]]))%do%{
        glist[[each]] <- geolist[[v]][[s]]
        ###geolist[[v]][[s]] <- NULL
        each <- each+1
        }
      }

    ####  if(gadmlevel!=lgeosub){gadmlevel}

      if(isTRUE(fill)){
        nv.geolist <- length(geolist[[v]])
      }else{nv.geolist <-1}
      tx2 <- list()
      rgadmlevel <- gadmlevel
      foreach(q=1:nv.geolist)%do%{
        r <- length(geolist)-rgadmlevel
        tframe <- as.data.frame(geolist[[v]][[r]])
        n.tframe <- nrow(tframe)
        maxID <- fmax(tframe)
        sub.tframe <- tframe[,c(paste0("ID_",maxID), paste0("geometry",maxID))]
        if(method=="SUM"){
          sub.tframe$ext <- as.numeric(vrastemp$extract(sub.tframe[,paste0("geometry",maxID)], fun=function(x){sum(x, na.rm=TRUE)}))
        }
        if(method=="MEAN"){
          sub.tframe$ext <- as.numeric(vrastemp$extract(sub.tframe[,paste0("geometry",maxID)], fun=function(x){mean(x, na.rm=TRUE)}))
        }
        ### maxID also length(geolist[[v]])-q
        sub.tframe <- sub.tframe[, c(1,3)]
        colnames(sub.tframe)[colnames(sub.tframe)=='ext'] <- paste0(method,"_", datatype,"_", paste(options, collapse="_"), "_", year, "_",maxID)
        drops <- c(paste0("geometry",maxID))
        tframe <- tframe[ , !(names(tframe) %in% drops)]
        tframe <- left_join(tframe, sub.tframe)
        tx2[[q]] <- tframe
        rgadmlevel <- rgadmlevel-1
      }
      x2[[v]]<-tx2
      if(isTRUE(deleteRAST)){
        file.remove(paste0(CountryName,"_", datatype,"_", paste(options, collapse="_"),"_",year,".tif"))
      }
    }
  }
  MASTERextract <- list()
  foreach(p=1:length(x2))%do%{
    MASTERextract[[p]] <- suppressMessages(Reduce(left_join, x2[[p]]))
  }
  MASTERextract <- suppressWarnings(do.call("bind_rows", MASTERextract))
  colnames(MASTERextract) <- paste("sp", colnames(MASTERextract), sep="_")
  MASTERextract <<- MASTERextract
  MASTERbackout1 <<- outdata
  MASTERout <<- suppressMessages(left_join(outdata, MASTERext))
  cat("Done. MASTERout updated. MASTERextract created. Not available=NA. ")
}
