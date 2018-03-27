


extlevel <- function(spdataframe, country,
                     gadmlevel, method, datatype,
                     options, year)
{


 ###Space for testing


#################
  if ("country" %in% ls(envir = .GlobalEnv) & missing(country)) {
    country <- get("country", envir = .GlobalEnv)
  }
  if ("gadmlevel" %in% ls(envir = .GlobalEnv) & missing(gadmlevel)) {
    gadmlevel <- get("gadmlevel", envir = .GlobalEnv)
  }
  if ("method" %in% ls(envir = .GlobalEnv) & missing(method)) {
    method <- get("method", envir = .GlobalEnv)
  }
  if ("datatype" %in% ls(envir = .GlobalEnv) & missing(datatype)) {
    datatype <- get("datatype", envir = .GlobalEnv)
  }
  if ("options" %in% ls(envir = .GlobalEnv) & missing(options)) {
    options <- get("options", envir = .GlobalEnv)
  }
  if ("year" %in% ls(envir = .GlobalEnv) & missing(year)) {
    year <- get("year", envir = .GlobalEnv)
  }


###Quick character corrections on gadmlevel
  if(is.character(gadmlevel)){
    gadmlevel <- gsub("^ *|(?<= ) | *$", "", gadmlevel, perl = TRUE)
    gadmlevel <- tolower(gadmlevel)

    ###Specific levels
    if(gadmlevel=="state"){gadmlevel <-"1" }
    if(gadmlevel=="province"){gadmlevel <-"1" }

    ###Highest
    if(gadmlevel=="country"){gadmlevel <-"0" }
    if(gadmlevel=="h"){gadmlevel <-"0" }
    if(gadmlevel=="high"){gadmlevel <-"0" }
    if(gadmlevel=="highest"){gadmlevel <- "0"}

    ###Lowest

    if(gadmlevel=="village"){gadmlevel <-"lowest" }
    if(gadmlevel=="low"){gadmlevel <-"lowest" }
    if(gadmlevel=="l"){gadmlevel <-"lowest" }

    if(gadmlevel=="lowest") {
      gadm28 <- filter(gadm28, iso3c==country )
      gadmlevel <- as.character(gadm28[1,"level"])
    }
  }
  if(is.integer(gadmlevel)){gadmlevel <- as.character(gadmlevel)}

  lcl <- as.character(gadmlevel)

  loggy <- FALSE
  loggyGADM<- FALSE
  shpadown <- FALSE


###Option: if NOT missing country: set country and countryiso
  if(!(missing(country))){

    country <- standardizeCountry(country, fuzzyDist = 25)
    countryiso <- invisible(countrycode(country, "country.name", "iso3c"))
  }


###Option: if NOT missing spdataframe: set master as spdataframe
  if(!(missing(spdataframe))){

    master <- spdataframe

###OPTIONS: If MISSING spdataframe
  }else{

###Option: If MISSING spdataframe and NOT missing country: read country gadm and set as master
    if(!missing(country)){

      tryCatch(shapetemp <- sf::st_read(dsn=paste0("gadm",countryiso),
                                    layer=paste0(countryiso,"_adm",lcl), quiet=TRUE),
                            error=function(err){loggyGADM <-TRUE})
      if(!isTRUE(loggyGADM)){
        master <- as.data.frame(shapetemp)
        shpadown <- TRUE
      }

###Option: If MISSING spdataframe and MISSING country: error message and break
    }else{
      cat("You are missing both a spdataset and/or a country. Fix and retry.")
      break
    }
  }
#testing for delete

###master exists from here, get iso3c column
  mastercolnamesreg <- colnames(master)
  mastercolnames <- gsub("^ *|(?<= ) | *$", "", mastercolnamesreg, perl = TRUE)
  mastercolnames <- tolower(mastercolnames)
  mastercID <- mastercolnames[which(mastercolnames %in% c("iso3c", "iso3n","iso" , "country.name",  "countryname", "country name", "country"))]
  if(length(mastercID)!=1){
    pcnames <- c("iso3c", "iso3n","iso", "country.name", "countryname", "country name", "country" )
    wpcnames <- which(pcnames %in% mastercID)
    take <- pcnames[wpcnames[1]]
  }else{
    take <-mastercID
  }
  if(length(mastercID)==0){
    cat("NO COUNTRY NAME IN DATASET, FIX AND TRY AGAIN.")
    break
  }


  ###Not match here
  if(take=="iso3n"){
    master$iso3c <- invisible(countrycode(master$iso3n, "iso3n", "iso3c"))
    take <- "iso3c"
  }else{
    if(take !="iso3c") {
      n.iso3c <- which(mastercolnames==take)
      master$iso3c <- standardizeCountry(master[,n.iso3c], fuzzyDist = 25)
      master$iso3c <- invisible(countrycode(master$iso3c, "country.name", "iso3c"))
      take <- "iso3c"
    }
  }



###Option: MISSING country and NOT missing spdataframe: set countryiso and country
  if(missing(country) & !missing(spdataframe)){
      countryiso <- master[1,"iso3c"]
      country <- standardizeCountry(countryiso, fuzzyDist = 25)
  }


  master <- as.data.frame(master)

  tempcountry<- filter(master, iso3c==countryiso)
  tempcountryname <- countryiso
  tempcountryname <- standardizeCountry(tempcountryname, fuzzyDist = 5)
  templocid <- as.character(tempcountry[,paste0("ID_",lcl)])
  u.templocid <- unique(templocid)
  nu.templocid <- length(u.templocid)

  ###Get shapefile layer for level
  if(file.exists(paste0("gadm", countryiso))){
    tryCatch(shapetemp <- sf::st_read(dsn=paste0("gadm",countryiso),
                                layer=paste0(countryiso,"_adm",lcl), quiet=TRUE),
                        error=function(err){loggyGADM <-TRUE})
  }else{loggyGADM <- TRUE}
  if(!isTRUE(loggyGADM)){

###Get raster
    if(file.exists(paste0(tempcountryname,"_", datatype,"_", paste(options, collapse="_"),"_",year,".tif"))){
      rastemp <- tryCatch(raster(paste0(tempcountryname,"_", datatype,"_", paste(options, collapse="_"),"_",year,".tif")),
                          error=function(err){loggy <- TRUE})
      n.ras <- length(rastemp)
    }else{loggy<-TRUE}
  }


  if(isTRUE(loggyGADM)){


    df.tempext <- data.frame(ID_0=as.character(tempcountry[1,"ID_0" ]),
                             ext=NA, area_a_msq=NA, area_a_kmsq=NA, stringsAsFactors = FALSE
                            )
    colnames(df.tempext)[colnames(df.tempext)=='ext'] <-  paste0(method,"_", datatype,"_", options, "_", year, "_",lcl)
    colnames(df.tempext)[colnames(df.tempext)=='area_a_msq'] <-  paste0("area_",lcl, "_msq")
    colnames(df.tempext)[colnames(df.tempext)=='area_a_kmsq'] <-  paste0("area_",lcl, "_kmsq")
    return(df.tempext)
  }
  if(isTRUE(loggy) & !isTRUE(loggyGADM)){


    if(as.character(lcl)=="0"){
      sumall <- sum(as.vector(st_area(shapetemp)))
      df.tempext <- data.frame(
        ID_0=as.character(tempcountry[1,"ID_0" ]),
        area_0_msq=sumall,
        area_0_kmsq=sumall/1000000,
        ext=NA,
        stringsAsFactors = FALSE
      )
      df.tempext[grep('^ID_', names(df.tempext))] <- lapply(df.tempext[grep('^ID_', names(df.tempext))], as.character)
      colnames(df.tempext)[colnames(df.tempext)=='ext'] <-  paste0(method,"_", datatype,"_", options, "_", year, "_",lcl)
      return(df.tempext)
    }
    else{
    fil.sftemp <- filter(shapetemp, get(paste0("ID_", lcl)) %in% u.templocid)

    ###Error here because
    fil.sftemp$area_a <- st_area(fil.sftemp)
    fil.sftemp$area_a <- as.numeric(fil.sftemp$area_a)
    tempag <- aggregate(area_a~get(paste0("ID_",lcl)), data=fil.sftemp, FUN=sum)
    colnames(tempag)[1] <-  paste0("ID_",lcl)
    fil.sftemp <- fil.sftemp[,1:(ncol(fil.sftemp)-1)]
    fil.sftemp <- left_join(fil.sftemp, tempag)
    fil.sftemp$area_a_kmsq <- fil.sftemp$area_a/1000000
    colnames(fil.sftemp)[colnames(fil.sftemp)=='area_a'] <-  paste0("area_",lcl, "_msq")
    colnames(fil.sftemp)[colnames(fil.sftemp)=='area_a_kmsq'] <-  paste0("area_",lcl, "_kmsq")
    df.tempext <- fil.sftemp[,c("ID_0", paste0("ID_",lcl),paste0("area_",lcl,"_msq"), paste0("area_",lcl,"_kmsq"))]
    st_geometry(df.tempext) <- NULL
    df.tempext$ext <- NA
    df.tempext[grep('^ID_', names(df.tempext))] <- lapply(df.tempext[grep('^ID_', names(df.tempext))], as.character)

    colnames(df.tempext)[colnames(df.tempext)=='ext'] <-  paste0(method,"_", datatype,"_", options, "_", year, "_",lcl)

    return(df.tempext)
    }
  }
  if(!isTRUE(loggy) & !isTRUE(loggyGADM)){



    if(method=="SUM"){rastemp[is.na(rastemp)]<-0}


    vrastemp <- velox(rastemp)


    if(as.character(lcl)!="0"){


      fil.sftemp <- filter(shapetemp, get(paste0("ID_", lcl)) %in% u.templocid)
      colnames(fil.sftemp)[colnames(fil.sftemp)=='geometry'] <- paste0("geometry",lcl)
      fil.sftemp$area_a <- st_area(fil.sftemp)
      fil.sftemp$area_a <- as.numeric(fil.sftemp$area_a)
      tempag <- aggregate(area_a~get(paste0("ID_",lcl)), data=fil.sftemp, FUN=sum)
      colnames(tempag)[1] <-  paste0("ID_",lcl)
      fil.sftemp <- fil.sftemp[,1:(ncol(fil.sftemp)-1)]
      fil.sftemp <- left_join(fil.sftemp, tempag)
      fil.sftemp$area_a_kmsq <- fil.sftemp$area_a/1000000
      colnames(fil.sftemp)[colnames(fil.sftemp)=='area_a'] <-  paste0("area_",lcl, "_msq")
      colnames(fil.sftemp)[colnames(fil.sftemp)=='area_a_kmsq'] <-  paste0("area_",lcl, "_kmsq")
      df.tempext <- fil.sftemp[,c("ID_0", paste0("ID_",lcl),paste0("area_",lcl,"_msq"), paste0("area_",lcl,"_kmsq"))]


    ###DOnt think need this, delete  colnames(df.tempext)[colnames(df.tempext)=='ID_a'] <- paste0("ID_", lcl)
    }
    if(as.character(lcl)=="0"){

      sumall <- sum(as.vector(st_area(shapetemp)))
      df.tempext <- data.frame(
        ID_0=as.character(tempcountry[1,"ID_0" ]),
        area_0_msq=sumall,
        area_0_kmsq=sumall/1000000,
        stringsAsFactors = FALSE
      )

    }

    reps <- 1

    writeLines(c("", paste0("Extracting ",datatype," for ", tempcountryname, ", ", year) ))

    dfburn <- unique(df.tempext)
    dfburn$ext <- NA

    if(lcl=="0" | nu.templocid>20){
      writeLines(c("", paste0("Extracting ",datatype," for ", tempcountryname, ", ", year, ", GADM.org level: ",lcl), "Please wait, large file. May take 5 or more mins to see progress. . . "))
    }else{
      writeLines(c("", paste0("Extracting ",datatype," for ", tempcountryname, ", ", year, ", GADM.org level: ",lcl) ))
    }
###Extraction level loop starts here
    for(k in u.templocid){

      ###TESTING Delete

###Cut out one of the polygons or not if only one polygon in shapefile
      if(nrow(shapetemp)>1){

        ###TESTING Delete

        ###NEED DYNAMIC ID NUMBER HERE
        single <- filter(shapetemp, get(paste0("ID_", lcl))==k)


        if(nrow(single)>1){
          grouped <- single %>%
            group_by(ID_2) %>%
            summarise(geometry = sf::st_union(geometry)) %>%
            ungroup()
          single <- grouped
        }

      }else{

        ###TESTING Delete

        single <-shapetemp
      }



      ###TESTING Delete


###Extract in velox (faster)
      if((datatype %in% c("Births", "Population", "Pregnancies", "AgeStructures")) | (method %in% c("SUM"))){
        extract <- vrastemp$extract(single, fun=sum)
      }else{
        extract <- vrastemp$extract(single,fun=mean)
      }

      ###TESTING Delete

      dfburn[reps,"ID_0"] <- as.character(df.loc[1,"ID_0"])
      if(as.character(lcl)!="0"){dfburn[reps,paste0("ID_",as.character(lcl))] <- k}
      dfburn[reps,"ext"] <- extract
      reps <- reps+1
    }

    ###TESTING Delete

    df.tempext <- dfburn

    colnames(df.tempext)[colnames(df.tempext)=='ext'] <- paste0(method,"_", datatype,"_", options, "_", year, "_",lcl)
    return(df.tempext)
  }
}

