library(httr)
library(rvest)
library(tidyverse)
library(raster)
library(rgdal)
library(reshape2)
library(rangeBuilder)
library(foreach)
library(stringr)
library(foreach)
library(dplyr)




getWPoptions <- function (country, datatype)  {
  ###standardize country name, takes care of stuff like Bolivia (Plurinational State of) or tildes like Côte d'Ivoire
  country <- standardizeCountry(paste(country),fuzzyDist=30)

  # Need to "prime" the session with a cookie
  res <- GET(url="http://www.worldpop.org.uk/data/data_sources/")
  # Get the page contents
  pg <- content(res)
  # Find the summary links
  summary_link_nodes <- html_nodes(pg, xpath=".//a[contains(@href,'summary')]")
  map(summary_link_nodes, html_nodes, xpath=".//../..") %>%
    map(html_children) %>%
    map(html_text) %>%
    map(~.[1:4]) %>%
    map(as.list) %>%
    map_df(set_names, c("continent", "country", "resolution", "data_type")) %>%
    bind_cols(
      data_frame(
        summary_link = sprintf("http://www.worldpop.org.uk%s", html_attr(summary_link_nodes, "href"))
      )
    ) -> world_pop_data
  world_pop_data$data_type <- gsub("Urban change", "UrbanChange", world_pop_data$data_type)
  world_pop_data$data_type <- gsub("Maternal and Newborn Health", "MaternalNewbornHealth", world_pop_data$data_type)
  world_pop_data$data_type <- gsub("Contraceptive Use", "ContraceptiveUse", world_pop_data$data_type)
  world_pop_data$data_type <- gsub("Age structures", "AgeStructures", world_pop_data$data_type)
  world_pop_data$data_type <- gsub("Dynamic Population", "DynamicPopulation", world_pop_data$data_type)
  countryreference <- as.data.frame(world_pop_data)
  countryreference <- countryreference[,c(1,2,4)]
  countryreference <- countryreference[!(countryreference$country)=="N/A",]
  world_pop_data <- world_pop_data[!(world_pop_data$country)=="N/A",]

  ###Filter country names so they match the desired country
  ###There is probably a better way to do this but I had this code from standardizing country name lists for matching a while back
  countryreference$CountryStandard <- standardizeCountry(countryreference[,"country"], fuzzyDist=20)
  countryreference$CountryEdit <- gsub("[()]", "", countryreference$country)
  countryreference$CountryEdit2 <- gsub("\\s*\\([^\\)]+\\)","",as.character(countryreference$country))
  foreach(a=1:nrow(countryreference)) %do% {
    if(countryreference[a,"CountryStandard"]==""){
      countryreference[a,"CountryStandard"] <- standardizeCountry(countryreference[a,"CountryEdit"], fuzzyDist=20)
      if(countryreference[a,"CountryStandard"]==""){
        countryreference[a,"CountryStandard"] <- standardizeCountry(countryreference[a,"CountryEdit2"], fuzzyDist=20)
        if(countryreference[a,"CountryStandard"]==""){
          countryreference[a,"CountryStandard"] <- toupper(countryreference[a,"country"])
        }
      }
    }
  }
  exists <- isTRUE(paste(country) %in% as.character(countryreference$CountryStandard))
  if(exists==FALSE){
    print("It appears this country is not in the WorldPop set, please check and try again")
    break
  }
  countryreference <- countryreference[,c(1,4,3)]
  world_pop_data$CountryStandard <- countryreference[,2]
  world_pop_data <- world_pop_data[,c(1,6,2,3,4,5)]
  countryreference <- suppressMessages(dcast(countryreference, continent+CountryStandard ~ data_type))
  countryreference <- filter(countryreference, countryreference$CountryStandard==country)
  countryreference <- countryreference[,colSums(is.na(countryreference))<nrow(countryreference)]
  #Filter Country Desired
  world_pop_data <- filter(world_pop_data, CountryStandard %in% countryreference$CountryStandard)
  world_pop_data <- filter(world_pop_data, data_type==paste(datatype))

  ##Get country link
  dataset_link <- as.character(world_pop_data[1,"summary_link"])

  #Follow the country URL for worldpop#
  GET(url=dataset_link) -> res2
  pg2 <- content(res2)


  # extract "form" fields (that page does a POST request)
  fields <- html_nodes(pg2, "form#conform > input")
  fields <- set_names(xml_attr(fields, "value"), html_attr(fields, "name"))

  ###Submit the form with the field data
  POST(
    url = "http://www.worldpop.org.uk/data/download/",
    add_headers(`Referer` = dataset_link),
    body = list(
      client_first_name = "",
      client_last_name = "",
      client_organization = "",
      client_country = "",
      client_email = "",
      client_message = "",
      zip_id = fields["zip_id"],
      zip_title = fields["zip_title"],
      decoy = fields["decoy"],
      website = "",
      download = "Browse Individual Files"
    ),
    encode = "form"
  ) -> res3

  # find the link that has the file list
  pg3 <- content(res3)
  html_nodes(pg3, xpath=".//a[contains(., 'switch to')]") %>%
    html_attr("href") -> file_list_query_string


  # follow that link (we need to use some of the previous captured fields)
  GET(
    url = "http://www.worldpop.org.uk/data/files/index.php",
    query = list(
      dataset = fields["zip_id"],
      action = "dir"
    )
  ) -> res4

  ###Get the datasets on the page###
  pg4 <- content(res4)
  data_frame(
    group_name = html_nodes(pg4, "a.dl") %>% html_text(),
    href = html_nodes(pg4, "a.dl") %>% html_attr("href")
  ) -> downloads



  ###Ditch non tif section
  downloads$istif <- str_sub(downloads$group_name,-4,-1)
  #Some such as senegal are inexplicably .TIF
  downloads$istif <- tolower(downloads$istif)
  downloads <- filter(downloads, istif==".tif")


  ###WorldPop decided to have super inconsistent filenames

  pg4charfile <- as.character(downloads[1,"group_name"])
  pg4charfile <- gsub(' {1,}','',pg4charfile)
  if(substr(pg4charfile,1,6)!="popmap"){
    if(grepl("\\d", pg4charfile)==TRUE){
      char4 <- substr(pg4charfile,4,4)
      char6 <-substr(pg4charfile,6,6)
      char9 <-substr(pg4charfile,9,9)
      char11 <-substr(pg4charfile,11,11)
      char4num <- suppressWarnings(!is.na(as.numeric(char4)))
      char6num <- suppressWarnings(!is.na(as.numeric(char6)))
      char9num <- suppressWarnings(!is.na(as.numeric(char9)))
      char11num <- suppressWarnings(!is.na(as.numeric(char11)))
      if(char4num==TRUE & char6num==TRUE){
        downloads$years <-substr(downloads$group_name,4,7)

      }
      if(char4num==TRUE & char6num==FALSE){
        downloads$years <-substr(downloads$group_name,4,5)
        getfouryear <- function (yearsvec)  {
          yrFlip = 50
          yearsvec <- as.numeric(yearsvec)
          yearsvec[yearsvec > yrFlip] <- yearsvec[yearsvec > yrFlip] + 1900
          yearsvec[yearsvec < yrFlip] <- yearsvec[yearsvec < yrFlip] + 2000
          return(yearsvec)
        }
        downloads$years <- getfouryear(downloads$years)
      }
      ####PROBLEM this can be true and two sections down can also be true
      if(char9num==FALSE & char11num==TRUE){
        downloads$years <-substr(downloads$group_name,11,12)
        getfouryear <- function (yearsvec)  {
          yrFlip = 50
          yearsvec <- as.numeric(yearsvec)
          yearsvec[yearsvec > yrFlip] <- yearsvec[yearsvec > yrFlip] + 1900
          yearsvec[yearsvec < yrFlip] <- yearsvec[yearsvec < yrFlip] + 2000
          return(yearsvec)
        }
        downloads$years <- getfouryear(downloads$years)
      }
      if(char4num==FALSE & char6num==FALSE & char9num==TRUE){
        downloads$years <- str_extract(downloads$group_name, "\\d{4}")
      }
      if(char4num==FALSE & char6num==FALSE & char9num==FALSE & char11num==FALSE){
        downloads$years <- str_extract(downloads$group_name, "\\d{4}")
      }
    }else{downloads$years <- 9999}
  }else{
    downloads$years<- as.numeric(substr(downloads$group_name,7,8))
    getfouryear <- function (yearsvec)  {
      yrFlip = 50
      yearsvec <- as.numeric(yearsvec)
      yearsvec[yearsvec > yrFlip] <- yearsvec[yearsvec > yrFlip] + 1900
      yearsvec[yearsvec < yrFlip] <- yearsvec[yearsvec < yrFlip] + 2000
      return(yearsvec)
    }
    downloads$years <- getfouryear(downloads$years)
  }
  downloads <- downloads[!is.na(downloads$years),]

  ###Possible Options due to the inexplicable nature of their inconsistent file names
  possopt <- c("_pph_", "_ppp_", "_pp_", "uncert", "adj","_M.",  "_M_","_F.", "_F_", "interdecile", "povsd", "125", "200","wpipov", "ppipov", "incpov", "mpipov", "ANC", "SBA", "PNC")
  opttext <- c("Persons per hectare", "Persons per pixel", "per pixel", " uncertainty dataset showing 95% credible intervals",
               "adjusted to match UN estimates", "MALE", "MALE", "FEMALE", "FEMALE", "Uncertainty map", "poverty standard deviation map", "$1.25/day",
               "$2.00/day", "mean wealth index", "mean likelihood of living in poverty per grid square", "Income estimate USD per grid square", "%poverty by Multidimensional Poverty Index",
               "prob of four or more antenatal care visits at time of delivery", "prob of skilled birth attendance during delivery", "prob of postnatal care received within 48 hours of delivery")
  possoptdf <- data.frame(possopt, opttext, stringsAsFactors = FALSE)
  groupsubstr <- str_sub(downloads$group_name,4,-4)

  ###get options for each file from the worldpop selected country and datatype###
  optionsforchoice<-foreach(a=1:nrow(downloads), .combine=rbind)%do%{
    theoptions<- foreach(b=1:length(opttext), .combine=cbind)%do%{
      matchoopt <- str_detect(downloads[a,"group_name"],coll(possopt[b]))
      if(matchoopt==TRUE){result <-possopt[b]}
      if(matchoopt==FALSE){result<- NA}
      if(b==13){
        mistake <- str_detect(downloads[a,"group_name"],"\\d{4}")
        if(mistake==TRUE){result <- NA}
      }
      result
    }
  }
  ###get rid of all the nonoptions for selection
  optionsforchoice<-do.call(rbind,lapply(1:nrow(optionsforchoice),function(x) t(matrix(optionsforchoice[x,order(is.na(optionsforchoice[x,]))])) ))
  optionsforchoice <- as.data.frame(optionsforchoice, stringsAsFactors=FALSE)
  optionsforchoice <- optionsforchoice[,colSums(is.na(optionsforchoice))<nrow(optionsforchoice)]
  downloads <- cbind(downloads,optionsforchoice)
  optiters <- as.data.frame(optionsforchoice)
  if(length(optionsforchoice)==1){colnames(downloads)[5]<-"V1"}
  if(ncol(as.data.frame(optionsforchoice))==1){colnames(downloads)[5]<-"V1"}
  optiters <- ncol(optiters)
  ###join all the options so they can be displayed
  foreach(a=1:optiters)%do%{
    downloads <- merge(downloads, possoptdf, by.x=paste0("V",a), by.y="possopt", all.x=TRUE)
    coltochange <- ncol(downloads)
    colnames(downloads)[coltochange] <- paste0("possopt",a)
  }

  downloads$years <- as.numeric(downloads$years)
  downloads <- downloads[order(downloads$years),]
  ##Subsetting downloads to columns that only contain possopt
  downpossopt <- downloads[ ,  grepl( "possopt" , colnames( downloads ) ) ]
  downpossopt <- as.data.frame(downpossopt)
  downpossopt$code <- c(1:nrow(downpossopt))
  downloads$code <- c(1:nrow(downloads))
  rownames(downloads) <- c(1:nrow(downloads))
  rownames(downpossopt) <- c(1:nrow(downpossopt))
  if(ncol(downpossopt)==1){
    colnames(downpossopt) <- "possopt1"}
  downpossopt <- cbind(downloads$years, downpossopt)
  names(downpossopt)[names(downpossopt) == 'downloads$years'] <- 'years'
  names(downpossopt)[names(downpossopt) == 'V1'] <- 'years'



  ###get the right codes for the function
  possoptcodes <- c("pph", "ppp", "pp", "uncert", "adj","M",  "M","F", "F", "interdecile", "povsd", "125", "200","wpipov", "ppipov", "incpov", "mpipov", "ANC", "SBA", "PNC")
  possoptcodes <- as.data.frame(cbind(possoptcodes, opttext))
  possoptcodes <- possoptcodes[c(1:6,8,10:nrow(possoptcodes)),]
  names(downpossopt)[names(downpossopt) == 'downpossopt'] <- 'possopt1'
  foreach(a=1:optiters)%do%{
    downpossopt <- merge(downpossopt, possoptcodes, by.x=paste0("possopt",a), by.y="opttext", all.x=TRUE)
    coltochange <- ncol(downpossopt)
    colnames(downpossopt)[coltochange] <- paste0("OptionCode",a)
  }
  downpossopt <- as.data.frame(downpossopt)
  downpossopt <- downpossopt[order(downpossopt$code),]
  downpossopt <- downpossopt[,c(which(colnames(downpossopt)=="possopt1"),which(colnames(downpossopt)!="possopt1"))]
  downpossopt <- downpossopt[,c(which(colnames(downpossopt)=="years"),which(colnames(downpossopt)!="years"))]
  downpossopt$country <- countryreference[1,"CountryStandard"]
  downpossopt <- downpossopt[,c(which(colnames(downpossopt)=="country"),which(colnames(downpossopt)!="country"))]
  row.names(downpossopt) <- c(1:nrow(downpossopt))
  downpossoptcodes <- downpossopt[ ,  grepl( "OptionCode" , colnames( downpossopt ) ) ]
  downpossoptcodes <- as.data.frame(downpossoptcodes)



  print(downpossopt)
  WP.options <<- downpossopt
  print("The above table has also been added to your working environment as dataframe: WP.options")
}
