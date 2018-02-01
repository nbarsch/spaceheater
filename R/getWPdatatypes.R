
getWPdatatypes <- function (country)  {
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
print(countryreference[1,])
WPdata.types <<- countryreference[1,]
print("The above table has also been added to your working environment as dataframe: WPdata.types")
}
