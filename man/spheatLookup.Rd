\name{spheatLookup}

\alias{spheatLookup}

\title{Geocode a list of places manually typed into the function in a loop}

\description{Geocode a list of places manually typed into spaceheater in a loop with administrative layers determined by GADM layers. The function will prompt the user to type place names until the user is finished (indicated by typing DONE). The function returns a dataframe of all typed locations with GADM administrative levels and GPS coordinates. This function is only useful if your locations are not already in a data frame, see spheatNames() if you already have locations in data frame format.}

\usage{
spheatLookup(googleapikey, oride=FALSE, deleteGADM=TRUE)}

\arguments{

\item{googleapikey}{(character), a valid Google Maps API key. See https://developers.google.com/maps/documentation/javascript/get-api-key to attain one.
}
\item{oride}{(logical), if you already have the specified shapefile downloads from GADM for each country in your data frame, you may override the GADM downloads included in a-heat using oride=TRUE. ONLY do this if you are sure you have the GADM shapefiles for every country in your set already in your R working directory. Default is oride=FALSE
}
\item{deleteGADM}{(logical), if after geocoding you would like to keep the GADM shapefiles downloaded in your working directory you may use deleteGADM=FALSE. The files can be large, especially if you have many countries in your dataset. Use carefully, could cause many large GADM shapefiles saved to your working directory. The default is deleteGADM=TRUE.}
}

\examples{

spheatLookup("mygoogleapikey")

#Keeping all GADM shapefiles
spheatLookup("mygoogleapikey", deleteGADM=FALSE)

#You already have the GADM shapefiles and don't want to redownload or delete them
spheatLookup("mygoogleapikey", oride=TRUE, deleteGADM=FALSE)

}

\references{
GADM DATA are attained through the GADM project website. Commercial use of this function is not allowed without prior permission from GADM.org.
\url{http://gadm.org/}.
}
\author{
Neal Thomas Barsch
}


