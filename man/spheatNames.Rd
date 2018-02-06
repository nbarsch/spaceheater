\name{spheatNames}

\alias{spheatNames}

\title{Geocode place names in a dataset}

\description{a vector of names in a dataset with administrative layers determined by GADM layers}

\usage{
spheatNames(dataset, colname, googleapikey, oride=FALSE, deleteGADM=TRUE)}

\arguments{

\item{dataset}{(character), the name of the data frame containing a column of place names. e.g. \code{“mydataframe”}
}
\item{colname}{(character), the name of the column in the data frame containing place names e.g. \code{“mycolname”}
}
\item{googleapikey}{(character), a valid Google Maps API key. See https://developers.google.com/maps/documentation/javascript/get-api-key to attain one.
}
\item{oride}{(logical), if you already have the specified shapefile downloads from GADM for each country in your data frame, you may override the GADM downloads included in a-heat using oride=TRUE. ONLY do this if you are sure you have the GADM shapefiles for every country in your set already in your R working directory. Default is oride=FALSE
}
\item{deleteGADM}{(logical), if after geocoding you would like to keep the GADM shapefiles downloaded in your working directory you may use deleteGADM=FALSE. The files can be large, especially if you have many countries in your dataset. Use carefully, could cause many large GADM shapefiles saved to your working directory. The default is deleteGADM=TRUE.
  }
}

\references{
GADM DATA is attained through the GADM project website. Commercial use of this function is not allowed without prior permission from GADM.org.
\url{http://gadm.org/}.
}
\author{
Neal Thomas Barsch
}

