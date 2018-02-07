\name{getWPoptions}

\alias{getWPoptions}

\title{Get the available options for a country and datatype set from WorldPopUK}

\description{This function pulls what are the available datasets from WorldPop UK for a given country and datatype. It returns a not available message if the country typed does not exist on WorldPop. To see the available datatypes for a country, see getWPdatatypes() which is intended to be used before this function.

Example options returned are "ppp" for persons per pixel, "pph" for persons per hectare, "M" or "F" for male/female tagged sets etc. This function returns all the available options for a given country and datatype.

The codes returned in the WP.options dataframe in "OptionCode" columns correspond to the options necessary to include in the getWPdownload() function.

If the function returns the year 9999 in the WP.options dataframe, this means the filename on WorldPop is not year tagged. The year will be included if you look up the set manually on WorldPop UK but for the getWPdownload() function you will need to type 9999 for the year as it corresponds to what is given here.

As a bonus and to make matching WorldPop sets easier, this function contains correction algorithms to standardize country names to match WorldPop UK sets (i.e. WorldPop has "Côte d'Ivoire", you can type "Ivory Coast" and it will return the WorldPop Côte d'Ivoire set). This function adds a dataframe of the available sets called WP.options to your working environment.}

\usage{
getWPoptions(countryname, datatype)}

\arguments{

\item{countryname}{(character), the name of a country you want to see what data is avaialble from WorldPop for. e.g. \code{“Tanzania”}
}
\item{datatype}{(character), the datatype you would like to see what options are available for WorldPop UK data. e.g. \code{“Population”}
}

}

\examples{

getWPoptions("Tanzania", "Population")

#Example that returns year 9999
getWPoptions("Nigeria", "Stunting")
}

\references{
WorldPop UK data are attained through the WorldPop UK website. These data are licensensed under the Creative Commons Attribution 4.0 License.
\url{http://www.worldpop.org.uk/}.

Eternal grattitude to StackOverflow member hrbrmstr without whom this function would not have been possible.
}
\author{
Neal Thomas Barsch
}


