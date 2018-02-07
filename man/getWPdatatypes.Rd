\name{getWPdatatypes}

\alias{getWPdatatypes}

\title{Get the available datatypes for a country from WorldPop UK datasets}

\description{This function pulls what are the available datatypes from WorldPop UK for a given country. It returns a not available message if the country typed does not exist on WorldPop. As a bonus and to make matching WorldPop sets easier, this function contains correction algorithms to standardize country names to match WorldPop UK sets (i.e. WorldPop has "Côte d'Ivoire", you can type "Ivory Coast" and it will return the WorldPop Côte d'Ivoire set). This function adds a dataframe of the available sets called WPdata.types to your working environment.}

\usage{
getWPdatatypes(countryname)}

\arguments{

\item{countryname}{(character), the name of a country you want to see what data is avaialble from WorldPop for. e.g. \code{“Tanzania”}
}
}

\examples{

getWPdatatypes("Nigeria")
}

\references{
WorldPop UK data are attained through the WorldPop UK website. These data are licensensed under the Creative Commons Attribution 4.0 License.
\url{http://www.worldpop.org.uk/}.

Eternal grattitude to StackOverflow member hrbrmstr without whom this function would not have been possible.
}
\author{
Neal Thomas Barsch
}


