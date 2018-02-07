\name{getWPdownload}

\alias{getWPdownload}

\title{Download a dataset from the WorldPopUK site}

\description{This function downloads a dataset in .tif format from the WorldPopUK

This function is intended to be used following the getWPdatatypes() and getWPoptions() functions as the inputs for getWPdownload() are given by the previous two functions.

As a bonus and to make matching WorldPop sets easier, this function contains correction algorithms to standardize country names to match WorldPop UK sets (i.e. WorldPop has "Côte d'Ivoire", you can type "Ivory Coast" and it will return the WorldPop Côte d'Ivoire set). This function adds a dataframe of the available sets called WP.options to your working environment.}

\usage{
getWPdownload(countryname, datatype, options, year)}

\arguments{

\item{countryname}{(character), the name of a country you want to see what data is avaialble from WorldPop for. e.g. \code{“Tanzania”}
}
\item{datatype}{(character), the datatype you would like to see what options are available for WorldPop UK data. The available datatypes can be attained with the function getWPdatatypes() Example datatypes: \code{“Population”} or \code{“Stunting”}
}
\item{options}{(character), the options of the tif dataset you would like to download. The available options can be attained with the function getWPoptions() e.g. \code{“ppp”} or \code{c(“F”, "interdecile")}
}
\item{year}{(numeric), the year of the dataset you would like to download. Available years are given by the getWPoptions() function.

If a dataset on WorldPop is not year coded in the filename you will need to type 9999 for the year. This is indicated by the getWPoptions() function. e.g. \code{2015} or \code{9999}
}

}

\examples{

getWPdownload("Tanzania", "Births", "pp", 2015)

#Example that has year 9999 and multiple options
getWPdownload("Nigeria", "Stunting", c("F", "interdecile"), 9999)

###FULL EXAMPLES###

###Full Example 1###
#see available Tanzania sets
getWPdatatypes("Tanzania")

#see available options for Tanzania Births Set
getWPoptions("Tanzania", "Births")

#Download as above with included options
getWPdownload("Tanzania", "Births", "pp", 2015)

###Full Example 2###
getWPdatatypes("Nigeria")

#see available options for Nigeria Stunting Set
getWPoptions("Nigeria", "Stunting")

#Download as above with included options for uncertainty map
getWPdownload("Nigeria", "Stunting", c("F", "interdecile"), 9999)

#or for the dataset NOT the uncertainty map
getWPdownload("Nigeria", "Stunting", "F", 9999)



}

\references{
WorldPop UK data are attained through the WorldPop UK website. These data are licensensed under the Creative Commons Attribution 4.0 License.
\url{http://www.worldpop.org.uk/}.

Eternal grattitude to StackOverflow member hrbrmstr without whom this function would not have been possible.
}
\author{
Neal Thomas Barsch
}


