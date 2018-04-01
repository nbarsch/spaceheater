# spaceheater

An `R` package that allows flexible geocoding from a column in a data frame of place names or GPS coordinates, and downloads and completes advanced spatial analysis of the locations in such a dataset. 

This package is being updated as of April 2018 and does not yet contain all functions. The base geocoding and WorldPop download functions have been added and are fully functional. The more consitent spheatGPS2 and spheatNAMES2 are in beta, but are faster and updated to the new format the extraction codes will be in.  The WorldPop extraction code is in beta but available, a new version is upcoming in the next week (as of April 1).  Upcoming extraction codes that should be available within the next few months include NASA MODIS Satellite layers, NASA NOAA Nightlights layers, and directions/distance calculations within google maps. 

Example WorldPop tif that Spaceheater can download and analyze (from WorldPop UK):

![Data](/SpaceheaterExampleBangladesh.png?raw=true "Spaceheater Data")

## Installation

```r
if (!require(devtools)) install.packages("devtools")
devtools::install_github("nbarsch/spaceheater")
```

## Usage

```r
library(spaceheater)

# Geocode my place names or addresses in my data frame:
spheatNames("myDFname", "myColumnOfNames", "myGoogleAPIKey")

# Also available, see manual for details
spheatGPS("myDFname", "myColumnOfLatitudes","myColumnOfLongitudes", "myGoogleAPIKey")
spheatLookup("myGoogleAPIKey")

# Get the possible tif sets from WorldPop UK for Bangladesh
getWPdatatypes("Bangladesh")

# Get the possible options for tif sets from WorldPop UK for Tanzania Population sets
getWPoptions("Bangladesh", "Population")

# Download the 2010 Persons per pixel, adjusted to match UN estimates tif for Bangladesh from WorldPop UK
getWPdownload("Bangladesh", "Population", c("ppp", "adj"), 2010)

```


