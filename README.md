# spaceheater

An `R` package that allows flexible geocoding from a column in a data frame of place NAMES or GPS coordinates, and downloads and completes advanced spatial analysis of the locations in such a dataset automatically. 

This package is being updated as of April 2018 and does not yet contain all functions. The function spheatLOOKUP is still upcoming, but spheatNAMES and spheatGPS are fully functional.  The WorldPop extraction code is in beta but should be fully functional.  Upcoming extraction codes that should be available within the next few months include NASA MODIS Satellite layers, NASA NOAA Nightlights layers, and directions/distance calculations within google maps. 

Example WorldPop tif that Spaceheater can download and analyze (from WorldPop UK):

![Data](/SpaceheaterExampleBangladesh.png?raw=true "Spaceheater Data")

## Installation

```r
if (!require(devtools)) install.packages("devtools")
devtools::install_github("nbarsch/spaceheater")
```

## Usage

```spaceheater``` assumes that you begin with a dataset that has some sort of location column(s) (place NAMES or GPS coordinates), or list of places that you would like to look up. You need to feed your dataset to spaceheater so it can be formatted for spaceheater's functions. 

## YOU MUST START WITH ONE OF THE FOLLOWING:
1. ```spheatNAMES()```: if you have a column with any type of place NAMES (addresses, states, cities, counties, villages, etc.).  Input type is very flexible and can be mixed within the location column. 
2. ```spheatGPS()```: if you have a LATITUDE COLUMN and LONGITUDE COLUMN in your dataset.
3. ```spheatLOOKUP()```: if you want to manually enter place names and create a dataset from scratch (upcoming update, only depreciated available).

## Using spheatNAMES, spheatGPS, or spheatLOOKUP:

```spheatNAMES(dataset=myDFname, colname="myColumnOfNames", googleapikey="myGoogleAPIKey")```

```spheatGPS(dataset="myDFname", latcol="myColumnOfLatitudes", loncol="myColumnOfLongitudes", googleapikey="myGoogleAPIKey")```

##Upcoming not currently available (depreciated geocoder lookup available through spheatLookup_dep, but does NOT work with extraction functions)

```spheatLOOKUP("myGoogleAPIKey")```


## Extraction

Complete Extraction ONLY AFTER completing one of the spheat functions

Currently (April 2018) extract code for WorldPop UK rasters are available

```extractWP(datatype="Births", options="pp", year=2015, gadmlevel="lowest", fill=TRUE, deleteRAST=TRUE)```

```extractWP(datatype="Population", options=c("ppp", "adj"), year=2010, gadmlevel="lowest", fill=TRUE, deleteRAST=TRUE)```

You dont want to delete the Raster files (warning, could take a lot of storage space if not deleted)

```extractWP(datatype="Births", options="pp", year=2015,deleteRAST=FALSE)```


## Lookup of what is available from WorldPop also available:

Get the possible tif sets from WorldPop UK for Bangladesh

```getWPdatatypes("Bangladesh")```

Get the possible options for tif sets from WorldPop UK for Tanzania Population sets

```getWPoptions("Bangladesh", "Population")```

Manual download of WP (happens automatically in extract code) also available:
Download the 2010 Persons per pixel, adjusted to match UN estimates tif for Bangladesh from WorldPop UK

```getWPdownload("Bangladesh", "Population", c("ppp", "adj"), 2010)```







