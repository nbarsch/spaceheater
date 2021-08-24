# spaceheater

```spaceheater``` is an `R` package that allows easy and flexible adding of a variety of spatial variables to any geocoded (lat/lon) dataset.  

```spaceheater``` is built to be fully automatic by downloading and completing advanced spatial analysis without requiring advanced coding capability by the user. This enables a wider research audience to be able to utilize advanced spatial controls previously only available to advanced users.

 ```spheatNAMES``` and ```spheatGPS``` are fully functional. WorldPop download and option/datatype lookups are all fully functional.  The WorldPop extraction code is in beta but should be functional, please report any issues.  Upcoming extraction codes that should be available within the next few months include NASA MODIS Satellite layers, NASA NOAA Nightlights layers, and OPEN STREET MAPS road density calculation.

Example WorldPop tif that Spaceheater can download and analyze (from WorldPop UK):

![Data](/SpaceheaterExampleBangladesh.png?raw=true "Spaceheater Data")

## Installation

```r
if (!require(remotes)) install.packages("remotes")
remotes::install_github("nbarsch/spaceheater")
```

## Usage
```spaceheater``` can be used with it's built in spatial data downloads as a standalone (i.e. the ```getWPdownload()``` function) -OR- may be used in fully automatic mode by using the SPheat() commands. 

## To Complete Extraction of Spatial Data you must geocode your dataset with spaceheater
1. ```spheatNAMES()```: if you have a column with any type of place NAMES (addresses, states, cities, counties, villages, etc.).  Input type is very flexible and can be mixed within the location column. 
2. ```spheatGPS()```: if you have a LATITUDE COLUMN and LONGITUDE COLUMN in your dataset.
3. ```spheatLOOKUP()```: if you want to manually enter place names and create a dataset from scratch (upcoming update, only depreciated available).

Using spheatNAMES, spheatGPS, or spheatLOOKUP:

```spheatNAMES(dataset=my_df_name, colname="my_column_of_place_names", googleapikey="my_Google_API_Key")```

```spheatGPS(dataset=my_df_name, latcol="my_column_of_latitudes", loncol="my_column_of_latitudes", googleapikey="my_Google_API_Key")```

##Upcoming not currently available (depreciated geocoder lookup available through spheatLookup_dep, but does NOT work with extraction functions)

```spheatLOOKUP("my_Google_API_Key")```


## Extraction

Complete Extraction ONLY AFTER completing one of the spheat functions

Currently (January 2019) extract code for WorldPop UK rasters are available

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







