
<!-- README.md is generated from README.Rmd. Please edit that file -->

# LidarIndexR

<!-- badges: start -->
<!-- badges: end -->

The goal of LidarIndexR is to provide functions to build index files for
lidar projects. The functions in the package have been tested using FTP
and HTTPS servers. USGS changed the format of the XML generated for
directory listing on the rockyweb server sometime between 2022 and 2024.
Code was modified in June 2024 to correctly parse the new output.
However, I can’t guarantee that the directory functions work with all
HTTP(S) servers due to differences in the HTML generated to show
directory contents. The changes in 2024 should make the functions more
reliable given different formats but I haven’t tested functions using
other servers.

## Installation

LidarIndexR is not available on CRAN.

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("bmcgaughey1/LidarIndexR")
```

## Example \#1: Create a tile index from files on a remote ftp server

This example creates a tile index for a single lidar project stored in
the USGS rockyweb server. In this case, you have to know the details for
the project including the folder name for the project and the projection
information for the data.

``` r
library(LidarIndexR)
library(sf)
library(ggplot2)
library(viridis)

## Basic example to build an index
URL <- "https://rockyweb.usgs.gov/vdelivery/Datasets/Staged/Elevation/LPC/Projects/WA_ElwhaRiver_2015"
pointFolder <- "WA_Elwha_TB_2015/LAZ"
outputFile <- "~/WA_ElwhaRiver_2015.gpkg"

# projection info...for most projects, this can be pulled from the WESM project index
pointCRS <- 26910

# create index
BuildIndexFromPoints(URL, pointFolder, outputFile, projString = pointCRS,
                     appendInfo = data.frame("Project" = "WA_ElwhaRiver_2015"))
#> [1] TRUE
```

``` r

# read the index and display
index <- st_read(outputFile)
#> Reading layer `WA_ElwhaRiver_2015' from data source 
#>   `C:\Users\bmcgaughey\Documents\WA_ElwhaRiver_2015.gpkg' using driver `GPKG'
#> Simple feature collection with 67 features and 17 fields
#> Geometry type: POLYGON
#> Dimension:     XY
#> Bounding box:  xmin: 456087.6 ymin: 5323597 xmax: 470339.9 ymax: 5333589
#> Projected CRS: NAD83 / UTM zone 5N
```

``` r

ggplot(index) +
  ggtitle("WA_ElwhaRiver_2015 lidar tiles") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_sf(aes(fill = PointCount), show.legend = TRUE) +
  scale_fill_viridis()
```

<img src="man/figures/README-example1-1.png" width="100%" />

## Example \#2: Create a tile index using a project from the USGS WESM project index

This example creates an index for the same lidar project as Example \#1.
However, this example uses the USGS project index to populate the new
tile index with information regarding the acquisition and to provide
projection information for the project.

You will need to change the line that reads the index to reflect the
location of the WESM index on your computer.

The example requires a local copy of the USGS project index from
[here](https://rockyweb.usgs.gov/vdelivery/Datasets/Staged/Elevation/metadata/WESM.gpkg).
The commented code in the example downloads the WESM.gpkg index.

**Note:** The call to can take quite a bit of time to execute. It
touches every point file on the server to read bounding box information
for the tiles. For projects with thousands of tiles, it can take hours
to build the index.

``` r
library(LidarIndexR)
library(sf)
library(utils)
library(ggplot2)
library(viridis)

# download the USGS WESM index
# utils::download.file("https://rockyweb.usgs.gov/vdelivery/Datasets/Staged/Elevation/metadata/WESM.gpkg",
#                      "WESM.gpkg", 
#                      method = "libcurl", 
#                      mode = "wb")

# read the FESM index from a local file
index <- st_read("G:/R_Stuff/EntwineIndex/WESM.gpkg")
#> Reading layer `WESM' from data source `G:\R_Stuff\EntwineIndex\WESM.gpkg' using driver `GPKG'
#> Simple feature collection with 2923 features and 28 fields
#> Geometry type: MULTIPOLYGON
#> Dimension:     XY
#> Bounding box:  xmin: -179.2501 ymin: 13.232 xmax: 179.8547 ymax: 71.507
#> Geodetic CRS:  NAD83
```

``` r
pointFolder <- "laz"
outputFile <- "~/WA_Elwha_TB_2015.gpkg"

item <- index[which(index$workunit == "WA_Elwha_TB_2015"), ]

# create index
BuildIndexFromUSGSProjectIndexItem(item, pointFolder, outputFile)
#> [1] TRUE
```

``` r

# read the index and display
tindex <- st_read(outputFile)
#> Reading layer `WA_Elwha_TB_2015' from data source 
#>   `C:\Users\bmcgaughey\Documents\WA_Elwha_TB_2015.gpkg' using driver `GPKG'
#> Simple feature collection with 67 features and 23 fields
#> Geometry type: POLYGON
#> Dimension:     XY
#> Bounding box:  xmin: 456087.6 ymin: 5323597 xmax: 470339.9 ymax: 5333589
#> Projected CRS: NAD83 / UTM zone 10N
```

``` r

ggplot(tindex) +
  ggtitle("WA_Elwha_TB_2015 lidar tiles") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_sf(aes(fill = PointCount), show.legend = TRUE) +
  scale_fill_viridis()
```

<img src="man/figures/README-example2-1.png" width="100%" />

## Example \#3: Create an overall polygon for a project from the tile index

This example builds on Example \#2 to create a polygon showing the area
covered by the point tiles. The default size for the raster used to
merge polygons is 512 by 512. By increasing this to 2048 by 2048, the
project polygon captures the small tiles that are otherwise omitted when
using the default size.

``` r
# build on the last example to generate a project polygon

tproject <- BuildProjectPolygonFromIndex(
  outputFile,
  item$workunit,
  appendInfo = item[, c("workunit_id", "collect_start", "collect_end", "p_method", "horiz_crs")],
  quiet = FALSE,
  nx = 2048, ny = 2048
)

# display the project polygon
ggplot(tproject) +
  ggtitle(paste0(item$workunit, " project area")) +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_sf(aes(fill = PID), show.legend = FALSE)
```

<img src="man/figures/README-example3-1.png" width="100%" />
