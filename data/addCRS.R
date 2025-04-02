# code to deal with missing CRS for lidar index file
#
if (FALSE) {
  library(terra)
  
  folder <- "H:/R6_IndexFiles"
  
  files <- list.files(folder, "\\.gpkg", full.names = TRUE, ignore.case = TRUE)
  
  # EPSG 32149 in point files has problems with unit US survey foot
  
  prjs <- c(
    c("usfsalbers_meters", 9674), # NAD83 R6 albers equal area
    c("wasp_north_feet", 1), # 2285:NAD83 WASP north usFt, 2926: NAD83(HARN), 6597: NAD83(2011)
    c("wasp_south_feet", 1), # 2286:NAD83 WASP south usFt, 2927: NAD83(HARN), 6599: NAD83(2011)
    c("orlambert_feet", 1), #
    c("oregonlambert_feet", 1), #
    c("utm10_meters", 26910), #
    c("utm11_meters", 1), #
    c("orsp_north_feet", 1), #
    c("utm10_3dep", 1), #
    c("utm11_3dep", 1), #
    c("", 1), #
  )
  
  for (file in files) {
    # read layer from geopackage and check for CRS (use $hasCRS)
    bb <- vect(file, layer = "boundingbox")
    
    if (!bb$hasCRS) {
      # if no CRS, look at index name for specific target strings indicating crs
      for (i in 1:length(prjs)) {
        if (grepl("", tolower(file), fixed = TRUE)) {
          
        }
      }
    }
  }
}

# oregon lambert (DOGAMI data)
# NAD83 / Oregon GIC Lambert (ft)
# EPSG:2992 with transformation: 1188
# 
# NAD83(NSRS2007) / Oregon GIC Lambert (ft)
# EPSG:3644 with transformation: 15931
# 
# NAD83(2011) / Oregon GIC Lambert (ft)
# EPSG:6557 with transformation: 9774
# 
# NAD83(HARN) / Oregon GIC Lambert (ft)
# EPSG:2994 with transformation: 1580
# 
# NAD83(CORS96) / Oregon GIC Lambert (ft)
# EPSG:6868


# UTM zone 10
# NAD83 / UTM zone 10N
# EPSG:26910 with transformation: 1188
# 
# NAD83(2011) / UTM zone 10N
# EPSG:6339 with transformation: 9774
# 
# NAD83(CSRS) / UTM zone 10N
# EPSG:3157 with transformation: 1946
# 
# NAD83(NSRS2007) / UTM zone 10N
# EPSG:3717 with transformation: 15931
# 
# NAD83(HARN) / UTM zone 10N
# EPSG:3740 with transformation: 1580
# 
# WGS 84 / UTM zone 10N
# EPSG:32610
# 
# NAD 1983 (CORS96) UTM zone 10N
# ESRI:102410
