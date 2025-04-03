# code to deal with missing CRS for lidar index file
#
# strategy is to read index files and look for files that don't have CRS ($hasCRS == FALSE)
# for index files without CRS, look at folder name to see if "magic" strings are 
# included to provide a clue. If so, assign CRS and add new field (assignedCRS)
# with the WKT for the new CRS.
#
# This also means I need to add the new field to index files that have CRS but
# set value to a blank string. Not a big deal but does mean all index files
# will be touched and rewritten.
#
if (FALSE) {
  library(terra)
  
  folder <- "H:/R6_IndexFiles"
  
  files <- list.files(folder, "\\.gpkg", full.names = TRUE, ignore.case = TRUE)
  
  # EPSG 32149 in point files has problems with unit US survey foot
  
  # these are best guesses for EPSG corresponding to various crs name strings used
  # in folder names. I suspect that many of these will be wrong regarding the datum
  #
  # there are a few folders that appear to have mixed UTM 10N and 11N data...can't 
  # do much with these.
  prjs <- list(
    c("usfsalbers_meters", 9674), # NAD83 R6 albers equal area
    c("wasp_north_feet", 2926), # 2285:NAD83 WASP north usFt, 2926: NAD83(HARN), 6597: NAD83(2011)
    c("wasp_south_feet", 2927), # 2286:NAD83 WASP south usFt, 2927: NAD83(HARN), 6599: NAD83(2011)
    c("south_feet", 2927), # 2286:NAD83 WASP south usFt, 2927: NAD83(HARN), 6599: NAD83(2011)
    c("wasps", 2927), # 2286:NAD83 WASP south usFt, 2927: NAD83(HARN), 6599: NAD83(2011)
    c("orlambert_feet", 2994), #
    c("oregonlambert_feet", 2994), #
    c("orlam_feet", 2994), #
    c("ogic", 2994), #
    c("utm10_meters", 6339), # 6339: NAD83(2011), 26910: NAD83. 3740: NAD83(HARN)
    c("utm11_meters", 6340), # 6340: NAD83(2011), 26911: NAD83. 3741: NAD83(HARN)
    c("orsp_north_feet", 6885), #6885: NAD83(CORS96) ORSP north feet
    c("utm10_3dep", 6339), # 6339: NAD83(2011), 26910: NAD83. 3740: NAD83(HARN)
    c("utm11_3dep", 6340), # 6340: NAD83(2011), 26911: NAD83. 3741: NAD83(HARN)
    c("umt11_3dep", 6340) # typo in folder name
  )
  
  totalCount <- length(files)
  CRSCount <- 0
  countNon3DEPMissingCRS <- 0
  
  totalStorage <- 0
  totalNon3DEPStorage <- 0
  countNon3DEP <- 0
  
  assignCRS <- function(index, epsg) {
    bb <- vect(index, layer = "boundingbox")
    wb <- vect(index, layer = "boundary")
    ass <- vect(index, layer = "assets")
    
    if (epsg != "") {
      # assign CRS
      crs(bb) <- paste0("EPSG:", epsg)
      crs(wb) <- paste0("EPSG:", epsg)
      crs(ass) <- paste0("EPSG:", epsg)
      
      # add field
      bb$assignedCRS <- crs(bb)
      wb$assignedCRS <- crs(wb)
    } else {
      # add empty string to assigned CRS
      bb$assignedCRS <- ""
      wb$assignedCRS <- ""
    }  
    
    # write new index (overwrite)
    writeVector(bb, index, layer = "boundingbox", overwrite = TRUE)
    writeVector(wb, index, layer = "boundary", overwrite = TRUE, insert = TRUE)
    writeVector(ass, index, layer = "assets", overwrite = TRUE, insert = TRUE)
  }
  
  for (file in files) {
    # cat(file, "...")
    # read layer from geopackage and check for CRS (use $hasCRS)
    bb <- vect(file, layer = "boundingbox")
    
    totalStorage <- totalStorage + (bb$assetsize / 1024 / 1024 / 1024 / 1024)
    
    non3DEP <- FALSE
    if (grepl("3dep", tolower(file), fixed = TRUE) == FALSE) {
      totalNon3DEPStorage <- totalNon3DEPStorage + (bb$assetsize / 1024 / 1024 / 1024 / 1024)
      countNon3DEP <- countNon3DEP + 1
      non3DEP <- TRUE
    }
    
    if (bb$hasCRS) {
      # cat(file, "...", "Has CRS!\n")
      CRSCount <- CRSCount + 1
      
      assignCRS(file, "")
    } else {
      # cat(file, "...", "No CRS...")
      if (non3DEP)
        countNon3DEPMissingCRS <- countNon3DEPMissingCRS + 1
      
      # if no CRS, look at index name for specific target strings indicating crs
      match <- FALSE
      for (i in 1:length(prjs)) {
       if (grepl(prjs[[i]][1], tolower(file), fixed = TRUE)) {
         # cat("Found match for", prjs[[i]][1], "\n")
         match <- TRUE
         
         # assign new CRS using EPSG code
         assignCRS(file, prjs[[i]][2])
       }
      }
      if (!match) {
        cat(file, "...", "No CRS...", "No match\n")
        assignCRS(file, "")
      }
    }
  }

  cat("Total non-3DEP storage:", totalNon3DEPStorage, "Tb in", countNon3DEP, "projects\n")
  cat("Non-3DEP projects without CRS:", countNon3DEPMissingCRS, "\n")

  # step through index files and display so we can check the CRS
  # ***** don't want to do this more than once...takes hours
  # library(mapview)
  # library(webshot)
  # for (i in 1:length(files)) {
  #   # cat(file, "...")
  #   # read layer from geopackage and check for CRS (use $hasCRS)
  #   bb <- vect(files[i], layer = "boundingbox")
  #   
  #   if (bb$assignedCRS != "") {
  #     m <- mapview(bb)
  #     mapshot(m, url = "test.html")
  #     
  #     invisible(readline(prompt = paste("Item", i, "is ready, Press [Enter] to continue...")))
  #   }
  # }
  
  # step through index files and build CSV table with boundary attributes...useful for summarizing and 
  # further analyses
  df <- data.frame()
  for (i in 1:length(files)) {
    # read layer from geopackage and check for CRS (use $hasCRS)
    bb <- vect(files[i], layer = "boundingbox")
    
    df <- rbind(df, as.data.frame(bb))
  }
  
  write.csv(df, "Documents/R6IndexEntries.csv", row.names = FALSE)
  
  df3DEP <- df[grepl("3dep", tolower(df$base)),]
  sprintf("Total size: %f Tb", sum(df$assetsize)/1024/1024/1024/1024)
  sprintf("Total 3DEP data size: %f Tb", sum(df3DEP$assetsize)/1024/1024/1024/1024)
  sprintf("Total non-3DEP data size: %f Tb", sum(df$assetsize)/1024/1024/1024/1024 - sum(df3DEP$assetsize)/1024/1024/1024/1024)
}

# found problems with 86, 91, 108, 124, 125, 173, 191 as in for (i in 1:length(files))

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
