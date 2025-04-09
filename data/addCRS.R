# code to deal with missing CRS for R6 lidar index files
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
# The first part of this code is specific to R6 data where CRS labels are included
# in some folder names. The second part works for any/all regions.
#
# I think this code is only useful for R6 index files. Other regions do not have
# projects labeled with CRS information or whether or not data are also held in 2DEP.
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
    c("r6albers", 9674), # NAD83 R6 albers equal area
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
    c("utm10_laz", 6339), # 6339: NAD83(2011), 26910: NAD83. 3740: NAD83(HARN)
    c("utm11_laz", 6340), # 6340: NAD83(2011), 26911: NAD83. 3741: NAD83(HARN)
    c("utm10_las", 6339), # 6339: NAD83(2011), 26910: NAD83. 3740: NAD83(HARN)
    c("utm11_las", 6340), # 6340: NAD83(2011), 26911: NAD83. 3741: NAD83(HARN)
    c("umt11_3dep", 6340) # typo in folder name
  )
  
  totalCount <- length(files)
  CRSCount <- 0
  countNon3DEPMissingCRS <- 0
  
  totalStorage <- 0
  totalNon3DEPStorage <- 0
  countNon3DEP <- 0
  
  assignCRS <- function(index, wkt) {
    bb <- vect(index, layer = "boundingbox")
    wb <- vect(index, layer = "boundary")
    ass <- vect(index, layer = "assets")
    
    if (wkt != "") {
      # assign CRS
      crs(bb) <- wkt
      crs(wb) <- wkt
      crs(ass) <- wkt
      
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
         assignCRS(file, paste0("EPSG:", prjs[[i]][2]))
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
  
  # ****************************************************************************
  # step through index files and build CSV table with boundary attributes...useful for summarizing and 
  # further analyses
  #
  # *****this is also done in the lidarIndexR_test.R file after adding CRS information
  #
  # df <- data.frame()
  # for (i in 1:length(files)) {
  #   # read layer from geopackage and check for CRS (use $hasCRS)
  #   bb <- vect(files[i], layer = "boundingbox")
  #   
  #   df <- rbind(df, as.data.frame(bb))
  # }
  # 
  # write.csv(df, "Documents/R6IndexEntries.csv", row.names = FALSE)
  # 
  # df3DEP <- df[grepl("3dep", tolower(df$base)),]
  # sprintf("Total size: %f Tb", sum(df$assetsize)/1024/1024/1024/1024)
  # sprintf("Total 3DEP data size: %f Tb", sum(df3DEP$assetsize)/1024/1024/1024/1024)
  # sprintf("Total non-3DEP data size: %f Tb", sum(df$assetsize)/1024/1024/1024/1024 - sum(df3DEP$assetsize)/1024/1024/1024/1024)
}

# old comment: several R6 projects have the wrong CRS...even after name matching and 
# reading CRS info from geoTIFF tags in point files
#
# found problems with 86, 91, 108, 124, 125, 173, 191 as in for (i in 1:length(files))
# these areas have completely wrong crs information (show up in the wrong place)


# ******************************************************************************
# the code below worked for all data that didn't have CRS information. You could
# might be able to skip the code that relies on projection information in the folder name.
#
# code to build PDAL pipelines to get CRS from first point file in each project
# PDAL info --metadata is used to write a json file with information for the files
# then the CRS is extracted from the json files and used to set the CRS for index
# files. First bit of code creates the commands to extract the metadata. These commands
# need to be run in a python prompt with PDAL installed. Once run, the next block of code
# reads the json outputs and gets the CRS, and assigns it to the index.
#
# code uses the assignCRS function duplicated from the code above
#
if (FALSE) {
  library(terra)
  library(tools)
  library(jsonlite)
  
  assignCRS <- function(index, wkt) {
    bb <- vect(index, layer = "boundingbox")
    wb <- vect(index, layer = "boundary")
    ass <- vect(index, layer = "assets")
    
    if (wkt != "") {
      # assign CRS
      crs(bb) <- wkt
      crs(wb) <- wkt
      crs(ass) <- wkt
      
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

  folder <- "H:/R6_IndexFiles"
  folder <- "h:/R10_TNF_IndexFiles"
  folder <- "h:/R10_CNF_IndexFiles"
  folder <- "H:/R3_IndexFiles"
  
  commandFile <- "data/PDAL_commands.bat"
  
  files <- list.files(folder, "\\.gpkg", full.names = TRUE, ignore.case = TRUE)
  
  # delete the existing command file to create a new one
  if (!file.exists(commandFile)) {
    # open command file...write
    cmdFile <- file(commandFile, "wt")
    
    for (file in files) {
      # read layer from geopackage and check for CRS (use $hasCRS)
      bb <- vect(file, layer = "boundingbox")
  
      # check for CRS    
      if (!bb$hasCRS) {
        # read tile assets
        ass <- vect(file, layer = "assets")
        
        # write PDAL command line to read header
        writeLines(paste0("pdal info ", shQuote(ass$filespec[1]), " --metadata>", folder, "/", file_path_sans_ext(basename(ass$filespec[1])), ".json"), cmdFile)
      }
    }
    close(cmdFile)
  }
  
  # *****************************
  # ***** Run the PDAL commands!!
  # *****************************
  
  # read json files to get CRS
  for (file in files) {
    # read layer from geopackage and check for CRS (use $hasCRS)
    bb <- vect(file, layer = "boundingbox")
    
    # check for CRS    
    if (!bb$hasCRS) {
      # read tile assets
      ass <- vect(file, layer = "assets")
      
      # check for json file
      jsonFile <- paste0(folder, "/", file_path_sans_ext(basename(ass$filespec[1])), ".json")
      if (file.exists(jsonFile) && file.info(jsonFile)$size > 0) {
        df <- fromJSON(jsonFile)
        if (!is.null(df$metadata$srs$compoundwkt)) {
          if (df$metadata$srs$compoundwkt == "") {
            cat("CRS (compoundwkt) is empty for:", file_path_sans_ext(basename(file)), "\n")
          } else {
            assignCRS(file, df$metadata$srs$compoundwkt)
          }
        } else {
          cat("No CRS (compoundwkt) string in json file:", file_path_sans_ext(basename(ass$filespec[1])), "\n")
        }
      } else {
        cat("No json file", jsonFile, "or file is empty. Did you run the PDAL commands?\n")
      }
    }
  }
  
  # count files that still don't have CRS
  cnt <- 0
  #file <- files[153]
  for (file in files) {
    # read layer from geopackage and check for CRS (use $hasCRS)
    bb <- vect(file, layer = "boundingbox")
    
    # check for CRS    
    if (!bb$hasCRS && bb$assignedCRS == "") {
      cnt <- cnt + 1
    }
  }
  cat("Files without CRS info:", cnt, "\n")  
  
  # *****************************
  # ***** This logic doesn't work for projects that have no CRS in point files...you 
  # ***** can use the code in lines 16-124 for R6 to use the projection information in the folder name
  # *****************************
}