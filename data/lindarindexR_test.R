# this flag needs to be set to FALSE when package is built. I have intentionally included this code
# file in the repo but it causes problems if the flag is set to TRUE. I run the commands manually
# for each area.
#
# ***** this is the code that actually builds the index files
#
Test <- FALSE
if (Test) {
  library(LidarIndexR)
  
  # outputFolder <- "h:\\R6_IndexFiles\\"
  # rootFolder <- "T:/FS/Reference/RSImagery/ProcessedData/r06/R06_DRM_Deliverables/PointCloud/"
  # folderList <- "data/TDrive_R6_FileList.csv"
  outputFolder <- "h:\\R10_TNF_IndexFiles\\"
  rootFolder <- "T:/FS/Reference/RSImagery/ProcessedData/r10_tnf/RSImagery/Geo/DEM/LiDAR/"
  folderList <- "data/R10_TNF_FileList.csv"
  # outputFolder <- "h:\\R10_CNF_IndexFiles\\"
  # rootFolder <- "T:/FS/Reference/RSImagery/ProcessedData/r10_cnf/RSImagery/Geo/DEM/LiDAR/"
  # folderList <- "data/R10_CNF_FileList.csv"
  slashReplacement <- "_][_"
  
  # read list of folders
  folders <- utils::read.csv(folderList, stringsAsFactors = FALSE)
  
  # drop folders with no data
  folders <- folders[folders$X..laz == 1 | folders$X..las == 1 | folders$X..zlas == 1, ]

  folders$Folder <- gsub("\\\\", "/", folders$Folder)
  
  # work through folders
  for (i in 1:nrow(folders)) {
    name <- sub(rootFolder, "", folders$Folder[i])
    
    cat("Processing", name, ":", i, "of", nrow(folders), "...")
    
    name <- gsub("/", slashReplacement, name)
    
    # work through file types
    if (folders$X..laz[i]) {
      cat("LAZ...")
      BuildAssetCatalog(folders$Folder[i], "\\.laz", outputFile = paste0(outputFolder, name, "_LAZ.gpkg"), rebuild = FALSE)
    }
    if (folders$X..las[i]) {
      cat("LAS...")
      BuildAssetCatalog(folders$Folder[i], "\\.las", outputFile = paste0(outputFolder, name, "_LAS.gpkg"), rebuild = FALSE)
    }
    # if (folders$X..zlas[i]) {
    #   cat("zLAS...")
    #   BuildAssetCatalog(folders$Folder[i], "\\.zlas", outputFile = paste0(outputFolder, name, "_zLAS.gpkg"), rebuild = FALSE)
    # }
    
    cat("Done!\n")
    
    # if (i > 2)
    #   break
  }
  
  # step through index files and build CSV table with boundary attributes...useful for summarizing and 
  # further analyses
  library(terra)
  summaryCSVFile <- "Documents/R6IndexEntries.csv"
  
  files <- list.files(outputFolder, "\\.gpkg", full.names = TRUE, ignore.case = TRUE)
  
  df <- data.frame()
  for (i in 1:length(files)) {
    # read layer from geopackage and check for CRS (use $hasCRS)
    bb <- vect(files[i], layer = "boundingbox")
    
    df <- rbind(df, as.data.frame(bb))
  }
  
  write.csv(df, summaryCSVFile, row.names = FALSE)
  
  df3DEP <- df[grepl("3dep", tolower(df$base)),]
  if (nrow(df3DEP) > 0) {
    sprintf("   Total 3DEP data size: %f Tb", sum(df3DEP$assetsize)/1024/1024/1024/1024)
    sprintf("   Total non-3DEP data size: %f Tb", sum(df$assetsize)/1024/1024/1024/1024 - sum(df3DEP$assetsize)/1024/1024/1024/1024)
  }
  print("Summary for", outputFolder, ":")
  sprintf("   Total size: %f Tb", sum(df$assetsize)/1024/1024/1024/1024)
  
}




# ignore the code below. It doesn't run and never will...
runMe <- FALSE
if (runMe) {
  # started looking at parallelizing the code to improve performance but didn't get this finished.
  # ****** needs work...how to call with row of a data frame?
  library(parallel)
  library(MASS)
  
  # function to do one folder
  doFolder <- function(folderName, baseFolder, hasLAZ, hasLAS, slashReplacement, outputFolder, rebuild) {
    name <- sub(baseFolder, "", foldeName)
    
    cat("Processing", name, ":", i, "of", nrow(folders), "...")
    
    name <- gsub("/", slashReplacement, name)
    
    # work through file types
    if (hasLAZ) {
      cat("LAZ...")
      BuildAssetCatalog(folderName, "\\.laz", outputFile = paste0(outputFolder, name, "_LAZ.gpkg"), rebuild = rebuild)
    }
    if (hasLAS) {
      cat("LAS...")
      BuildAssetCatalog(folderName, "\\.las", outputFile = paste0(outputFolder, name, "_LAS.gpkg"), rebuild = rebuild)
    }
  } 
  
  # use mclapply to run 
  mclapply
}
