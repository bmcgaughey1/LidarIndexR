R6Test <- FALSE
if (R6Test) {
  library(LidarIndexR)
  
  outputFolder <- "h:\\R6_IndexFiles\\"
  slashReplacement <- "_][_"
  
  # read list of folders
  folders <- utils::read.csv("data/TDrive_R6_FileList.csv", stringsAsFactors = FALSE)
  
  # drop folders with no data
  folders <- folders[folders$X..laz == 1 | folders$X..las == 1, ]

  folders$Folder <- gsub("\\\\", "/", folders$Folder)
  
  # work through folders
  for (i in 1:nrow(folders)) {
    name <- sub("T:/FS/Reference/RSImagery/ProcessedData/r06/R06_DRM_Deliverables/PointCloud/", "", folders$Folder[i])
    
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

    cat("Done!\n")
    
    # if (i > 2)
    #   break
  }
}

runMe <- FALSE
if (runMe) {
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
