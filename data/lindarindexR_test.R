R6Test <- FALSE
if (R6Test) {
  library(LidarIndexR)
  
  slashReplacement <- "_][_"
  
  # read list of folders
  folders <- utils::read.csv("../data/TDrive_R6_FileList.csv", stringsAsFactors = FALSE)
  
  # drop folders with no data
  folders <- folders[folders$X..laz == 1 | folders$X..las == 1, ]

  folders$ Folder <- gsub("\\\\", "/", folders$Folder)
  
  # work through folders
  for (i in 1:nrow(folders)) {
    name <- sub("T:/FS/Reference/RSImagery/ProcessedData/r06/R06_DRM_Deliverables/PointCloud/", "", folders$Folder[i])
    
    cat("Processing", name, ":", i, "of", nrow(folders), "...")
    
    name <- sub("/", slashReplacement, name)
    
    # work through file types
    if (folders$X..laz[i]) {
      cat("LAZ...")
      BuildAssetCatalog(f$Folder, "\\.laz", outputFile = paste0(name, "_LAZ.gpkg"))
    }
    if (folders$X..las[i]) {
      cat("LAS...")
      BuildAssetCatalog(f$Folder, "\\.las", outputFile = paste0(name, "_LAS.gpkg"))
    }

    cat("Done!\n")
  }
}

