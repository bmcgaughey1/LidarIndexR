# code derived from older code to build project and tile index files for 
# R6 lidar data collection
#
# The data are stored on NAS on the FS network so all paths to files
# are relative to the root of the NAS. Users of the indexes will need to
# prepend the drive letter for the NAS as mapped on their computer.
#
# *****************************************************************************
# Functions
# *****************************************************************************
# this function is needed to read unsigned long data types
# unsignedFourByteIntToDouble <- function(i) {
#   d <- as.numeric(i)
#   d[d<0] <- d[d<0] + 2^32
#   d
# }

# ---------- ReadLocalLASInfo
#
#' LidarIndexR -- Read LAS/LAZ header info and CRS information
#'
#' Read the header information for a LAS/LAZ file including CRS information.
#' Only the file header is read (including VLRs) so you don't have to worry 
#' about the size of the LAS/LAZ file.
#'
#' @param path Path for a LAS/LAZ file.
#' @param quiet Boolean to control display of status information. If TRUE,
#'   information is *not* displayed. Otherwise, status information is displayed.
#' @return A data frame (invisible) containing the CRS WKT information string and
#'   header info. If file has no CRS information, the crs column in the data frame
#'   will be an empty string.
#' @examples
#' \dontrun{
#' ReadLocalLASInfo()
#' }
#' @export
ReadLocalLASInfo <- function (
    path,
    quiet = TRUE
) {
  crs <- ""
  hdf <- data.frame()
  
  if (file.exists(path)) {
    # get file size
    filesize <- file.info(path)$size
    
    # read header
    # lidR (rlas) strips off VLRs related to compression and COPC so you can't
    # tell if a file is compressed or COPC without relying on the file extensions
    #
    # lidR offers as las_is_compressed() function but it only works when you read
    # data from the file (not just the header)
    t <- tryCatch(lidR::readLASheader(path), error = function(e) {NA})
    if (is.object(t)) {
      crs <- lidR::st_crs(t)
      
      # check for bad crs
      if (is.na(crs$wkt)) {
        crs <- ""
      } else {
        crs <- crs$wkt
      }
      
      # build data frame with header info
      #
      # there should be a better way to deterimine if file is compressed or copc
      # lidR has is_file_compressed() but it only takes an LAS object so you would
      # have to read the entire file. EVLR has a 'copc' identifier but it doesn't
      # look like lidR is ready EVLRs
      hdf <- data.frame(
        "filespec" = path,
        "filesize" = filesize,
        #"filename" = basename(path),
        "pointcount" = t@PHB$`Number of point records`,
        "compressed" = grepl(".laz", tolower(basename(path))),
        "copc" = grepl(".copc.laz", tolower(basename(path))),
        "creation_day" = t@PHB$`File Creation Day of Year`,
        "creation_year" = t@PHB$`File Creation Year`,
        "point_record_format" = t@PHB$`Point Data Format ID`,
        "point_record_length" = t@PHB$`Point Data Record Length`,
        "major_version" = t@PHB$`Version Major`,
        "minor_version" = t@PHB$`Version Minor`,
        #"HeaderSize" = HeaderSize,
        #"VLRCount" = VLRCount,
        "minx" = t@PHB$`Min X`,
        "miny" = t@PHB$`Min Y`,
        "minz" = t@PHB$`Min Z`,
        "maxx" = t@PHB$`Max X`,
        "maxy" = t@PHB$`Max Y`,
        "maxz" = t@PHB$`Max Z`,
        "crs" = crs
      )
      
    } else {
      if (!quiet) cat("Invalid LAS/LAZ file:", path, "\n")
    }
  } else {
    if (!quiet) cat("File does not exist:", path, "\n")
  }
  
  if (!quiet) {
    print(hdf)
  }
  
  return(invisible(hdf))
}

# ---------- ReadLocalLASProjection
#
#' LidarIndexR -- Read LAS/LAZ header and extract CRS information
#'
#' Read the header for a LAS/LAZ file and read CRS information from the header.
#' Only the file header is read (including VLRs) so you don't have to worry 
#' about the size of the LAS/LAZ file.
#'
#' @param path Path for a LAS/LAZ file.
#' @param quiet Boolean to control display of status information. If TRUE,
#'   information is *not* displayed. Otherwise, status information is displayed.
#' @return A string (invisible) containing the CRS WKT information retrieved from the LAS/LAZ
#'   header. Returns an empty string if file has no CRS information.
#' @examples
#' \dontrun{
#' ReadLocalLASProjection()
#' }
#' @export
ReadLocalLASProjection <- function (
    path,
    quiet = TRUE
) {
  crs <- ""
  
  if (file.exists(path)) {
    # read header
    t <- tryCatch(lidR::readLASheader(path), error = function(e) {NA})
    if (is.object(t)) {
      crs <- lidR::st_crs(t)
      
      # check for bad crs
      if (is.na(crs$wkt)) {
        crs <- ""
      } else {
        crs <- crs$wkt
      }
    } else {
      if (!quiet) cat("Invalid LAS/LAZ file:", path, "\n")
    }
  } else {
    if (!quiet) cat("File does not exist:", path, "\n")
  }

  if (!quiet) print(crs)
  
  return(invisible(crs))
}

# ---------- ReadLocalLASHeader
#
#' LidarIndexR -- Read the header for a local LAS/LAZ file
#'
#' Read the header information for a LAS/LAZ file including CRS information.
#' Only the file header is read (including VLRs) so you don't have to worry 
#' about the size of the LAS/LAZ file. *Does not rely on the lidR package.*
#'
#' @param path Path for a LAS/LAZ file.
#' @param useVLRs Read VLRs to determine if file is compressed and COPC format.
#' @param quiet Boolean to control display of status information. If TRUE,
#'   information is *not* displayed. Otherwise, status information is displayed.
#' @return A data frame (invisible) containing the CRS WKT information string and
#'   header info. If file has no CRS information, the crs column in the data frame
#'   will be an empty string.
#' @examples
#' \dontrun{
#' ReadLocalLASHeader()
#' }
#' @export
ReadLocalLASHeader <- function(
  path,
  useVLRs = TRUE,
  quiet = TRUE
) {
  crs <- ""
  df <- data.frame
  copc <- FALSE
  compressed <- FALSE
  
  if (file.exists(path)) {
    # get file size
    filesize <- file.info(path)$size
    
    # open file and read header...value by value
    con = file(path, open = "rb")
    if (isOpen(con)) {
      Signaturebytes <- readBin(con, "raw", n = 4, size = 1, endian = "little")

      Signature <- readBin(Signaturebytes, "character", size = 4, endian = "little")
      if (Signature == "LASF") {
        if (!quiet) print(paste("Reading header for ", basename(path)))
        readBin(con, "raw", 4) # skip bytes
        readBin(con, "raw", 16) # skip bytes
        VersionMajor <- readBin(con, "integer", size = 1, n = 1, signed = FALSE)
        VersionMinor <- readBin(con, "integer", size = 1, n = 1, signed = FALSE)
        readBin(con, "raw", 64) # skip bytes
        DayOfYear <- readBin(con, "int", n = 1, size = 2, signed = FALSE)
        Year <- readBin(con, "integer", n = 1, size = 2, signed = FALSE)
        HeaderSize <- readBin(con, "integer", n = 1, size = 2, signed = FALSE)
        readBin(con, "raw", 4) # skip bytes
        VLRCount <- readBin(con, "integer", n = 1, size = 4)
        VLRCount <- unsignedFourByteIntToDouble(VLRCount)
        PointRecordFormat <- readBin(con, "integer", n = 1, size = 1, signed = FALSE)
        if (PointRecordFormat > 127) PointRecordFormat <- (PointRecordFormat - 128)
        PointRecordLength <- readBin(con, "int", 1, size = 2, signed = FALSE)
        PointCount <- readBin(con, "integer", 1, size = 4)
        PointCount <- unsignedFourByteIntToDouble(PointCount)
        readBin(con, "raw", 68) # skip bytes
        MaxX <- readBin(con, "numeric", 1, size = 8)
        MinX <- readBin(con, "numeric", 1, size = 8)
        MaxY <- readBin(con, "numeric", 1, size = 8)
        MinY <- readBin(con, "numeric", 1, size = 8)
        MaxZ <- readBin(con, "numeric", 1, size = 8)
        MinZ <- readBin(con, "numeric", 1, size = 8)
        if (VersionMajor == 1 && VersionMinor > 3) {
          readBin(con, "raw", 20) # skip bytes
          PointCount <- readBin(con, "integer", 1, size = 8)
        }
        
        if (useVLRs) {
          # close file and reopen...seek is not recommended on windows
          close(con)
          con = file(path, open = "rb")
          
          # read to start of VLRs
          readBin(con, "raw", HeaderSize) # skip bytes
          
          # read VLRs looking for user id 'copc' or 'laszip encoded' in User ID
          for (i in 1:VLRCount) {
            readBin(con, "raw", 2) # skip bytes...reserved
            b <- readBin(con, "raw", n = 16, size = 1, endian = "little")
            userid <- readBin(b, "character", size = 16, endian = "little")
            if (userid == 'copc') copc <- TRUE
            if (userid == 'laszip encoded') compressed <- TRUE
            
            recordID <- readBin(con, "integer", n = 1, size = 2, signed = FALSE)
            
            vlrLength <- readBin(con, "integer", n = 1, size = 2, signed = FALSE)
            b <- readBin(con, "raw", n = 32, size = 1, endian = "little")
            desc <- readBin(b, "character", size = 32, endian = "little")
            if (userid == 'LASF_Projection' && recordID == 2112) {
              # read WKT  
              b <- readBin(con, "raw", n = vlrLength, size = 1, endian = "little")
              crs <- readBin(b, "character", size = vlrLength, endian = "little")
            } else
              readBin(con, "raw", vlrLength)    # skip description and VLR content
            
            #print(paste(userid, recordID, desc, vlrLength))
          }
        } else {
          compressed <- grepl(".laz", tolower(basename(path)))
          copc <- grepl(".copc.laz", tolower(basename(path)))
        }
      }
      close(con)
      
      # build data frame to return
      df <- data.frame(
        "filespec" = path,
        "filesize" = filesize,
        #"filename" = basename(path),
        "pointcount" = PointCount,
        #"compressed" = grepl(".laz", tolower(basename(path))),
        #"copc" = grepl(".copc.laz", tolower(basename(path))),
        "compressed" = compressed,
        "copc" = copc,
        "creation_day" = DayOfYear,
        "creation_year" = Year,
        "point_record_format" = PointRecordFormat,
        "point_record_length" = PointRecordLength,
        "major_version" = VersionMajor,
        "minor_version" = VersionMinor,
        #"HeaderSize" = HeaderSize,
        #"VLRCount" = VLRCount,
        "minx" = MinX,
        "miny" = MinY,
        "minz" = MinZ,
        "maxx" = MaxX,
        "maxy" = MaxY,
        "maxz" = MaxZ,
        "crs" = crs
      )
    } else {
      stop(paste("File:", path, "could not be opened!!"))
    }
    
    # df <- data.frame(
    #   "path" = path,
    #   "FileName" = basename(path),
    #   "LASVersion" = VersionMajor + VersionMinor / 10,
    #   "FileDayOfYear" = DayOfYear,
    #   "FileYear" = Year,
    #   "HeaderSize" = HeaderSize,
    #   "VLRCount" = VLRCount,
    #   "PointRecordFormat" = PointRecordFormat,
    #   "PointRecordLength" = PointRecordLength,
    #   "PointCount" = PointCount,
    #   "MinX" = MinX,
    #   "MinY" = MinY,
    #   "MinZ" = MinZ,
    #   "MaxX" = MaxX,
    #   "MaxY" = MaxY,
    #   "MaxZ" = MaxZ
    # )
  } else {
    stop(paste("File:", path, "doesn't exist!!"))
  }
  
  if (!quiet) {
    print(df)
  }
  
  return(invisible(df))
}

# ---------- BuildIndexFromLocalPoints
#
#' LidarIndexR -- Create spatial index by reading LAS/LAZ file headers
#'
#' Reads LAS/LAZ file headers to build bounding boxes for each tile and
#' stores header attributes with each bounding box. Optionally writes
#' information to \code{outputFile}. Typically used by passing a folder
#' name in \code{basePath} and a file extension template such as "//.las|//.laz".
#' You can also specify folders under the \code{basePath} using \code{folderName}
#' and \code{pointFolder}. You can omit all folder information and provide a 
#' list of point files in \code{fullFileList}. CRS information is read from 
#' point files. If you provide \code{projString}, it will override the CRS
#' found in the point files.
#' 
#' @param basePath Path for a folder. Trailing slash should *not*
#'   be included.
#' @param fileType Any valid string for the pattern parameter in \code{grep()}.
#'   "$" will be appended to the string to search for file/folder names ending
#'   with values in \code{fileType}.
#' @param folderName Folder name on the \code{basePath} containing LAS/LAZ files.
#'   Can be an empty string.
#' @param pointFolder Folder under \code{folderName} containing point files. Can be
#'   an empty string.
#' @param projString A valid projection string that can be used with the \code{crs}
#'   parameter in \code{st_sf}. \code{projString} should represent the projection
#'   pf the point data. If using EPSG codes, do not enclose the EPSG number in quotes.
#' @param outputCRS A valid projection string that can be used with the \code{crs}
#'     parameter in \code{st_transform} to reproject the index.
#' @param fullFileList List of point tiles. Will be used instead of generating 
#'   new list.
#' @param dimensionThreshold Size threshold used to omit files from the index.
#'   This is intended to help omit invalid LAS/LAZ files from the index. If
#'   the height or width of the point tile exceeds the threshold, the tile
#'   will be omitted.
#' @param headerMethod String indicating the method that should be used to
#'   read LAS file headers. Choices are: "direct", "lidr", or "rcpp".
#' @param outputFile Full path and filename on the local file system for the index
#'   file.
#' @param rebuild Boolean. If TRUE, the index is always created. If FALSE,
#'   the index is only created if it does not already exist.
#' @param quiet Boolean to control display of status information. If TRUE,
#'   information is *not* displayed. Otherwise, status information is displayed.
#' @return An \code{sf} object (invisible) containing tile bounding boxes and attributes.
#' @examples
#' \dontrun{
#' BuildIndexFromPoints()
#' }
#' @export
BuildIndexFromLocalPoints <- function (
  basePath = "",
  fileType = "\\.las|\\.laz",
  folderName = "",
  pointFolder = "",
  projString = NA,
  outputCRS = NA,
  fullFileList = character(0),
  dimensionThreshold = 50000,
  headerMethod = "rcpp",        #direct: binary read of header, lidr: use lidR package
  outputFile = NULL,
  rebuild = FALSE,
  quiet = FALSE
) {
  if (!is.null(outputFile)) {
    if (file.exists(outputFile) && !rebuild) {
      if (!quiet) cat("Index already exist...skipping: ", basename(outputFile),"\n")
      return(TRUE)
    }
  }
  
  if (folderName == "" && pointFolder != "") {
    if (!quiet) cat("folderName cannot be blank when pointFolder is not blank\n")
    return(FALSE)
  }
  
  if (pointFolder == "" && folderName == "") {
    folderPath <- basePath
  } else if (pointFolder == "") {
    folderPath <- paste0(basePath, "/", folderName)
  } else {
    folderPath <- paste0(basePath, "/", folderName, "/", pointFolder)
  }
  
  # we only need to get a list of files if fullFileList == NA
  if (length(fullFileList) == 0) {
    # get list of .laz & .las files...full directory info
    flist <- LocalDirList(folderPath, fileType = fileType)
  } else {
    flist <- fullFileList
  }
  
  if (length(flist) > 0) {
    if (!quiet) cat("Building index...")
    
    if (length(fullFileList) == 0) {
      # prepend folder path to file names
      fileURLs <- paste0(folderPath, "/", flist)
    } else {
      fileURLs <- flist
    }
    
    # read headers
    if (headerMethod == 'direct')
      t <- lapply(fileURLs, ReadLocalLASHeader, quiet = quiet)    # returns a list of dataframes
    else if (headerMethod == 'lidr')
      t <- lapply(fileURLs, ReadLocalLASInfo, quiet = quiet)    # returns a list of dataframes
    else
      t <- lapply(fileURLs, ReadLASHeader)    # returns a list of dataframes
    
    if (length(t)) {
      # convert to a simple dataframe
      t_df <- do.call("rbind", t)
      
      # drop any rows with NA values...bad LAS file...only check min/max XYZ values
      t_df <- t_df[stats::complete.cases(t_df[, 12:17]), ]
      
      # drop rows where width or height is >dimensionThreshold units
      t_df <- t_df[((t_df$maxx - t_df$minx) < dimensionThreshold & (t_df$maxy - t_df$miny) < dimensionThreshold), ]
      
      # if we don't have projection info, try to get it from the first point file
      if (is.na(projString)) {
        projString <- t_df$crs[1]
      }
      
      # create sf set of tile polygons
      if (nrow(t_df) > 0) {
        lst <- lapply(1:nrow(t_df), function(x) {
          # create a matrix of coordinates that also 'close' the polygon
          res <- matrix(c(t_df[x, 'minx'], t_df[x, 'miny'],
                          t_df[x, 'minx'], t_df[x, 'maxy'],
                          t_df[x, 'maxx'], t_df[x, 'maxy'],
                          t_df[x, 'maxx'], t_df[x, 'miny'],
                          t_df[x, 'minx'], t_df[x, 'miny'])  ## need to close the polygon
                        , ncol =2, byrow = TRUE
          )
          # create polygon objects
          sf::st_polygon(list(res))
        }
        )
        
        tiles_sf <- sf::st_sf(t_df, sf::st_sfc(lst), crs = projString)
        
        # reproject to outputCRS
        if (!is.na(projString) && !is.na(outputCRS)) {
          tiles_sf <- sf::st_transform(tiles_sf, crs = outputCRS)
        }
        
        if (!quiet) cat("Done!!\n")
        
        # write output
        if (!is.null(outputFile))
          sf::st_write(tiles_sf, outputFile, delete_dsn = TRUE, quiet = TRUE)
        
        return(invisible(tiles_sf))
      } else {
        cat("   ***Could not read LAS file headers\n")
      }
    } else {
      cat("   ***No LAS polygons\n")
    }
  } else {
    cat("   ***No LAS files\n")
  }
  
  return(invisible(data.frame()))
}

# ---------- ExtractCRSFromLocalPoints
#
#' LidarIndexR -- Retrieve CRS information from a LAS/LAZ point file
#'
#' Reads CRS information from local LAS/LAZ files. This is done by
#' reading only the file header \code{headerOnly = TRUE} or retrieving the entire
#' file and reading the header \code{headerOnly = FALSE}. 
#'
#' @param basePath Path for a folder on a remote host. Trailing slash should *not*
#'   be included.
#' @param folderName Folder name on the \code{basePath} containing LAS/LAZ
#'   files.
#' @param headerOnly Boolean. If TRUE, only the file header is read. If FALSE,
#'   the entire LAS/LAZ file is retrieved and then the header is read to 
#'   extract CRS information.
#' @param quiet Boolean to control display of status information. If TRUE,
#'   information is *not* displayed. Otherwise, status information is displayed.
#' @param ... Arguments passed to \code{RCurl::getURL()}.
#' @return String (invisible) containing a valid input value for \code{st_crs()}.
#' @examples
#' \dontrun{
#' FetchAndExtractCRSFromPoints()
#' }
#' @export
ExtractCRSFromLocalPoints <- function (
  basePath,
  folderName,
  headerOnly = TRUE,
  quiet = FALSE,
  ...
) {
  crs <- ""
  
  t <- paste0(basePath, "/", folderName, "/")
  
  # build directory list...first file will be smallest
  fileList <- LocalDirList(t, fileType = "\\.las|\\.laz")
  
  # download first file
  if (length(fileList)) {
    if (!quiet) cat(folderName, "/", fileList$Name[1], "--")
    if (headerOnly) {
      crs <- ReadLocalLASProjection(paste0(t, fileList$Name[1]), quiet = TRUE)
    } else {
      # load file and extract CRS (if any)
      las <- tryCatch(lidR::readLASheader(paste0(t, fileList$Name[1])), error = function(e) {NA})
      if (is.object(las)) {
        crs <- lidR::st_crs(las)
        
        # check for bad crs but geotiff VLR records
        if (is.na(crs$wkt)) {
          crs <- ""
          # flag <- tryCatch(grepl("NAD_1983_USFS_R6_Albers", las@VLR$GeoAsciiParamsTag$tags[1]), error = function(e) {NA})
          # if (length(flag)) {
          #   if (!is.na(flag)) {
          #     if (flag)
          #       crs <- "+proj=aea +lat_0=34 +lon_0=-120 +lat_1=43 +lat_2=48 +x_0=600000 +y_0=0 +datum=NAD83 +units=m +vunits=m +no_defs" #R6Albers
          #   }
          # }
        } else {
          crs <- crs$wkt
        }
      } else {
        crs <- ""
      }
    }
  }
  if (!quiet) {
    cat(basename(basePath), ":", crs, "\n")
  }
  
  return(invisible(crs))
}

# ---------- ExtractCRSFromLocalIndex
#
#' LidarIndexR -- Retrieve a shapefile (all files) and read CRS information
#'
#' Retrieves all files related to a shapefile (same name as \code{fileName} but
#' different extensions) and then reads CRS information from the projection 
#' file.
#'
#' @param basePath Path for a folder on a remote host. Trailing slash should *not*
#'   be included.
#' @param folderName Folder name on the \code{basePath} containing the shapefile.
#' @param fileName A string containing the name of a single file. All
#'   files with the same name (but different extensions) will be retrieved. The 
#'   projection file is then read to provide CRS information.
#' @param quiet Boolean to control display of status information. If TRUE,
#'   information is *not* displayed. Otherwise, status information is displayed.
#' @param ... Arguments passed to \code{RCurl::getURL()}.
#' @return String (invisible) containing a valid input value for \code{st_crs()}.
#' @examples
#' \dontrun{
#' ExtractCRSFromLocalIndex("ftp://rockyftp.cr.usgs.gov/vdelivery/Datasets/",
#'     "Staged/Hydrography/NHD/HU4/HighResolution/Shape/",
#'     "NHD_H_0101_HU4_Shape.shp",
#'     ".")
#' }
#' @export
ExtractCRSFromLocalIndex <- function (
  basePath,
  folderName,
  fileName,
  quiet = FALSE,
  ...
) {
  crs <- ""
  
  t <- paste0(basePath, "/", folderName, "/")
  
  # this returns a list of files with the same name but different extensions
  filenames <- LocalDirListByName(t, fileName)
  
  # get all the files associated with the index shapefile
  if (length(filenames)) {
    # load index file and extract CRS (if any)
    index <- tryCatch(sf::st_read(paste0(t, fileName)), error = function(e) {NA})
    if (is.object(index)) {
      crs <- lidR::st_crs(index)
      crs <- crs$wkt
    } else {
      crs <- ""
    }
  }
  if (!quiet) {
    cat(basename(basePath), ":", crs, "\n")
  }
  
  return(invisible(crs))
}

# ---------- ExtractCRSFromLocalPrj
#
#' LidarIndexR -- Retrieve a projection file and read CRS information
#'
#' Retrieves only the projection file related to a shapefile (same name as 
#' \code{fileName} but with .prj extensions) and then reads CRS information 
#' from the projection file.
#'
#' @param basePath Path for a folder on a remote host. Trailing slash should *not*
#'   be included.
#' @param folderName Folder name on the \code{basePath} containing the shapefile.
#' @param fileName A string containing the name of a single file. The projection
#'   file with the same name (but extension of .prj) will be retrieved. The 
#'   projection file is then read to provide CRS information.
#' @param quiet Boolean to control display of status information. If TRUE,
#'   information is *not* displayed. Otherwise, status information is displayed.
#' @param ... Arguments passed to \code{RCurl::getURL()}.
#' @return String (invisible) containing a valid input value for \code{st_crs()}.
#' @examples
#' \dontrun{
#' FetchAndExtractCRSFromPrj("ftp://rockyftp.cr.usgs.gov/vdelivery/Datasets/",
#'     "Staged/Hydrography/NHD/HU4/HighResolution/Shape/",
#'     "NHD_H_0101_HU4_Shape.prj",
#'     ".")
#' }
#' @export
ExtractCRSFromLocalPrj <- function (
  basePath,
  folderName,
  fileName,             # must be .prj file
  quiet = FALSE,
  ...
) {
  crs <- ""
  
  t <- paste0(basePath, "/", folderName)
  cat (t, "\n")
  
  # # this returns a list of files with the same name but different extensions
  # filenames <- LocalDirListByName(t, fileName)
  # cat(filenames)
  # 
  # # we just need the .prj file
  # filenames <- grep(paste0(".prj", "$"), filenames, ignore.case=TRUE, value=TRUE)
  # cat(filenames)
  # 
  # # get all the files associated with the index shapefile
  # if (length(filenames)) {
    f <- file(file.path(t, fileName), open = "rt")
    cat("Opening:", file.path(t, fileName), "\n")
    if (isOpen(f)) {
      l <- readLines(f, n = -1, warn = FALSE)
      close(f)
      cat(l, "\n")
      
      crs <- sp::CRS(l)@projargs
    } else {
      crs <- ""
    }
#  }
  if (!quiet) {
    cat(basename(basePath), ":", crs, "\n")
  }
  
  return(invisible(crs))
}

# syntax for fileType can be any valid grep pattern but the "$" will be appended to
# search for entries ending with the pattern
# e.g. "\\.las|\\.laz" will return all las and laz files

# ---------- LocalDirList
#
#' LidarIndexR -- Retrieve a directory listing
#'
#' Retrieve a directory listing from a local file system as either a simple list
#' of file/folder names or a data frame with file/folder information.
#'
#' @param path Path for a folder. Trailing slash is optional.
#' @param fileType Any valid string for the pattern parameter in \code{grep()}.
#'   "$" will be appended to the string to search for file/folder names ending
#'   with values in \code{fileType}.
#' @param namesOnly Boolean indicating you want only file names (TRUE) or all
#'   directory information (FALSE).
#' @param directoryOnly Boolean indicating you only want folder names included
#'   in the list of returned files/folders.
#' @param ... Arguments passed to \code{RCurl::getURL()}. This is ignored
#'   if the path uses http or https schemes.
#' @return A list of file names (invisible) or a data frame (invisible) with file names and attribute
#'   information. The return value depends on \code{namesOnly}.
#' @examples
#' \dontrun{
#' DirList("vdelivery/Datasets/Staged/")
#' }
#' @export
LocalDirList <- function (
  path,
  fileType = NULL,
  namesOnly = TRUE,
  directoryOnly = FALSE,
  ...
) {
  # get folder listing and parse into individual files
  # create an empty character vector on error
  if (directoryOnly) {
    filenames <- list.dirs(path, full.names = FALSE, recursive = FALSE)
  } else {
    filenames <- list.files(path, pattern = fileType)    
  }
  
  if (length(filenames)) {
    # if not getting directories, get file size and sort
    if (!directoryOnly) {
      # create a dataframe and sort on the attribute (size for files)
      if (!namesOnly) {
        # build full paths to filenames
        tnames <- paste0(path, "/", filenames)
        
        # get file info
        df <- file.info(tnames, extra_cols = FALSE)
        
        # add filenames
        df$FileName <- filenames
        
        # drop row labels
        rownames(df) <- NULL
        
        df <- df[, c(7, 1:6)]

        return(invisible(df))
      }
    }
  }
  return(invisible(filenames))
}

# ---------- LocalDirListByType
#
#' LidarIndexR -- Retrieve a directory listing
#'
#' Retrieve a directory listing from a local file system as a simple list
#' of file names.
#' 
#' DirListByType is an alias for \code{DirList(path, fileType = fileType, namesOnly = TRUE, ...)}
#'
#' @param path Path for a folder on a remote host. Trailing slash is optional.
#' @param fileType Any valid string for the pattern parameter in \code{grep()}.
#'   "$" will be appended to the string to search for file/folder names ending
#'   with values in \code{fileType}.
#' @param ... Arguments passed to \code{RCurl::getURL()}. This is ignored
#'   if the path uses http or https schemes.
#' @return A list of file names (invisible).
#' @examples
#' \dontrun{
#' DirListByType(paste0("ftp://rockyftp.cr.usgs.gov/vdelivery/Datasets/",
#'     "Staged/Elevation/LPC/Projects/AK_BrooksCamp_2012/laz/"),
#'     "\\.las|\\.laz")
#' }
#' @export
LocalDirListByType <- function (
  path,
  fileType,
  ...
) {
  # get folder listing and parse into individual files
  # create an empty character vector on error
  filenames <- list.files(path, pattern = fileType)
  return(invisible(filenames))
}

# ---------- LocalDirListByName
#
#' LidarIndexR -- Retrieve a directory listing of all files
#'   with a specific name (various extensions)
#'
#' Retrieve a simple list of all files with the specified name but various
#' extensions.
#'
#' @param path Path for a folder. Trailing slash is optional.
#' @param fileName A string containing the name of a single file. All files
#'   with the same name (but different extensions) will be returned.
#' @param ... Arguments passed to \code{RCurl::getURL()}.
#' @return A list of file names (invisible).
#' @examples
#' \dontrun{
#' DirListByName(paste0("ftp://rockyftp.cr.usgs.gov/vdelivery/Datasets/",
#'     "Staged/Hydrography/NHD/HU4/HighResolution/Shape/"),
#'     "NHD_H_0101_HU4_Shape.jpg")
#' }
#' @export
LocalDirListByName <- function (
  path,
  fileName,
  ...
) {
  filenames <- list.files(paste0(path), pattern = paste0(fileName, ".*"))
  return(invisible(filenames))
}

# *****************************************************************************
# Collect information about the folders and files
# *****************************************************************************
if (FALSE) {
  setwd("G:/R_Stuff/R6Server")
  
  DriveLetter <- "Z"
  ServerRoot <- paste0(DriveLetter, ":", "/Lidar/Vendor_Deliverables_by_Project")
  
  BaseFolder <- "G:/R_Stuff/LIDAR_Data/R6Server"
  MainIndexFolder <- "Index"
  ProjectIndexFolder <- "ProjectIndex"
  WMProjectIndexFolder <- "ProjectIndex_WebMercator"
  TempFolder <- "Temp"
  NewIndexFile <- "Main_Index.gpkg"
  MergedTileIndexFile <- "Tile_Index.gpkg"
  MergedProjectIndexFile <- "Project_Index.gpkg"
  
  dir.create(BaseFolder, recursive = TRUE)
  dir.create(file.path(BaseFolder, MainIndexFolder))
  dir.create(file.path(BaseFolder, ProjectIndexFolder))
  dir.create(file.path(BaseFolder, WMProjectIndexFolder))
  dir.create(file.path(BaseFolder, TempFolder))
  
  commonProjection <- CRS(SRS_string="EPSG:3857")
  R6Albers <- ExtractCRSFromLocalPrj("G:/R_Stuff", "R6Server", "R6Albers.prj")
  
  DriveLetter <- "Z"
  ServerRoot <- paste0(DriveLetter, ":", "/Lidar/Vendor_Deliverables_by_Project")
  
  # test code just to make sure functions are working
  # LocalDirList(ServerRoot, directoryOnly = FALSE, namesOnly = FALSE)
  # LocalDirList(paste0(ServerRoot, "/COL_2014/1_LAZ"), fileType = "\\.las|\\.laz", directoryOnly = FALSE, namesOnly = TRUE)
  # LocalDirListByType(paste0(ServerRoot, "/COL_2014/1_LAZ"), fileType = "\\.las|\\.laz")
  # LocalDirListByName(paste0(ServerRoot, "/COL_2014/3_Vectors"), fileName = "OLC_COLVILLE_AOI_USFS_R6_2011")
  
  # Basic structure is a set of directories under ServerRoot that represent lidar projects. Some projects have been
  # combined. These have "_see_" in their directory name and the directory is empty. Most projects have a "fixed"
  # directory structure but a few projects have point data in the root folder for the project (river corridors).
  #
  # There are a few directories that do not have valid data: 1_Folders_Template, Tile_Index, and z_metadata_QC_misc.
  #
  # The "fixed" folder structure looks like this:
  # 1_LAZ
  # 1b_LAS
  # 2_DTM
  # 3_Vectors
  # 4_Report
  # 5_Other_Vendor_Products
  # 6_Fusion_dtm
  #
  # Basic idea is to start in the ServerRoot directory and get a list of all project folders (remove those without valid data).
  # Then check the structure in the folder. Start by looking for .laz/.las files then look for folders. If there are no files
  # or folders, directory isn't a valid project. Once we have a list of valid project folders, build tile index files for each.
  
  # get list of project folders
  projectFolders <- LocalDirList(ServerRoot, directoryOnly = TRUE, namesOnly = TRUE)
  
  # remove known invalid folders
  projectFolders <- projectFolders[!grepl("1_Folders_Template|Tile_Index|z_metadata_QC_misc", projectFolders)]
  
  # look for las/laz files in project folder
  havePoints <- lapply(projectFolders, function(x){
    # look for las/laz files
    f <- LocalDirListByType(paste0(ServerRoot, "/", x), fileType = "\\.las|\\.laz")
    
    return(length(f) > 0)
  })
  
  # look for 1_LAZ and/or 1b_LAS folders
  haveLAZ <- lapply(projectFolders, function(x){
    t <- LocalDirList(paste0(ServerRoot, "/", x), directoryOnly = TRUE)
    return(any(grepl("1_LAZ", t)))
  })
  
  haveLAS <- lapply(projectFolders, function(x){
    t <- LocalDirList(paste0(ServerRoot, "/", x), directoryOnly = TRUE)
    return(any(grepl("1b_LAS", t)))
  })
  
  # build data frame with information
  projects <- data.frame("Project" = projectFolders
                         , "havePoints" = unlist(havePoints)
                         , "haveLAZ" = unlist(haveLAZ)
                         , "haveLAS" = unlist(haveLAS)
                         )
  
  # write results
  saveRDS(projects, "R6ProjectData_0.rds")
  projects <- readRDS("R6ProjectData_0.rds")
  
  # correction for WAW_WIL_HJA_2020_BakerCity_MorganNesbitt_SheepCreek...has 1_LAZ folder
  # but it doesn't have any files
  # for DES_2017_PoleCreek, there is a 1b_LAS folder but no files
  projects$haveLAZ[which(projects$Project == "WAW_WIL_HJA_2020_BakerCity_MorganNesbitt_SheepCreek")] <- FALSE
  projects$haveLAS[which(projects$Project == "DES_2017_PoleCreek")] <- FALSE
  
  # write results
  saveRDS(projects, "R6ProjectData_1.rds")
  projects <- readRDS("R6ProjectData_1.rds")
  
  # work through the projects and identify 1 LAS file from which we can get projection info
  # and get the projection information
  pointFile <- apply(projects, MARGIN = 1, FUN = function(x){
    if (x[["havePoints"]]) {
      # points in main folder
      folder <- paste0(ServerRoot, "/", x[["Project"]])
    } else if (x[["haveLAZ"]]) {
      # points in 1_LAZ folder
      folder <- paste0(ServerRoot, "/", x[["Project"]], "/1_LAZ")
    } else if (x[["haveLAS"]]) {
      # points in 1b_LAS folder
      folder <- paste0(ServerRoot, "/", x[["Project"]], "/1b_LAS")
    } else {
      folder <- NA
    }
    if (!is.na(folder)) {
      f <- LocalDirListByType(folder, fileType = "\\.las|\\.laz")
    
      if (length(f))
        return(paste0(folder, "/", f[1]))
      else
        return(NA)
    }
  
    return(NA)
  })
  
  projects$pointFile <- pointFile
  
  saveRDS(projects, "R6ProjectData_2.rds")
  projects <- readRDS("R6ProjectData_2.rds")
  
  # read point file to get projection information
  proj <- apply(projects, MARGIN = 1, FUN = function(x){
    if (!is.na(x[["pointFile"]])) {
      return(ReadLocalLASProjection(x[["pointFile"]]))
    } else
      return(NA)
  })
  
  projects$proj4string <- proj
  
  saveRDS(projects, "R6ProjectData_3.rds")
  projects <- readRDS("R6ProjectData_3.rds")
  
  # look at the projects where we don't have projection information and see if there
  # are shapefiles that could provide projection info
  potentialIndexFolders <- c(
    "3_Vectors",
    "3_Vectors/SHAPES",
    "3_Vectors/Indices_FinalOverall",
    "3_Vectors/Index",
    "3_Vectors/INDEX",
    "3_Vectors/Indices",
    "3_Vectors/SHAPEFILES",
    "3_Vectors/UpperWenatchee_Lidar_Index",
    "SHAPES",
    "VECTORS",
    "metadata/shapefiles/Indices",
    "2018 Eagle Creek Lidar Metadata/Eagle_Creek_AOI",
    "5_Other_Vendor_Products/usgs2006nps",
    "VECTORS/SHAPEFILES",
    "Baker_Pass/Vector_Layers",
    "3_Vectors/Tiling_Scheme",
    "Mapping/GIS/LiDAR_Tiling_Scheme"
  )
  
  # case is ignored for these
  potentialSubstrings <- c(
    "0_75",
    "750",
    "100TH",
    "Bins",
    "Bin",
    "lidar_index",
    "index",
    "tiles",
    "tile",
    "075",
    "0375",
    "tileindex",
    "datatile",
    "Tiling",
    "Collection",
    "Valley",
    "AOI",
    "GIP_2017",
    "blk1",
    "OLY_2017"
  )
  
  IndexFolder <- character(nrow(projects)) # pre-allocating vector is much faster than growing it
  IndexFile <- character(nrow(projects)) # pre-allocating vector is much faster than growing it
  proj4string_prj <- character(nrow(projects))
  
  #i <- 3
  for (i in 1:nrow(projects)) {
  #for (i in 1:3) {
    IndexFolder[i] <- NA
    IndexFile[i] <- NA
    proj4string_prj[i] <- NA
    
    if (is.na(projects$proj4string[i])) {
      path <- paste0(ServerRoot, "/", projects$Project[i])
      #cat(path, "\n")
      
      # try folders
      for (f in potentialIndexFolders) {
        url2 <- paste0(path, "/", f)
        #cat(url2, "\n")
        
        # look for .prj
        t2 <- LocalDirListByType(url2, ".prj")
        if (length(t2)) {
          #cat(t2, "\n")
          for (s in potentialSubstrings) {
            #cat(f, "  ", s, "\n")
            l <- grep(s, t2, ignore.case=TRUE, value = TRUE)
            if (length(l)) {
              IndexFolder[i] <- f
              IndexFile[i] <- l[1]
              
              proj4string_prj[i] <- ExtractCRSFromLocalPrj(path, IndexFolder[i], IndexFile[i], quiet = TRUE)
              
              break
            }
          }
          if (!is.na(IndexFolder[i]))
            break
        }
      }
      cat(i, projects$Project[i], ":", IndexFolder[i], "   ", IndexFile[i], "   ", proj4string_prj[i], "\n")
    } else {
      cat(i, "Projection information OK\n")
    }
  }
  
  # add to project information
  projects$IndexFolder <- IndexFolder
  projects$IndexFile <- IndexFile
  projects$proj4string_prj <- proj4string_prj
  
  # add the folder with point data
  pointFolder <- character(nrow(projects))
  pointFolder[projects$havePoints] <- ""
  pointFolder[projects$haveLAS] <- "1b_LAS"
  pointFolder[projects$haveLAZ] <- "1_LAZ"
  
  projects$pointFolder <- pointFolder
  
  saveRDS(projects, "R6ProjectData_4.rds")
  projects <- readRDS("R6ProjectData_4.rds")
  
  missing <- dplyr::filter(projects, is.na(proj4string)&is.na(proj4string_prj))
  
  # *****************************************************************************
  # Drop all projects that have "_see" in their name...these have no data
  # *****************************************************************************
  t <- !grepl("_see", projects$Project)
  good_projects <- projects[t, ]
  missing <- dplyr::filter(good_projects, is.na(proj4string)&is.na(proj4string_prj))
  
  good_projects$proj4string[is.na(good_projects$proj4string)] <- good_projects$proj4string_prj[is.na(good_projects$proj4string)]
  
  # add a project label...in all but a few cases this will be the same as the project folder name
  good_projects$ProjectLabel <- good_projects$Project
  
  # add a file name template used to select files for the index. In all but a few cases, this will
  # be "\\.las|\\.laz". For the OFF_2012_EASTERN_OR_TIR_LIDAR project, there are files mixed together
  # that represent different areas and are in different projections.
  good_projects$fileTemplate <- "\\.las|\\.laz"
  
  # *****************************************************************************
  # Manual corrections...mostly to add projection information and/or point folders
  #
  # There are many projects that have the wrong projection information in the LAS
  # files. Most seem to be from Atlantic but not all. In most cases, the projection
  # is correct but the units are wrong. The code favors the projection information
  # from the point files over that from index files...maybe not such a good idea.
  # *****************************************************************************
  good_projects$proj4string[which(good_projects$Project == "COL_2007_RiverCorridor_LakeRoosevelt")] <- CRS(SRS_string="EPSG:26911")@projargs
  good_projects$proj4string[which(good_projects$Project == "MAL_2007_RiverCorridor_JohnDay")] <- CRS(SRS_string="EPSG:26911")@projargs
  good_projects$proj4string[which(good_projects$Project == "OKW_2007_RiverCorridor_Methow")] <- CRS(SRS_string="EPSG:26910")@projargs
  good_projects$proj4string[which(good_projects$Project == "OKW_2007_RiverCorridor_OkanoganLower")] <- CRS(SRS_string="EPSG:26911")@projargs
  good_projects$proj4string[which(good_projects$Project == "OKW_2007_RiverCorridor_OkanoganUpper")] <- CRS(SRS_string="EPSG:26911")@projargs
  good_projects$proj4string[which(good_projects$Project == "WAW_2007_RiverCorridor_Sumpter")] <- CRS(SRS_string="EPSG:26911")@projargs
  good_projects$proj4string[which(good_projects$Project == "WAW_2007_RiverCorridor_Wenatchee")] <- CRS(SRS_string="EPSG:26910")@projargs
  good_projects$proj4string[which(good_projects$Project == "WAW_2007_RiverCorridor_Yakima")] <- CRS(SRS_string="EPSG:26911")@projargs
  
  good_projects$proj4string[which(good_projects$Project == "2017_IdahoPanhandle")] <- CRS(SRS_string="EPSG:26911")@projargs
  good_projects$pointFolder[which(good_projects$Project == "2017_IdahoPanhandle")] <- "All_FPC_LAZ"
  good_projects$havePoints[which(good_projects$Project == "2017_IdahoPanhandle")] <- TRUE
  
  good_projects$pointFolder[which(good_projects$Project == "CRG_2018_EagleCreek")] <- "LAS"
  good_projects$havePoints[which(good_projects$Project == "CRG_2018_EagleCreek")] <- TRUE
  
  good_projects$pointFolder[which(good_projects$Project == "OFF_2013_SCAPPOOSE")] <- "POINTS"
  good_projects$havePoints[which(good_projects$Project == "OFF_2013_SCAPPOOSE")] <- TRUE
  
  good_projects$pointFolder[which(good_projects$Project == "OFF_Glass_Buttes")] <- "POINTS"
  good_projects$havePoints[which(good_projects$Project == "OFF_Glass_Buttes")] <- TRUE
  
  good_projects$pointFolder[which(good_projects$Project == "OFF_OLC_2013_BLM_FIRES")] <- "POINTS"
  good_projects$havePoints[which(good_projects$Project == "OFF_OLC_2013_BLM_FIRES")] <- TRUE
  
  good_projects$pointFolder[which(good_projects$Project == "RRS_2020")] <- "point_cloud/tilescl"
  good_projects$havePoints[which(good_projects$Project == "RRS_2020")] <- TRUE
  
  good_projects$pointFolder[which(good_projects$Project == "WIL_2018_TopoBathy_McKenzieRiver")] <- "Elevation/LAS"
  good_projects$havePoints[which(good_projects$Project == "WIL_2018_TopoBathy_McKenzieRiver")] <- TRUE
  
  good_projects$pointFolder[which(good_projects$Project == "OLY_2017")] <- "1_LAZ/3_FS_Edited"
  good_projects$havePoints[which(good_projects$Project == "OLY_2017")] <- TRUE
  
  good_projects$proj4string[which(good_projects$Project == "GIP_2009_LewisCounty")] <- R6Albers # no point data for project
  good_projects$proj4string[which(good_projects$Project == "MBS_2014_Oso")] <- R6Albers # no point data for project
  good_projects$proj4string[which(good_projects$Project == "OCH_2011")] <- CRS(SRS_string="EPSG:2992")@projargs
  good_projects$proj4string[which(good_projects$Project == "SIU_2005_Yaquina_LEAF_ON")] <- CRS(SRS_string="EPSG:26911")@projargs
  good_projects$proj4string[which(good_projects$Project == "MAL_2008_CampCreek")] <- CRS(SRS_string="EPSG:2994")@projargs
  
  # correct projection info for some projects
  # MTH_2013_CLACKAMOLE: LAS files indicate Or State plane N ft but data are in OGIC
  good_projects$proj4string[which(good_projects$Project == "MTH_2013_CLACKAMOLE")] <- CRS(SRS_string="EPSG:6557")@projargs
  # OCH_2013_West: LAS files indicate Or State plane N ft but data are in OGIC
  good_projects$proj4string[which(good_projects$Project == "OCH_2013_West")] <- CRS(SRS_string="EPSG:6557")@projargs
  # OLY_2012_Quinault: LAS files indicate WA south with meters but data are in feet
  good_projects$proj4string[which(good_projects$Project == "OLY_2012_Quinault")] <- CRS(SRS_string="EPSG:2927")@projargs
  # LAS files indicate WA south with meters but data are in feet
  good_projects$proj4string[which(good_projects$Project == "COL_OKW_2017_QL2_NEWA")] <- CRS(SRS_string="EPSG:2286")@projargs
  good_projects$proj4string[which(good_projects$Project == "MBS_2016_King")] <- CRS(SRS_string="EPSG:2286")@projargs
  good_projects$proj4string[which(good_projects$Project == "MBS_2016_King2")] <- CRS(SRS_string="EPSG:2286")@projargs
  good_projects$proj4string[which(good_projects$Project == "GIP_2015_Klickitat")] <- CRS(SRS_string="EPSG:2286")@projargs
  good_projects$proj4string[which(good_projects$Project == "MBS_2012_CedarRiver")] <- CRS(SRS_string="EPSG:2286")@projargs
  good_projects$proj4string[which(good_projects$Project == "MBS_2013_Nooksack")] <- CRS(SRS_string="EPSG:2286")@projargs
  good_projects$proj4string[which(good_projects$Project == "MBS_2013_Tulalip_WASP_South")] <- CRS(SRS_string="EPSG:2286")@projargs
  good_projects$proj4string[which(good_projects$Project == "MBS_2014_CedarRiver")] <- CRS(SRS_string="EPSG:2286")@projargs
  good_projects$proj4string[which(good_projects$Project == "MBS_2015_Baker")] <- CRS(SRS_string="EPSG:2286")@projargs
  good_projects$proj4string[which(good_projects$Project == "OKW_2012_UpperNaches")] <- CRS(SRS_string="EPSG:2286")@projargs
  good_projects$proj4string[which(good_projects$Project == "OKW_2014_BumpingLake")] <- CRS(SRS_string="EPSG:2286")@projargs
  good_projects$proj4string[which(good_projects$Project == "OKW_2015_Chelan")] <- CRS(SRS_string="EPSG:2286")@projargs
  good_projects$proj4string[which(good_projects$Project == "OKW_2015_Teanaway")] <- CRS(SRS_string="EPSG:2286")@projargs
  good_projects$proj4string[which(good_projects$Project == "MBS_2013_Tulalip_WASP_North")] <- CRS(SRS_string="EPSG:2926")@projargs
  good_projects$proj4string[which(good_projects$Project == "OKW_2013_Entiat")] <- CRS(SRS_string="EPSG:2926")@projargs
  good_projects$proj4string[which(good_projects$Project == "OKW_2014_K2K")] <- CRS(SRS_string="EPSG:2927")@projargs
  good_projects$proj4string[which(good_projects$Project == "OCH_2014_CrookedRiver")] <- CRS(SRS_string="EPSG:6557")@projargs
  good_projects$proj4string[which(good_projects$Project == "RRS_2010_CraterLake")] <- CRS(SRS_string="EPSG:2994")@projargs
  good_projects$proj4string[which(good_projects$Project == "GIP_2017_SWWA_Foothills")] <- CRS(SRS_string="EPSG:2927")@projargs
  good_projects$proj4string[which(good_projects$Project == "WAW_2007_RiverCorridor_Yakima")] <- CRS(SRS_string="EPSG:2927")@projargs
  
  # OFF_2012_EASTERN_OR_TIR_LIDAR has 5 separate areas. Four are NAD83(CORS96) UTM zone 10 and 1 is zone 11 (Baker Pass). However, the point files
  # are mixed together. EPSG for NAD83(HARN) zone 10 is 3740 and for zone 11 is 3741. Having mixed point files with different
  # projections is a problem. Easiest solution is to add an extra project for the zone 11 area and modify the name for the zone 10
  # project. However, the existing logic in BuildIndexFromLocalPoints() won't support mixed files in the same folder. I added the fileTemplate
  # field to allow us to select specific files for inclusion in a project.
  good_projects$ProjectLabel[which(good_projects$Project == "OFF_2012_EASTERN_OR_TIR_LIDAR")] <- "OFF_2012_EASTERN_OR_TIR_LIDAR_UTM10"
  good_projects$proj4string[which(good_projects$Project == "OFF_2012_EASTERN_OR_TIR_LIDAR")] <- CRS(SRS_string="EPSG:3740")@projargs
  good_projects$fileTemplate[which(good_projects$Project == "OFF_2012_EASTERN_OR_TIR_LIDAR")] <- "Summer_Lake\\.laz|Christmas_Valley\\.laz|Paulina_Marsh\\.laz|Military\\.laz"
  good_projects[nrow(good_projects) + 1, ] <- good_projects[which(good_projects$Project == "OFF_2012_EASTERN_OR_TIR_LIDAR"), ]
  good_projects$ProjectLabel[nrow(good_projects)] <- "OFF_2012_EASTERN_OR_TIR_LIDAR_UTM11"
  good_projects$proj4string[nrow(good_projects)] <- CRS(SRS_string="EPSG:3741")@projargs
  good_projects$fileTemplate[nrow(good_projects)] <- "Baker_Pass\\.laz"
  
  good_projects$proj4string[which(good_projects$Project == "CRG_2018_EagleCreek")] <- CRS(SRS_string="EPSG:6339")@projargs
  
  # CRG_2010_ColumbiaRiver_Lidar has data split between UTM zones 10 & 11. Create a "new" project for the zone 11
  # data and modify the existing project to point to the zone 10 data
  # also projection info is wrong as extracted from .prj file
  good_projects$havePoints[which(good_projects$Project == "CRG_2010_ColumbiaRiver_Lidar")] <- TRUE
  good_projects$haveLAZ[which(good_projects$Project == "CRG_2010_ColumbiaRiver_Lidar")] <- FALSE
  good_projects$pointFolder[which(good_projects$Project == "CRG_2010_ColumbiaRiver_Lidar")] <- "1_LAZ/UTM10"
  good_projects$ProjectLabel[which(good_projects$Project == "CRG_2010_ColumbiaRiver_Lidar")] <- "CRG_2010_ColumbiaRiver_Lidar_UTM10"
  
  # add new project...copy of UTM10 project
  good_projects[nrow(good_projects) + 1, ] <- good_projects[which(good_projects$Project == "CRG_2010_ColumbiaRiver_Lidar"), ]
  good_projects$ProjectLabel[nrow(good_projects)] <- "CRG_2010_ColumbiaRiver_Lidar_UTM11"
  good_projects$pointFolder[nrow(good_projects)] <- "1_LAZ/UTM11"
  
  # set projection info
  good_projects$proj4string[which(good_projects$ProjectLabel == "CRG_2010_ColumbiaRiver_Lidar_UTM10")] <- CRS(SRS_string="EPSG:26910")@projargs
  good_projects$proj4string[which(good_projects$ProjectLabel == "CRG_2010_ColumbiaRiver_Lidar_UTM11")] <- CRS(SRS_string="EPSG:26911")@projargs
  
  # drop extra columns
  good_projects <- good_projects[, c(1, 11, 12, 2:4, 6, 10)]
  
  saveRDS(good_projects, "R6ProjectData_5.rds")
  good_projects <- readRDS("R6ProjectData_5.rds")
  
  # *****************************************************************************
  # Build the indexes
  # *****************************************************************************
  # it is OK to set flag to FALSE...if the index exists, BuildIndexFromLocalPoints()
  # returns TRUE
  good_projects$haveIndex <- FALSE
  
  for (i in 1:nrow(good_projects)) {
  #i <- 127
    if (good_projects$havePoints[i] | good_projects$haveLAS[i] | good_projects$haveLAZ[i]) {
      projectPath <- paste0(ServerRoot, "/", good_projects$Project[i])
      
      good_projects$haveIndex[i] <- BuildIndexFromLocalPoints(ServerRoot
                           , good_projects$Project[i]
                           , good_projects$pointFolder[i]
                           , paste0(BaseFolder, "/", ProjectIndexFolder, "/", good_projects$ProjectLabel[i], ".gpkg")
                           , projString = good_projects$proj4string[i]
                           , fileType = good_projects$fileTemplate[i]
    #                         , outputCRS = commonProjection
                           )
    }
  }
  
  # after building the indexes, the following projects have problems
  # GIP_2009_LewisCounty has no point data in any folder
  # MBS_2014_Oso has no point data in any folder
  # OFF_2007_2008_PDX_Delivery_5 has no point data. Has zip files in las folder
  # GIP_2017_SWWA_Foothills is missing data (point files). This is part of a large collection that
  # includes areas in NW and SW WA but only data from the SW portion is on the server. It could be that
  # only the national forest portion is included but I didn't dig into this idea.
  # MTH_GIP_CRG_2014_2015_Wasco has point files in OGIC and WASP mixed in the 1_LAZ folder
  # and a separate folder for some WASP files
  
  # major problem with using proj4string for projection. Some are 3D and st_write fails to accept the 
  # string. Solution is to add EPSG codes for projections and then reassign projection info to all files
  good_projects$EPSG <- 0
  for (i in 1:nrow(good_projects)) {
    if (!is.na(good_projects$proj4string[i])) {
      # look for specific strings
      if (grepl("proj=aea", good_projects$proj4string[i])) {
        good_projects$EPSG[i] <- 9674
      } else if (grepl("proj=lcc", good_projects$proj4string[i])) {
        if (grepl("lat_0=41\\.75", good_projects$proj4string[i])) {
          if (grepl("datum=NAD83", good_projects$proj4string[i])) {
            good_projects$EPSG[i] <- 6557
          } else {
            good_projects$EPSG[i] <- 6557          
          }
        } else if (grepl("lat_0=45\\.33", good_projects$proj4string[i])) {
          if (grepl("datum=NAD83", good_projects$proj4string[i])) {
            good_projects$EPSG[i] <- 2286 #Wa South NAD83
          } else {
            good_projects$EPSG[i] <- 2927 #Wa South NAD83(HARN)          
          }
        } else if (grepl("lat_0=47", good_projects$proj4string[i])) {
          good_projects$EPSG[i] <- 2926   # WA SP N NAD83(HARN) ftUS
        } else if (grepl("lat_0=43\\.6666", good_projects$proj4string[i])) {
          good_projects$EPSG[i] <- 32126   # OR SP N NAD83 meters
        }
    } else if (grepl("proj=utm", good_projects$proj4string[i])) {
        if (grepl("zone=10", good_projects$proj4string[i])) {
          if (grepl("datum=NAD83", good_projects$proj4string[i])) {
            good_projects$EPSG[i] <- 26910
          } else {
            good_projects$EPSG[i] <- 6339          
          }
        } else if (grepl("zone=11", good_projects$proj4string[i])) {
          if (grepl("datum=NAD83", good_projects$proj4string[i])) {
            good_projects$EPSG[i] <- 26911
          } else {
            good_projects$EPSG[i] <- 6340        
          }
        }
      }
    }
  }
  
  # drop projects that don't have an index
  good_projects <- dplyr::filter(good_projects, haveIndex == TRUE)
  
  good_projects$indexHasCRS <- FALSE
  # *!@#$%^&* I misinterpreted the meaning of "unknown" in the CRS. It
  # doesn't mean that there is no projection (as I thought). It simply
  # means that there isn't a simple name for the projection. NA in CRS
  # means there is no projection
  #
  # in addition, the projection information wasn't added to the indexes
  # fix projection
  for (i in 1:nrow(good_projects)) {
      if (good_projects$haveIndex[i]) {
      t_sf <- st_read(paste0(BaseFolder, "/", ProjectIndexFolder, "/", good_projects$ProjectLabel[i], ".gpkg"))
      # drop the vertical units...st_crs doesn't like them
      p4s <- gsub("vunits=m", "", good_projects$proj4string[i])
      p4s <- gsub("vunits=ft", "", p4s)
      p4s <- gsub("vunits=us-ft", "", p4s)
      st_crs(t_sf) <- p4s
      st_write(t_sf, paste0(BaseFolder, "/", ProjectIndexFolder, "/", good_projects$ProjectLabel[i], ".gpkg"), delete_dsn = TRUE, quiet = FALSE)
    }
  }
  
  saveRDS(good_projects, "R6ProjectData_6.rds")
  good_projects <- readRDS("R6ProjectData_6.rds")
  
  # *****************************************************************************
  # read the separate tile indexes, reproject to our common projection, and write new tile index
  # *****************************************************************************
  #
  # also add the Project name to each tile polygon...ideally this would be happening when the index is created
  tilelist <- apply(good_projects, MARGIN = 1, FUN = function(x) {
    if (x[["haveIndex"]]) {
      t_sf <- st_read(paste0(BaseFolder, "/", ProjectIndexFolder, "/", x[["ProjectLabel"]], ".gpkg"))
      
      if (is.object(t_sf)) {
        t_sf$Project <- x[["Project"]]
        t_sf_wm <- st_transform(t_sf, crs = 3857)
        st_write(t_sf_wm, paste0(BaseFolder, "/", WMProjectIndexFolder, "/", x[["ProjectLabel"]], ".gpkg"), delete_dsn = TRUE, quiet = FALSE)
        cat ("Done with:", x[["Project"]], "\n")
        return(t_sf_wm)
      }
    }
    return(NA)
  })
  
  # merge all tiles
  tiles_webmercator <- do.call(rbind, tilelist)
  
  # remove drive letter from path
  tiles_webmercator$path <- gsub("Z:", "", tiles_webmercator$path)
  
  # rename path to FullPath
  colnames(tiles_webmercator)[1] <- "FullPath"
  
  # move project column to first position
  tiles_webmercator <- tiles_webmercator[, c(17, 1:16, 18)]
  
  # write tile index
  st_write(tiles_webmercator, file.path(BaseFolder, MainIndexFolder, MergedTileIndexFile), delete_dsn = TRUE)
  mapview(tiles_webmercator)
  
  
  
  # Read tile index and display
  #tiles_webmercator <- st_read(file.path(BaseFolder, MainIndexFolder, MergedTileIndexFile))
  #mapview(tiles_webmercator)
  
  
  
  # create project polygons from the tiles, reprojects to web mercator and merges them
  # all together into a single index.
  projectlist <- apply(good_projects, MARGIN = 1, FUN = function(x) {
    if (x[["haveIndex"]]) {
      t_sf <- st_read(paste0(BaseFolder, "/", ProjectIndexFolder, "/", x[["ProjectLabel"]], ".gpkg"), quiet = TRUE)
      
      if (is.object(t_sf)) {
        # rasterize
        r_poly <- st_rasterize(t_sf["PointRecordFormat"], dx = 512, dy = 512)
        
        # convert back to polygons...merge tiles
        t_rp <- st_as_sf(r_poly, as_points = FALSE, merge = TRUE)
        st_make_valid(t_rp)
        t_rp <- st_transform(t_rp, crs = 3857)
        
        # add fields
        t_rp$Project <- x[["Project"]]
        t_rp$PointFolder <- paste0(x[["Project"]], "\\", x[["pointFolder"]], "\\")
        t_rp$TileCount <- nrow(t_sf)
        t_rp$EPSG <- x[["EPSG"]]
        
        cat ("Done with:", x[["ProjectLabel"]], "\n")
        return(t_rp)
      }
    }
    return(NA)
  })
  
  # merge all projects
  project_webmercator <- do.call(rbind, projectlist)
  
  st_write(project_webmercator, file.path(BaseFolder, MainIndexFolder, MergedProjectIndexFile), delete_dsn = TRUE)
  mapview(project_webmercator)
  
  
  
  
  
  
  
  
  
  
  # the MTH_GIP_CRG_2014_2015_Wasco project has files in OGIC and WASP. Some of the WASP files are
  # in a separate folder but some are mixed in the 1_LAZ folder with OGIC files. I need to produce
  # a list for each projection and use the lists to build separate index files for each projection.
  # It also looks like some of the files are missing (part of delivery 6). This might be an area
  # added on for WADNR but it isn't clear. The missing files are on the Washington side covering the 
  # area from White Salmon east to Dallesport (roughly)
  #
  # SPECIAL NOTE: I built index files for the files present on the server but did not add new a new project
  # for the WASP portion. This code should probably be moved prior to line ~830 (dropping columns)
  # after adding a new project to cover the WASP portion. The existing project is for the OGIC portion.
  # I might also have to add logic to skip some projects (better yet use the HaveIndex flag) so the 
  # special index files are not overwritten.
  
  # testing code to investigate projections used for the 
  # ReadLocalLASProjection("Z:/Lidar/Vendor_Deliverables_by_Project/MTH_GIP_CRG_2014_2015_Wasco/1_LAZ/45120F8206_CRG.laz")
  # ReadLocalLASProjection("Z:/Lidar/Vendor_Deliverables_by_Project/MTH_GIP_CRG_2014_2015_Wasco/1_LAZ/WASCO_WASP/WASCO_WASP_05881_GIP.laz")
  # ReadLocalLASProjection("Z:/Lidar/Vendor_Deliverables_by_Project/MTH_GIP_CRG_2014_2015_Wasco/1_LAZ/45120F6125_CRG.laz")
  # 
  # t <- lidR::readLASheader("Z:/Lidar/Vendor_Deliverables_by_Project/MTH_GIP_CRG_2014_2015_Wasco/1_LAZ/45120F8206_CRG.laz")
  # t@VLR$GeoAsciiParamsTag$tags[1]
  
  
  
  # testing method to determine the number of projections used for LAS/LAZ files
  # for the MTH_GIP_CRG_2014_2015_Wasco project. It has at least 2 projections
  # in the 1_LAZ folder and a subfolder with WA data
  GetProjName <- function(
    fileSpec,
    quiet = FALSE
  ) {
    # read file header
    t <- tryCatch(lidR::readLASheader(fileSpec), error = function(e) {NA})
    if (is.object(t)) {
      # show status info
      if (t@PHB[["Number of variable length records"]] > 0 && !quiet) {
        cat(basename(fileSpec), ":", t@VLR[["GeoKeyDirectoryTag"]][["description"]], "\n")
      }
      # return name
      return(t@VLR[["GeoKeyDirectoryTag"]][["description"]])
    } else {
      cat("Failed to read", basename(fileSpec), "\n")
      return("")
    }
  }
  
  # The problem with getting only the EPSG code from the LAS header is that the header also
  # contains codes for the horizontal and vertical datum and units. For example, you can have 
  # EPSG:32149 (NAD83 WA SP South in meters) and then have key 3076 with a value of 9003
  # indicating that horizontal units are in us-ft. This assumes a geoTIff VLR...you can also
  # have a WKT VLR that gives projection information.
  #
  # Note that I would consider the above example an incorrect use of the GeoTiff VLR because there
  # is an EPSG code for NAD83 WA SP South in us-ft (EPSG:2286). It just seems wrong to provide
  # the EPSG for the wrong units and then modify it to add the correct units instead of simply
  # using the "correct" EPSG code.
  GetEPSG <- function(
    fileSpec,
    quiet = FALSE
  ) {
    ret <- 0
    # read file header
    t <- tryCatch(lidR::readLASheader(fileSpec), error = function(e) {NA})
    if (is.object(t)) {
      # show status info
      if (t@PHB[["Number of variable length records"]] > 0 && !quiet) {
        if (length(t@VLR[["GeoKeyDirectoryTag"]][["tags"]]) > 0) {
          for (i in 1:length(t@VLR[["GeoKeyDirectoryTag"]][["tags"]])) {
            if (t@VLR[["GeoKeyDirectoryTag"]][["tags"]][[i]][["key"]] == 3072)
              ret <- t@VLR[["GeoKeyDirectoryTag"]][["tags"]][[i]][["value offset"]]
          }
        }
        cat(basename(fileSpec), ":", t@VLR[["GeoKeyDirectoryTag"]][["description"]], " EPSG: ", ret, "\n")
      }
      # return name
      return(ret)
    } else {
      cat("Failed to read", basename(fileSpec), "\n")
      return(ret)
    }
  }
  
  # get list of .laz & .las files...full directory info
  folder <- "Z:/Lidar/Vendor_Deliverables_by_Project/MTH_GIP_CRG_2014_2015_Wasco/1_LAZ/WASCO_WASP"
  flist <- LocalDirList(folder, fileType = "\\.las|\\.laz")
  files <- data.frame("FileName" = flist)
  files$FileSpec <- paste0(folder, "/", files$FileName)
  files$ProjDescription <- ""
  
  if (length(flist) > 0) {
    for (i in 1:nrow(files)) {
  #  for (i in 1:10) {
      if (files$ProjDescription[i] == "") {
        cat(i, "   ")
        files$ProjDescription[i] <- GetProjName(files$FileSpec[i])
      }
    }
  }
  
  #files <- baseFiles
  files <- WASPFiles
  files$EPSG <- 0
  if (length(flist) > 0) {
    for (i in 1:nrow(files)) {
      #  for (i in 1:10) {
      if (files$EPSG[i] == 0) {
        cat(i, "   ")
        files$EPSG[i] <- GetEPSG(files$FileSpec[i])
      }
    }
  }
  
  baseFiles <- files
  #WASPFiles <- files
  saveRDS(baseFiles, "R6basefiles.rds")
  saveRDS(WASPFiles, "R6WASPfiles.rds")
  baseFiles <- readRDS("R6basefiles.rds")
  WASPFiles <- readRDS("R6WASPfiles.rds")
  
  # These files have the lists of files...need to sort by projection and add separate projects for each
  # projection. Plan to use the list directly to build the indexes
  OGICfiles <- baseFiles[baseFiles$EPSG == "6557", ]
  WASPfiles <- baseFiles[baseFiles$EPSG == "32149", ]
  WASPfiles <- rbind(WASPfiles, WASPFiles)
  
  # build index for OGIC files
  t <- BuildIndexFromLocalPoints(ServerRoot
                                                     , "MTH_GIP_CRG_2014_2015_Wasco"
                                                     , ""
                                                     , paste0(BaseFolder, "/", ProjectIndexFolder, "/", "MTH_GIP_CRG_2014_2015_Wasco", "_OGIC", ".gpkg")
                                                     , projString = CRS(SRS_string="EPSG:6557")@projargs
                                                     , fullFileList = OGICfiles$FileSpec
  )
  
  # build index for WASP files
  t <- BuildIndexFromLocalPoints(ServerRoot
                                                     , "MTH_GIP_CRG_2014_2015_Wasco"
                                                     , ""
                                                     , paste0(BaseFolder, "/", ProjectIndexFolder, "/", "MTH_GIP_CRG_2014_2015_Wasco", "_WASP", ".gpkg")
                                                     , projString = CRS(SRS_string="EPSG:6599")@projargs
                                                     , fullFileList = WASPfiles$FileSpec
  )
  
  
  
  t_sf_OGIC <- st_read(paste0(BaseFolder, "/", ProjectIndexFolder, "/", "MTH_GIP_CRG_2014_2015_Wasco_OGIC", ".gpkg"))
  projection(t_sf)
  mapview(t_sf)
  
  t_sf <- st_read(paste0(BaseFolder, "/", ProjectIndexFolder, "/", "MTH_GIP_CRG_2014_2015_Wasco_WASP", ".gpkg"))
  projection(t_sf)
  mapview(t_sf) +
    mapview(t_sf_OGIC)
  
  
  
  
  
  
  
  
  
  CRS(SRS_string="EPSG:2927")@projargs
  CRS(SRS_string="EPSG:2286")@projargs
  
  
  
  
  
  i <- 63
  t_sf <- st_read(paste0(BaseFolder, "/", ProjectIndexFolder, "/", good_projects$ProjectLabel[i], ".gpkg"))
  st_crs(t_sf) <- 2927
  
  # rasterize
  r_poly <- st_rasterize(t_sf["PointRecordFormat"], dx = 512, dy = 512)
  
  # convert back to polygons...merge tiles
  t_rp <- st_as_sf(r_poly, as_points = FALSE, merge = TRUE)
  st_make_valid(t_rp)
  t_rp <- st_transform(t_rp, crs = 3857)
  
  mapview(t_rp) +
    mapview(t_sf)
  
  mapview(t_sf)
  
  
  
  min(t_sf$MaxX - t_sf$MinX)
  min(t_sf$MaxY - t_sf$MinY)
  min(t_sf$MaxZ - t_sf$MinZ)
  
  
  
  #t_sf_wm <- st_transform(t_sf, crs = 3857)
  t_sp <- as(t_sf, "Spatial")
  #t_sp <- spTransform(t_sp, CRS("+init=epsg:3857"))
  values(r) <- NA
  
  
  
  #t_sp <- as(t_sf, "Spatial")
  extent(r) <- extent(t_sp)
  r_poly <- rasterize(t_sp, r, field = t_sp@data$PointRecordFormat, update=FALSE, updateValue="NA")
  
  # t_rp <- rasterToPolygons(r_poly, dissolve = TRUE)
  # t_rp@proj4string <- CRS(good_projects$proj4string[i])
  # t_rp <- gBuffer(t_rp, width = 0)
  
  t_rp <- (sf::st_as_sf(stars::st_as_stars(r_poly), as_points = FALSE, merge = TRUE))
  st_crs(t_rp) <- p4s
  t_rp <- st_transform(t_rp, crs = 3857)
  
  mapview(t_rp) +
    mapview(t_sf)
  mapview(t_sf)
  
  
  
  
  if (is.object(t_sf)) {
    t_sf$Project <- good_projects$Project[i]
    st_crs(t_sf) <- 2286
    t_sf_wm <- st_transform(t_sf, crs = 3857)
    st_write(t_sf_wm, paste0(BaseFolder, "/", WMProjectIndexFolder, "/", good_projects$ProjectLabel[i], ".gpkg"), delete_dsn = TRUE, quiet = FALSE)
    cat ("Done with:", good_projects$Project[i], "\n")
  }
  
  projection(t_sf_wm)
  mapview(t_sf_wm)
  
  projection(t_sf)
  mapview(t_sf)
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  for (i in 1:nrow(good_projects)) {
    print(rgdal::showEPSG(good_projects$proj4string[i]))
  }
  
  
  
  
  
  
  
  
  
  
  
  index <- st_read(paste0(BaseFolder, "/", ProjectIndexFolder, "/", good_projects$Project[34], ".gpkg"))
  st_crs(index) <- st_crs(2994)
  st_write(index, "test.gpkg", delete_dsn = TRUE, quiet = FALSE)
  st_crs(index)
  mapview(index)
  
  
  path <- paste0(ServerRoot, "/", good_projects$Project[i], "/1_LAZ", "/", "LAZ_FRESY1_502088_966808_20180629.laz")
  t <- tryCatch(lidR::readLASheader(path), error = function(e) {NA})
  if (is.object(t@VLR$GeoAsciiParamsTag)) {
    
  }
}  
  
