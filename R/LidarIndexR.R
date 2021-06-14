# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           Ctrl + Shift + B
#   Check Package:             Ctrl + Shift + E
#   Test Package:              Ctrl + Shift + T
#   Build documentation:       Ctrl + Shift + D...not working
#   Build vignette:            Ctrl + Shift + K
#
# setwd("G:/R_Stuff/LidarIndexR")
#
# To build documentation:
# devtools::document()

# Utility functions

# ---------- unsignedFourByteIntToDouble
#
#' LidarIndexR -- Utility function to help read unsigned long data types
#'
#' This is a helper function used in the LidarIndexR package to help read
#' \code{unsigned long} data types. It takes a 4-byte block and interprets
#' it as and \code{unsigned long} value. The function is used mainly to read
#' value from LAS file headers.
#'
#' @param i A 4-byte block of data.
#' @return An (invisible) double value.
unsignedFourByteIntToDouble <- function(i) {
  d <- as.numeric(i)
  d[d<0] <- d[d<0] + 2^32
  invisible(d)
}

# ---------- DirList
#
#' LidarIndexR -- Retrieve a directory listing from a remote host
#'
#' Retrieve a directory listing from a remote host as either a simple list
#' of file/folder names or a data frame with file/folder information.
#'
#' @param URL URL for a folder on a remote host. Trailing slash is optional.
#' @param fileType Any valid string for the pattern parameter in \code{grep()}.
#'   "$" will be appended to the string to search for file/folder names ending
#'   with values in \code{fileType}.
#' @param namesOnly Boolean indicating you want only file names (TRUE) or all
#'   directory information (FALSE).
#' @param excludeDirectories Boolean indicating you want to exclude folders
#'   from the list of returned files/folders.
#' @param directoryOnly Boolean indicating you only want folder names included
#'   in the list of returned files/folders.
#' @param dirFormat String indicating the expected directory format for the
#'   \code{URL}. Valid values are "dir" if the URL returns the Date, Time,
#'   Attribute/Size, and file name or "ls" if the URL returns directory
#'   information similar to the UNIX \code{ls -al} command.
#' @return A list of file names or a data frame with file names and attribute
#'   information. The return value depends on \code{namesOnly}.
#' @examples
#' \dontrun{
#' DirList("ftp://rockyftp.cr.usgs.gov/vdelivery/Datasets/Staged/")
#' DirList("ftp://rockyftp.cr.usgs.gov/vdelivery/Datasets/Staged/Elevation/LPC/Projects/AK_BrooksCamp_2012/laz/", "\\.las|\\.laz")
#' }
#' @export
DirList <- function (
  URL,
  fileType = NULL,
  namesOnly = FALSE,
  excludeDirectories = TRUE,
  directoryOnly = FALSE,
  dirFormat = "dir",     # "ls" or "dir"
  ...
) {
  # make sure URL ends with "/"
  if (!endsWith(URL, "/"))
    URL <- paste0(URL, "/")

  # get folder listing and parse into individual files
  # create an empty character vector on error
  filenames <- tryCatch(RCurl::getURL(URL, ftp.use.epsv = FALSE, dirlistonly = namesOnly, ...), error = function(e) {character(0)})
  if (length(filenames)) {
    filenames <- strsplit(filenames, "\r\n")
    filenames <- unlist(filenames)

    if (!is.null(fileType))
      filenames <- grep(paste0(fileType, "$"), filenames, ignore.case=TRUE, value=TRUE)

    # create a dataframe and sort on the attribute (size for files)
    if (!namesOnly) {
      if (dirFormat != "dir") {
        df <- read_table2(filenames, col_names = c("Permissions", "Links", "Owner", "Group", "Attribute", "Month", "Day", "Year", "Name"))
      } else {
        df <- read_fwf(filenames, fwf_widths(c(8, 9, 21, NA), c("Date", "Time", "Attribute", "Name")))
      }

      # df <- read.table(text = filenames)
      # colnames(df) <- c("Date", "Time", "Attribute", "Name")
      df <- df[order(df$Attribute),]

      if (dirFormat != "dir") {
        if (directoryOnly) {
          df <- df[startsWith(df$Permissions, "d"), ]
        } else if (excludeDirectories) {
          df <- df[!startsWith(df$Permissions, "d"), ]
        }
      } else {
        if (directoryOnly) {
          df <- df[df$Attribute == "<DIR>", ]
        } else if (excludeDirectories) {
          df <- df[df$Attribute != "<DIR>", ]
        }
      }

      return(df)
    }
  }
  return(filenames)
}

# ---------- DirListByType
#
#' LidarIndexR -- Retrieve a directory listing from a remote host
#'
#' Retrieve a directory listing from a remote host as a simple list
#' of file names.
#'
#' @param URL URL for a folder on a remote host. Trailing slash is optional.
#' @param fileType Any valid string for the pattern parameter in \code{grep()}.
#'   "$" will be appended to the string to search for file/folder names ending
#'   with values in \code{fileType}.
#' @return A list of file names.
#' @examples
#' \dontrun{
#' DirListByType("ftp://rockyftp.cr.usgs.gov/vdelivery/Datasets/Staged/Elevation/LPC/Projects/AK_BrooksCamp_2012/laz/", "\\.las|\\.laz")
#' }
#' @export
DirListByType <- function (
  URL,
  fileType,
  ...
) {
  # make sure URL ends with "/"
  if (!endsWith(URL, "/"))
    URL <- paste0(URL, "/")

  # get folder listing and parse into individual files
  # create an empty character vector on error
  filenames <- tryCatch(RCurl::getURL(URL, ftp.use.epsv = FALSE, dirlistonly = TRUE, ...), error = function(e) {character(0)})
  if (length(filenames)) {
    filenames <- strsplit(filenames, "\r\n")
    filenames <- unlist(filenames)

    # find files ending with the desired type (extension)
    filenames <- grep(paste0(fileType, "$"), filenames, ignore.case=TRUE, value=TRUE)
  }
}

# ---------- DirListByName
#
#' LidarIndexR -- Retrieve a directory listing from a remote host of all files
#'   with a specific name (various extensions)
#'
#' Retrieve a simple list of all files with the specified name but various
#' extensions.
#'
#' @param URL URL for a folder on a remote host. Trailing slash is optional.
#' @param fileName A string containing the name of a single file. All files
#'   with the same name (but different extensions) will be returned.
#' @return A list of file names.
#' @examples
#' \dontrun{
#' DirListByName("ftp://rockyftp.cr.usgs.gov/vdelivery/Datasets/Staged/Hydrography/NHD/HU4/HighResolution/Shape/", "NHD_H_0101_HU4_Shape.jpg")
#' }
#' @export
DirListByName <- function (
  URL,
  fileName,
  ...
) {
  # make sure URL ends with "/"
  if (!endsWith(URL, "/"))
    URL <- paste0(URL, "/")

  fileName <- sub(pattern = "(.*)\\..*$", replacement = "\\1", basename(fileName))

  # get folder listing and parse into individual files
  # create an empty character vector on error
  filenames <- tryCatch(RCurl::getURL(URL, ftp.use.epsv = FALSE, dirlistonly = TRUE, ...), error = function(e) {character(0)})
  if (length(filenames)) {
    filenames <- strsplit(filenames, "\r\n")
    filenames <- unlist(filenames)

    # find files matching the name...various extensions
    filenames <- grep(paste0("^", fileName), filenames, ignore.case=TRUE, value=TRUE)
  }
}

# Functions to access LAS/LAZ files
ReadRemoteLASProjection <- function (
  url,
  tempFolder,
  quiet = FALSE
) {
  # open a connection and read only enough information to get the header size. Then read the entire
  # header and save to local file and use lidR::readLASheader to read the header and return
  # the projection information for the file
  #
  # NOTE: we will get a larger offset to the point data due to the VLR for LAZ compression. LAZ readers
  # reduce the offset to compensate for this VLR (effectively ignore that it exists)
  OffsettoPointData <- 0
  crs <- NA

  con <- tryCatch(url(url, open = "rb"), error = function(e) {NA})
  if (is.object(con)) {
    bs <- readBin(con, "raw", 100)
    close(con)
  } else {
    return(crs)
  }

  # use raw vector as input stream and parse header values
  con = rawConnection(bs, open = "rb")
  Signaturebytes <- readBin(con, "raw", n = 4, size = 1, endian = "little")

  Signature <- readBin(Signaturebytes, "character", size = 4, endian = "little")
  if (Signature == "LASF") {
    readBin(con, "raw", 92) # skip bytes
    OffsettoPointData <- readBin(con, "integer", n = 1, size = 4)
    OffsettoPointData <- unsignedFourByteIntToDouble(OffsettoPointData)
  }
  close(con)

  if (Signature == "LASF" && OffsettoPointData > 0) {
    # read the entire header and write to local file
    con <- url(url, open = "rb")
    bs <- readBin(con, "raw", OffsettoPointData)
    close(con)

    writeBin(bs, con = file.path(tempFolder, "__temp__.las"), useBytes = TRUE)
    if (file.exists(file.path(tempFolder, "__temp__.las"))) {
      # read header
      t <- tryCatch(lidR::readLASheader(file.path(tempFolder, "__temp__.las")), error = function(e) {NA})
      if (is.object(t)) {
        crs <- projection(t)
      }

      # delete temp file
      file.remove(file.path(tempFolder, "__temp__.las"))
    } else {
      if (!quiet) cat("Temp file not created:", file.path(tempFolder, "__temp__.las"), "\n")
    }
  }
  if (!quiet) cat(basename(url), ":", crs, "\n")
  return(crs)
}

ReadRemoteLASHeader <- function(
  url,
  quiet = FALSE
) {
  # open connection and read 375 bytes...header for V1.4 is 375 bytes
  # hopefully this is faster than reading individual values via ftp...but i didn't test
  if (!quiet)
    cat("reading header for", basename(url), "\n")

  con <- url(url, open = "rb")
  bs <- readBin(con, "raw", 375)
  close(con)

  # use raw vector as input stream and parse header values
  con = rawConnection(bs, open = "rb")
  Signaturebytes <- readBin(con, "raw", n = 4, size = 1, endian = "little")

  Signature <- readBin(Signaturebytes, "character", size = 4, endian = "little")
  if (Signature == "LASF") {
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

    if (!quiet)
      cat("Read extent of", basename(url), "\n")
  } else {
    VersionMajor <- NA
    VersionMinor <- NA
    DayOfYear <- NA
    Year <- NA
    HeaderSize <- NA
    VLRCount <- NA
    PointRecordFormat <- NA
    PointRecordLength <- NA
    PointCount <- NA
    MaxX <- NA
    MinX <- NA
    MaxY <- NA
    MinY <- NA
    MaxZ <- NA
    MinZ <- NA

    if (!quiet)
      cat("Failed to read extent of", basename(url), "\n")
  }
  close(con)

  # build data frame to return
  df <- data.frame(
    "URL" = url,
    "FileName" = basename(url),
    "LASVersion" = VersionMajor + VersionMinor / 10,
    "FileDayOfYear" = DayOfYear,
    "FileYear" = Year,
    "HeaderSize" = HeaderSize,
    "VLRCount" = VLRCount,
    "PointRecordFormat" = PointRecordFormat,
    "PointRecordLength" = PointRecordLength,
    "PointCount" = PointCount,
    "MinX" = MinX,
    "MinY" = MinY,
    "MinZ" = MinZ,
    "MaxX" = MaxX,
    "MaxY" = MaxY,
    "MaxZ" = MaxZ
  )

  return(df)
}

# Functions to find CRS information
# function to download smallest LAS file in a folder, load it with lidR and
# get the crs. Return is a CRS in sp format...get WKT using sp::wkt(crs)
#
# if headerOnly = TRUE, only the LAS/LAZ file header is downloaded to get the crs
# partial LAS/LAZ file is deleted after crs is extracted
#
FetchAndExtractCRSFromPoints <- function (
  baseURL,
  folderName,
  tempFolder,
  headerOnly = TRUE,
  fetchnew = FALSE,
  quiet = FALSE,
  ...
) {
  crs <- NA

  t <- paste0(baseURL, "/", folderName, "/")

  # build directory list...first file will be smallest
  fileList <- DirList(t, fileType = "\\.las|\\.laz")

  # download first file
  if (length(fileList)) {
    if (!quiet) cat(folderName, "/", fileList$Name[1], "--")
    if (headerOnly) {
      crs <- ReadRemoteLASProjection(paste0(t, fileList$Name[1]), tempFolder, quiet = TRUE)
    } else {
      if (!file.exists(file.path(tempFolder, fileList$Name[1])) || fetchnew) {
        download.file(paste0(t, fileList$Name[1]), file.path(tempFolder, fileList$Name[1]), quiet = quiet, mode = "wb", ...)
      }

      # load file and extract CRS (if any)
      las <- tryCatch(lidR::readLASheader(file.path(tempFolder, fileList$Name[1])), error = function(e) {NA})
      if (is.object(las)) {
        crs <- projection(las)
      } else {
        crs <- NA
      }
    }
  }
  if (!quiet) {
    cat(basename(baseURL), ":", crs, "\n")
  }

  return(crs)
}

# function to download a shapefile, load it with st_read and
# get the crs. Return is a CRS in a proj4string
FetchAndExtractCRSFromIndex <- function (
  baseURL,
  folderName,
  fileName,
  tempFolder,
  fetchnew = FALSE,
  quiet = FALSE,
  ...
) {
  crs <- NA

  t <- paste0(baseURL, "/", folderName, "/")

  # this returns a list of files with the same name but different extensions
  filenames <- DirListByName(t, fileName)

  # get all the files associated with the index shapefile
  if (length(filenames)) {
    for (filename in filenames) {
      if (!file.exists(paste0(tempFolder, "/", filename)) || fetchnew) {
        download.file(paste0(t, filename), paste0(tempFolder, "/", filename), mode = "wb")
      }
    }

    # load index file and extract CRS (if any)
    index <- tryCatch(sf::st_read(file.path(tempFolder, fileName)), error = function(e) {NA})
    if (is.object(index)) {
      crs <- projection(index)
    } else {
      crs <- NA
    }
  }
  if (!quiet) {
    cat(basename(baseURL), ":", crs, "\n")
  }

  return(crs)
}

# function to download .prj file associated with the .shp file passed in fileName
# Use the string read form the .prj file to get the crs. Return is a CRS in proj4string
FetchAndExtractCRSFromPrj <- function (
  baseURL,
  folderName,
  fileName,             # must be .prj file
  tempFolder,
  fetchnew = FALSE,
  quiet = FALSE,
  ...
) {
  crs <- NA

  t <- paste0(baseURL, "/", folderName, "/")

  # this returns a list of files with the same name but different extensions
  filenames <- DirListByName(t, fileName)

  # we just need the .prj file
  filenames <- grep(paste0(".prj", "$"), filenames, ignore.case=TRUE, value=TRUE)

  # get all the files associated with the index shapefile
  if (length(filenames)) {
    for (f in filenames) {
      if (!file.exists(paste0(tempFolder, "/", f)) || fetchnew) {
        download.file(paste0(t, f), paste0(tempFolder, "/", f), mode = "wb")
      }
    }

    f <- file(file.path(tempFolder, fileName), open = "rt")
    #    cat("Opening:", file.path(tempFolder, fileName), "\n")
    if (isOpen(f)) {
      l <- readLines(f, n = -1, warn = FALSE)
      close(f)
      #      cat(l, "\n")

      crs <- CRS(l)@projargs
    } else {
      crs <- NA
    }
  }
  if (!quiet) {
    cat(basename(baseURL), ":", crs, "\n")
  }

  return(crs)
}

# Functions to build index files
BuildIndexFromPoints <- function (
  baseURL,
  folderName,
  outputFile,
  projString = NA,
  outputCRS = NA,
  fileType = "\\.las|\\.laz",
  dirFormat = "short",
  dimensionThreshold = 50000,
  rebuild = FALSE,
  quiet = FALSE
) {
  if (file.exists(outputFile) && !rebuild) {
    cat("Index already exist...skipping: ", basename(outputFile),"\n")
    return(TRUE);
  }

  folderURL <- paste0(baseURL, "/", folderName, "/")

  # get list of .laz & .las files...full directory info
  flist <- DirList(folderURL, "\\.las|\\.laz", dirFormat = dirFormat)

  if (nrow(flist) > 0) {
    cat("Building index: ", basename(outputFile), "\n")

    # prepend folder path to file names
    fileURLs <- paste0(folderURL, flist$Name)

    # read headers
    t <- lapply(fileURLs, ReadRemoteLASHeader)    # returns a list of dataframes

    # convert to a simple dataframe
    t_df <- do.call("rbind", t)

    # drop any rows with NA values...bad LAS file...only check min/max XYZ values
    t_df <- t_df[complete.cases(t_df[, 11:16]), ]

    # drop rows where width or height is >dimensionThreshold units
    t_df <- t_df[((t_df$MaxX - t_df$MinX) < dimensionThreshold & (t_df$MaxY - t_df$MinY) < dimensionThreshold), ]

    # create sf set of tile polygons
    if (nrow(t_df) > 0) {
      lst <- lapply(1:nrow(t_df), function(x) {
        # create a matrix of coordinates that also 'close' the polygon
        res <- matrix(c(t_df[x, 'MinX'], t_df[x, 'MinY'],
                        t_df[x, 'MinX'], t_df[x, 'MaxY'],
                        t_df[x, 'MaxX'], t_df[x, 'MaxY'],
                        t_df[x, 'MaxX'], t_df[x, 'MinY'],
                        t_df[x, 'MinX'], t_df[x, 'MinY'])  ## need to close the polygon
                      , ncol =2, byrow = T
        )
        # create polygon objects
        st_polygon(list(res))
      }
      )

      tiles_sf <- st_sf(t_df, st_sfc(lst), crs = projString)

      # reproject to outputCRS
      if (!is.na(projString) && !is.na(outputCRS)) {
        tiles_sf <- st_transform(tiles_sf, crs = outputCRS)
      }

      # write output
      st_write(tiles_sf, outputFile, delete_dsn = TRUE, quiet = TRUE)

      return(TRUE)
    } else {
      cat("   ***No LAS polygons\n")
    }
  } else {
    cat("   ***No LAS files\n")
  }

  return(FALSE)
}







# notes:
#
# The goal for this package is to have a set of functions that can create either a tile
# index or a general polygon (created from the tiles) for a project (or both). I keep
# running into USGS projects that don't have valid entries in their tile index so it
# would be helpful to be able to build a new index for projects when needed.
#
# I have some code in the testpackage.R code that does this for the Mendocino area
