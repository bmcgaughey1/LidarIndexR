# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           Ctrl + Shift + B
#   Check Package:             Ctrl + Shift + E
#   Test Package:              Ctrl + Shift + T
#   Build documentation:       Ctrl + Shift + D...not working
#   Build vignette:            Ctrl + Shift + K
#
# library(devtools)
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

# ---------- makeFileSize
#
#' LidarIndexR -- Utility function to convert file size string to actual
#' file size.
#'
#' This is a helper function used in the LidarIndexR package to help read
#' file sizes from http or https directory listings. It looks for a trailing
#' character to determine the multipliuer for the numeric portion of the size
#' value. In the case of directories, the \code{sizeString} will be "-". This
#' value is explicitly recognized.
#'
#' @param sizeString A string containing the file size or "-" for directories.
#' @return A (invisible) double value for files and a string for directories.
makeFileSize <- function(sizeString) {
  if (sizeString == "-")
    fileSize <- "<DIR>"
  else {
    # have strings with K, M, G, T indicating multiplier for leading numeric portion of string
    # get last character
    flag <- stringr::str_extract(sizeString, "[A-Z]{1}")
    
    # get all but last character
    if (is.na(flag))
      fileSize <- as.numeric(substr(sizeString, 1, nchar(sizeString)))
    else
      fileSize <- as.numeric(substr(sizeString, 1, nchar(sizeString) - 1))
    
    if (!is.na(fileSize) && !is.na(flag)) {
      if (flag == "K")
        fileSize <- fileSize * 1024.0
      if (flag == "M")
        fileSize <- fileSize * 1024.0 * 1024.0
      if (flag == "G")
        fileSize <- fileSize * 1024.0 * 1024.0 * 1024.0
      if (flag == "T")
        fileSize <- fileSize * 1024.0 * 1024.0 * 1024.0 * 1024.0
    } else {
      if (!is.na(flag))
        fileSize <- 0
    }  
    
    if (is.na(fileSize))
      fileSize <- 0
  }
  
  invisible(fileSize)
}

# ---------- DirList_http
#
#' LidarIndexR -- Retrieve a directory listing from a remote http(s) host
#'
#' Retrieve a http or https directory listing from a remote host as either
#' a simple list of file/folder names or a data frame with file/folder information.
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
#' @return A list of file names or a data frame with file names and attribute
#'   information. The return value depends on \code{namesOnly}.
#' @examples
#' \dontrun{
#' DirList_http(pasteo("https://rockyweb.usgs.gov/vdelivery/Datasets/Staged/",
#'   "Elevation/LPC/Projects/AK_BrooksCamp_2012/laz/"))
#' }
#' @export
DirList_http <- function (
  URL,
  fileType = NULL,
  namesOnly = FALSE,
  excludeDirectories = TRUE,
  directoryOnly = FALSE
) {
  # make sure URL ends with "/"
  if (!endsWith(URL, "/"))
    URL <- paste0(URL, "/")
  
  dir_read <- readLines(URL)
  parsed_dir <- XML::htmlParse(dir_read)
  
  # set up data frame
  df <- data.frame("line" = dir_read, isXML = FALSE, isParent = FALSE)
  
  # see if lines are XML
  df$isXML <- as.integer(sapply(dir_read, function(x) {XML::isXMLString(x)}))

  # drop non-XML lines
  df <- df[df$isXML == 1, ]
  
  # see if line has parent directory...look for "  -  "
  df$isParent <- as.integer(sapply(df$line, function(x) {!is.na(stringr::str_extract(x, "[ ]{2}-[ ]{2}"))}))
  
  # drop lines up to and including parent line
  parentRow <- match(1, df$isParent)
  if (parentRow > 0) 
    df <- df[(parentRow + 1):nrow(df), ]
  
  # 6/27/2024...USGS index format has changed to add a footer. We need to keep all
  # lines that have img tags
  # see if lines has "img src="
  df$isImg <- as.integer(sapply(df$line, function(x) {!is.na(stringr::str_extract(x, "img src="))}))
  
  # drop non-img lines
  df <- df[df$isImg == 1, ]
  
  ## drop last row...should be <hr></pre>
  ##df <- df[1:(nrow(df) -1), ]
  
  # make sure we still have rows...not an empty directory
  if (nrow(df) > 0) {
    # extract file name
    df$Name <- XML::getHTMLLinks(df$line)
    
    # remove slashes in filename
    df$Name <- stringr::str_remove_all(df$Name, "/")
    df$Name <- stringr::str_remove_all(df$Name, "\\\\")
    
    # extract date
    df$Date <- stringr::str_extract(df$line, "[0-9]{4}-[0-9]{2}-[0-9]{2}")
    
    # extract time
    df$Time <- stringr::str_extract(df$line, "[0-9]{2}:[0-9]{2}")
    
    # check for directory...look for "  -  "
    df$isFolder <- as.integer(sapply(df$line, function(x) {!is.na(stringr::str_extract(x, "[ ]{2}-[ ]{2}"))}))
    
    t <- regexpr("[0-9]{2}:[0-9]{2}", df$line)
    attributes(t) <- NULL
    
    # get file size...assumes time is 5 characters and file size is no more than 8 characters
    df$Attribute <- trimws(
      substr(df$line, 
             t + 6, 
             t + 14
      )
    )
    
    df$Attribute <- sapply(df$Attribute, makeFileSize)
    
    # clean up
    df <- df[, c("Date", "Time", "Attribute", "Name")]
    
    # sort on attribute...<DIR> or size
    df <- df[order(df$Attribute), ]
  } else {
    df <- NULL
  }
  
  # we now have the directory listing...apply the options
  if (!is.null(df)) {
    if (!is.null(fileType)) {
      nameFlags <- grepl(paste0(fileType, "$"), df$Name, ignore.case=TRUE)
      
      df <- df[nameFlags, ]
    }
    
    if (excludeDirectories) {
      df <- df[df$Attribute != "<DIR>", ]
    }
    
    if (directoryOnly) {
      df <- df[df$Attribute == "<DIR>", ]
    }
    
    if (namesOnly) {
      df <- df$Name
    }
  }
  
  return(df)
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
#'   Attribute/Size, and file name; "ls" if the URL returns directory
#'   information similar to the UNIX \code{ls -al} command; or "" if you
#'   want the function to try to determine the directory format. This is
#'   ignored if the URL uses http or https schemes.
#' @param ... Arguments passed to \code{RCurl::getURL()}. This is ignored
#'   if the URL uses http or https schemes.
#' @return A list of file names or a data frame with file names and attribute
#'   information. The return value depends on \code{namesOnly}.
#' @examples
#' \dontrun{
#' DirList("ftp://rockyftp.cr.usgs.gov/vdelivery/Datasets/Staged/")
#' DirList(paste0("ftp://rockyftp.cr.usgs.gov/vdelivery/Datasets/",
#'    "Staged/Elevation/LPC/Projects/AK_BrooksCamp_2012/laz/"),
#'     "\\.las|\\.laz")
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

  # if URL is http or https, return list from DirList_http
  if (urltools::scheme(URL) == "http" || urltools::scheme(URL) == "https") {
    return(DirList_http(URL, 
                        fileType = fileType, 
                        namesOnly = namesOnly, 
                        excludeDirectories = excludeDirectories,
                        directoryOnly = directoryOnly
                        )
           )
  }
  
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
      if (dirFormat == "") {
        # look at first 10 characters of 1st string
        if (grepl("/", filenames[1], fixed = TRUE))
          dirFormat <- "dir"
        else
          dirFormat <- "ls"
      }
      
      # parse information
      if (dirFormat == "ls") {
        # add a blank filename entry
        filenames[length(filenames) + 1] <- ""
        df <- readr::read_table2(filenames, col_names = c("Permissions", "Links", "Owner", "Group", "Attribute", "Month", "Day", "Year", "Name"), n_max = length(filenames) - 1)
      } else {
        df <- readr::read_fwf(filenames, readr::fwf_widths(c(8, 9, 21, NA), c("Date", "Time", "Attribute", "Name")))
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
#' DirListByType is an alias for \code{DirList(URL, fileType = fileType, namesOnly = TRUE, ...)}
#'
#' @param URL URL for a folder on a remote host. Trailing slash is optional.
#' @param fileType Any valid string for the pattern parameter in \code{grep()}.
#'   "$" will be appended to the string to search for file/folder names ending
#'   with values in \code{fileType}.
#' @param ... Arguments passed to \code{RCurl::getURL()}. This is ignored
#'   if the URL uses http or https schemes.
#' @return A list of file names.
#' @examples
#' \dontrun{
#' DirListByType(paste0("ftp://rockyftp.cr.usgs.gov/vdelivery/Datasets/",
#'     "Staged/Elevation/LPC/Projects/AK_BrooksCamp_2012/laz/"),
#'     "\\.las|\\.laz")
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

  return(DirList(URL, fileType = fileType, namesOnly = TRUE, ...))
  
  # # get folder listing and parse into individual files
  # # create an empty character vector on error
  # filenames <- tryCatch(RCurl::getURL(URL, ftp.use.epsv = FALSE, dirlistonly = TRUE, ...), error = function(e) {character(0)})
  # if (length(filenames)) {
  #   filenames <- strsplit(filenames, "\r\n")
  #   filenames <- unlist(filenames)
  # 
  #   # find files ending with the desired type (extension)
  #   filenames <- grep(paste0(fileType, "$"), filenames, ignore.case=TRUE, value=TRUE)
  # }
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
#' @param ... Arguments passed to \code{RCurl::getURL()}.
#' @return A list of file names.
#' @examples
#' \dontrun{
#' DirListByName(paste0("ftp://rockyftp.cr.usgs.gov/vdelivery/Datasets/",
#'     "Staged/Hydrography/NHD/HU4/HighResolution/Shape/"),
#'     "NHD_H_0101_HU4_Shape.jpg")
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

  # get list of file names in URL
  filenames <- DirList(URL, namesOnly = TRUE, excludeDirectories = TRUE, ...)
  
  # get folder listing and parse into individual files
  # create an empty character vector on error
  # filenames <- tryCatch(RCurl::getURL(URL, ftp.use.epsv = FALSE, dirlistonly = TRUE, ...), error = function(e) {character(0)})
  if (length(filenames)) {
    # filenames <- strsplit(filenames, "\r\n")
    # filenames <- unlist(filenames)

    # find files matching the name...various extensions
    filenames <- grep(paste0("^", fileName), filenames, ignore.case=TRUE, value=TRUE)
  }
  
  return(filenames)
}

# ---------- ReadRemoteLASProjection
#
#' LidarIndexR -- Read LAS/LAZ header and extract CRS information
#'
#' Download the header for a LAS/LAZ file and read CRS information from the header.
#' Only the file header is downloaded (including VLRs) so you don't have to worry 
#' about the size of the LAS/LAZ file.
#'
#' @param URL URL for a LAS/LAZ file on a remote host.
#' @param tempFolder A valid folder name where the LAS/LAZ file header can be 
#'   temporarily stored. The header file is deleted after use.
#' @param quiet Boolean to control display of status information. If TRUE,
#'   information is *not* displayed. Otherwise, status information is displayed.
#' @return A string containing the CRS information retrieved from the LAS/LAZ
#'   header.
#' @examples
#' \dontrun{
#' ReadRemoteLASProjection()
#' }
#' @export
ReadRemoteLASProjection <- function (
  URL,
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

  con <- tryCatch(url(URL, open = "rb"), error = function(e) {NA})
  if (is.object(con)) {
    bs <- readBin(con, "raw", 120)
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
    con <- url(URL, open = "rb")
    bs <- readBin(con, "raw", OffsettoPointData)
    close(con)

    writeBin(bs, con = file.path(tempFolder, "__temp__.las"), useBytes = TRUE)
    if (file.exists(file.path(tempFolder, "__temp__.las"))) {
      # read header
      t <- tryCatch(lidR::readLASheader(file.path(tempFolder, "__temp__.las")), error = function(e) {NA})
      if (is.object(t)) {
        crs <- lidR::st_crs(t)
#        crs <- raster::projection(t)
      }

      # delete temp file
      file.remove(file.path(tempFolder, "__temp__.las"))
    } else {
      if (!quiet) message("Temp file not created: ", file.path(tempFolder, "__temp__.las"))
    }
  }
  if (!quiet) message(basename(url), ": ", crs)
  return(crs)
}

# ---------- ReadRemoteLASHeader
#
#' LidarIndexR -- Read the header for a remote LAS/LAZ file
#'
#' Reads the header of a LAS/LAZ file and returns header information in a
#' data frame. Only the header is read.
#'
#' @param URL URL for a LAS/LAZ file on a remote host.
#' @param quiet Boolean to control display of status information. If TRUE,
#'   information is *not* displayed. Otherwise, status information is displayed.
#' @return A data frame containing header information.
#' @examples
#' \dontrun{
#' ReadRemoteLASHeader()
#' }
#' @export
ReadRemoteLASHeader <- function(
  URL,
  quiet = FALSE
) {
  # open connection and read 375 bytes...header for V1.4 is 375 bytes
  # hopefully this is faster than reading individual values via ftp...but i didn't test
#  if (!quiet)
#    message("reading header for ", basename(URL))

  con <- url(URL, open = "rb")
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
      message("Read extent of ", basename(URL))
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
      message("Failed to read extent of ", basename(URL))
  }
  close(con)

  # build data frame to return
  df <- data.frame(
    "URL" = URL,
    "FileName" = basename(URL),
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

# ---------- FetchAndExtractCRSFromPoints
#
#' LidarIndexR -- Retrieve CRS information from a LAS/LAZ point file
#'
#' Reads CRS information from LAS/LAZ files on a remote host. This is done by
#' reading only the file header \code{headerOnly = TRUE} or retrieving the entire
#' file and reading the header \code{headerOnly = FALSE}. 
#'
#' @param baseURL URL for a folder on a remote host. Trailing slash should *not*
#'   be included.
#' @param folderName Folder name on the \code{baseURL} containing LAS/LAZ
#'   files.
#' @param tempFolder A valid folder name where the LAS/LAZ file header can be 
#'   temporarily stored. The header file is deleted after use.
#' @param headerOnly Boolean. If TRUE, only the file header is read. If FALSE,
#'   the entire LAS/LAZ file is retrieved and then the header is read to 
#'   extract CRS information.
#' @param fetchnew Boolean. If TRUE, the file is always retrieved. If FALSE,
#'   the file is only retrieved if it does not already exist in the \code{tempFolder}.
#' @param quiet Boolean to control display of status information. If TRUE,
#'   information is *not* displayed. Otherwise, status information is displayed.
#' @param ... Arguments passed to \code{RCurl::getURL()}.
#' @return String containing a valid input value for \code{st_crs()}.
#' @examples
#' \dontrun{
#' FetchAndExtractCRSFromPoints()
#' }
#' @export
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
    if (!quiet) message(folderName, "/", fileList$Name[1], " -- ")
    if (headerOnly) {
      crs <- ReadRemoteLASProjection(paste0(t, fileList$Name[1]), tempFolder, quiet = TRUE)
    } else {
      if (!file.exists(file.path(tempFolder, fileList$Name[1])) || fetchnew) {
        utils::download.file(paste0(t, fileList$Name[1]), file.path(tempFolder, fileList$Name[1]), quiet = quiet, mode = "wb", ...)
      }

      # load file and extract CRS (if any)
      las <- tryCatch(lidR::readLASheader(file.path(tempFolder, fileList$Name[1])), error = function(e) {NA})
      if (is.object(las)) {
        crs <- lidR::st_crs(las)
#        crs <- raster::projection(las)
      } else {
        crs <- NA
      }
    }
  }
  if (!quiet) {
    message(basename(baseURL), ": ", crs)
  }

  return(crs)
}

# function to download a shapefile, load it with st_read and
# get the crs. Return is a CRS in a proj4string


# ---------- FetchAndExtractCRSFromIndex
#
#' LidarIndexR -- Retrieve a shapefile (all files) and read CRS information
#'
#' Retrieves all files related to a shapefile (same name as \code{fileName} but
#' different extensions) and then reads CRS information from the projection 
#' file.
#'
#' @param baseURL URL for a folder on a remote host. Trailing slash should *not*
#'   be included.
#' @param folderName Folder name on the \code{baseURL} containing the shapefile.
#' @param fileName A string containing the name of a single file. All
#'   files with the same name (but different extensions) will be retrieved. The 
#'   projection file is then read to provide CRS information.
#' @param tempFolder A valid folder name where the files can be 
#'   temporarily stored.
#' @param fetchnew Boolean. If TRUE, the files are always retrieved. If FALSE,
#'   the files are only retrieved if they do not already exist in the \code{tempFolder}.
#' @param quiet Boolean to control display of status information. If TRUE,
#'   information is *not* displayed. Otherwise, status information is displayed.
#' @param ... Arguments passed to \code{RCurl::getURL()}.
#' @return String containing a valid input value for \code{st_crs()}.
#' @examples
#' \dontrun{
#' FetchAndExtractCRSFromIndex("ftp://rockyftp.cr.usgs.gov/vdelivery/Datasets/",
#'     "Staged/Hydrography/NHD/HU4/HighResolution/Shape/",
#'     "NHD_H_0101_HU4_Shape.shp",
#'     ".")
#' }
#' @export
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
        utils::download.file(paste0(t, filename), paste0(tempFolder, "/", filename), mode = "wb")
      }
    }

    # load index file and extract CRS (if any)
    index <- tryCatch(sf::st_read(file.path(tempFolder, fileName)), error = function(e) {NA})
    if (is.object(index)) {
      crs <- lidR::st_crs(index)
#      crs <- raster::projection(index)
    } else {
      crs <- NA
    }
  }
  if (!quiet) {
    message(basename(baseURL), ": ", crs)
  }

  return(crs)
}

# function to download .prj file associated with the .shp file passed in fileName
# Use the string read form the .prj file to get the crs. Return is a CRS in proj4string


# ---------- FetchAndExtractCRSFromPrj
#
#' LidarIndexR -- Retrieve a projection file and read CRS information
#'
#' Retrieves only the projection file related to a shapefile (same name as 
#' \code{fileName} but with .prj extensions) and then reads CRS information 
#' from the projection file.
#'
#' @param baseURL URL for a folder on a remote host. Trailing slash should *not*
#'   be included.
#' @param folderName Folder name on the \code{baseURL} containing the shapefile.
#' @param fileName A string containing the name of a single file. The projection
#'   file with the same name (but extension of .prj) will be retrieved. The 
#'   projection file is then read to provide CRS information.
#' @param tempFolder A valid folder name where the projection file can be 
#'   temporarily stored.
#' @param fetchnew Boolean. If TRUE, the file is always retrieved. If FALSE,
#'   the file is only retrieved if it does not already exist in the \code{tempFolder}.
#' @param quiet Boolean to control display of status information. If TRUE,
#'   information is *not* displayed. Otherwise, status information is displayed.
#' @param ... Arguments passed to \code{RCurl::getURL()}.
#' @return String containing a valid input value for \code{st_crs()}.
#' @examples
#' \dontrun{
#' FetchAndExtractCRSFromPrj("ftp://rockyftp.cr.usgs.gov/vdelivery/Datasets/",
#'     "Staged/Hydrography/NHD/HU4/HighResolution/Shape/",
#'     "NHD_H_0101_HU4_Shape.prj",
#'     ".")
#' }
#' @export
FetchAndExtractCRSFromPrj <- function (
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

  # we just need the .prj file
  filenames <- grep(paste0(".prj", "$"), filenames, ignore.case=TRUE, value=TRUE)

  # get all the files associated with the index shapefile
  if (length(filenames)) {
    for (f in filenames) {
      if (!file.exists(paste0(tempFolder, "/", f)) || fetchnew) {
        utils::download.file(paste0(t, f), paste0(tempFolder, "/", f), mode = "wb")
      }
    }

    f <- file(file.path(tempFolder, fileName), open = "rt")
    #    cat("Opening:", file.path(tempFolder, fileName), "\n")
    if (isOpen(f)) {
      l <- readLines(f, n = -1, warn = FALSE)
      close(f)
      #      cat(l, "\n")

      crs <- sp::CRS(l)@projargs
    } else {
      crs <- NA
    }
  }
  if (!quiet) {
    message(basename(baseURL), ": ", crs)
  }

  return(crs)
}

# ---------- BuildIndexFromPoints
#
#' LidarIndexR -- Create spatial index by reading LAS/LAZ file headers
#'
#' Longer description
#'
#' @param baseURL URL for a folder on a remote host. Trailing slash should *not*
#'   be included.
#' @param folderName Folder name on the \code{baseURL} containing LAS/LAZ files.
#' @param outputFile Full path and filename on the local file system for the index
#'   file.
#' @param projString A valid projection string that can be used with the \code{crs}
#'   parameter in \code{st_sf}. \code{projString} should represent the projection
#'   pf the point data. If using EPSG codes, do not enclose the EPSG number in quotes.
#' @param outputCRS A valid projection string that can be used with the \code{crs}
#'     parameter in \code{st_transform} to reproject the index.
#' @param fileType Any valid string for the pattern parameter in \code{grep()}.
#'   "$" will be appended to the string to search for file/folder names ending
#'   with values in \code{fileType}.
#' @param dirFormat String indicating the expected directory format for the
#'   \code{URL}. Valid values are "dir" if the URL returns the Date, Time,
#'   Attribute/Size, and file name; "ls" if the URL returns directory
#'   information similar to the UNIX \code{ls -al} command; or "" if you
#'   want the function to try to determine the directory format.
#' @param dimensionThreshold Size threshold used to omit files from the index.
#'   This is intended to help omit invalid LAS.LAZ files from the index. If
#'   the height or width of the point tile exceeds the threshold, the tile
#'   will ne omitted.
#' @param appendInfo Data frame (single row) with values to append to each 
#'   feature in the return \code{sf} object. If NULL, no information is appended.
#'   If \code{appendInfo} is \code{sf} object, the geometry will be removed
#'   before appending to each feature.
#' @param rebuild Boolean. If TRUE, the index is always created. If FALSE,
#'   the index is only created if it does not already exist.
#' @param quiet Boolean to control display of status information. If TRUE,
#'   information is *not* displayed. Otherwise, status information is displayed.
#' @return Boolean indicating success (TRUE) or failure (FALSE).
#' @examples
#' \dontrun{
#' BuildIndexFromPoints()
#' }
#' @export
BuildIndexFromPoints <- function (
  baseURL,
  folderName,
  outputFile,
  projString = NULL,
  outputCRS = NULL,
  fileType = "\\.las|\\.laz",
  dirFormat = "ls",
  dimensionThreshold = 50000,
  appendInfo = NULL,
  rebuild = FALSE,
  quiet = FALSE
) {
  if (file.exists(outputFile) && !rebuild) {
    if (!quiet) message("Index already exist...skipping: ", basename(outputFile))
    return(TRUE);
  }

  folderURL <- paste0(baseURL, "/", folderName, "/")

  # get list of .laz & .las files...full directory info
  flist <- DirList(folderURL, "\\.las|\\.laz", dirFormat = dirFormat)

  if (nrow(flist) > 0) {
    if (!quiet) message("Building index: ", basename(outputFile))

    # prepend folder path to file names
    fileURLs <- paste0(folderURL, flist$Name)

    # read headers
    t <- lapply(fileURLs, ReadRemoteLASHeader)    # returns a list of dataframes

    # convert to a simple dataframe
    t_df <- do.call("rbind", t)

    # drop any rows with NA values...bad LAS file...only check min/max XYZ values
    t_df <- t_df[stats::complete.cases(t_df[, 11:16]), ]

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
        sf::st_polygon(list(res))
      }
      )

      tiles_sf <- sf::st_sf(t_df, sf::st_sfc(lst), crs = projString)

      if (!is.null(projString)) {
        tiles_sf <- sf::st_set_crs(tiles_sf, projString)  
      }
      
      # reproject to outputCRS
      if (!is.null(projString) && !is.null(outputCRS)) {
        tiles_sf <- sf::st_transform(tiles_sf, crs = outputCRS)
      }

      if (!is.null(appendInfo)) {
        # see if appendInfo is sf object...if so drop the geometry
        if (inherits(appendInfo, "sf"))
          appendInfo$geometry <- NULL
        
        # duplicate rows
        info <- do.call("rbind", replicate(nrow(tiles_sf), appendInfo, simplify = FALSE))
        
        # cbind to tile data frame
        tiles_sf <- cbind(tiles_sf, info)
      }

            # write output
      sf::st_write(tiles_sf, outputFile, delete_dsn = TRUE, quiet = TRUE)

      return(TRUE)
    } else {
      if (!quiet) message("   ***No LAS polygons")
    }
  } else {
    if (!quiet) message("   ***No LAS files")
  }

  return(FALSE)
}

# ---------- BuildIndexFromUSGSProjectIndexItem
#
#' LidarIndexR -- Create spatial index by reading LAS/LAZ file headers
#' associated with a USGS project index item.
#'
#' Longer description
#'
#' @param projectItem Data frame of sf object with information for a single USGS
#'   lidar project as found in the USGS FESM index.
#' @param folderName Folder name on the \code{baseURL} containing LAS/LAZ files.
#' @param outputFile Full path and file name on the local file system for the index
#'   file.
#' @param projString A valid projection string that can be used with the \code{crs}
#'   parameter in \code{st_sf}. \code{projString} should represent the projection
#'   of the point data. If using EPSG codes, do not enclose the EPSG number in quotes.
#' @param outputCRS A valid projection string that can be used with the \code{crs}
#'     parameter in \code{st_transform} to reproject the index.
#' @param fileType Any valid string for the pattern parameter in \code{grep()}.
#'   "$" will be appended to the string to search for file/folder names ending
#'   with values in \code{fileType}.
#' @param dirFormat String indicating the expected directory format for the
#'   \code{URL}. Valid values are "dir" if the URL returns the Date, Time,
#'   Attribute/Size, and file name; "ls" if the URL returns directory
#'   information similar to the UNIX \code{ls -al} command; or "" if you
#'   want the function to try to determine the directory format.
#' @param appendCols List of column numbers or column names for information from the 
#'   \code{projectItem} to append to individual tile records in the index. If 
#'   column names conflict with existing column names, they will be adjusted
#'   to remove the conflict. if NULL, no column information is appended.
#' @param dimensionThreshold Size threshold used to omit files from the index.
#'   This is intended to help omit invalid LAS.LAZ files from the index. If
#'   the height or width of the point tile exceeds the threshold, the tile
#'   will ne omitted.
#' @param rebuild Boolean. If TRUE, the index is always created. If FALSE,
#'   the index is only created if it does not already exist.
#' @param quiet Boolean to control display of status information. If TRUE,
#'   information is *not* displayed. Otherwise, status information is displayed.
#' @return Boolean indicating success (TRUE) or failure (FALSE).
#' @examples
#' \dontrun{
#' BuildIndexFromUSGSProjectIndexItem()
#' }
#' @export
BuildIndexFromUSGSProjectIndexItem <- function (
  projectItem,
  folderName,
  outputFile,
  projString = NULL,
  outputCRS = NULL,
  fileType = "\\.las|\\.laz",
  dirFormat = "ls",
  appendCols = c("workunit_id",
                 "ql", 
                 "collect_start",
                 "collect_end",
                 "p_method",
                 "horiz_crs",
                 "vert_crs"),
  dimensionThreshold = 50000,
  rebuild = FALSE,
  quiet = FALSE
) {
  if (file.exists(outputFile) && !rebuild) {
    if(!quiet) message("Index already exists...skipping: ", basename(outputFile))
    return(TRUE);
  }

  URL <- projectItem$lpc_link
  if (!endsWith(URL, "/"))
    URL <- paste0(URL, "/")

  folderURL <- paste0(URL, folderName, "/")
#  cat("Folder:", folderURL, "\n")
  
  # get list of .laz & .las files...full directory info
  flist <- DirList(folderURL, "\\.las|\\.laz", dirFormat = dirFormat)
  
  if (nrow(flist) > 0) {
    if (!quiet) message("Building index: ", basename(outputFile))
    
    # prepend folder path to file names
    fileURLs <- paste0(folderURL, flist$Name)
    
    # read headers
    t <- lapply(fileURLs, ReadRemoteLASHeader)    # returns a list of dataframes
    
    # convert to a simple dataframe
    t_df <- do.call("rbind", t)
    
    # drop any rows with NA values...bad LAS file...only check min/max XYZ values
    t_df <- t_df[stats::complete.cases(t_df[, 11:16]), ]
    
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
        sf::st_polygon(list(res))
      }
      )
      
      if (is.null(projString)) {
        prs <- strtoi(projectItem$horiz_crs)
      } else {
        prs <- projString
      }
      
      tiles_sf <- sf::st_sf(t_df, sf::st_sfc(lst), crs = prs)
      
      tiles_sf <- sf::st_set_crs(tiles_sf, prs)  

      # reproject to outputCRS
      if (!is.na(lidR::st_crs(tiles_sf)) && !is.null(outputCRS)) {
#      if (!is.na(raster::projection(tiles_sf)) && !is.null(outputCRS)) {
          tiles_sf <- sf::st_transform(tiles_sf, crs = outputCRS)
      }
      
      # see if we have a list of columns to add to the tiles
      if (!is.null(appendCols)) {
        # build data frame with requested columns repeated over rows
        info <- data.frame(projectItem[, appendCols])
        
        # drop geometry
        info$geometry <- NULL
        
        # duplicate rows
        info <- do.call("rbind", replicate(nrow(tiles_sf), info, simplify = FALSE))
        
        # cbind to tile data frame
        tiles_sf <- cbind(tiles_sf, info)
      }
      # else {
      #   # add fields from the project information
      #   tiles_sf$workunit_id <- projectItem$workunit_id
      #   tiles_sf$ql <- projectItem$ql
      #   tiles_sf$collect_start <- projectItem$collect_start
      #   tiles_sf$collect_end <- projectItem$collect_end
      #   tiles_sf$p_method <- projectItem$p_method
      #   tiles_sf$horiz_crs <- projectItem$horiz_crs
      #   tiles_sf$vert_crs <- projectItem$vert_crs
      # 
      #   # rearrange
      #   tiles_sf <- tiles_sf[, c(1:2, 17:23, 3:16, 24)]
      # }

      # write output
      sf::st_write(tiles_sf, outputFile, delete_dsn = TRUE, quiet = TRUE)
      
      return(TRUE)
    } else {
      if (!quiet) message("   ***No LAS polygons")
    }
  } else {
    if (!quiet) message("   ***No LAS files")
  }
  
  return(FALSE)
}

# ---------- BuildProjectPolygonFromIndex
#
#' LidarIndexR -- Create polygon(s) representing the area covered by a tile index
#'
#' Creates a bounding polygon(s) given a set of point tile polygons. This is useful to
#' create project polygons to display the areas covered by a lidar project.
#'
#' @param indexFile Full path and filename on the local file system for the tile index
#'   file.
#' @param projectIdentifier Character string containing an identifier for the project
#'   area. This will be added to each feature in the return \code{sf} object.
#' @param nx Integer specifying the number of cells (width) of the raster used
#'   to merge polygons.
#' @param ny Integer specifying the number of cells (height) of the raster used
#'   to merge polygons.
#' @param outputCRS A valid projection string that can be used with the \code{crs}
#'   parameter in \code{st_transform} to reproject the index. If using EPSG codes,
#'   do not enclose the EPSG number in quotes.
#' @param appendInfo Data frame (single row) with values to append to each feature 
#'   in the return \code{sf} object. If NULL, no information is appended.
#'   If \code{appendInfo} is \code{sf} object, the geometry will be removed
#'   before appending to each feature.
#' @param outputFile Full path and file name on the local file system for the index
#'   file.
#' @param rebuild Boolean. If TRUE, the index is always created. If FALSE,
#'   the index is only created if it does not already exist.
#' @param quiet Boolean to control display of status information. If TRUE,
#'   information is *not* displayed. Otherwise, status information is displayed.
#' @return An \code{sf} object containing the polygon(s) for the project area.
#' @examples
#' \dontrun{
#' BuildProjectPolygonFromIndex()
#' }
#' @export
BuildProjectPolygonFromIndex <- function (
  indexFile,
  projectIdentifier,
  nx = 512,
  ny = 512,
  outputCRS = NULL,
  appendInfo = NULL,
  outputFile = NULL,
  rebuild = FALSE,
  quiet = TRUE
) {
  if (!rebuild && !is.null(outputFile)) {
    # see if output file exists
    if (file.exists(outputFile)) {
      if (!quiet) message("Project polygon file already exists...skipping: ", basename(outputFile))
      return(TRUE);
    }
  }
  
  t_sf <- sf::st_read(indexFile, quiet = TRUE)
  
  if (is.object(t_sf)) {
    t_sf$PID <- 1
    
    # rasterize
    r_poly <- stars::st_rasterize(t_sf[, "PID"], nx = nx, ny = ny)
    
    # convert back to polygons...merge tiles
    t_rp <- sf::st_as_sf(r_poly, as_points = FALSE, merge = TRUE)
    sf::st_make_valid(t_rp)
    
    if (!is.null(outputCRS)) {
      t_rp <- sf::st_transform(t_rp, crs = outputCRS)
    }
    
    t_rp$PID <- projectIdentifier

    if (!is.null(appendInfo)) {
      # see if appendInfo is sf object...if so drop the geometry
      if (inherits(appendInfo, "sf"))
        appendInfo$geometry <- NULL
      
      # duplicate rows
      info <- do.call("rbind", replicate(nrow(t_rp), appendInfo, simplify = FALSE))
      
      # cbind to tile data frame
      t_rp <- cbind(t_rp, info)
    }
    
    if (!quiet) message("Done with: ", indexFile)
    
    if (!is.null(outputFile)) {
      sf::st_write(t_rp, outputFile, projectIdentifier, delete_dsn = TRUE, quiet = TRUE)
    }
    
    return(t_rp)
  } else {
    if (!quiet) message("Problems reading index file: ", indexFile)
  }
  return(NULL)
}

