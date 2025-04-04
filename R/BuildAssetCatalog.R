# BuildAssetCatalog
#
# ---------- BuildAssetCatalog
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
#' @param folder Path for a folder. Trailing slash should *not*
#'   be included.
#' @param fileType Any valid string for the pattern parameter in \code{grep()}.
#'   "$" will be appended to the string to search for file/folder names ending
#'   with values in \code{fileType}.
#' @param crs A valid projection string that can be used with the \code{crs}
#'   parameter in \code{st_sf}. \code{projString} should represent the projection
#'   of the point data. \code{crs} should be WKT.
#' @param outputcrs A valid projection string that can be used with the \code{crs}
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
#'   file. The only supported format for catalogs is geopackage.
#' @param rebuild Boolean. If TRUE, the index is always created. If FALSE,
#'   the index is only created if it does not already exist.
#' @param quiet Boolean to control display of status information. If TRUE,
#'   information is *not* displayed. Otherwise, status information is displayed.
#' @return Boolean (invisible) indicating success or failure. If \code{rebuild = FALSE} 
#'   and \code{outputFile} exists, returns TRUE.
#' @examples
#' \dontrun{
#' BuildAssetCatalog()
#' }
#' @export
BuildAssetCatalog <- function (
    folder = "",
    fileType = "\\.las|\\.laz",
    crs = "",
    outputcrs = "",
    fullFileList = character(0),
    dimensionThreshold = 50000,
    headerMethod = "rcpp",        #direct: binary read of header, lidr: use lidR package
    outputFile = "",
    rebuild = FALSE,
    quiet = TRUE
) {
  # check for existing outputFile
  if (rebuild == FALSE && file.exists(outputFile)) {
    cat("Index aleady exists\n")
    return(TRUE)
  }
  
  # clean up the folder...deals with slashes and trailing slash
  folder <- normalizePath(folder)
  
  # make sure folder exists
  if (!file.exists(folder)) {
    cat(paste("Folder does not exit:", folder, "\n"))
    return(invisible(FALSE))
  }
  
  # check output format
  fileExt <- tools::file_ext(outputFile)
  if (tolower(fileExt) != "gpkg") {
    cat("Only geopackage format is supported for asset catalogs!!\n")
    return(invisible(FALSE))
  }
  
  # generate a temporary file for tile index
  tempof <- tempfile(fileext = ".gpkg")
  
  # look for target files and create index using temp file name
  tileIndex <- BuildIndexFromLocalPoints(folder, fileType, outputFile = tempof, rebuild = rebuild, headerMethod = "rcpp", quiet = quiet)
  
  if (nrow(tileIndex) > 0) {
    # build attributes for bounding box and wrapping polygon
    odf <- data.frame(
      base = folder,
      pattern = fileType,
      assettype = "points",
      assetcount = nrow(tileIndex),
      assetsize = sum(tileIndex$filesize),
      totalpointcount = sum(tileIndex$pointcount),
      hasCRS = (tileIndex$crs[1] != ""),
      minx = as.list(sf::st_bbox(tileIndex))[[1]],
      miny = as.list(sf::st_bbox(tileIndex))[[3]],
      maxx = as.list(sf::st_bbox(tileIndex))[[2]],
      maxy = as.list(sf::st_bbox(tileIndex))[[4]],
      crs = tileIndex$crs[1],
      assignedCRS = ""
    )
    
    # build wrapping polygon
    p <- BuildProjectPolygonFromIndex(tempof, projectIdentifier = "", nx = 2048, ny = 2048, appendInfo = odf)
    # drop PID column...first column
    p <- p[, -1]
    
    # delete temp file
    unlink(tempof)
    
    # build bounding box polygon
    bb <- sf::st_bbox(tileIndex)
    op <- sf::st_as_sf(sf::st_sfc(sf::st_polygon(list(matrix(c(
      bb[[1]], bb[[2]],  
      bb[[3]], bb[[2]],
      bb[[3]], bb[[4]],  
      bb[[1]], bb[[4]],
      bb[[1]], bb[[2]]   
    ), ncol = 2, byrow = TRUE))), crs = odf$crs))
    op <- cbind(odf, op)

    # write index...
    if (tolower(fileExt) == "gpkg") {
      # write layers to single file
      sf::st_write(op, outputFile, layer = "boundingbox", delete_dsn = TRUE, quiet = TRUE)
      sf::st_write(p, outputFile, layer = "boundary", delete_layer = TRUE, quiet = TRUE)
      sf::st_write(tileIndex, outputFile, layer = "assets", delete_layer = TRUE, quiet = TRUE)
    } else {
      # for now, only support geopackage format. Shapefile format has problems with large numbers,
      # column widths and column names
      cat("Only geopackage format is supported for asset catalogs!!\n")
      return(FALSE)
      # # get filename and extension
      # fileTitle <- tools::file_path_sans_ext(outputFile)
      # fileExt <- tools::file_ext(outputFile)
      # 
      # # write separate files
      # sf::st_write(op, paste0(fileTitle, "_boundingbox.", fileExt), delete_dsn = TRUE, quiet = TRUE)
      # sf::st_write(p, paste0(fileTitle, "_boundary.", fileExt), delete_dsn = TRUE, quiet = TRUE)
      # sf::st_write(tileIndex, paste0(fileTitle, "_assets.", fileExt), delete_dsn = TRUE, quiet = TRUE)
    }
    return(invisible(TRUE))
  } 
}
