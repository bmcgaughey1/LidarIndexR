% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/LocalFileFunctions.R
\name{ReadLocalLASProjection}
\alias{ReadLocalLASProjection}
\title{LidarIndexR -- Read LAS/LAZ header and extract CRS information}
\usage{
ReadLocalLASProjection(path, quiet = TRUE)
}
\arguments{
\item{path}{Path for a LAS/LAZ file.}

\item{quiet}{Boolean to control display of status information. If TRUE,
information is \emph{not} displayed. Otherwise, status information is displayed.}
}
\value{
A string (invisible) containing the CRS WKT information retrieved from the LAS/LAZ
header. Returns an empty string if file has no CRS information.
}
\description{
Read the header for a LAS/LAZ file and read CRS information from the header.
Only the file header is read (including VLRs) so you don't have to worry
about the size of the LAS/LAZ file.
}
\examples{
\dontrun{
ReadLocalLASProjection()
}
}
