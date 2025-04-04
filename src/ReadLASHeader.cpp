#include <Rcpp.h>
#include <fstream>
#include <iostream>
#include <string>
#include <vector>

using namespace Rcpp;
using namespace std;

// ---------- ReadLocalLASHeader
//
//' LidarIndexR -- Read the header for a local LAS/LAZ file
//'
//' Read the header information for a LAS/LAZ file including CRS information.
//' Only the file header is read (including VLRs) so you don't have to worry 
//' about the size of the LAS/LAZ file. This function is implemented in C++
//' and is 2+ orders of magnitude faster than \code{ReadLocalLASHeader()}.
//'
//' @param path Path for a LAS/LAZ file.
//' @return A data frame containing the CRS WKT information string and
//'   header info. If file has no CRS information, the crs column in the data frame
//'   will be an empty string.
//' @examples
//' \dontrun{
//' ReadLASHeader("C:/data")
//' }
//' @export
// [[Rcpp::export]]
Rcpp::DataFrame ReadLASHeader(std::string path) {
  // declare variables
  size_t fileSize;
  char signature[4];
  unsigned char VersionMajor;
  unsigned char VersionMinor;
  unsigned short HeaderSize;
  unsigned long VLRCount;
  unsigned long LegacyPointCount;
  unsigned long long PointCount;
  bool compressed = false;
  bool copc = false;
  unsigned short DayOfYear;
  unsigned short Year;
  unsigned char PointRecordFormat;
  unsigned short PointRecordLength;
  double MinX, MinY, MinZ, MaxX, MaxY, MaxZ;
  unsigned short prf, majv, minv;
  std::vector<char> trash(128);

  // VLR variables
  unsigned short reserved;
  char userID[17];
  unsigned short recordID;
  unsigned short recordLength;
  char description[33];
  
  std::string crs;
  
  std::ifstream infile;
  
  // open file and read header information into local variables
  infile.open(path, std::ios::binary);
  
  if (infile) {
    // get file size...seek to end and ask
    infile.seekg(0, std::ios::end);
    fileSize = infile.tellg();
    infile.seekg(0, std::ios::beg);
    
    // signature for LAS/LAZ is "LASF", signature for zLIDAR is "ZLDR" (otherwise header is the same)
    infile.read(&signature[0], sizeof(signature));
    if ((signature[0] == 'L' && signature[1] == 'A' && signature[2] == 'S' && signature[3] == 'F') || 
        (signature[0] == 'Z' && signature[1] == 'L' && signature[2] == 'D' && signature[3] == 'R')) {
      infile.read(trash.data(), 20);
      infile.read(reinterpret_cast<char *>(&VersionMajor), sizeof(VersionMajor));
      infile.read(reinterpret_cast<char *>(&VersionMinor), sizeof(VersionMinor));
      infile.read(trash.data(), 64);
      infile.read(reinterpret_cast<char *>(&DayOfYear), sizeof(DayOfYear));
      infile.read(reinterpret_cast<char *>(&Year), sizeof(Year));
      infile.read(reinterpret_cast<char *>(&HeaderSize), sizeof(HeaderSize));
      infile.read(trash.data(), 4);
      infile.read(reinterpret_cast<char *>(&VLRCount), sizeof(VLRCount));
      infile.read(reinterpret_cast<char *>(&PointRecordFormat), sizeof(PointRecordFormat));
      infile.read(reinterpret_cast<char *>(&PointRecordLength), sizeof(PointRecordLength));
      infile.read(reinterpret_cast<char *>(&LegacyPointCount), sizeof(LegacyPointCount));
      PointCount = (unsigned long long) LegacyPointCount;
      infile.read(trash.data(), 68);
      infile.read(reinterpret_cast<char *>(&MaxX), sizeof(MaxX));
      infile.read(reinterpret_cast<char *>(&MinX), sizeof(MinX));
      infile.read(reinterpret_cast<char *>(&MaxY), sizeof(MaxY));
      infile.read(reinterpret_cast<char *>(&MinY), sizeof(MinY));
      infile.read(reinterpret_cast<char *>(&MaxZ), sizeof(MaxZ));
      infile.read(reinterpret_cast<char *>(&MinZ), sizeof(MinZ));
      if (VersionMajor == 1 && VersionMinor > 3) {
        infile.read(trash.data(), 20);
        infile.read(reinterpret_cast<char *>(&PointCount), sizeof(PointCount));
      }
      
      if (PointRecordFormat > 127) PointRecordFormat -= 127;
      prf = (unsigned int) PointRecordFormat;
      majv = (unsigned int) VersionMajor;
      minv = (unsigned int) VersionMinor;
      
      // read VLRs
      if (VLRCount > 0) {
        // seek to end of header
        infile.seekg(HeaderSize, std::ios::beg);
        for(unsigned long vlr = 0; vlr < VLRCount; vlr++) {
          infile.read(reinterpret_cast<char *>(&reserved), sizeof(reserved));
          infile.read(&userID[0], sizeof(userID) - 1);
          infile.read(reinterpret_cast<char *>(&recordID), sizeof(recordID));
          infile.read(reinterpret_cast<char *>(&recordLength), sizeof(recordLength));
          infile.read(&description[0], sizeof(description) - 1);
          
          // terminate strings
          userID[16] = '\0';
          description[32] = '\0';
          
          // check for LAZ and COPC VLRs
          if (strcmp(userID, "copc") == 0) copc = true;
          if (strcmp(userID, "laszip encoded") == 0) compressed = true;
          
          // check for WKT
          if (strcmp(userID, "LASF_Projection") == 0 && recordID == 2112) {
            // read WKT into crs
            std::vector<char> buffer(recordLength);
            infile.read(buffer.data(), recordLength);
            crs = std::string(buffer.begin(), buffer.end());
          } else {
            // seek past VLR data
            infile.seekg(recordLength, infile.cur);
          }
        }
      }
      
      // create DataFrame using local variables
      Rcpp::DataFrame df = DataFrame::create(
        Named("filespec") = path,
        Named("filesize") = fileSize,
        Named("pointcount") = PointCount,
        Named("compressed") = compressed,
        Named("copc") = copc,
        Named("creation_day") = DayOfYear,
        Named("creation_year") = Year,
        Named("point_record_format") = prf,
        Named("point_record_length") = PointRecordLength,
        Named("major_version") = majv,
        Named("minor_version") = minv,
        Named("minx") = MinX,
        Named("miny") = MinY,
        Named("minz") = MinZ,
        Named("maxx") = MaxX,
        Named("maxy") = MaxY,
        Named("maxz") = MaxZ,
        Named("crs") = crs
      );
      return df;
    }
  }
  // return empty dataframe
  return Rcpp::DataFrame();
}
