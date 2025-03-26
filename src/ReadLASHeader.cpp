#include <Rcpp.h>
#include <fstream>
#include <iostream>

using namespace Rcpp;
using namespace std;

// [[Rcpp::export]]
DataFrame ReadLASHeader(std::string path) {
  // declare variables
  unsigned char signature[4];
  unsigned char VersionMajor;
  unsigned char VersionMinor;
  unsigned short HeaderSize;
  unsigned long VLRCount;
  unsigned long LegacyPointCount;
  unsigned long long PointCount;
  bool compressed;
  bool copc;
  unsigned short DayOfYear;
  unsigned short Year;
  unsigned char PointRecordFormat;
  unsigned short PointRecordLength;
  double MinX, MinY, MinZ, MaxX, MaxY, MaxZ;
  unsigned char trash[128];
  
  std::string crs;
  
  ifstream infile;
  
  // open file and read header information into local variables
  infile.open(path, ios::in | ios::binary);
  infile.read(signature, sizeof(signature));
  if (signature[0] == 'L' && signature[1] == 'A' && signature[2] == 'S' && signature[3] == 'F') {
    infile.read(trash, 20);
    infile.read(versionMajor, sizeof(VersionMajor));
    infile.read(versionMinor, sizeof(VersionMinor));
    infile.read(trash, 64);
    infile.read(DayOfYear, sizeof(DayOfYear));
    infile.read(Year, sizeof(Year));
    infile.read(HeaderSize, sizeof(HeaderSize));
    infile.read(trash, 4);
    infile.read(VLRCount, sizeof(VLRCount));
    infile.read(PointRecordFormat, sizeof(PointRecordFormat));
    infile.read(PointRecordLength, sizeof(PointRecordLength));
    infile.read(LegacyPointCount, sizeof(LegacyPointCount));
    PointCount = (unsigned long long) LegacyPointCount;
    infile.read(trash, 68);
    infile.read(MaxX, sizeof(MaxX));
    infile.read(MaxY, sizeof(MaxY));
    infile.read(MaxZ, sizeof(MaxZ));
    infile.read(MinX, sizeof(MinX));
    infile.read(MinY, sizeof(MinY));
    infile.read(MinZ, sizeof(MinZ));
    if (VersionMajor == 1 && VersionMinor > 3) {
      infile.read(trash, 20);
      infile.read(PointCount, sizeof(PointCount));
    }
    
    // read VLRs
    if (VLRCount > 0) {
      for(unsigned long vlr = 0; vlr < VLRCount; vlr++) {
        infile.seekg(HeaderSize, infile.beg);
        
      }
    }
    
    
    // create DataFrame using local variables
    DataFrame df = DataFrame::create(
      Named("filespec") = path,
      Named("pointcount") = PointCount,
      Named("compressed") = compressed,
      Named("copc") = copc,
      Named("creation_day") = DayOfYear,
      Named("creation_year") = Year,
      Named("point_record_format") = PointRecordFormat,
      Named("point_record_length") = PointRecordLength,
      Named("major_version") = VersionMajor,
      Named("minor_version") = VersionMinor,
      Named("minx") = MinX,
      Named("miny") = MinY,
      Named("minz") = MinZ,
      Named("maxx") = MaxX,
      Named("maxy") = MaxY,
      Named("maxz") = MaxZ,
      Named("crs") = crs
    );
  }
  return df;
}
