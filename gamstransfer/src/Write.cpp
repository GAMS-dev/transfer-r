// GAMS - General Algebraic Modeling System Python API
//
// Copyright (c) 2017-2022 GAMS Software GmbH <support@gams.com>
// Copyright (c) 2017-2022 GAMS Development Corp. <support@gams.com>
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in all
// copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.

#include <Rcpp.h>
#include "gdxcc.h"
#include "gclgms.h"
using namespace Rcpp;

void WriteData(gdxHandle_t PGX, StringVector s,
std::vector<double> V, int VarType, int Dim,
std::string elemText, std::string sym_name) {

  gdxStrIndexPtrs_t Indx;
  gdxStrIndex_t Indx_labels;
  gdxValues_t       Values;
	int rc;
  char gdx_err_msg[GMS_SSSIZE];
  std::string rec_name;
  GDXSTRINDEXPTRS_INIT(Indx_labels, Indx);
  for (int D=0; D < Dim; D++) {
    strcpy(Indx[D], s[D]);
  }

  if (VarType == GMS_DT_VAR || VarType == GMS_DT_EQU) {
    Values[GMS_VAL_LEVEL] = V[GMS_VAL_LEVEL];
    Values[GMS_VAL_MARGINAL] = V[GMS_VAL_MARGINAL];
    Values[GMS_VAL_UPPER] = V[GMS_VAL_UPPER];
    Values[GMS_VAL_LOWER] = V[GMS_VAL_LOWER];
    Values[GMS_VAL_SCALE] = V[GMS_VAL_SCALE];
  }
  else if (VarType == GMS_DT_SET) {
    if (elemText.compare("") != 0 ) {
      int txtnr;
      if (!gdxAddSetText(PGX, elemText.c_str(), &txtnr)) {
        stop("WriteData:gdxAddSetText GDX error (gdxAddSetText)");
      }
      Values[GMS_VAL_LEVEL] = txtnr;
      Values[GMS_VAL_MARGINAL] = 0;
      Values[GMS_VAL_UPPER] = 0;
      Values[GMS_VAL_LOWER] = 0;
      Values[GMS_VAL_SCALE] = 0;

    }
    else {
      Values[GMS_VAL_LEVEL]=0;
    }
  }
  else {
    Values[GMS_VAL_LEVEL] = V[GMS_VAL_LEVEL];
  }

	rc = gdxDataWriteStr(PGX, (const char **)Indx, Values);
  if (!rc) {
    gdxErrorStr(PGX, gdxGetLastError(PGX), gdx_err_msg);

    rec_name = rec_name + sym_name;
    rec_name = rec_name + "(";
    for (int i = 0; i < Dim; i++)
    {
        if (i > 0)
           rec_name = rec_name + ",";
        rec_name = rec_name + s[i];
    }

    rec_name = rec_name + ")";

    stop("WriteData:gdxDataWriteStr GDX error in record %s:%s", rec_name, gdx_err_msg );
  }

  return;
}




// [[Rcpp::export]]
void CPP_gdxWriteSuper(List data, LogicalVector enable, CharacterVector sysDir, 
CharacterVector fileName, CharacterVector uel_priority, 
bool is_uel_priority, bool compress) {

  std::string myUEL;
  std::string mysysDir = Rcpp::as<std::string>(sysDir);
  std::string myFileName = Rcpp::as<std::string>(fileName);
  gdxHandle_t PGX = NULL;
	char        Msg[GMS_SSSIZE];
	int         ErrNr, rc, varType, varSubType;
  gdxStrIndexPtrs_t domains_ptr;
  gdxStrIndex_t domains;
  GDXSTRINDEXPTRS_INIT(domains, domains_ptr);


  rc = gdxCreateD(&PGX, mysysDir.c_str(), Msg, sizeof(Msg));
  if (!rc) stop("CPP_gdxWriteSuper:gdxCreateD GDX init failed: %s", Msg);

	gdxGetDLLVersion(PGX, Msg);

	/* Write demand data */
  if (!compress) {
    rc = gdxOpenWrite(PGX, myFileName.c_str(), "GAMS Transfer", &ErrNr);
    if (!rc) stop("CPP_gdxWriteSuper:gdxOpenWrite Error opening the file %s with error code %i", myFileName, ErrNr);
  }
  else {
    rc = gdxOpenWriteEx(PGX, myFileName.c_str(), "GAMS Transfer", 1, &ErrNr);
    if (!rc) stop("CPP_gdxWriteSuper:gdxOpenWriteEx Error opening the file %s with error code %i", myFileName, ErrNr);
  }

  gdxSVals_t sVals;
  gdxGetSpecialValues(PGX, sVals);

  sVals[GMS_SVIDX_NA] = NA_REAL;
  sVals[GMS_SVIDX_EPS] = -0.0;
  sVals[GMS_SVIDX_UNDEF] = R_NaN;
  sVals[GMS_SVIDX_PINF] = R_PosInf;
  sVals[GMS_SVIDX_MINF] = R_NegInf;

  rc = gdxSetSpecialValues(PGX, sVals);
  if (!rc) stop("CPP_gdxWriteSuper:gdxSetSpecialValues GDX error (gdxSetSpecialValues)");

  // register UELs
  int UELno;
  if (is_uel_priority) {
    rc = gdxUELRegisterStrStart(PGX);
    if (!rc) stop("CPP_gdxWriteSuper:gdxUELRegisterStrStart GDX error (gdxUELRegisterStrStart)");
    for (int i = 0; i < uel_priority.length(); i++) {
      myUEL = uel_priority(i);
      rc = gdxUELRegisterStr(PGX, myUEL.c_str(), &UELno);
      if (!rc) stop("Error registering UEL: %s", myUEL);
    }
    if (!gdxUELRegisterDone(PGX))
      stop("CPP_gdxWriteSuper:gdxUELRegisterDone GDX error (gdxUELRegisterDone)");
  }

  DataFrame df;
  List domain;
  int Dim, sym_nr;
  std::vector<double> values;
  std::string elemText;
  StringVector colString, colElemText;
  NumericVector colDouble;

  sym_nr = 0;
  for (int d=0; d < data.length(); d++) {

    if (!enable[d]) continue;
    sym_nr++;
    Environment symname = data[d];
    std::string mysym = symname["name"];
    varType = symname[".gams_type"];
    varSubType = symname[".gams_subtype"];

    if (varType == GMS_DT_ALIAS) {
      bool isUniverseAlias = symname[".isUniverseAlias"];

      if (isUniverseAlias == true) {
        std::string alias_with = symname["aliasWith"];

        if (!gdxAddAlias(PGX, mysym.c_str(), alias_with.c_str()))
        stop("CPP_gdxWriteSuper:gdxAddAlias GDX error (gdxAddAlias)");
        continue;
      }
      else {
        Environment alias_with_env = symname["aliasWith"];
        std::string alias_with = alias_with_env["name"];

        if (!gdxAddAlias(PGX, mysym.c_str(), alias_with.c_str()))
        stop("CPP_gdxWriteSuper:gdxAddAlias GDX error (gdxAddAlias)");
        continue;
      }

    }
    Dim = symname["dimension"];
    StringVector names(Dim);
    df = symname["records"];

    domain = symname["domain"];
    List domainstr;
    if (Dim != 0) {
      domainstr = symname["domainNames"];
      std::string blah = domainstr[0];
    }
    std::string expltxt = symname["description"];
    if (!gdxDataWriteStrStart(PGX, mysym.c_str(), 
    expltxt.c_str(), Dim, varType, varSubType))
    stop("CPP_gdxWriteSuper:gdxDataWriteStrStart GDX error (gdxDataWriteStrStart)");

    for (int D=0; D < Dim; D++) {
      strcpy(domains_ptr[D], domainstr[D]);
    }

    std::string domaintype = symname["domainType"];
    if (domaintype == "regular") {
      rc = gdxSymbolSetDomain(PGX, (const char **)domains_ptr);
      if (!rc) {
        gdxGetLastError(PGX); // clears last error
        rc = gdxSymbolSetDomainX(PGX, sym_nr, (const char **)domains_ptr);
        if (!rc) {
          gdxErrorStr(PGX, gdxGetLastError(PGX), Msg);
          stop("CPP_gdxWriteSuper:gdxSymbolSetDomain GDX error: %s",Msg);
        }
      }
    }
    else if (domaintype == "relaxed") {
      rc = gdxSymbolSetDomainX(PGX, sym_nr, (const char **)domains_ptr);
      if (!rc) {
        gdxErrorStr(PGX, gdxGetLastError(PGX), Msg);
        stop("CPP_gdxWriteSuper:gdxSymbolSetDomainX GDX error: %s",Msg);
      }
    }
    int nrows = df.nrows();
    int ncols = df.size();

    for (int i =0; i < nrows; i++) {
      for (int j=0; j < ncols; j++) {
        if (j < Dim) {
          colString = df[j];
          names[j] = colString[i];
        }
        else {
          if (varType != GMS_DT_SET){
            colDouble = df[j];
            values.push_back(colDouble[i]);
          }
          else {
            colElemText = df[j];
            values.push_back(0);
            elemText = colElemText[i];
          }
        }
      }


      if (varType != GMS_DT_SET){
        WriteData(PGX, names, values, varType, Dim, elemText, mysym);
      }
      else {
        WriteData(PGX, names, values, varType, Dim, elemText, mysym);
      }
      elemText.clear();
      values.clear();
    }

    if (!gdxDataWriteDone(PGX)) stop("CPP_gdxWriteSuper:gdxDataWriteDone GDX error (gdxDataWriteDone)");

  // get the error count
   if (gdxDataErrorCount(PGX)) {
      gdxErrorStr(PGX, gdxGetLastError(PGX), Msg);
      stop("CPP_gdxWriteSuper:gdxError GDX error for %s: %s", mysym, Msg);
   }
  }

  gdxAutoConvert(PGX, 0);
  if (gdxClose(PGX)) stop("CPP_gdxWriteSuper:gdxClose GDX error (gdxClose)");
  return;
}
