
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
std::string error_message = "Something went wrong. Please "
"contact support@gams.com with a minimal reproducible example.";

void WriteData(gdxHandle_t PGX, StringVector s,
std::vector<double> V, int VarType, int Dim,
std::string elemText) {

  gdxStrIndexPtrs_t Indx;
  gdxStrIndex_t Indx_labels;
  gdxValues_t       Values;
	int rc;
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
      if (VarType == GMS_DT_SET) {
        gdxAddSetText(PGX, elemText.c_str(), &txtnr);
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
  if (!rc) stop("error calling gdxDataWriteStr");
  return;
}



// [[Rcpp::export]]
List CPP_getSpecialValues(CharacterVector gdxName, CharacterVector sysDir) {
gdxHandle_t PGX = NULL;
char Msg[GMS_SSSIZE];
gdxSVals_t sVals;
int rc;
std::string mysysDir = Rcpp::as<std::string>(sysDir);
rc = gdxCreateD(&PGX, mysysDir.c_str(), Msg, sizeof(Msg));
if (!rc) stop("Error creating GDX object: %s", Msg);

gdxGetSpecialValues (PGX, sVals);
List L = List::create(
_["NA"] = sVals[GMS_SVIDX_NA],
_["EPS"] = sVals[GMS_SVIDX_EPS],
_["UNDEF"] = sVals[GMS_SVIDX_UNDEF],
_["POSINF"] = sVals[GMS_SVIDX_PINF],
_["NEGINF"]= sVals[GMS_SVIDX_MINF]
);
return L;
}

// [[Rcpp::export]]
void CPP_gdxWriteSuper(List data, CharacterVector sysDir, 
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
  if (!rc) stop("Error creating GDX object: %s", Msg);

  gdxSVals_t sVals;
  gdxGetSpecialValues(PGX, sVals);

  sVals[GMS_SVIDX_NA] = NA_REAL;
  sVals[GMS_SVIDX_EPS] = -0.0;
  sVals[GMS_SVIDX_UNDEF] = R_NaN;
  sVals[GMS_SVIDX_PINF] = R_PosInf;
  sVals[GMS_SVIDX_MINF] = R_NegInf;

  rc = gdxSetSpecialValues(PGX, sVals);

	gdxGetDLLVersion(PGX, Msg);

	/* Write demand data */
  if (!compress) {
    rc = gdxOpenWrite(PGX, myFileName.c_str(), "GAMS Transfer", &ErrNr);
    if (!rc) stop("Error opening the file %s with error code %i", myFileName, ErrNr);
  }
  else {
    rc = gdxOpenWriteEx(PGX, myFileName.c_str(), "GAMS Transfer", 1, &ErrNr);
    if (!rc) stop("Error opening the file %s with error code %i", myFileName, ErrNr);
  }

  // register UELs
  int UELno;
  if (is_uel_priority) {
    rc = gdxUELRegisterStrStart(PGX);
    if (!rc) stop("Error in gdxUELRegisterStrStart.");
    for (int i = 0; i < uel_priority.length(); i++) {
      myUEL = uel_priority(i);
      rc = gdxUELRegisterStr(PGX, myUEL.c_str(), &UELno);
      if (!rc) stop("Error registering UEL: %s", myUEL);
    }
    gdxUELRegisterDone(PGX);
  }

  DataFrame df;
  List domain;
  int Dim;
  std::vector<double> values;
  std::string elemText;
  StringVector colString, colElemText;
  NumericVector colDouble;

  for (int d=0; d < data.length(); d++){
    Environment symname = data[d];
    std::string mysym = symname["name"];
    varType = symname[".gams_type"];
    varSubType = symname[".gams_subtype"];

    if (varType == GMS_DT_ALIAS) {
      Environment alias_with_env = symname["aliasWith"];
      std::string alias_with = alias_with_env["name"];
      if (!gdxAddAlias(PGX, mysym.c_str(), alias_with.c_str()))
      stop("Error in gdxAddAlias: cannot add alias");
      continue;
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
    stop("Error in gdxDataWriteStrStart");

    for (int D=0; D < Dim; D++) {
      strcpy(domains_ptr[D], domainstr[D]);
    }

    std::string domaintype = symname["domainType"];
    if (domaintype == "regular") {
      rc = gdxSymbolSetDomain(PGX, (const char **)domains_ptr);
      if (!rc) stop("gdxSymbolSetDomain failed");
    }
    else if (domaintype == "relaxed") {
      rc = gdxSymbolSetDomainX(PGX, d + 1, (const char **)domains_ptr);
      if (!rc) stop("gdxSymbolSetDomainX failed");
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
        WriteData(PGX, names, values, varType, Dim, elemText);
      }
      else {
        WriteData(PGX, names, values, varType, Dim, elemText);
      }
      elemText.clear();
      values.clear();
    }

    if (!gdxDataWriteDone(PGX)) stop("Error finishing the write operation with gdxDataWriteDone");
  }

  gdxAutoConvert(PGX, 0);
  if (gdxClose(PGX)) stop("Error in closing GDX file");
  return;
}

void readInternal(gdxHandle_t PGX, int varNr, bool records, 
  List templistAlias,List templist,List &L1, int l1count,
  gdxStrIndexPtrs_t Indx, gdxValues_t Values, gdxStrIndexPtrs_t domains_ptr) {
    std::vector<double> levels, marginals, lower, upper, scale;
    int NrRecs, N, Dim, rc, iDummy, sym_type, nrecs, dummy, subtype,
    domain_type;
    char symbolID[GMS_SSSIZE], Msg[GMS_SSSIZE];

    std::vector<std::string> domain, index, elemText;
    std::vector<std::vector<std::string>> indices;
    char aliasForID[GMS_SSSIZE], explText[GMS_SSSIZE];

    // loop over symbols to get metadata
		gdxSymbolInfo(PGX, varNr, symbolID, &Dim, &sym_type);

    gdxSymbolInfoX(PGX, varNr, &nrecs, &subtype, explText);

    domain_type = gdxSymbolGetDomainX(PGX, varNr, domains_ptr);
    if (!domain_type) stop("Error calling gdxSymbolGetDomainX");
    for (int j=0; j < Dim; j++) {
    domain.push_back(domains_ptr[j]);
    }
    if (sym_type == GMS_DT_ALIAS) {
      int parent_set_subtype;
      gdxSymbolInfo(PGX, subtype, aliasForID, &Dim, &dummy);
      gdxSymbolInfoX(PGX, subtype, &nrecs, &parent_set_subtype, explText);
      domain_type = gdxSymbolGetDomainX(PGX, subtype, domains_ptr);
      gdxDataReadStrStart(PGX, subtype, &NrRecs);

      templistAlias["name"] = symbolID;
      templistAlias["type"] = sym_type;
      templistAlias["aliasfor"] = aliasForID;
      templistAlias["domain"] = domain;
      templistAlias["subtype"] = parent_set_subtype;
      templistAlias["expltext"] = explText;
      templistAlias["domaintype"] = domain_type;
      templistAlias["numRecs"] = NrRecs;

      L1[l1count] = clone(templistAlias);
    }
    else {
      templist["name"] = symbolID;
      templist["type"] = sym_type;
      templist["dimensions"] = Dim;
      templist["domain"] = domain;
      templist["subtype"] = subtype;
      templist["expltext"] = explText;
      templist["domaintype"] = domain_type;
      templist["numRecs"] = nrecs;

      L1[l1count] = clone(templist);
    }
    domain.clear();

  // if we just need metadata, stop here and return
  if (!records) {
    return;
  }

  // if we also want records
  if (sym_type == GMS_DT_ALIAS) return;

  if (!gdxDataReadStrStart(PGX, varNr, &NrRecs)) {
    stop("Error in gdxDataReadStrStart");
  }

  if (NrRecs == 0) {
    templist["records"]=R_NilValue;
    L1[l1count] = clone(templist);
  }
  else {
    while (gdxDataReadStr(PGX, Indx, Values, &N)) {
      if (sym_type == GMS_DT_SET || sym_type == GMS_DT_PAR) {
        if (sym_type == GMS_DT_SET){
          rc = gdxGetElemText(PGX, Values[GMS_VAL_LEVEL], Msg, &iDummy);
          if (rc != 0) {
            elemText.push_back(Msg);
          }
          else {
            elemText.push_back("");
          }
        } else {
          levels.push_back(Values[GMS_VAL_LEVEL]);
        }
        for (int D = 0; D < Dim; D++) {
          index.push_back(Indx[D]);
        }
      }
      else if (sym_type == GMS_DT_VAR || sym_type == GMS_DT_EQU) {
        levels.push_back(Values[GMS_VAL_LEVEL]);
        marginals.push_back(Values[GMS_VAL_MARGINAL]);
        upper.push_back(Values[GMS_VAL_UPPER]);
        lower.push_back(Values[GMS_VAL_LOWER]);
        scale.push_back(Values[GMS_VAL_SCALE]);
        for (int D = 0; D < Dim; D++) {
          index.push_back(Indx[D]);
        }
      }
      indices.push_back(index);
      index.clear();
    }
    DataFrame df;
    std::vector<std::string> index_columns;
    for (int D = 0; D < Dim; D++) {
      for (int i=0; i < NrRecs; i++){
        index_columns.push_back(indices[i][D]);
      }
      if (strcmp(domain[D].c_str(), "*")== 0) {
        df["uni_"+std::to_string(D)] = index_columns;
      }
      else {
        df[domain[D] + "_" + std::to_string(D)] = index_columns;
      }
      index_columns.clear();
    }
    indices.clear();
    if (sym_type == GMS_DT_VAR || sym_type == GMS_DT_EQU) {
      df["level"] = levels;
      df["marginal"] = marginals;
      df["lower"] = lower;
      df["upper"] = upper;
      df["scale"] = scale;
    }
    else {
      if (sym_type == GMS_DT_PAR) {
        df["values"] = levels;
      } else {
        df["element_text"] = elemText;
      }
    }
    elemText.clear();
    levels.clear();
    marginals.clear();
    upper.clear();
    lower.clear();
    scale.clear();
    domain.clear();
    // copy end
    templist["records"]=df;
    L1[l1count] = clone(templist);
  }

  gdxDataReadDone(PGX);
  return;

}

// [[Rcpp::export]]
List CPP_readSuper(CharacterVector symNames, CharacterVector gdxName,
                CharacterVector sysDir, LogicalVector records, bool symisnull) {
  gdxHandle_t PGX = NULL;
	char        Msg[GMS_SSSIZE], Producer[GMS_SSSIZE], acrName[GMS_SSSIZE];
	int         ErrNr, VarNr, rc;

  std::string mysymName;
  std::string mygdxName = Rcpp::as<std::string>(gdxName);
  std::string mysysDir = Rcpp::as<std::string>(sysDir);

  gdxStrIndexPtrs_t Indx;

  gdxStrIndex_t Indx_labels;
  gdxValues_t       Values;
  GDXSTRINDEXPTRS_INIT(Indx_labels, Indx);

  gdxStrIndexPtrs_t domains_ptr;
	gdxStrIndex_t domains;
	GDXSTRINDEXPTRS_INIT(domains, domains_ptr);

  int symCount, UelCount;
// open GDX file and start reading
  rc = gdxCreateD(&PGX, mysysDir.c_str(), Msg, sizeof(Msg));
  if (!rc) stop("Error creating GDX object: %s", Msg);

  gdxOpenRead(PGX, mygdxName.c_str(), &ErrNr);
  if (ErrNr) stop("Error in gdxOpenRead with error code %i", ErrNr);

  gdxFileVersion(PGX, Msg, Producer);
  // check for acronyms
  int nAcronym;
  List acronymList;
  nAcronym = gdxAcronymCount(PGX);
  if (nAcronym > 0) {
    warning("GDX file contains acronyms. "
          "Acronyms are not supported and are set to GAMS NA.\n");
    int acrID;
    std::vector<int> acronyms;
    for (int i=0; i < nAcronym; i++){
      gdxAcronymGetInfo(PGX, i+1, acrName, Msg, &acrID);
      acronyms.push_back(acrID);
    }
    acronymList = List::create(_["nAcronyms"]=nAcronym, _["acronyms"]=acronyms);
  }
  else {
    acronymList = List::create(_["nAcronyms"]=nAcronym);
  }

  // get symbol count
  gdxSystemInfo(PGX, &symCount, &UelCount);
  // initialize empty lists for metadata
  List templist, templistAlias;
  templistAlias =List::create(_["name"] = "", _["type"] = -1,
         _["aliasfor"] = "", _["domain"]="", _["subtype"] = 
         -1, _["expltext"]="", _["domaintype"]=-1,
         _["numRecs"] = -1);
  templist = List::create(_["name"] = "", _["type"] = -1, 
  _["dimensions"]=-1, _["domain"]="", _["subtype"] = -1,
  _["expltext"]="", _["domaintype"]=-1, _["numRecs"] = -1,
  _["records"]=-1);

  int l1_preallocate_size;
  if (symisnull == 1) {
    l1_preallocate_size = symCount + 1;
  }
  else {
    l1_preallocate_size = symNames.size() + 1;
  }
  List L1(l1_preallocate_size);
  L1[0] = acronymList;

  // if user wants to read all symbols, iterate over gdx symbols
  // otherwise iterate over symbols provided by user

  if (records) {
    //set special values
    gdxSVals_t sVals;
    gdxGetSpecialValues(PGX, sVals);

    sVals[GMS_SVIDX_NA] = NA_REAL;
    sVals[GMS_SVIDX_EPS] = -0.0;
    sVals[GMS_SVIDX_UNDEF] = R_NaN;
    sVals[GMS_SVIDX_PINF] = R_PosInf;
    sVals[GMS_SVIDX_MINF] = R_NegInf;

    rc = gdxSetSpecialValues(PGX, sVals);
  }
  int l1count;
  l1count = 1;
  if (symisnull == 1) {
    for (int i=1; i <= symCount; i++) {
      // do something
      readInternal(PGX, i, records, templistAlias, 
      templist, L1, l1count, Indx, Values, domains_ptr);
      l1count += 1;
    }
  }
  else {
    for(int symcount=0; symcount < symNames.size(); symcount++) {
      mysymName = symNames(symcount);
      if (!gdxFindSymbol(PGX, mysymName.c_str(), &VarNr)) {
        stop("User specified to read symbol %s, but it does not "
        "exist in the source file", mysymName);
      }


      // do the same something
      readInternal(PGX, VarNr, records, templistAlias, 
      templist, L1, l1count, Indx,Values, domains_ptr);
      l1count += 1;
    }
  }
  // close the file and return
  if (gdxClose(PGX)) stop("Error in closing GDX file");
  return L1;

}
