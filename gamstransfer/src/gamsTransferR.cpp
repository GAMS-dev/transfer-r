
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

#define GET_DOM_MAP(dim,idx) ((dom_symid[dim] <= 0) ? idx-1 : sym_uel_map[dom_symid[dim]][idx])

// [[Rcpp::export]]
List CPP_getGDXSymbolTypes() {
  List v = List::create(
    _["GMS_DT_SET"] = GMS_DT_SET,
    _["GMS_DT_PAR"] = GMS_DT_PAR,
    _["GMS_DT_VAR"] = GMS_DT_VAR,
    _["GMS_DT_EQU"] = GMS_DT_EQU,
    _["GMS_DT_ALIAS"] = GMS_DT_ALIAS,
    _["GMS_DT_MAX"] = GMS_DT_MAX,
    _["GMS_MAX_INDEX_DIM"] = GMS_MAX_INDEX_DIM
  );
  return(v);
}

// [[Rcpp::export]]
List CPP_getGDXVarTypes() {
  List v = List::create(
    _["GMS_VARTYPE_BINARY"] = GMS_VARTYPE_BINARY,
    _["GMS_VARTYPE_INTEGER"] = GMS_VARTYPE_INTEGER,
    _["GMS_VARTYPE_POSITIVE"] = GMS_VARTYPE_POSITIVE,
    _["GMS_VARTYPE_NEGATIVE"] = GMS_VARTYPE_NEGATIVE,
    _["GMS_VARTYPE_FREE"] = GMS_VARTYPE_FREE,
    _["GMS_VARTYPE_SOS1"] = GMS_VARTYPE_SOS1,
    _["GMS_VARTYPE_SOS2"] = GMS_VARTYPE_SOS2,
    _["GMS_VARTYPE_SEMICONT"] = GMS_VARTYPE_SEMICONT,
    _["GMS_VARTYPE_SEMIINT"] = GMS_VARTYPE_SEMIINT
  );
  return(v);
}

// [[Rcpp::export]]
List CPP_getGDXEqTypes() {
  List v = List::create(
    _["GMS_EQUTYPE_E"] = GMS_EQUTYPE_E,
    _["GMS_EQUTYPE_G"] = GMS_EQUTYPE_G,
    _["GMS_EQUTYPE_L"] = GMS_EQUTYPE_L,
    _["GMS_EQUTYPE_N"] = GMS_EQUTYPE_N,
    _["GMS_EQUTYPE_X"] = GMS_EQUTYPE_X,
    _["GMS_EQUTYPE_C"] = GMS_EQUTYPE_C,
    _["GMS_EQUTYPE_B"] = GMS_EQUTYPE_B,


    _["GMS_EQU_USERINFO_BASE"] = GMS_EQU_USERINFO_BASE,
    _["GMS_EQUTYPE_B"] = GMS_EQUTYPE_B
  );
  return(v);
}

// [[Rcpp::export]]
List CPP_getGDXVarTypeSubtype() {
  List v = List::create(
    _["binary"] = GMS_VARTYPE_BINARY,
    _["integer"] = GMS_VARTYPE_INTEGER,
    _["positive"] = GMS_VARTYPE_POSITIVE,
    _["negative"] = GMS_VARTYPE_NEGATIVE,
    _["free"] = GMS_VARTYPE_FREE,
    _["sos1"] = GMS_VARTYPE_SOS1,
    _["sos2"] = GMS_VARTYPE_SOS2,
    _["semicont"] = GMS_VARTYPE_SEMICONT,
    _["semiint"] = GMS_VARTYPE_SEMIINT
  );
  return(v);
}

// [[Rcpp::export]]
List CPP_getGDXEqTypeSubtype() {
  List v = List::create(
  _["eq"] = GMS_EQUTYPE_E + GMS_EQU_USERINFO_BASE,
  _["geq"] = GMS_EQUTYPE_G + GMS_EQU_USERINFO_BASE,
  _["leq"] = GMS_EQUTYPE_L + GMS_EQU_USERINFO_BASE,
  _["nonbinding"] = GMS_EQUTYPE_N + GMS_EQU_USERINFO_BASE,
  _["external"] = GMS_EQUTYPE_X + GMS_EQU_USERINFO_BASE,
  _["cone"] = GMS_EQUTYPE_C + GMS_EQU_USERINFO_BASE,
  _["boolean"] = GMS_EQUTYPE_B + GMS_EQU_USERINFO_BASE
  );
  return(v);
}

// [[Rcpp::export]]
List CPP_getGDXSetTypeSubtype() {
  List v = List::create(
  _["set"] = GMS_SETTYPE_DEFAULT,
  _["singleton_set"] = GMS_SETTYPE_SINGLETON
  );
  return(v);
}

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
List CPP_getSpecialValues(CharacterVector gdxName, CharacterVector sysDir) {
gdxHandle_t PGX = NULL;
char Msg[GMS_SSSIZE];
gdxSVals_t sVals;
int rc;
std::string mysysDir = Rcpp::as<std::string>(sysDir);
rc = gdxCreateD(&PGX, mysysDir.c_str(), Msg, sizeof(Msg));
if (!rc) stop("CPP_getSpecialValues:gdxCreateD GDX init failed: %s", Msg);

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

void readInternal(gdxHandle_t PGX, int varNr, bool records, 
  List templistAlias,List templist,List &L1, int l1count,
  gdxUelIndex_t gdx_uel_index, gdxValues_t gdx_values,
  gdxStrIndexPtrs_t domains_ptr, 
  std::map<int, std::map<int, int>> &sym_uel_map, int uel_count) {
    std::vector<double> levels, marginals, lower, upper, scale;
    int NrRecs, N, Dim, rc, iDummy, sym_type, nrecs, dummy, subtype,
    domain_type, idx;
    gdxUelIndex_t dom_symid;
    char symbolID[GMS_SSSIZE], Msg[GMS_SSSIZE], buf[GMS_SSSIZE];

    std::vector<std::string> domain, index, elemText;
    std::vector<std::vector<std::string>> indices;
    char aliasForID[GMS_SSSIZE], explText[GMS_SSSIZE];

    std::map<int, int> uel_map; //for each symbol
    // loop over symbols to get metadata
		if (!gdxSymbolInfo(PGX, varNr, symbolID, &Dim, &sym_type))
      stop("readInternal:gdxSymbolInfo GDX error (gdxSymbolInfo)");


    int** dom_uel_used =new int*[Dim];

    if (!gdxSymbolInfoX(PGX, varNr, &nrecs, &subtype, explText))
      stop("readInternal:gdxSymbolInfoX GDX error (gdxSymbolInfoX)");

    domain_type = gdxSymbolGetDomainX(PGX, varNr, domains_ptr);
    if (!domain_type) stop("readInternal:gdxSymbolGetDomainX GDX error (gdxSymbolGetDomainX)");

    for (int j=0; j < Dim; j++) {
      domain.push_back(domains_ptr[j]);
    }
    if (sym_type == GMS_DT_ALIAS) {
      int parent_set_subtype;
      if (subtype == 0) {
        // alias to the Universe
        strcpy(aliasForID, "*");

        if (!gdxDataReadRawStart(PGX, varNr, &NrRecs))
        stop("readInternal:gdxDataReadRawStart GDX error (gdxDataReadStrStart)");
      }
      else {
        // normal Alias
        if (!gdxSymbolInfo(PGX, subtype, aliasForID, &Dim, &dummy))
          stop("readInternal:gdxSymbolInfo GDX error (gdxSymbolInfo)");

        if (!gdxSymbolInfoX(PGX, subtype, &nrecs, &parent_set_subtype, explText))
          stop("readInternal:gdxSymbolInfoX GDX error (gdxSymbolInfoX)");

        domain_type = gdxSymbolGetDomainX(PGX, subtype, domains_ptr);
        if (!domain_type) stop("readInternal:gdxSymbolGetDomainX GDX error (gdxSymbolGetDomainX)");

        if (!gdxDataReadRawStart(PGX, subtype, &NrRecs))
        stop("readInternal:gdxDataReadRawStart GDX error (gdxDataReadStrStart)");
      }
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

  // if we just need metadata, stop here and return
  if (!records) {
    return;
  }
  int dom_dim, dom_type, dom_nrecs;
  // get domain symbol number and read if records is true
  if (!gdxSymbolGetDomain(PGX, varNr, dom_symid))
  stop("readInternal:gdxSymbolGetDomain GDX error (gdxSymbolGetDomain)");

  int all_dom_nrecs[Dim];
  for (int D = 0; D < Dim; D++) {
		// get sym info for domain D
    if (!gdxSymbolInfo(PGX, dom_symid[D], buf, &dom_dim, &dom_type))
      stop("readInternal:gdxSymbolInfo GDX error (gdxSymbolInfo)");
    if (!(dom_type == GMS_DT_SET || dom_type == GMS_DT_ALIAS)) {
      stop("Invalid domain data type.");
    }
    if (dom_dim != 1) stop("Invalid domain dimension.");

    // read raw start for domain D
    if (!gdxDataReadRawStart(PGX, dom_symid[D], &dom_nrecs))
    stop("readInternal:gdxDataReadRawStart GDX error (gdxDataReadStrStart)");
    if (dom_nrecs < 0) stop("Invalid number of symbol records.");
    all_dom_nrecs[D] = dom_nrecs;

    dom_uel_used[D] = new int[dom_nrecs];
    std::fill_n(dom_uel_used[D], dom_nrecs, 0); // initialize all to false

    // if domain is not read before store a sym uel map
    if (dom_symid[D] > 0 || sym_uel_map.count(dom_symid[D]) == 0) {
      for (int k=0; k < dom_nrecs; k++) {
        if (!gdxDataReadRaw(PGX, gdx_uel_index, gdx_values, &N))
        stop("readInternal:gdxDataReadRaw GDX error (gdxDataReadRaw)");

        uel_map.insert(std::make_pair(gdx_uel_index[0], k));
      }
      sym_uel_map.insert(std::make_pair(dom_symid[D], uel_map));
      uel_map.clear();
    }

    // read done for domain
    if (!gdxDataReadDone(PGX))
      stop("readInternal:gdxDataReadDone GDX error (gdxDataReadDone)");
  }

  // start reading the symbol
  if (!gdxDataReadRawStart(PGX, varNr, &NrRecs)) {
    stop("readInternal:gdxDataReadRawStart GDX error (gdxDataReadStrStart)");
  }

  if (NrRecs == 0) {
    if (sym_type == GMS_DT_ALIAS) {
      templistAlias["records"] = R_NilValue;
      L1[l1count] = clone(templistAlias);
    }
    else {
      templist["records"] = R_NilValue;
      L1[l1count] = clone(templist);
    }
  }
  else {

    NumericMatrix indx_matrix(NrRecs, Dim);
    int n_attr;
    if (sym_type == GMS_DT_VAR || sym_type == GMS_DT_EQU) {
      n_attr = 5;
    }
    else {
      n_attr = 1;
    }
    NumericMatrix record_values(NrRecs, n_attr);
    CharacterVector elem_text(NrRecs); // for elem_text
    int rec_nr = -1;
    while (gdxDataReadRaw(PGX, gdx_uel_index, gdx_values, &N)) {

      rec_nr++;
      if (sym_type == GMS_DT_SET || sym_type == GMS_DT_PAR || sym_type == GMS_DT_ALIAS) {
        if (sym_type == GMS_DT_SET || sym_type == GMS_DT_ALIAS) {
          rc = gdxGetElemText(PGX, gdx_values[GMS_VAL_LEVEL], Msg, &iDummy);
          if (rc != 0) {
            elem_text(rec_nr) = Msg;
          }
          else {
            elem_text(rec_nr) = "";
          }
        } else {
          record_values(rec_nr, 0) = gdx_values[GMS_VAL_LEVEL];
        }
        for (int D = 0; D < Dim; D++) {
          indx_matrix(rec_nr, D) = GET_DOM_MAP(D, gdx_uel_index[D]) + 1;
        }
      }
      else if (sym_type == GMS_DT_VAR || sym_type == GMS_DT_EQU) {
        record_values(rec_nr, 0) = gdx_values[GMS_VAL_LEVEL];
        record_values(rec_nr, 1) = gdx_values[GMS_VAL_MARGINAL];
        record_values(rec_nr, 2) = gdx_values[GMS_VAL_UPPER];
        record_values(rec_nr, 3) = gdx_values[GMS_VAL_LOWER];
        record_values(rec_nr, 4) = gdx_values[GMS_VAL_SCALE];
        for (int D = 0; D < Dim; D++) {
          indx_matrix(rec_nr, D) = GET_DOM_MAP(D, gdx_uel_index[D]) + 1;
        }
      }
        //store domain labels
        for (int D = 0; D < Dim; D++) {
          idx = GET_DOM_MAP(D, gdx_uel_index[D]);
          dom_uel_used[D][idx] = true; // set used to true
        }
    }

    // now map the numerical indx_matrix to string using from_codes
    std::vector<std::string> used_uels;

    DataFrame df;

    for (int D = 0; D < Dim; D++) {
      if (dom_symid[D] < 0) continue;

      // get number of uels used
      int num_used = 0;
      for (int k = 0; k < all_dom_nrecs[D]; k++) {
        if (dom_uel_used[D][k] > 0) {
          dom_uel_used[D][k] = num_used++;
        }
        else {
          dom_uel_used[D][k] = -1;
        }
      }

      for (int k = 1; k <= uel_count; k++) {
        if (dom_symid[D] == 0 || sym_uel_map.at((int) dom_symid[D]).count(k) != 0) {
          idx = GET_DOM_MAP(D, k);
          if (dom_uel_used[D][idx] < 0) continue; // if not used, continue
          if (!gdxUMUelGet(PGX, k, Msg, &iDummy)) {
            stop("readInternal:gdxUMUelGet GDX error(gdxUMUelGet)");
          }
          used_uels.push_back(Msg);
        }
      }

      // shift domain indices
      for (int k = 0; k < nrecs; k++) {
        indx_matrix(k, D) = dom_uel_used[D][(int) indx_matrix(k, D) - 1] + 1;
      }

      // create a factor v
      IntegerVector v = wrap(indx_matrix(_, D));

      CharacterVector ch = wrap(used_uels);
      v.attr("class") = "factor";
      v.attr("levels") = ch;
      df[std::to_string(D)] = v;
      used_uels.clear();
    }

    if (sym_type == GMS_DT_VAR || sym_type == GMS_DT_EQU) {
      df["level"] = record_values(_, 0);
      df["marginal"] = record_values(_, 1);
      df["lower"] = record_values(_, 2);
      df["upper"] = record_values(_, 3);
      df["scale"] = record_values(_, 4);
    }
    else {
      if (sym_type == GMS_DT_PAR) {
        df["values"] = record_values(_, 0);
      } else {
        df["element_text"] = elem_text;
      }
    }

    if (sym_type == GMS_DT_ALIAS) {
      templistAlias["records"]= df;
      L1[l1count] = clone(templistAlias);
    }
    else {
      templist["records"]=df;
      L1[l1count] = clone(templist);
    }
  }

  // delete used uel array
  for (int D = 0; D < Dim; D++) {
    delete[] dom_uel_used[D];
  }
  delete[] dom_uel_used;

  if (!gdxDataReadDone(PGX))
    stop("readInternal:gdxDataReadDone GDX error (gdxDataReadDone)");
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

  gdxUelIndex_t gdx_uel_index;
  gdxValues_t gdx_values;

  gdxStrIndexPtrs_t domains_ptr;
	gdxStrIndex_t domains;
	GDXSTRINDEXPTRS_INIT(domains, domains_ptr);

  int symCount, uel_count;
// open GDX file and start reading
  rc = gdxCreateD(&PGX, mysysDir.c_str(), Msg, sizeof(Msg));
  if (!rc) stop("CPP_readSuper:gdxCreateD GDX init failed: %s", Msg);

  gdxOpenRead(PGX, mygdxName.c_str(), &ErrNr);
  if (ErrNr) stop("CPP_readSuper:gdxOpenRead GDX error with error code %i", ErrNr);

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
  if (!gdxSystemInfo(PGX, &symCount, &uel_count))
    stop("CPP_readSuper:gdxSystemInfo GDX error (gdxSystemInfo)");

std::map<int, std::map<int, int>> sym_uel_map;

  // initialize empty lists for metadata
  List templist, templistAlias;
  templistAlias =List::create(_["name"] = "", _["type"] = -1,
         _["aliasfor"] = "", _["domain"]="", _["subtype"] = 
         -1, _["expltext"]="", _["domaintype"]=-1,
         _["numRecs"] = -1, _["records"]=-1);
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

  int sym_nrs[l1_preallocate_size - 1];
  if (!symisnull) {
    for(int symcount=0; symcount < symNames.size(); symcount++) {
      mysymName = symNames(symcount);
      if (!gdxFindSymbol(PGX, mysymName.c_str(), &VarNr)) {
        stop("User specified to read symbol %s, but it does not "
        "exist in the source file", mysymName);
      }
      sym_nrs[symcount] = VarNr;
    }
  }
  else {
    for (int i=0; i < (l1_preallocate_size - 1); i++) {
      sym_nrs[i] = i + 1;
    }
  }

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
    if (!rc) stop("CPP_readSuper:gdxSetSpecialValues GDX error (gdxSetSpecialValues)");

  }
  int l1count;
  l1count = 1;

  for (int i : sym_nrs) {
    readInternal(PGX, i, records, templistAlias, 
    templist, L1, l1count, gdx_uel_index, gdx_values, domains_ptr,
    sym_uel_map, uel_count);
    l1count ++;
  }

  // close the file and return
  if (gdxClose(PGX)) stop("CPP_readSuper:gdxClose GDX error (gdxClose)");
  return L1;
}
