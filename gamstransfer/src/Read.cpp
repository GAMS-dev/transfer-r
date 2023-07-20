// GAMS - General Algebraic Modeling System Python API
//
// Copyright (c) 2017-2023 GAMS Software GmbH <support@gams.com>
// Copyright (c) 2017-2023 GAMS Development Corp. <support@gams.com>
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
#include "utilities.hpp"
using namespace Rcpp;

#define GET_DOM_MAP(dim,idx) ((dom_symid[dim] <= 0) ? idx-1 : sym_uel_map[dom_symid[dim]][idx])

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
    // sym uel map gives the place of a given uel in given domain sym id.
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
    // indx_matrix stores positions of UELs in the domain set
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
        record_values(rec_nr, 2) = gdx_values[GMS_VAL_LOWER];
        record_values(rec_nr, 3) = gdx_values[GMS_VAL_UPPER];
        record_values(rec_nr, 4) = gdx_values[GMS_VAL_SCALE];
        for (int D = 0; D < Dim; D++) {
          indx_matrix(rec_nr, D) = GET_DOM_MAP(D, gdx_uel_index[D]) + 1;
        }
      }
        //store domain labels
        //dom_uel_used is true if idx positioned UEL for domain D, 
        // is used in the symbol
        for (int D = 0; D < Dim; D++) {
          idx = GET_DOM_MAP(D, gdx_uel_index[D]);
          dom_uel_used[D][idx] = true; // set used to true
        }
    }

    // now map the numerical indx_matrix to string using from_codes
    std::vector<std::string> used_uels;

    List df;

    for (int D = 0; D < Dim; D++) {
      if (dom_symid[D] < 0) continue;

      // change dom_uel_used from true/false to position in the symbol records
      int num_used = 0;
      for (int k = 0; k < all_dom_nrecs[D]; k++) {
          dom_uel_used[D][k] = num_used++;
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
        df["value"] = record_values(_, 0);
      } else {
        df["element_text"] = elem_text;
      }
    }

    df.attr("class") = "data.frame";
    df.attr("row.names") = Rcpp::seq(1, NrRecs);

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
  // gdxHandle_t PGX = NULL;
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

  // create gdx object
  gt_gdx gdxobj;
  gdxobj.init_library(mysysDir.c_str());

  gdxOpenRead(gdxobj.gdx, mygdxName.c_str(), &ErrNr);
  if (ErrNr) stop("CPP_readSuper:gdxOpenRead GDX error with error code %i", ErrNr);

  gdxFileVersion(gdxobj.gdx, Msg, Producer);
  // check for acronyms
  int nAcronym;
  List acronymList;
  nAcronym = gdxAcronymCount(gdxobj.gdx);
  if (nAcronym > 0) {
    warning("GDX file contains acronyms. "
          "Acronyms are not supported and are set to GAMS NA.\n");
    int acrID;
    std::vector<int> acronyms;
    for (int i=0; i < nAcronym; i++){
      gdxAcronymGetInfo(gdxobj.gdx, i+1, acrName, Msg, &acrID);
      acronyms.push_back(acrID);
    }
    acronymList = List::create(_["nAcronyms"]=nAcronym, _["acronyms"]=acronyms);
  }
  else {
    acronymList = List::create(_["nAcronyms"]=nAcronym);
  }

  // get symbol count
  if (!gdxSystemInfo(gdxobj.gdx, &symCount, &uel_count))
    stop("CPP_readSuper:gdxSystemInfo GDX error (gdxSystemInfo)");

  std::vector<bool> sym_enabled(symCount + 1, false); // initialize sym_enabled with false

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

  if (!symisnull) {
    for(int symcount=0; symcount < symNames.size(); symcount++) {
      mysymName = symNames(symcount);
      if (!gdxFindSymbol(gdxobj.gdx, mysymName.c_str(), &VarNr)) {
        stop("User specified to read symbol %s, but it does not "
        "exist in the source file", mysymName);
      }
      sym_enabled.at(VarNr) = true;
    }
  }


  // if user wants to read all symbols, iterate over gdx symbols
  // otherwise iterate over symbols provided by user

  if (records) {
    //set special values
    gdxSVals_t sVals;
    gdxGetSpecialValues(gdxobj.gdx, sVals);

    sVals[GMS_SVIDX_NA] = NA_REAL;
    sVals[GMS_SVIDX_EPS] = -0.0;
    sVals[GMS_SVIDX_UNDEF] = R_NaN;
    sVals[GMS_SVIDX_PINF] = R_PosInf;
    sVals[GMS_SVIDX_MINF] = R_NegInf;

    rc = gdxSetSpecialValues(gdxobj.gdx, sVals);
    if (!rc) stop("CPP_readSuper:gdxSetSpecialValues GDX error (gdxSetSpecialValues)");

  }
  int l1count;
  l1count = 1;

  for (int i=1; i < symCount + 1; i++) {
    if (!symisnull && !sym_enabled.at(i)) continue;
    readInternal(gdxobj.gdx, i, records, templistAlias, 
    templist, L1, l1count, gdx_uel_index, gdx_values, domains_ptr,
    sym_uel_map, uel_count);
    l1count ++;
  }


  return L1;
}
