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

double gt_map_acronyms(std::vector<int> acronyms, double value) {
for (auto &a:acronyms) {
    if (value == a * GMS_SV_ACR) {
      return NA_REAL;
    }
}
return value;
}

void gt_read_symbol(gdxHandle_t PGX, int sym_Nr, bool read_records, 
    List &read_list, int read_list_size,
    std::map<int, std::map<int, int>> &sym_uel_map, int uel_count,
    int n_acronyms, std::vector<int> acronyms) {

    gdxUelIndex_t gdx_uel_index;
    gdxValues_t gdx_values;

    gdxStrIndexPtrs_t domains_ptr;
    gdxStrIndex_t domains;
    GDXSTRINDEXPTRS_INIT(domains, domains_ptr);
    std::vector<double> levels, marginals, lower, upper, scale;
    int nr_recs, dim, rc, iDummy, sym_type, nrecs, dummy, subtype,
    domain_type, idx;
    gdxUelIndex_t dom_symid;
    char sym_id[GMS_SSSIZE], Msg[GMS_SSSIZE], buf[GMS_SSSIZE];

    std::string d_col_name;
    bool is_duplicated;
    std::vector<std::string> domain, index, elemText;
    std::vector<std::vector<std::string>> indices;
    char alias_for_id[GMS_SSSIZE], description[GMS_SSSIZE];

    std::map<int, int> uel_map; //for each symbol
    // loop over symbols to get metadata
    if (!gdxSymbolInfo(PGX, sym_Nr, sym_id, &dim, &sym_type))
      stop("gt_read_symbol:gdxSymbolInfo GDX error (gdxSymbolInfo)");


    // initialize empty lists for data
    List sym_list;
    if (sym_type == GMS_DT_ALIAS) {
      sym_list =List::create(_["type"] = -1, _["name"] = "",
      _["aliasWith"] = "");
    }
    else if (sym_type == GMS_DT_SET) {
      sym_list = List::create( _["type"] = -1, _["name"] = "",
      _["description"]="", _["isSingleton"] = -1, _["domain"]="", _["domainType"]=-1,
      _["dimension"]=-1, _["numberRecords"] = -1, _["records"]=-1);
    }
    else if (sym_type == GMS_DT_PAR) {
      sym_list = List::create( _["type"] = -1, _["name"] = "",
      _["description"]="", _["domain"]="", _["domainType"]=-1,
      _["dimension"]=-1, _["numberRecords"] = -1, _["records"]=-1);
    }
    else { // var of equ
      sym_list = List::create( _["type"] = -1, _["name"] = "",
      _["description"]="", _["subtype"] = -1, _["domain"]="", _["domainType"]=-1,
      _["dimension"]=-1, _["numberRecords"] = -1, _["records"]=-1);
    }

    int** dom_uel_used =new int*[dim];

    if (!gdxSymbolInfoX(PGX, sym_Nr, &nrecs, &subtype, description))
      stop("gt_read_symbol:gdxSymbolInfoX GDX error (gdxSymbolInfoX)");

    domain_type = gdxSymbolGetDomainX(PGX, sym_Nr, domains_ptr);
    if (!domain_type) stop("gt_read_symbol:gdxSymbolGetDomainX GDX error (gdxSymbolGetDomainX)");

    for (int d=0; d < dim; d++) {
      domain.push_back(domains_ptr[d]);
    }
    if (sym_type == GMS_DT_ALIAS) {
      if (subtype == 0) {
        // alias to the Universe
        strcpy(alias_for_id, "*");

        if (!gdxDataReadRawStart(PGX, sym_Nr, &nr_recs))
        stop("gt_read_symbol:gdxDataReadRawStart GDX error (gdxDataReadStrStart)");
      }
      else {
        // normal Alias
        if (!gdxSymbolInfo(PGX, subtype, alias_for_id, &dim, &dummy))
          stop("gt_read_symbol:gdxSymbolInfo GDX error (gdxSymbolInfo)");
      }
      sym_list["name"] = sym_id;
      sym_list["type"] = gmsGdxTypeText[sym_type];
      sym_list["aliasWith"] = alias_for_id;
      read_list[read_list_size] = clone(sym_list);
    }
    else {
      sym_list["name"] = sym_id;
      sym_list["type"] = gmsGdxTypeText[sym_type];
      sym_list["dimension"] = dim;
      sym_list["domain"] = domain;

      if (sym_type == GMS_DT_VAR) {
        sym_list["subtype"] = gmsVarTypeText[subtype];
      }
      if (sym_type == GMS_DT_EQU) {
        sym_list["subtype"] = gmsEquTypeText[subtype - GMS_EQU_USERINFO_BASE];
      }
      else if (sym_type == GMS_DT_SET) {
        sym_list["isSingleton"] = subtype;
      }

      sym_list["description"] = description;
      if (domain_type == 1) {
        sym_list["domainType"] = "none";
      }
      else if (domain_type == 2) {
        sym_list["domainType"] = "relaxed";
      }
      else {
        sym_list["domainType"] = "regular";
      }
      sym_list["numberRecords"] = nrecs;

      read_list[read_list_size] = clone(sym_list);
    }

  // if we just need metadata, stop here and return
  if (!read_records) {
    return;
  }
  int dom_dim, dom_type, dom_nrecs;
  // get domain symbol number and read if records is true
  if (!gdxSymbolGetDomain(PGX, sym_Nr, dom_symid))
  stop("gt_read_symbol:gdxSymbolGetDomain GDX error (gdxSymbolGetDomain)");

  std::vector<int> all_dom_nrecs(dim);
  // int all_dom_nrecs[dim];
  for (int d = 0; d < dim; d++) {
    // get sym info for domain d
    if (!gdxSymbolInfo(PGX, dom_symid[d], buf, &dom_dim, &dom_type))
      stop("gt_read_symbol:gdxSymbolInfo GDX error (gdxSymbolInfo)");
    if (!(dom_type == GMS_DT_SET || dom_type == GMS_DT_ALIAS)) {
      stop("Invalid domain data type.");
    }
    if (dom_dim != 1) stop("Invalid domain dimension.");

    // read raw start for domain d
    if (!gdxDataReadRawStart(PGX, dom_symid[d], &dom_nrecs))
    stop("gt_read_symbol:gdxDataReadRawStart GDX error (gdxDataReadStrStart)");
    if (dom_nrecs < 0) stop("Invalid number of symbol records.");
    all_dom_nrecs.at(d) = dom_nrecs;

    dom_uel_used[d] = new int[dom_nrecs];
    std::fill_n(dom_uel_used[d], dom_nrecs, 0); // initialize all to false

    // if domain is not read before store a sym uel map
    // sym uel map gives the place of a given uel in given domain sym id.
    if (dom_symid[d] > 0 || sym_uel_map.count(dom_symid[d]) == 0) {
      for (int k=0; k < dom_nrecs; k++) {
        if (!gdxDataReadRaw(PGX, gdx_uel_index, gdx_values, &dummy))
        stop("gt_read_symbol:gdxDataReadRaw GDX error (gdxDataReadRaw)");

        uel_map.insert(std::make_pair(gdx_uel_index[0], k));
      }
      sym_uel_map.insert(std::make_pair(dom_symid[d], uel_map));
      uel_map.clear();
    }

    // read done for domain
    if (!gdxDataReadDone(PGX))
      stop("gt_read_symbol:gdxDataReadDone GDX error (gdxDataReadDone)");
  }

  // start reading the symbol
  if (!gdxDataReadRawStart(PGX, sym_Nr, &nr_recs)) {
    stop("gt_read_symbol:gdxDataReadRawStart GDX error (gdxDataReadStrStart)");
  }

  if (nr_recs == 0) {
    if (sym_type != GMS_DT_ALIAS) {
      sym_list["records"] = R_NilValue;
      read_list[read_list_size] = clone(sym_list);
    }
  }
  else {
    // indx_matrix stores positions of UELs in the domain set
    NumericMatrix indx_matrix(nr_recs, dim);
    int n_attr;
    if (sym_type == GMS_DT_VAR || sym_type == GMS_DT_EQU) {
      n_attr = 5;
    }
    else {
      n_attr = 1;
    }
    NumericMatrix record_values(nr_recs, n_attr);
    CharacterVector elem_text(nr_recs); // for elem_text
    int rec_nr = -1;
    while (gdxDataReadRaw(PGX, gdx_uel_index, gdx_values, &dummy)) {
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
          if (n_acronyms > 0) {
            record_values(rec_nr, 0) = gt_map_acronyms(acronyms, gdx_values[GMS_VAL_LEVEL]);
          }
          else {
            record_values(rec_nr, 0) = gdx_values[GMS_VAL_LEVEL];
          }
        }
        for (int d = 0; d < dim; d++) {
          indx_matrix(rec_nr, d) = GET_DOM_MAP(d, gdx_uel_index[d]) + 1;
        }
      }
      else if (sym_type == GMS_DT_VAR || sym_type == GMS_DT_EQU) {
        for (int i = 0; i < 5; i++) {
          if (n_acronyms > 0) {
            record_values(rec_nr, i) = gt_map_acronyms(acronyms, gdx_values[i]);
          }
          else {
            record_values(rec_nr, i) = gdx_values[i];
          }
        }

        for (int d = 0; d < dim; d++) {
          indx_matrix(rec_nr, d) = GET_DOM_MAP(d, gdx_uel_index[d]) + 1;
        }
      }
        //store domain labels
        //dom_uel_used is true if idx positioned UEL for domain d, 
        // is used in the symbol
        for (int d = 0; d < dim; d++) {
          idx = GET_DOM_MAP(d, gdx_uel_index[d]);
          dom_uel_used[d][idx] = true; // set used to true
        }
    }

    // now map the numerical indx_matrix to string using from_codes
    std::vector<std::string> used_uels;

    List records;

    for (int d = 0; d < dim; d++) {
      if (dom_symid[d] < 0) continue;

      // change dom_uel_used from true/false to position in the symbol records
      int num_used = 0;
      for (int k = 0; k < all_dom_nrecs.at(d); k++) {
        if (dom_uel_used[d][k] > 0) {
          dom_uel_used[d][k] = num_used++;
        }
        else {
          dom_uel_used[d][k] = -1;
        }
      }

      for (int k = 1; k <= uel_count; k++) {
        if (dom_symid[d] == 0 || sym_uel_map.at((int) dom_symid[d]).count(k) != 0) {
          idx = GET_DOM_MAP(d, k);
          if (dom_uel_used[d][idx] < 0) continue; // if not used, continue
          if (!gdxUMUelGet(PGX, k, Msg, &iDummy)) {
            stop("gt_read_symbol:gdxUMUelGet GDX error(gdxUMUelGet)");
          }
          used_uels.push_back(Msg);
        }
      }

      // shift domain indices
      for (int k = 0; k < nrecs; k++) {
        indx_matrix(k, d) = dom_uel_used[d][(int) indx_matrix(k, d) - 1] + 1;
      }

      // create a factor v
      IntegerVector v = wrap(indx_matrix(_, d));
      CharacterVector ch = wrap(used_uels);
      v.attr("class") = "factor";
      v.attr("levels") = ch;

      // check if domain is duplicated and set domain labels accordingly
      std::set<std::string> unique_domain(domain.begin(), domain.end());
      is_duplicated = (unique_domain.size() != domain.size());
      if (is_duplicated) {
        if (domain.at(d).compare("*") == 0) {
          d_col_name = "uni_" + std::to_string(d + 1);
        }
        else {
          d_col_name = domain.at(d) + "_" + std::to_string(d + 1);
        }
      }
      else {
        if (domain.at(d).compare("*") == 0) {
          d_col_name = "uni";
        }
        else {
          d_col_name = domain.at(d);
        }
      }
      records[d_col_name] = v;
      used_uels.clear();
    }

    if (sym_type == GMS_DT_VAR || sym_type == GMS_DT_EQU) {
      records["level"] = record_values(_, 0);
      records["marginal"] = record_values(_, 1);
      records["lower"] = record_values(_, 2);
      records["upper"] = record_values(_, 3);
      records["scale"] = record_values(_, 4);
    }
    else {
      if (sym_type == GMS_DT_PAR) {
        records["value"] = record_values(_, 0);
      } else {
        records["element_text"] = elem_text;
      }
    }

    records.attr("class") = "data.frame";
    records.attr("row.names") = Rcpp::seq(1, nr_recs);

    if (sym_type != GMS_DT_ALIAS) {
      sym_list["records"]=records;
      read_list[read_list_size] = clone(sym_list);
    }
  }

  // delete used uel array
  for (int d = 0; d < dim; d++) {
    delete[] dom_uel_used[d];
  }
  delete[] dom_uel_used;

  if (!gdxDataReadDone(PGX))
    stop("gt_read_symbol:gdxDataReadDone GDX error (gdxDataReadDone)");
  return;

}

// [[Rcpp::export]]
List CPP_readSuper(Nullable<CharacterVector> symNames_, CharacterVector gdxName,
                CharacterVector sysDir, LogicalVector read_records) {
  // gdxHandle_t PGX = NULL;
  char        Msg[GMS_SSSIZE], Producer[GMS_SSSIZE], acrName[GMS_SSSIZE];
  int         ErrNr, sym_Nr;

  std::string sym_name;
  std::string gdx_name = Rcpp::as<std::string>(gdxName);
  std::string sys_dir = Rcpp::as<std::string>(sysDir);

  int symCount, uel_count;

  // create gdx object
  gt_gdx gdxobj;
  gdxobj.init_library(sys_dir.c_str());

  gdxOpenRead(gdxobj.gdx, gdx_name.c_str(), &ErrNr);
  if (ErrNr) stop("CPP_readSuper:gdxOpenRead GDX error with error code %i", ErrNr);

  gdxFileVersion(gdxobj.gdx, Msg, Producer);
  // check for acronyms
  int n_acronyms;
  n_acronyms = gdxAcronymCount(gdxobj.gdx);
  std::vector<int> acronyms;

  if (n_acronyms > 0) {
    warning("GDX file contains acronyms. "
          "Acronyms are not supported and are set to GAMS NA.\n");
    int acrID;
    for (int i=0; i < n_acronyms; i++){
      gdxAcronymGetInfo(gdxobj.gdx, i+1, acrName, Msg, &acrID);
      acronyms.push_back(acrID);
    }
  }

  // get symbol count
  if (!gdxSystemInfo(gdxobj.gdx, &symCount, &uel_count))
    stop("CPP_readSuper:gdxSystemInfo GDX error (gdxSystemInfo)");

  std::vector<bool> sym_enabled(symCount + 1, false); // initialize sym_enabled with false

  std::map<int, std::map<int, int>> sym_uel_map;

  int l1_preallocate_size;
  CharacterVector symNames;

  if (symNames_.isNotNull()) {
    symNames = symNames_;
  }

  if (symNames_.isNull()) {
    l1_preallocate_size = symCount;
  }
  else {
    l1_preallocate_size = symNames.size();
  }

  List read_list(l1_preallocate_size);

  if (symNames_.isNotNull()) {
    for(int symcount=0; symcount < symNames.size(); symcount++) {
      sym_name = symNames(symcount);
      if (!gdxFindSymbol(gdxobj.gdx, sym_name.c_str(), &sym_Nr)) {
        stop("User specified to read symbol %s, but it does not "
        "exist in the source file", sym_name);
      }
      sym_enabled.at(sym_Nr) = true;
    }
  }


  // if user wants to read all symbols, iterate over gdx symbols
  // otherwise iterate over symbols provided by user

  if (read_records) {
    gt_set_special_values(gdxobj);
  }
  int read_list_size;
  read_list_size = 0;

  for (int i=1; i < symCount + 1; i++) {
    if (symNames_.isNotNull() && !sym_enabled.at(i)) continue;
    gt_read_symbol(gdxobj.gdx, i, read_records, read_list, read_list_size,
    sym_uel_map, uel_count, n_acronyms, acronyms);

    read_list_size ++;
  }


  return read_list;
}
