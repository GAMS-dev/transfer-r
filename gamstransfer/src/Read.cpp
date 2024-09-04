// GAMS - General Algebraic Modeling System R API
//
// Copyright (c) 2017-2024 GAMS Software GmbH <support@gams.com>
// Copyright (c) 2017-2024 GAMS Development Corp. <support@gams.com>
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
#include <array>
#include "utilities.hpp"
using namespace Rcpp;

using namespace std::literals::string_literals;

#define GET_DOM_MAP(dim,idx) ((dom_symid[dim] <= 0) ? idx-1 : sym_uel_map[dom_symid[dim]][idx])

double gt_map_acronyms(const std::vector<int> & acronyms, double value) {
for (auto &a:acronyms) {
    if (value == a * GMS_SV_ACR)
      return NA_REAL;
}
return value;
}

void gt_read_symbol(gdx::TGXFileObj & PGX, int sym_nr, bool read_records,
    List &read_list, int read_list_size,
    std::vector<std::vector<int>> &sym_uel_map, int uel_count,
    int n_acronyms, const std::vector<int> & acronyms) {

    char sym_id[GMS_SSSIZE];
    char alias_for_id[GMS_SSSIZE], description[GMS_SSSIZE];

    // loop over symbols to get metadata
    int dim, sym_type;
    if (!PGX.gdxSymbolInfo(sym_nr, sym_id, dim, sym_type))
      stop("gt_read_symbol:gdxSymbolInfo GDX error (gdxSymbolInfo). Symbol name = "s + sym_id );


    // initialize empty lists for data
    List sym_list;
    if (sym_type == GMS_DT_ALIAS) {
      sym_list =List::create(_["class"] = -1, _["name"] = "",
      _["aliasWith"] = "");
    }
    else if (sym_type == GMS_DT_SET) {
      sym_list = List::create( _["class"] = -1, _["name"] = "",
      _["description"]="", _["isSingleton"] = -1, _["domain"]="", _["domainType"]=-1,
      _["dimension"]=-1, _["numberRecords"] = -1, _["records"]=R_NilValue);
    }
    else if (sym_type == GMS_DT_PAR) {
      sym_list = List::create( _["class"] = -1, _["name"] = "",
      _["description"]="", _["domain"]="", _["domainType"]=-1,
      _["dimension"]=-1, _["numberRecords"] = -1, _["records"]=R_NilValue);
    }
    else { // var of equ
      sym_list = List::create( _["class"] = -1, _["name"] = "",
      _["description"]="", _["type"] = -1, _["domain"]="", _["domainType"]=-1,
      _["dimension"]=-1, _["numberRecords"] = -1, _["records"]=R_NilValue);
    }

    int nr_recs, subtype;
    if (!PGX.gdxSymbolInfoX(sym_nr, nr_recs, subtype, description))
      stop("gt_read_symbol:gdxSymbolInfoX GDX error (gdxSymbolInfoX). Symbol name = "s + sym_id);
    gdxStrIndexPtrs_t domains_ptr;
    gdxStrIndex_t domains;
    GDXSTRINDEXPTRS_INIT(domains, domains_ptr);
    auto domain_type = PGX.gdxSymbolGetDomainX(sym_nr, domains_ptr);
    if (!domain_type)
      stop("gt_read_symbol:gdxSymbolGetDomainX GDX error (gdxSymbolGetDomainX). Symbol name = "s + sym_id);

    std::vector<std::string> domain(dim);
    for (int d=0; d < dim; d++)
      domain[d] = domains_ptr[d];

    int dummy;
    if (sym_type == GMS_DT_ALIAS) {
      if (!subtype) {
        // alias to the Universe
        strcpy(alias_for_id, "*");

        if (!PGX.gdxDataReadRawStart(sym_nr, nr_recs))
          stop("gt_read_symbol:gdxDataReadRawStart GDX error (gdxDataReadStrStart). Symbol name = "s + sym_id);
        sym_list["class"] = "UniverseAlias";
      }
      else {
        // normal Alias
        if (!PGX.gdxSymbolInfo(subtype, alias_for_id, dim, dummy))
          stop("gt_read_symbol:gdxSymbolInfo GDX error (gdxSymbolInfo). Symbol name = "s + sym_id);
        sym_list["class"] = "Alias";
      }
      sym_list["name"] = sym_id;
      sym_list["aliasWith"] = alias_for_id;
      read_list[read_list_size] = clone(sym_list);
    }
    else {
      sym_list["name"] = sym_id;
      sym_list["class"] = gmsGdxTypeText[sym_type];
      sym_list["dimension"] = dim;
      sym_list["domain"] = domain;

      if (sym_type == GMS_DT_VAR) {
        sym_list["type"] = gmsVarTypeText[subtype];
      }
      else if (sym_type == GMS_DT_EQU) {
        int subtype_adjusted = subtype - GMS_EQU_USERINFO_BASE;
        sym_list["type"] = (subtype_adjusted >= 0 && subtype_adjusted <= 6) ? gmsEquTypeText[subtype_adjusted] : gmsEquTypeText[0];
      }
      else if (sym_type == GMS_DT_SET) {
        sym_list["isSingleton"] = bool(subtype);
      }

      sym_list["description"] = description;

      sym_list["domainType"] = (domain_type == 1) ? "none": (domain_type == 2 ? "relaxed" : "regular");

      sym_list["numberRecords"] = nr_recs;

      read_list[read_list_size] = clone(sym_list);
    }

  // if we just need metadata, stop here and return
  if (!read_records)
    return;

  int dom_dim, dom_type, dom_nrecs;
  // get domain symbol number and read if records is true
  gdxUelIndex_t dom_symid;
  if (!PGX.gdxSymbolGetDomain(sym_nr, dom_symid))
    stop("gt_read_symbol:gdxSymbolGetDomain GDX error (gdxSymbolGetDomain). Symbol name = "s + sym_id);

  std::vector<int> all_dom_nrecs(dim);
  std::vector<std::vector<int>> dom_uel_used(dim);

  gdxUelIndex_t gdx_uel_index;
  gdxValues_t gdx_values;
  for (int d = 0; d < dim; d++) {
    // get sym info for domain d
    std::array<char, GMS_SSSIZE> buf {};
    if (!PGX.gdxSymbolInfo(dom_symid[d], buf.data(), dom_dim, dom_type))
      stop("gt_read_symbol:gdxSymbolInfo GDX error (gdxSymbolInfo). Symbol name = "s + domain[d]);

    if (!(dom_type == GMS_DT_SET || dom_type == GMS_DT_ALIAS))
      stop("Invalid domain data type. Symbol name = "s + sym_id);

    if (dom_dim != 1)
      stop("Invalid domain dimension. Symbol name = "s + sym_id);

    // read raw start for domain d
    if (!PGX.gdxDataReadRawStart(dom_symid[d], dom_nrecs))
      stop("gt_read_symbol:gdxDataReadRawStart GDX error (gdxDataReadStrStart). Symbol name = "s + domain[d]);

    if (dom_nrecs < 0)
      stop("Invalid number of symbol records. Symbol name = "s + domain[d]);

    all_dom_nrecs[d] = dom_nrecs;
    dom_uel_used[d] = std::vector(dom_nrecs, 0);

    // if domain is not read before store a sym uel map
    // sym uel map gives the place of a given uel in given domain sym id.
    if (dom_symid[d] > 0 || sym_uel_map[dom_symid[d]].empty()) {
      std::vector<int> uel_map (uel_count + 1, -1); //for each symbol
      for (int k=0; k < dom_nrecs; k++) {
        if (!PGX.gdxDataReadRaw(gdx_uel_index, gdx_values, dummy))
          stop("gt_read_symbol:gdxDataReadRaw GDX error (gdxDataReadRaw). Symbol name = "s + domain[d]);

        uel_map[gdx_uel_index[0]] = k;
      }
      sym_uel_map[dom_symid[d]] = uel_map;
      uel_map.clear();
    }

    // read done for domain
    if (!PGX.gdxDataReadDone())
      stop("gt_read_symbol:gdxDataReadDone GDX error (gdxDataReadDone). Symbol name = "s + domain[d]);
  }

  // start reading the symbol
  if (!PGX.gdxDataReadRawStart(sym_nr, nr_recs))
    stop("gt_read_symbol:gdxDataReadRawStart GDX error (gdxDataReadStrStart). Symbol name = "s + sym_id);

  int num_out_of_bounds {};
  if (!nr_recs) {
    if (sym_type != GMS_DT_ALIAS) {
      sym_list["records"] = R_NilValue;
      read_list[read_list_size] = clone(sym_list);
    }
  }
  else {
    // indx_matrix stores positions of UELs in the domain set
    IntegerMatrix indx_matrix = Rcpp::no_init(nr_recs, dim);
    NumericMatrix record_values = Rcpp::no_init(nr_recs, sym_type == GMS_DT_VAR || sym_type == GMS_DT_EQU ? 5: 1);
    CharacterVector elem_text(nr_recs); // for elem_text
    int rec_nr {-1};
    std::array<char, GMS_SSSIZE> Msg {};
    int idx;
    for (int j=0; j < nr_recs; j++) {
      bool out_of_bounds {false};
      if (!PGX.gdxDataReadRaw(gdx_uel_index, gdx_values, dummy))
        stop("gt_read_symbol:gdxDataReadRaw GDX error (gdxDataReadRaw). Symbol name = "s + sym_id);

      rec_nr++;
      for (int d = 0; d < dim; d++) {
        if (gdx_uel_index[d] < 1 || gdx_uel_index[d] > uel_count) {
          out_of_bounds = true;
          indx_matrix(rec_nr, d) = 0;
        }
        else {
          idx = GET_DOM_MAP(d, gdx_uel_index[d]);
          indx_matrix(rec_nr, d) = idx + 1;
          //store domain labels
          //dom_uel_used is true if idx positioned UEL for domain d,
          // is used in the symbol
          dom_uel_used[d][idx] = true; // set used to true
        }
      }

      if (out_of_bounds)
        num_out_of_bounds++;

      if (sym_type == GMS_DT_SET || sym_type == GMS_DT_PAR || sym_type == GMS_DT_ALIAS) {
        if (sym_type == GMS_DT_SET || sym_type == GMS_DT_ALIAS) {
          elem_text(rec_nr) = PGX.gdxGetElemText(static_cast<int>(gdx_values[GMS_VAL_LEVEL]), Msg.data(), dummy) ? Msg.data() : "";
        } else
          record_values(rec_nr, 0) = n_acronyms > 0 ?gt_map_acronyms(acronyms, gdx_values[GMS_VAL_LEVEL]) : gdx_values[GMS_VAL_LEVEL];
      }
      else if (sym_type == GMS_DT_VAR || sym_type == GMS_DT_EQU) {
        for (int i = 0; i < 5; i++)
          record_values(rec_nr, i) = n_acronyms > 0 ?gt_map_acronyms(acronyms, gdx_values[i]) : gdx_values[i];

      }
    }

    // now map the numerical indx_matrix to string using from_codes
    std::vector<std::string> used_uels;

    List records;

    for (int d = 0; d < dim; d++) {
      if (dom_symid[d] < 0) continue;

      // change dom_uel_used from true/false to position in the symbol records
      int num_used{};
      for (int k = 0; k < all_dom_nrecs[d]; k++) {
        if (dom_uel_used[d][k] > 0) {
          dom_uel_used[d][k] = num_used++;
        }
        else {
          dom_uel_used[d][k] = -1;
        }
      }

      for (int k = 1; k <= uel_count; k++) {
        if (!dom_symid[d] || sym_uel_map[(int) dom_symid[d]][k] >= 0) {
          auto idx = GET_DOM_MAP(d, k);
          if (dom_uel_used[d][idx] < 0) continue; // if not used, continue

          if (!PGX.gdxUMUelGet(k, Msg.data(), dummy))
            stop("gt_read_symbol:gdxUMUelGet GDX error(gdxUMUelGet). Symbol name = "s + sym_id);
          used_uels.emplace_back(Msg.data());
        }
      }

      // shift domain indices
      for (int k = 0; k < nr_recs; k++) {
        if (indx_matrix(k, d) > 0)
          indx_matrix(k, d) = dom_uel_used[d][indx_matrix(k, d) - 1] + 1;
      }
      // create a factor v
      IntegerVector v = indx_matrix(_, d);

      // remap 0 to NA. This is preferred over setting NA in the first place because
      // is_na() check could be expensive
      if (num_out_of_bounds) {
        for (int i = 0; i < v.length(); i++) {
          if (v[i] == 0)
            v[i] = NA_INTEGER;
        }
      }

      CharacterVector ch = wrap(used_uels);

      v.attr("class") = "factor";
      v.attr("levels") = ch;

      // check if domain is duplicated and set domain labels accordingly
      std::set<std::string> unique_domain(domain.begin(), domain.end());
      std::string d_col_name;
      bool is_duplicated = (unique_domain.size() != domain.size());
      bool attr_as_dom {false};
      std::vector<std::string> var_eq_attr{"level", "maginal", "lower", "upper", "scale"};
      if (sym_type == GMS_DT_VAR || sym_type == GMS_DT_EQU) {
        attr_as_dom = (std::find(var_eq_attr.begin(), var_eq_attr.end(), domain.at(d)) != var_eq_attr.end()) ? true : false;
      }
      else if (sym_type == GMS_DT_PAR) {
        attr_as_dom = !domain.at(d).compare("value") ? true : false;
      }
      else if (sym_type == GMS_DT_SET)
        attr_as_dom = !domain.at(d).compare("element_text") ? true : false;

      if (is_duplicated || attr_as_dom) {
        d_col_name = !domain.at(d).compare("*") ? "uni_" + std::to_string(d + 1) : domain.at(d) + "_" + std::to_string(d + 1);
      }
      else
        d_col_name = !domain.at(d).compare("*") ? "uni" : domain.at(d);

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
      } else
        records["element_text"] = elem_text;
    }

    records.attr("class") = "data.frame";
    records.attr("row.names") = Rcpp::seq(1, nr_recs);

    if (sym_type != GMS_DT_ALIAS) {
      sym_list["records"]=records;
      read_list[read_list_size] = clone(sym_list);
    }
  }

  if (!PGX.gdxDataReadDone())
    stop("gt_read_symbol:gdxDataReadDone GDX error (gdxDataReadDone). Symbol name = "s + sym_id);

  if (num_out_of_bounds)
    warning("gt_read_symbol: Symbol '"s + sym_id + "' has "s + std::to_string(num_out_of_bounds) + " uels without a label.");
  return;

}

// [[Rcpp::export(.CPP_readSuper)]]
List CPP_readSuper(Nullable<CharacterVector> symNames_, CharacterVector gdxName,
                LogicalVector read_records) {
  auto gdx_name {Rcpp::as<std::string>(gdxName)};

  // create gdx object
  gt_gdx gdxobj;

  int err_nr;
  gdxobj.gdx.gdxOpenRead(gdx_name.c_str(), err_nr);
  if (err_nr)
    stop("CPP_readSuper:gdxOpenRead GDX error with error code %i", err_nr);

  std::array<char, GMS_SSSIZE> Msg {}, Producer {};
  gdxobj.gdx.gdxFileVersion(Msg.data(), Producer.data());

  // check for acronyms
  auto n_acronyms = gdxobj.gdx.gdxAcronymCount();
  std::vector<int> acronyms(n_acronyms);
  std::array<char, GMS_SSSIZE> acrName {};
  if (n_acronyms > 0) {
    warning("GDX file contains acronyms. "
          "Acronyms are not supported and are set to GAMS NA.\n");
    int acrID;
    for (int i=0; i < n_acronyms; i++){
      gdxobj.gdx.gdxAcronymGetInfo(i+1, acrName.data(), Msg.data(), acrID);
      acronyms[i] = acrID;
    }
  }

  // get symbol count
  int symCount, uel_count;
  if (!gdxobj.gdx.gdxSystemInfo(symCount, uel_count))
    stop("CPP_readSuper:gdxSystemInfo GDX error (gdxSystemInfo)");

  std::vector<bool> sym_enabled(symCount + 1, false); // initialize sym_enabled with false

  std::vector<std::vector<int>> sym_uel_map (symCount + 1);
  int l1_preallocate_size;
  CharacterVector symNames;

  if (symNames_.isNotNull())
    symNames = symNames_;

  l1_preallocate_size = symNames_.isNull() ? symCount : symNames.size();

  List read_list(l1_preallocate_size);

  if (symNames_.isNotNull()) {
    int sym_nr;
    std::string sym_name;
    for(int symcount=0; symcount < symNames.size(); symcount++) {
      sym_name = symNames(symcount);
      if (!gdxobj.gdx.gdxFindSymbol(sym_name.c_str(), sym_nr))
        stop("User specified to read symbol %s, but it does not "
        "exist in the source file", sym_name);
      sym_enabled.at(sym_nr) = true;
    }
  }


  // if user wants to read all symbols, iterate over gdx symbols
  // otherwise iterate over symbols provided by user
  if (read_records)
    gt_set_special_values(gdxobj.gdx);

  int read_list_size;
  read_list_size = 0;

  for (int i=1; i < symCount + 1; i++) {
    if (symNames_.isNotNull() && !sym_enabled.at(i)) continue;

    gt_read_symbol(gdxobj.gdx, i, Rcpp::as<bool>(read_records), read_list, read_list_size,
    sym_uel_map, uel_count, n_acronyms, acronyms);

    read_list_size ++;
  }


  return read_list;
}
