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
// #include "gdxcc.h"
#include "gclgms.h"
#include "utilities.hpp"
#include "gdx.h"

using namespace Rcpp;
using namespace std::literals::string_literals;

void WriteData(gdx::TGXFileObj & PGX, const sym_info& mysym_info, const bool missing_attr[], const StringVector & names,
const std::vector<int>& uel_ids, const NumericVector & V, const std::string & elemText, int mode) {
  gdxUelIndex_t gdx_uel_index;
  gdxStrIndexPtrs_t Indx;
  gdxStrIndex_t Indx_labels;
  gdxValues_t       Values;

  if (mode == 1) {
    GDXSTRINDEXPTRS_INIT(Indx_labels, Indx);
    for (int d=0; d < mysym_info.dim; d++)
      strcpy(Indx[d], names[d]);
  }
  else {
    for (int d=0; d < mysym_info.dim; d++)
      gdx_uel_index[d] = uel_ids[d];
  }

  int v_count = 0;
  const double* default_values;
  if (mysym_info.type == GMS_DT_VAR || mysym_info.type == GMS_DT_EQU) {
    default_values = mysym_info.type == GMS_DT_VAR ? gmsDefRecVar[mysym_info.subtype] : gmsDefRecEqu[mysym_info.subtype - GMS_EQU_USERINFO_BASE];

    for (int i =0; i < 5; i++) {
      if (missing_attr[i]) {
        Values[i] = default_values[i] == GMS_SV_MINF ? R_NegInf : (default_values[i] == GMS_SV_PINF ? R_PosInf : default_values[i]);
      }
      else {
        Values[i] = V[v_count];
        v_count++;
      }
    }
  }
  else if (mysym_info.type == GMS_DT_SET) {
    if (elemText.compare("") != 0 ) {
      int txtnr;
      if (!PGX.gdxAddSetText(elemText.c_str(), txtnr))
        stop("WriteData:gdxAddSetText GDX error (gdxAddSetText). Symbol name = "s + mysym_info.name);

      Values[GMS_VAL_LEVEL] = txtnr;
      Values[GMS_VAL_MARGINAL] = 0;
      Values[GMS_VAL_UPPER] = 0;
      Values[GMS_VAL_LOWER] = 0;
      Values[GMS_VAL_SCALE] = 0;

    }
    else
      Values[GMS_VAL_LEVEL]=0;
  }
  else
    // parameter
    Values[GMS_VAL_LEVEL] = missing_attr[GMS_VAL_LEVEL] ? 0 : V[GMS_VAL_LEVEL];

  int rc;
  if (mode == 1) {
    rc = PGX.gdxDataWriteStr((const char **)Indx, Values);
  }
  else
    rc = PGX.gdxDataWriteMap(gdx_uel_index, Values);

  if (!rc) {
    StringVector s(mysym_info.dim);
    if (mode != 1) {
      int iDummy;
      std::array<char, GMS_SSSIZE> Msg {};
      for (int d = 0; d < mysym_info.dim; d++) {
        if (!PGX.gdxUMUelGet(gdx_uel_index[d], Msg.data(), iDummy))
          stop("WriteData:gdxUMUelGet GDX error(gdxUMUelGet)");
        s[d] = Msg.data();
      }
    }
    else {
      for (int d = 0; d < mysym_info.dim; d++)
        s[d] = names[d];
    }

    std::array<char, GMS_SSSIZE> gdx_err_msg {};
    PGX.gdxErrorStr(PGX.gdxGetLastError(), gdx_err_msg.data());

    std::string rec_name;
    rec_name = rec_name + mysym_info.name;
    rec_name = rec_name + "(";
    for (int d = 0; d < mysym_info.dim; d++)
    {
        if (d > 0)
           rec_name = rec_name + ",";
        rec_name = rec_name + s[d];
    }

    rec_name = rec_name + ")";

    if (mode == 1) {
      stop("WriteData:gdxDataWriteStr GDX error for symbol %s in record %s:%s", mysym_info.name, rec_name, gdx_err_msg.data() );
    }
    else
      stop("WriteData:gdxDataWriteMap GDX error for symbol %s in record %s:%s", mysym_info.name, rec_name, gdx_err_msg.data() );
  }

  return;
}

void gt_register_uels(gdx::TGXFileObj & gdx, const CharacterVector & arr, int* uel_id ) {
  int uel_no, N, rc;
  std::string myUEL;
  rc = gdx.gdxUELRegisterStrStart();
  if (!rc)
    stop("gt_register_uels:gdxUELRegisterStrStart GDX error (gdxUELRegisterStrStart)");

  N = arr.length();
  for (int i = 0; i < N; i++) {
    myUEL = arr[i];
    rc = gdx.gdxUELRegisterStr(myUEL.c_str(), uel_no);
    if (!rc)
      stop("Error registering UEL: %s", myUEL);

    if (uel_id)
      uel_id[i] = uel_no;
  }

  if (!gdx.gdxUELRegisterDone())
    stop("gt_register_uels:gdxUELRegisterDone GDX error (gdxUELRegisterDone)");
}

void gt_open_write(gdx::TGXFileObj & gdx, const std::string & filename, bool compress) {
  int rc, err_nr;
  if (!compress) {
    rc = gdx.gdxOpenWrite(filename.c_str(), "GAMS Transfer", err_nr);
    if (!rc)
      stop("gt_open_write:gdxOpenWrite Error opening the file %s with error code %i", filename, err_nr);
  }
  else {
    rc = gdx.gdxOpenWriteEx(filename.c_str(), "GAMS Transfer", 1, err_nr);
    if (!rc)
      stop("gt_open_write:gdxOpenWriteEx Error opening the file %s with error code %i", filename, err_nr);
  }
}

void gt_register_priority_uels(gdx::TGXFileObj & gdx, const CharacterVector & uel_priority) {
  int rc, uel_no;
  std::string uel;

  rc = gdx.gdxUELRegisterStrStart();
  if (!rc)
    stop("gt_register_priority_uels:gdxUELRegisterStrStart GDX error (gdxUELRegisterStrStart)");

  for (int i = 0; i < uel_priority.length(); i++) {
    uel = Rcpp::as<std::string>(uel_priority[i]);
    rc = gdx.gdxUELRegisterStr(uel.c_str(), uel_no);
    if (!rc)
      stop("Error registering UEL: %s", uel);
  }

  if (!gdx.gdxUELRegisterDone())
    stop("gt_register_priority_uels:gdxUELRegisterDone GDX error (gdxUELRegisterDone)");
}


void gt_write_symbol(gdx::TGXFileObj & gdx, const sym_info & info, int mode) {
    int ncols{0}, nrows{0};
    gdxStrIndexPtrs_t domains_ptr;
    gdxStrIndex_t domains;
    GDXSTRINDEXPTRS_INIT(domains, domains_ptr);

    std::vector<std::vector<int>> uel_map (info.dim);
    if (info.records) {
      nrows = info.records->nrows();
      ncols = info.records->size();

      // register UELs
      CharacterVector dom_col_string = Rcpp::no_init(nrows);
      for (int d = 0; d < info.dim; d++) {
        dom_col_string = Rcpp::as<IntegerVector>((*info.records)[d]).attr("levels");
        if (mode == 1) {
          gt_register_uels(gdx, dom_col_string, NULL);
        }
        else {
          // mapped mode
          uel_map[d] = std::vector<int>(dom_col_string.length());
          gt_register_uels(gdx, dom_col_string, uel_map[d].data());
        }
      }
    }

    if (mode == 1) {
      if (!gdx.gdxDataWriteStrStart(info.name.c_str(),
      info.description.c_str(), info.dim, info.type, info.subtype))
        stop("gt_write_symbol:gdxDataWriteStrStart GDX error (gdxDataWriteStrStart). Symbol name = "s + info.name);
    }
    else {
      if (!gdx.gdxDataWriteMapStart(info.name.c_str(),
      info.description.c_str(), info.dim, info.type, info.subtype))
        stop("gt_write_symbol:gdxDataWriteMapStart GDX error (gdxDataWriteMapStart). Symbol name = "s + info.name);
    }

    for (int d=0; d < info.dim; d++)
      strcpy(domains_ptr[d], info.domain[d].c_str());

    std::array<char, GMS_SSSIZE> Msg {};
    int rc;
    if (info.domain_type == "regular") {
      rc = gdx.gdxSymbolSetDomain((const char **)domains_ptr);
      if (!rc) {
        gdx.gdxGetLastError(); // clears last error
        rc = gdx.gdxSymbolSetDomainX(info.sym_nr, (const char **)domains_ptr);
        if (!rc) {
          gdx.gdxErrorStr(gdx.gdxGetLastError(), Msg.data());
          stop("gt_write_symbol:gdxSymbolSetDomain GDX error: %s, Symbol name: %s",Msg.data(), info.name);
        }
      }
    }
    else if (info.domain_type == "relaxed") {
      rc = gdx.gdxSymbolSetDomainX(info.sym_nr, (const char **)domains_ptr);
      if (!rc) {
        gdx.gdxErrorStr(gdx.gdxGetLastError(), Msg.data());
        stop("gt_write_symbol:gdxSymbolSetDomainX GDX error: %s, Symbol name: %s", Msg.data(), info.name);
      }
    }

    if (!info.records) {
      if (!gdx.gdxDataWriteDone())
        stop("gt_write_symbol:gdxDataWriteDone GDX error (gdxDataWriteDone). Symbol name = "s + info.name);
      return;
    }

    int n_attr;
    n_attr = info.type == GMS_DT_PAR ? 1 : 5;
    bool missing_attr[5] = {false};

    std::vector<int> dummy_int_vec;
    if (!nrows) {
      if (info.dim != 0 || info.type == GMS_DT_SET) {
        if (!gdx.gdxDataWriteDone())
          stop("gt_write_symbol:gdxDataWriteDone GDX error (gdxDataWriteDone). Symbol name = "s + info.name);
      }
      else {
        NumericVector dummyvec;
        // for scalars, write the default values
        // no attribute column. Fill all values with default
        if (info.type == GMS_DT_PAR) {
          missing_attr[GMS_VAL_LEVEL] = true;
        }
        else {
          for (int i = 0; i < n_attr; i++)
            missing_attr[i] = true;
        }

        WriteData(gdx, info, missing_attr, "", dummy_int_vec, dummyvec, "", mode);

        if (!gdx.gdxDataWriteDone())
          stop("gt_write_symbol:gdxDataWriteDone GDX error (gdxDataWriteDone). Symbol name = "s + info.name);
      }
    }
    else {
      StringMatrix rec_domain_str = Rcpp::no_init(nrows, info.dim);
      IntegerMatrix rec_domain_int = Rcpp::no_init(nrows, info.dim);

      if (mode == 1) {
        for (int d = 0; d < info.dim; d++)
          rec_domain_str(_, d) = Rcpp::as<StringVector>((*info.records)[d]);
      }
      else {
        for (int d = 0; d < info.dim; d++)
          rec_domain_int(_, d) = Rcpp::as<IntegerVector>((*info.records)[d]);
      }

      NumericMatrix rec_vals = Rcpp::no_init(nrows, n_attr);
      StringVector elem_text = Rcpp::no_init(nrows);
      if (info.type == GMS_DT_SET) {
        if (info.records->length() == info.dim + 1) {
          elem_text = (*info.records)[info.dim];
        }
        else {
          elem_text = "";
        }
      }
      else {
        if (ncols > info.dim) {
          // one or more attribute columns
          // for parameters this is enough to say all attributes are present
          if (ncols - info.dim == n_attr) {
            // all attribute columns are present
            for (int d = info.dim; d < ncols; d++)
              rec_vals(_, d-info.dim) = Rcpp::as<NumericVector>((*info.records)[d]);
          }
          else {
            // some attribute columns are missing
            // can only happen for variables and equations
            CharacterVector colnames = info.records->names();
            std::string attributes[5] = {"level", "marginal", "lower", "upper", "scale"};

            std::vector<std::string> colnames_vec(colnames.size());

            for (int i = 0; i < colnames.size(); i++)
                colnames_vec[i] = colnames(i);

            int rec_val_column_count = 0;
            std::string attr;
            for (int i =0; i < 5; i++) {
              attr = attributes[i];

              if ( std::any_of(colnames_vec.begin(), colnames_vec.end(), [attr](std::string i){return i==attr;}) ) {
                rec_vals(_, rec_val_column_count) = Rcpp::as<NumericVector>((*info.records)[attr]);
                rec_val_column_count++;
              }
              else
                missing_attr[i] = true;
            }
          }
        }
        else {
          // no attribute column. Fill all values with default
          for (int i = 0; i < n_attr; i++)
            missing_attr[i] = true;
        }
      }

      int rel_id;
      std::vector<int> uel_ids(info.dim);
      StringVector names(info.dim);
      for (int i =0; i < nrows; i++) {
        if (mode != 1) {
          for (int d =0; d< info.dim; d++) {
            rel_id = rec_domain_int(i, d) - 1;
            uel_ids[d] = uel_map[d][rel_id];
          }
        }
        else
          names = rec_domain_str(i, _);

        if (info.type != GMS_DT_SET) {
          if (mode == 1) {
            WriteData(gdx, info, missing_attr, names, dummy_int_vec, rec_vals(i, _), "", mode);
          }
          else {
            WriteData(gdx, info, missing_attr, "", uel_ids, rec_vals(i, _), "", mode);
          }
        }
        else {
          NumericVector zero_vec = {0};
          std::string text;
          text = info.records->length() == info.dim + 1 ? Rcpp::as<std::string>(elem_text[i]) : "";

          if (mode == 1) {
            WriteData(gdx, info, missing_attr, names, dummy_int_vec, zero_vec, text, mode);
          }
          else {
            WriteData(gdx, info, missing_attr, "", uel_ids, zero_vec, text, mode);
          }
        }
      }
      if (!gdx.gdxDataWriteDone())
        stop("gt_write_symbol:gdxDataWriteDone GDX error (gdxDataWriteDone). Symbol name = "s + info.name);
    }

  // get the error count
  if (gdx.gdxDataErrorCount()) {
      gdx.gdxErrorStr(gdx.gdxGetLastError(), Msg.data());
      stop("gt_write_symbol:gdxError GDX error for %s: %s", info.name, Msg.data());
  }
}

// [[Rcpp::export]]
void CPP_gdxWriteSuper(const List & writeList, const LogicalVector & enable,
const CharacterVector & fileName, const Nullable<CharacterVector> & uel_priority_,
bool compress, int mode) {
// takes list input instead of container input
  std::string myFileName = Rcpp::as<std::string>(fileName);

  // create gdx object
  gt_gdx gdxobj;

  gt_open_write(gdxobj.gdx, myFileName, compress);

  gt_set_special_values(gdxobj.gdx);

  if (uel_priority_.isNotNull()) {
    CharacterVector uel_priority(uel_priority_);
    gt_register_priority_uels(gdxobj.gdx, uel_priority);
  }

  DataFrame df;
  int sym_nr;
  sym_nr = 0;

  for (int i=0; i < writeList.length(); i++) {
    if (!enable[i]) continue;

    sym_info mysym_info;
    sym_nr++;
    List sym_data = writeList[i];

    // get all data from the object
    std::string sym_name = sym_data["name"];
    mysym_info.name = sym_name;
    std::string type_str;
    type_str = Rcpp::as<std::string>(sym_data["class"]);
    mysym_info.type = symTypeText_to_int.at(type_str);

    if (mysym_info.type == GMS_DT_ALIAS) {
      std::string alias_with = sym_data["aliasWith"];
      if (!gdxobj.gdx.gdxAddAlias(sym_name.c_str(), alias_with.c_str()))
      stop("CPP_gdxWriteSuper:gdxAddAlias GDX error (gdxAddAlias)");
      continue;
    }

    mysym_info.dim =  sym_data["dimension"];
    std::string subtype_str;
    if (mysym_info.type == GMS_DT_VAR) {
      subtype_str = Rcpp::as<std::string>(sym_data["type"]);
      mysym_info.subtype = varTypeText_to_int.at(subtype_str);
    }
    else if (mysym_info.type == GMS_DT_EQU) {
      subtype_str = Rcpp::as<std::string>(sym_data["type"]);
      mysym_info.subtype = equTypeText_to_int.at(subtype_str) + GMS_EQU_USERINFO_BASE;
    }
    else if (mysym_info.type == GMS_DT_SET) {
      mysym_info.subtype = sym_data["isSingleton"];
    }

    mysym_info.description = Rcpp::as<std::string>(sym_data["description"]);

    std::string domaintype = sym_data["domainType"];

    mysym_info.domain_type = domaintype;
    mysym_info.sym_nr = sym_nr;

    bool df_is_null;
    df_is_null = Rf_isNull(sym_data["records"]) ? true : false;

    df = sym_data["records"];

    CharacterVector domain;
    if (mysym_info.dim != 0) {
      domain = sym_data["domain"];
      mysym_info.domain = new std::string[mysym_info.dim];
    }

    for (int d = 0; d < mysym_info.dim; d++)
      mysym_info.domain[d] = Rcpp::as<std::string>(domain[d]);

    mysym_info.records = df_is_null ? NULL : &df;

    gt_write_symbol(gdxobj.gdx, mysym_info, mode);
  }

  gdxobj.gdx.gdxAutoConvert(0);
  gdxobj.gdx.gdxClose();
  return;
}

// [[Rcpp::export]]
void CPP_gdxWrite(Environment container, LogicalVector enable,
CharacterVector fileName, Nullable<CharacterVector> uel_priority_,
bool compress, int mode) {
  Environment data_dict = container["data"];
  Function as_list = data_dict["as_list"];
  List data = as_list();

  std::string myFileName = Rcpp::as<std::string>(fileName);

  // create gdx object
  gt_gdx gdxobj;

  /* open */
  gt_open_write(gdxobj.gdx, myFileName, compress);
  gt_set_special_values(gdxobj.gdx);

  if (uel_priority_.isNotNull()) {
    std::string uel;
    CharacterVector uel_priority(uel_priority_);

    gt_register_priority_uels(gdxobj.gdx, uel_priority);
  }

  DataFrame df;
  int sym_nr;
  sym_nr = 0;

  for (int i=0; i < data.length(); i++) {
    if (!enable[i]) continue;

    sym_info mysym_info;
    sym_nr++;
    Environment sym_obj = data[i];

    // get all data from the object
    std::string sym_name = sym_obj["name"];
    mysym_info.name = sym_name;
    std::string type_str;
    CharacterVector class_vec;
    class_vec = sym_obj.attr("class");
    type_str = class_vec[0];
    mysym_info.type = symTypeText_to_int.at(type_str);
    mysym_info.dim = sym_obj["dimension"];

    std::string subtype_str;
    if (mysym_info.type == GMS_DT_VAR) {
      subtype_str = Rcpp::as<std::string>(sym_obj["type"]);
      mysym_info.subtype = varTypeText_to_int.at(subtype_str);
    }
    else if (mysym_info.type == GMS_DT_EQU) {
      subtype_str = Rcpp::as<std::string>(sym_obj["type"]);
      mysym_info.subtype = equTypeText_to_int.at(subtype_str) + GMS_EQU_USERINFO_BASE;
    }
    else if (mysym_info.type == GMS_DT_SET) {
      mysym_info.subtype = sym_obj["isSingleton"];
    }

    mysym_info.description = Rcpp::as<std::string>(sym_obj["description"]);
    std::string domaintype = sym_obj["domainType"];
    mysym_info.domain_type = domaintype;
    mysym_info.sym_nr = sym_nr;
    List domainstr;
    if (mysym_info.dim != 0) {
      domainstr = sym_obj["domainNames"];
      mysym_info.domain = new std::string[mysym_info.dim];
    }

    for (int d = 0; d < mysym_info.dim; d++)
      mysym_info.domain[d] = Rcpp::as<std::string>(domainstr[d]);

    std::string alias_with;
    if (mysym_info.type == GMS_DT_ALIAS) {
      bool isUniverseAlias = sym_obj[".isUniverseAlias"];
      if (isUniverseAlias) {
        alias_with = Rcpp::as<std::string>(sym_obj["aliasWith"]);
      }
      else {
        Environment alias_with_env = sym_obj["aliasWith"];
        alias_with = Rcpp::as<std::string>(alias_with_env["name"]);
      }

      if (!gdxobj.gdx.gdxAddAlias(mysym_info.name.c_str(), alias_with.c_str()))
        stop("CPP_gdxWrite:gdxAddAlias GDX error (gdxAddAlias)");
      continue;
    }

    bool df_is_null;

    df_is_null = Rf_isNull(sym_obj["records"]) ? true : false;

    df = sym_obj["records"];
    mysym_info.records = df_is_null ? NULL : &df;

    gt_write_symbol(gdxobj.gdx, mysym_info, mode);
  }

  gdxobj.gdx.gdxAutoConvert(0);
  gdxobj.gdx.gdxClose();
  return;
}
