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

void WriteData(gdxHandle_t PGX, sym_info& mysym_info, StringVector names,
IntegerVector uel_ids, NumericVector V, std::string elemText, int mode) {
  gdxUelIndex_t gdx_uel_index;
  gdxStrIndexPtrs_t Indx;
  gdxStrIndex_t Indx_labels;
  gdxValues_t       Values;
  int rc, iDummy;
  char gdx_err_msg[GMS_SSSIZE], Msg[GMS_SSSIZE];
  std::string rec_name;
  if (mode == 1) {
    GDXSTRINDEXPTRS_INIT(Indx_labels, Indx);
    for (int D=0; D < mysym_info.dim; D++) {
      strcpy(Indx[D], names[D]);
    }
  }
  else {
    for (int D=0; D < mysym_info.dim; D++) {
      gdx_uel_index[D] = uel_ids[D];
    }
  }

  int v_count = 0;
  const double* default_values;
  if (mysym_info.type == GMS_DT_VAR || mysym_info.type == GMS_DT_EQU) {
    if (mysym_info.type == GMS_DT_VAR) {
      default_values = gmsDefRecVar[mysym_info.subtype];
    }
    else {
      default_values = gmsDefRecEqu[mysym_info.subtype - GMS_EQU_USERINFO_BASE];
    }

    for (int i =0; i < 5; i++) {
      if (mysym_info.missing_attributes[i]) {
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
    // parameter
    if (mysym_info.missing_attributes[GMS_VAL_LEVEL]) {
      Values[GMS_VAL_LEVEL] = 0;
    }
    else {
      Values[GMS_VAL_LEVEL] = V[GMS_VAL_LEVEL];
    }
  }
  if (mode == 1) {
    rc = gdxDataWriteStr(PGX, (const char **)Indx, Values);
  }
  else {
    rc = gdxDataWriteMap(PGX, gdx_uel_index, Values);
  }

  if (!rc) {
    StringVector s(mysym_info.dim);
    if (mode != 1) {
      for (int i = 0; i < mysym_info.dim; i++) {
        if (!gdxUMUelGet(PGX, gdx_uel_index[i], Msg, &iDummy)) {
          stop("WriteData:gdxUMUelGet GDX error(gdxUMUelGet)");
        }
        s[i] = Msg;
      }
    }
    else {
      for (int i = 0; i < mysym_info.dim; i++) {
        s[i] = names[i];
      }
    }
    gdxErrorStr(PGX, gdxGetLastError(PGX), gdx_err_msg);

    rec_name = rec_name + mysym_info.name;
    rec_name = rec_name + "(";
    for (int i = 0; i < mysym_info.dim; i++)
    {
        if (i > 0)
           rec_name = rec_name + ",";
        rec_name = rec_name + s[i];
    }

    rec_name = rec_name + ")";

    if (mode == 1) {
      stop("WriteData:gdxDataWriteStr GDX error in record %s:%s", rec_name, gdx_err_msg );
    }
    else {
      stop("WriteData:gdxDataWriteMap GDX error in record %s:%s", rec_name, gdx_err_msg );
    }
  }

  return;
}

void gt_register_uels(gt_gdx& gdxobj, CharacterVector arr, int* uel_id ) {
  int uel_no, N, rc;
  std::string myUEL;
  rc = gdxUELRegisterStrStart(gdxobj.gdx);
  if (!rc) stop("gt_register_uels:gdxUELRegisterStrStart GDX error (gdxUELRegisterStrStart)");

  N = arr.length();
  for (int i = 0; i < N; i++) {
    myUEL = arr[i];
    rc = gdxUELRegisterStr(gdxobj.gdx, myUEL.c_str(), &uel_no);
    if (!rc) stop("Error registering UEL: %s", myUEL);

    if (uel_id) {
      uel_id[i] = uel_no;
    }
  }
  if (!gdxUELRegisterDone(gdxobj.gdx))
    stop("gt_register_uels:gdxUELRegisterDone GDX error (gdxUELRegisterDone)");

}

void gt_open_write(gt_gdx& gdxobj, std::string filename, bool compress) {
  int rc, err_nr;
  if (!compress) {
    rc = gdxOpenWrite(gdxobj.gdx, filename.c_str(), "GAMS Transfer", &err_nr);
    if (!rc) stop("gt_open_write:gdxOpenWrite Error opening the file %s with error code %i", filename, err_nr);
  }
  else {
    rc = gdxOpenWriteEx(gdxobj.gdx, filename.c_str(), "GAMS Transfer", 1, &err_nr);
    if (!rc) stop("gt_open_write:gdxOpenWriteEx Error opening the file %s with error code %i", filename, err_nr);
  }
}

void gt_set_special_values(gt_gdx& gdxobj) {
  gdxSVals_t sVals;
  gdxGetSpecialValues(gdxobj.gdx, sVals);
  int rc;

  sVals[GMS_SVIDX_NA] = NA_REAL;
  sVals[GMS_SVIDX_EPS] = -0.0;
  sVals[GMS_SVIDX_UNDEF] = R_NaN;
  sVals[GMS_SVIDX_PINF] = R_PosInf;
  sVals[GMS_SVIDX_MINF] = R_NegInf;

  rc = gdxSetSpecialValues(gdxobj.gdx, sVals);
  if (!rc) stop("gt_set_special_values:gdxSetSpecialValues GDX error (gdxSetSpecialValues)");
  return;
}

void gt_register_priority_uels(gt_gdx& gdxobj, CharacterVector uel_priority) {
  int rc, uel_no;
  std::string uel;

  rc = gdxUELRegisterStrStart(gdxobj.gdx);
  if (!rc) stop("gt_register_priority_uels:gdxUELRegisterStrStart GDX error (gdxUELRegisterStrStart)");

  for (int i = 0; i < uel_priority.length(); i++) {
    uel = Rcpp::as<std::string>(uel_priority[i]);
    rc = gdxUELRegisterStr(gdxobj.gdx, uel.c_str(), &uel_no);
    if (!rc) stop("Error registering UEL: %s", uel);
  }

  if (!gdxUELRegisterDone(gdxobj.gdx))
  stop("gt_register_priority_uels:gdxUELRegisterDone GDX error (gdxUELRegisterDone)");

}


void gt_write_symbol(gt_gdx& gdxobj, sym_info& info, int mode) {
    StringVector names(info.dim);
    int** uel_map = new int*[info.dim];
    IntegerVector uel_ids(info.dim);
    IntegerVector tempcol;
    int ncols, nrows, rc;
    char Msg[GMS_SSSIZE];

    gdxStrIndexPtrs_t domains_ptr;
    gdxStrIndex_t domains;
    GDXSTRINDEXPTRS_INIT(domains, domains_ptr);

    if (info.records) {
      nrows = info.records->nrows();
      ncols = info.records->size();

      // register UELs
      CharacterVector dom_col_string(nrows);
      IntegerVector tempcol(nrows);
      for (int d = 0; d < info.dim; d++) {
        tempcol = (*info.records)[d];
        dom_col_string = tempcol.attr("levels");
        if (mode == 1) {
          gt_register_uels(gdxobj, dom_col_string, NULL);
        }
        else {
          // mapped mode
          uel_map[d] = new int[dom_col_string.length()];
          gt_register_uels(gdxobj, dom_col_string, uel_map[d]);
        }
      }
    }

    if (mode == 1) {
      if (!gdxDataWriteStrStart(gdxobj.gdx, info.name.c_str(), 
      info.description.c_str(), info.dim, info.type, info.subtype))
      stop("gt_write_symbol:gdxDataWriteStrStart GDX error (gdxDataWriteStrStart)");
    }
    else {
      if (!gdxDataWriteMapStart(gdxobj.gdx, info.name.c_str(), 
      info.description.c_str(), info.dim, info.type, info.subtype))
      stop("gt_write_symbol:gdxDataWriteMapStart GDX error (gdxDataWriteMapStart)");
    }

    for (int D=0; D < info.dim; D++) {
      strcpy(domains_ptr[D], info.domain[D].c_str());
    }

    if (info.domain_type == "regular") {
      rc = gdxSymbolSetDomain(gdxobj.gdx, (const char **)domains_ptr);
      if (!rc) {
        gdxGetLastError(gdxobj.gdx); // clears last error
        rc = gdxSymbolSetDomainX(gdxobj.gdx, info.sym_nr, (const char **)domains_ptr);
        if (!rc) {
          gdxErrorStr(gdxobj.gdx, gdxGetLastError(gdxobj.gdx), Msg);
          stop("gt_write_symbol:gdxSymbolSetDomain GDX error: %s",Msg);
        }
      }
    }
    else if (info.domain_type == "relaxed") {
      rc = gdxSymbolSetDomainX(gdxobj.gdx, info.sym_nr, (const char **)domains_ptr);
      if (!rc) {
        gdxErrorStr(gdxobj.gdx, gdxGetLastError(gdxobj.gdx), Msg);
        stop("gt_write_symbol:gdxSymbolSetDomainX GDX error: %s",Msg);
      }
    }

    if (!info.records) {
      if (!gdxDataWriteDone(gdxobj.gdx)) stop("gt_write_symbol:gdxDataWriteDone GDX error (gdxDataWriteDone)");
      delete[] uel_map;
      return;
    }

    int n_attr;
    if (info.type == GMS_DT_PAR) {
      n_attr = 1;
    }
    else {
      n_attr = 5;
    }

    NumericMatrix rec_vals(nrows, n_attr);

    if (nrows == 0) {
      if (info.dim != 0 || info.type == GMS_DT_SET) {
        if (!gdxDataWriteDone(gdxobj.gdx)) stop("gt_write_symbol:gdxDataWriteDone GDX error (gdxDataWriteDone)");
      }
      else {
        NumericVector dummyvec;
        IntegerVector dummyintvec;
        // for scalars, write the default values
        // no attribute column. Fill all values with default
        if (info.type == GMS_DT_PAR) {
          info.missing_attributes[GMS_VAL_LEVEL] = true;
        }
        else {
          for (int d = 0; d < n_attr; d++) {
            info.missing_attributes[d] = true;
          }
        }

        WriteData(gdxobj.gdx, info, "", dummyintvec, dummyvec, "", mode);

        if (!gdxDataWriteDone(gdxobj.gdx)) stop("gt_write_symbol:gdxDataWriteDone GDX error (gdxDataWriteDone)");
      }
    }
    else {
      StringMatrix rec_domain_str(nrows, info.dim);
      IntegerMatrix rec_domain_int(nrows, info.dim);
      StringVector tempcol_str(nrows);

      if (mode == 1) {
        for (int d = 0; d < info.dim; d++) {
          tempcol_str = (*info.records)[d];
          rec_domain_str(_, d) = tempcol_str;
        }
      }
      else {
        for (int d = 0; d < info.dim; d++) {
          tempcol = (*info.records)[d];
          rec_domain_int(_, d) = tempcol;
        }
      }

      StringVector elem_text(nrows);
      std::string text;
      if (info.type == GMS_DT_SET) {
        if (info.records->length() == info.dim + 1) {
          elem_text = (*info.records)[info.dim];
        }
        else {
          elem_text = "";
        }
      }
      else {
        NumericVector temp_num_col(nrows);
        if (ncols > info.dim) {
          // one or more attribute columns
          // for parameters this is enough to say all attributes are present
          if (ncols - info.dim == n_attr) {
            // all attribute columns are present
            for (int d = info.dim; d < ncols; d++) {
              temp_num_col = (*info.records)[d];
              rec_vals(_, d-info.dim) = temp_num_col;
            }
          }
          else {
            // some attribute columns are missing
            // can only happen for variables and equations
            CharacterVector colnames = info.records->names();
            std::string attributes[5] = {"level", "marginal", "lower", "upper", "scale"};

            std::vector<std::string> colnames_vec(colnames.size());

            for (int i = 0; i < colnames.size(); i++){
                colnames_vec[i] = colnames(i);
            }

            int rec_val_column_count = 0;
            for (int i =0; i < 5; i++) {
              std::string attr = attributes[i];

              if ( std::any_of(colnames_vec.begin(), colnames_vec.end(), [attr](std::string i){return i==attr;}) ) {
                temp_num_col = (*info.records)[attr];
                rec_vals(_, rec_val_column_count) = temp_num_col;
                rec_val_column_count++;
              }
              else {
                info.missing_attributes[i] = true;
              }
            }
          }
        }
        else {
          // no attribute column. Fill all values with default
          if (info.type == GMS_DT_PAR) {
            info.missing_attributes[GMS_VAL_LEVEL] = true;
          }
          else {
            for (int d = 0; d < n_attr; d++) {
              info.missing_attributes[d] = true;
            }
          }
        }
      }

      int rel_id;
      for (int i =0; i < nrows; i++) {
        if (mode != 1) {
          for (int d =0; d< info.dim; d++) {
            rel_id = rec_domain_int(i, d) - 1;
            uel_ids[d] = uel_map[d][rel_id];
          }
        }
        else {
          names = rec_domain_str(i, _);
        }
        if (info.type != GMS_DT_SET){
          if (mode == 1) {
            WriteData(gdxobj.gdx, info, names, 0, rec_vals(i, _), "", mode);
          }
          else {
            WriteData(gdxobj.gdx, info, "", uel_ids, rec_vals(i, _), "", mode);
          }
        }
        else {
          NumericVector zero_vec = {0};
          if (info.records->length() == info.dim + 1) {
            text = Rcpp::as<std::string>(elem_text[i]);
          }
          else {
            text = "";
          }
          if (mode == 1) {
            WriteData(gdxobj.gdx, info, names, 0, zero_vec, text, mode);
          }
          else {
            WriteData(gdxobj.gdx, info, "", uel_ids, zero_vec, text, mode);
          }
        }
      }
      if (!gdxDataWriteDone(gdxobj.gdx)) stop("gt_write_symbol:gdxDataWriteDone GDX error (gdxDataWriteDone)");
    }

    if (mode != 1) {
      for (int d =0; d < info.dim; d++) {
        delete[] uel_map[d];
      }
    }
    delete[] uel_map;

  // get the error count
  if (gdxDataErrorCount(gdxobj.gdx)) {
      gdxErrorStr(gdxobj.gdx, gdxGetLastError(gdxobj.gdx), Msg);
      stop("gt_write_symbol:gdxError GDX error for %s: %s", info.name, Msg);
  }
}

// [[Rcpp::export]]
void CPP_gdxWriteSuper(List writeList, CharacterVector sysDir, LogicalVector enable,
CharacterVector fileName, Nullable<CharacterVector> uel_priority_,
bool compress, int mode) {
// takes list input instead of container input
  std::string mysysDir = Rcpp::as<std::string>(sysDir);
  std::string myFileName = Rcpp::as<std::string>(fileName);
  gt_gdx gdxobj;
  gdxobj.init_library(mysysDir.c_str());

  gt_open_write(gdxobj, myFileName, compress);

  gt_set_special_values(gdxobj);

  if (uel_priority_.isNotNull()) {
    CharacterVector uel_priority(uel_priority_);
    gt_register_priority_uels(gdxobj, uel_priority);
  }

  DataFrame df;
  List domain;
  int Dim, sym_nr;
  StringVector colString, colElemText;
  NumericVector colDouble;
  sym_nr = 0;

  for (int d=0; d < writeList.length(); d++) {
    if (!enable[d]) continue;
    sym_info mysym_info;
    sym_nr++;
    List sym_data = writeList[d];

    // get all data from the object
    std::string sym_name = sym_data["name"];
    mysym_info.name = sym_name;
    mysym_info.type = sym_data["type"];

    if (mysym_info.type == GMS_DT_ALIAS) {
      std::string alias_with = sym_data["aliasWith"];
      if (!gdxAddAlias(gdxobj.gdx, sym_name.c_str(), alias_with.c_str()))
      stop("CPP_gdxWriteSuper:gdxAddAlias GDX error (gdxAddAlias)");
      continue;
    }

    mysym_info.dim =  sym_data["dimension"];

    if (mysym_info.type == GMS_DT_VAR || mysym_info.type == GMS_DT_EQU) {
      mysym_info.subtype = sym_data["subtype"];
    }
    else if (mysym_info.type == GMS_DT_SET) {
      mysym_info.subtype = sym_data["isSingleton"];
    }

    mysym_info.description = Rcpp::as<std::string>(sym_data["description"]);

    std::string domaintype = sym_data["domainType"];

    mysym_info.domain_type = domaintype;



    bool df_is_null;
    df_is_null = false;
    if (Rf_isNull(sym_data["records"])) {
      df_is_null =  true;
    }

    df = sym_data["records"];

    CharacterVector domain;
    if (mysym_info.dim != 0) {
      domain = sym_data["domain"];
      mysym_info.domain = new std::string[mysym_info.dim];
    }

    for (int s = 0; s < mysym_info.dim; s++) {
      mysym_info.domain[s] = Rcpp::as<std::string>(domain[s]);
    }

    if (df_is_null) {
      mysym_info.records = NULL;
    }
    else {
      mysym_info.records = &df;
    }

    gt_write_symbol(gdxobj, mysym_info, mode);

  }

  gdxAutoConvert(gdxobj.gdx, 0);
  return;
}

// [[Rcpp::export]]
void CPP_gdxWrite(Environment container, LogicalVector enable,
CharacterVector fileName, Nullable<CharacterVector> uel_priority_,
bool compress, int mode) {
  Environment data_dict = container["data"];
  Function as_list = data_dict["as_list"];
  List data = as_list();

  CharacterVector sysDir = container["systemDirectory"];

  std::string mysysDir = Rcpp::as<std::string>(sysDir);
  std::string myFileName = Rcpp::as<std::string>(fileName);

  gt_gdx gdxobj;
  gdxobj.init_library(mysysDir.c_str());

  /* open */
  gt_open_write(gdxobj, myFileName, compress);
  gt_set_special_values(gdxobj);

  if (uel_priority_.isNotNull()) {
    std::string uel;
    CharacterVector uel_priority(uel_priority_);

    gt_register_priority_uels(gdxobj, uel_priority);
  }

  DataFrame df;
  List domain;
  int sym_nr;
  StringVector colString, colElemText;
  NumericVector colDouble;
  sym_nr = 0;

  for (int d=0; d < data.length(); d++) {
    if (!enable[d]) continue;

    sym_info mysym_info;
    sym_nr++;
    Environment sym_obj = data[d];

    // get all data from the object
    std::string sym_name = sym_obj["name"];
    mysym_info.name = sym_name;
    mysym_info.type = sym_obj[".gams_type"];
    mysym_info.dim = sym_obj["dimension"];
    mysym_info.subtype = sym_obj[".gams_subtype"];
    mysym_info.description = Rcpp::as<std::string>(sym_obj["description"]);
    std::string domaintype = sym_obj["domainType"];
    mysym_info.domain_type = domaintype;
    mysym_info.sym_nr = sym_nr;
    List domainstr;
    if (mysym_info.dim != 0) {
      domainstr = sym_obj["domainNames"];
      mysym_info.domain = new std::string[mysym_info.dim];
    }

    for (int s = 0; s < mysym_info.dim; s++) {
      mysym_info.domain[s] = Rcpp::as<std::string>(domainstr[s]);
    }

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

      if (!gdxAddAlias(gdxobj.gdx, mysym_info.name.c_str(), alias_with.c_str()))
      stop("CPP_gdxWrite:gdxAddAlias GDX error (gdxAddAlias)");
      continue;
    }

    bool df_is_null;
    df_is_null = false;
    if (Rf_isNull(sym_obj["records"])) {
      df_is_null =  true;
    }
    df = sym_obj["records"];
    if (df_is_null) {
      mysym_info.records = NULL;
    }
    else {
      mysym_info.records = &df;
    }

    gt_write_symbol(gdxobj, mysym_info, mode);
  }

  gdxAutoConvert(gdxobj.gdx, 0);
  return;
}
