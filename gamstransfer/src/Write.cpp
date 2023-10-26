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

// struct contains symbol info and missing attributes to 
// populate default values
struct sym_info
{
  std::string name;
  int dim, type;
  bool missing_attributes[5] = {false};
  double default_values[5];
};


void WriteData(gdxHandle_t PGX, sym_info mysym_info, StringVector names,
IntegerVector uel_ids, NumericVector V, std::string elemText, 
DataFrame df, int mode) {
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
  if (mysym_info.type == GMS_DT_VAR || mysym_info.type == GMS_DT_EQU) {
    for (int i =0; i < 5; i++) {
      if (mysym_info.missing_attributes[i]) {
        Values[i] = mysym_info.default_values[i];
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
    if (mysym_info.missing_attributes[GMS_VAL_LEVEL]) {
      Values[GMS_VAL_LEVEL] = mysym_info.default_values[GMS_VAL_LEVEL];
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

void registerUELs(gdxHandle_t gdx, CharacterVector arr, int* uel_id ) {
  int uel_no, N, rc;
  std::string myUEL;
  rc = gdxUELRegisterStrStart(gdx);
  if (!rc) stop("CPP_gdxWriteSuper:gdxUELRegisterStrStart GDX error (gdxUELRegisterStrStart)");

  N = arr.length();
  for (int i = 0; i < N; i++) {
    myUEL = arr[i];
    rc = gdxUELRegisterStr(gdx, myUEL.c_str(), &uel_no);
    if (!rc) stop("Error registering UEL: %s", myUEL);

    if (uel_id) {
      uel_id[i] = uel_no;
    }
  }
  if (!gdxUELRegisterDone(gdx))
    stop("CPP_gdxWriteSuper:gdxUELRegisterDone GDX error (gdxUELRegisterDone)");

}

// [[Rcpp::export]]
void CPP_gdxWriteSuper(Environment container, LogicalVector enable,
CharacterVector fileName, Nullable<CharacterVector> uel_priority_,
bool compress, int mode) {
  Environment data_dict = container["data"];
  Function as_list = data_dict["as_list"];
  List data = as_list();

  CharacterVector sysDir = container["systemDirectory"];
  std::string myUEL;
  std::string mysysDir = Rcpp::as<std::string>(sysDir);
  std::string myFileName = Rcpp::as<std::string>(fileName);
  char        Msg[GMS_SSSIZE];
  int         ErrNr, rc, varType, varSubType;
  gdxStrIndexPtrs_t domains_ptr;
  gdxStrIndex_t domains;
  GDXSTRINDEXPTRS_INIT(domains, domains_ptr);

  gt_gdx gdxobj;
  gdxobj.init_library(mysysDir.c_str());
  /* open */
  if (!compress) {
    rc = gdxOpenWrite(gdxobj.gdx, myFileName.c_str(), "GAMS Transfer", &ErrNr);
    if (!rc) stop("CPP_gdxWriteSuper:gdxOpenWrite Error opening the file %s with error code %i", myFileName, ErrNr);
  }
  else {
    rc = gdxOpenWriteEx(gdxobj.gdx, myFileName.c_str(), "GAMS Transfer", 1, &ErrNr);
    if (!rc) stop("CPP_gdxWriteSuper:gdxOpenWriteEx Error opening the file %s with error code %i", myFileName, ErrNr);
  }

  gdxSVals_t sVals;
  gdxGetSpecialValues(gdxobj.gdx, sVals);

  sVals[GMS_SVIDX_NA] = NA_REAL;
  sVals[GMS_SVIDX_EPS] = -0.0;
  sVals[GMS_SVIDX_UNDEF] = R_NaN;
  sVals[GMS_SVIDX_PINF] = R_PosInf;
  sVals[GMS_SVIDX_MINF] = R_NegInf;
  rc = gdxSetSpecialValues(gdxobj.gdx, sVals);
  if (!rc) stop("CPP_gdxWriteSuper:gdxSetSpecialValues GDX error (gdxSetSpecialValues)");
  // register UELs
  int UELno;
  rc = gdxUELRegisterStrStart(gdxobj.gdx);
  if (!rc) stop("CPP_gdxWriteSuper:gdxUELRegisterStrStart GDX error (gdxUELRegisterStrStart)");
  if (uel_priority_.isNotNull()) {
    CharacterVector uel_priority(uel_priority_);

    for (int i = 0; i < uel_priority.length(); i++) {
      myUEL = uel_priority(i);
      rc = gdxUELRegisterStr(gdxobj.gdx, myUEL.c_str(), &UELno);
      if (!rc) stop("Error registering UEL: %s", myUEL);
    }
  }
  if (!gdxUELRegisterDone(gdxobj.gdx))
    stop("CPP_gdxWriteSuper:gdxUELRegisterDone GDX error (gdxUELRegisterDone)");
  DataFrame df;
  List domain;
  int Dim, sym_nr;
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
    sym_info mysym_info;
    mysym_info.name = mysym;
    mysym_info.type = varType;

    if (varType == GMS_DT_ALIAS) {
      bool isUniverseAlias = symname[".isUniverseAlias"];

      if (isUniverseAlias == true) {
        std::string alias_with = symname["aliasWith"];

        if (!gdxAddAlias(gdxobj.gdx, mysym.c_str(), alias_with.c_str()))
        stop("CPP_gdxWriteSuper:gdxAddAlias GDX error (gdxAddAlias)");
        continue;
      }
      else {
        Environment alias_with_env = symname["aliasWith"];
        std::string alias_with = alias_with_env["name"];

        if (!gdxAddAlias(gdxobj.gdx, mysym.c_str(), alias_with.c_str()))
        stop("CPP_gdxWriteSuper:gdxAddAlias GDX error (gdxAddAlias)");
        continue;
      }

    }
    Dim = symname["dimension"];
    StringVector names(Dim);
    int** uel_map = new int*[Dim];
    IntegerVector uel_ids(Dim);
    IntegerVector tempcol;
    bool df_is_null;
    df_is_null = false;
    if (Rf_isNull(symname["records"])) {
      df_is_null =  true;
    }
    df = symname["records"];
    int ncols =0;
    int nrows;
    std::string expltxt;
    List domainstr;
    if (Dim != 0) {
      domainstr = symname["domainNames"];
    }
    expltxt = Rcpp::as<std::string>(symname["description"]);
    domain = symname["domain"];
    mysym_info.dim = Dim;
    if (!df_is_null) {
      nrows = df.nrows();
      ncols = df.size();

      // register UELs
      CharacterVector dom_col_string(nrows);
      IntegerVector tempcol(nrows);
      for (int d = 0; d < Dim; d++) {
        tempcol = df[d];
        dom_col_string = tempcol.attr("levels");
        if (mode == 1) {
          registerUELs(gdxobj.gdx, dom_col_string, NULL);
        }
        else {
          // mapped mode
          uel_map[d] = new int[dom_col_string.length()];
          registerUELs(gdxobj.gdx, dom_col_string, uel_map[d]);
        }
      }
    }
    if (mode == 1) {
      if (!gdxDataWriteStrStart(gdxobj.gdx, mysym.c_str(), 
      expltxt.c_str(), Dim, varType, varSubType))
      stop("CPP_gdxWriteSuper:gdxDataWriteStrStart GDX error (gdxDataWriteStrStart)");
    }
    else {
      if (!gdxDataWriteMapStart(gdxobj.gdx, mysym.c_str(), 
      expltxt.c_str(), Dim, varType, varSubType))
      stop("CPP_gdxWriteSuper:gdxDataWriteMapStart GDX error (gdxDataWriteMapStart)");
    }
    for (int D=0; D < Dim; D++) {
      strcpy(domains_ptr[D], domainstr[D]);
    }
    std::string domaintype = symname["domainType"];
    if (domaintype == "regular") {
      rc = gdxSymbolSetDomain(gdxobj.gdx, (const char **)domains_ptr);
      if (!rc) {
        gdxGetLastError(gdxobj.gdx); // clears last error
        rc = gdxSymbolSetDomainX(gdxobj.gdx, sym_nr, (const char **)domains_ptr);
        if (!rc) {
          gdxErrorStr(gdxobj.gdx, gdxGetLastError(gdxobj.gdx), Msg);
          stop("CPP_gdxWriteSuper:gdxSymbolSetDomain GDX error: %s",Msg);
        }
      }
    }
    else if (domaintype == "relaxed") {
      rc = gdxSymbolSetDomainX(gdxobj.gdx, sym_nr, (const char **)domains_ptr);
      if (!rc) {
        gdxErrorStr(gdxobj.gdx, gdxGetLastError(gdxobj.gdx), Msg);
        stop("CPP_gdxWriteSuper:gdxSymbolSetDomainX GDX error: %s",Msg);
      }
    }
    if (df_is_null) {
      if (!gdxDataWriteDone(gdxobj.gdx)) stop("CPP_gdxWriteSuper:gdxDataWriteDone GDX error (gdxDataWriteDone)");
      delete[] uel_map;
    continue;
    }
    int n_attr;
    if (varType == GMS_DT_PAR) {
      n_attr = 1;
    }
    else {
      n_attr = 5;
    }

    NumericMatrix rec_vals(nrows, n_attr);

    // get default values
    if (varType != GMS_DT_SET) {
      List default_values = symname["defaultValues"];
      for (int d = 0; d < n_attr; d++) {
         mysym_info.default_values[d] = default_values[d];
      }
    }
    if (nrows == 0) {
      if (Dim != 0 || varType == GMS_DT_SET) {
        if (!gdxDataWriteDone(gdxobj.gdx)) stop("CPP_gdxWriteSuper:gdxDataWriteDone GDX error (gdxDataWriteDone)");
      }
      else {
        NumericVector dummyvec;
        IntegerVector dummyintvec;
        // for scalars, write the default values
        // no attribute column. Fill all values with default
        if (varType == GMS_DT_PAR) {
          mysym_info.missing_attributes[GMS_VAL_LEVEL] = true;
        }
        else {
          for (int d = 0; d < n_attr; d++) {
            mysym_info.missing_attributes[d] = true;
          }
        }

        WriteData(gdxobj.gdx, mysym_info, "", dummyintvec, dummyvec, "", df, mode);

        if (!gdxDataWriteDone(gdxobj.gdx)) stop("CPP_gdxWriteSuper:gdxDataWriteDone GDX error (gdxDataWriteDone)");
      }
    }
    else {
      StringMatrix rec_domain_str(nrows, Dim);
      IntegerMatrix rec_domain_int(nrows, Dim);
      StringVector tempcol_str(nrows);
      if (mode == 1) {
        for (int d = 0; d < Dim; d++) {
          tempcol_str = df[d];
          rec_domain_str(_, d) = tempcol_str;
        }
      }
      else {
        for (int d = 0; d < Dim; d++) {
          // rec_domain_int(_, d) = IntegerVector(df[d]);
          tempcol = df[d];
          rec_domain_int(_, d) = tempcol;
        }
      }
      StringVector elem_text(nrows);
      std::string text;
      if (varType == GMS_DT_SET) {
        if (df.length() == Dim + 1) {
          elem_text = df[Dim];
        }
        else {
          elem_text = "";
        }
      }
      else {
        NumericVector temp_num_col(nrows);
        if (ncols > Dim) {
          // one or more attribute columns
          // for parameters this is enough to say all attributes are present
          if (ncols - Dim == n_attr) {
            // all attribute columns are present
            for (int d = Dim; d < ncols; d++) {
              temp_num_col = df[d];
              rec_vals(_, d-Dim) = temp_num_col;
            }
          }
          else {
            // some attribute columns are missing
            // can only happen for variables and equations
            CharacterVector colnames = df.names();
            std::string attributes[5] = {"level", "marginal", "lower", "upper", "scale"};

            std::vector<std::string> colnames_vec(colnames.size());

            for (int i = 0; i < colnames.size(); i++){
                colnames_vec[i] = colnames(i);
            }

            int rec_val_column_count = 0;
            for (int i =0; i < 5; i++) {
              std::string attr = attributes[i];

              if ( std::any_of(colnames_vec.begin(), colnames_vec.end(), [attr](std::string i){return i==attr;}) ) {
                temp_num_col = df[attr];
                rec_vals(_, rec_val_column_count) = temp_num_col;
                rec_val_column_count++;
              }
              else {
                mysym_info.missing_attributes[i] = true;
              }
            }
          }
        }
        else {
          // no attribute column. Fill all values with default
          if (varType == GMS_DT_PAR) {
            mysym_info.missing_attributes[GMS_VAL_LEVEL] = true;
          }
          else {
            for (int d = 0; d < n_attr; d++) {
              mysym_info.missing_attributes[d] = true;
            }
          }
        }
      }
      int rel_id;
      for (int i =0; i < nrows; i++) {
        if (mode != 1) {
          for (int d =0; d< Dim; d++) {
            rel_id = rec_domain_int(i, d) - 1;
            uel_ids[d] = uel_map[d][rel_id];
          }
        }
        else {
          names = rec_domain_str(i, _);
        }
        if (varType != GMS_DT_SET){
          if (mode == 1) {
            WriteData(gdxobj.gdx, mysym_info, names, 0, rec_vals(i, _), "", df, mode);
          }
          else {

            WriteData(gdxobj.gdx, mysym_info, "", uel_ids, rec_vals(i, _), "",df, mode);
          }
        }
        else {
          NumericVector zero_vec = {0};
          if (df.length() == Dim + 1) {
            text = Rcpp::as<std::string>(elem_text[i]);
          }
          else {
            text = "";
          }
          if (mode == 1) {
            WriteData(gdxobj.gdx, mysym_info, names, 0, zero_vec, text, df, mode);
          }
          else {
            WriteData(gdxobj.gdx, mysym_info, "", uel_ids, zero_vec, text, df, mode);
          }
        }
      }
      if (!gdxDataWriteDone(gdxobj.gdx)) stop("CPP_gdxWriteSuper:gdxDataWriteDone GDX error (gdxDataWriteDone)");
    }
    if (mode != 1) {
      for (int d =0; d < Dim; d++) {
        delete[] uel_map[d];
      }
    }
    delete[] uel_map;

  // get the error count
  if (gdxDataErrorCount(gdxobj.gdx)) {
      gdxErrorStr(gdxobj.gdx, gdxGetLastError(gdxobj.gdx), Msg);
      stop("CPP_gdxWriteSuper:gdxError GDX error for %s: %s", mysym, Msg);
  }
  }

  gdxAutoConvert(gdxobj.gdx, 0);
  return;
}
