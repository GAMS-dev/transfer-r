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
