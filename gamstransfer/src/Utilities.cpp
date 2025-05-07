// GAMS - General Algebraic Modeling System R API
//
// Copyright (c) 2017-2025 GAMS Software GmbH <support@gams.com>
// Copyright (c) 2017-2025 GAMS Development Corp. <support@gams.com>
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
#include "gclgms.h"
#include "utilities.hpp"
using namespace Rcpp;

// [[Rcpp::export(.CPP_getMaxDim)]]
IntegerVector CPP_getMaxDim() {
  IntegerVector v {GMS_MAX_INDEX_DIM};
  return(v);
}

void gt_set_special_values(gdx::TGXFileObj & gdx) {
  gdxSVals_t sVals;
  gdx.gdxGetSpecialValues(sVals);
  int rc;

  sVals[GMS_SVIDX_NA] = NA_REAL;
  sVals[GMS_SVIDX_EPS] = -0.0;
  sVals[GMS_SVIDX_UNDEF] = R_NaN;
  sVals[GMS_SVIDX_PINF] = R_PosInf;
  sVals[GMS_SVIDX_MINF] = R_NegInf;

  rc = gdx.gdxSetSpecialValues(sVals);
  if (!rc) stop("gt_set_special_values:gdxSetSpecialValues GDX error (gdxSetSpecialValues)");
  return;
}
