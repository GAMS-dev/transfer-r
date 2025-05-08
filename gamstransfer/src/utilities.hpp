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

#include "gclgms.h"
using namespace Rcpp;
#include "gdx.hpp"

class gt_gdx
{
  public:
    std::string msg;
    gdx::TGXFileObj gdx {msg};
    gt_gdx() {
      if( !msg.empty() )
        stop( "CPP_readSuper:Could not load GDX library." );
    }
};

// struct contains symbol info and missing attributes to
// populate default values
class sym_info
{
  public:
    std::string name;
    int dim {}, type {}, sym_nr {}, subtype {};
    std::string* domain {};
    std::string description, domain_type;
    DataFrame* records {};

    ~sym_info() {
        delete[] domain;
    }
};

static const char* const gmsEquTypeText[GMS_EQUTYPE_MAX] = {
   "eq",
   "geq",
   "leq",
   "nonbinding",
   "external",
   "cone",
   "boolean"
};

const std::map<std::string, int> symTypeText_to_int
{  {"Set", 0},
   {"Parameter", 1},
   {"Variable", 2},
   {"Equation", 3},
   {"Alias", 4},
   {"UniverseAlias", 4}
};

const std::map<std::string, int> varTypeText_to_int
{{"unknown", 0}, {"binary", 1}, {"integer", 2},
{"positive", 3}, {"negative", 4},{"free", 5},
{"sos1", 6}, {"sos2", 7},{"semicont", 8},
{"semiint", 9}
};

const std::map<std::string, int> equTypeText_to_int
{{"eq", 0}, {"geq", 1}, {"leq", 2},
{"nonbinding", 3}, {"external", 4},{"cone", 5},
{"boolean", 6}};

void gt_set_special_values(gdx::TGXFileObj &);
