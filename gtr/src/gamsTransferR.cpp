
#include <Rcpp.h>
#include "gdxcc.h"
#include "gclgms.h"
using namespace Rcpp;
std::string error_message = "Something went wrong. Please "
"contact support@gams.com with a minimal reproducible example.";

void WriteData(gdxHandle_t PGX, StringVector s,
std::vector<double> V, int VarType, int Dim,
std::string elemText) {

  gdxStrIndexPtrs_t Indx;
  gdxStrIndex_t Indx_labels;
  gdxValues_t       Values;
	int rc;
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
      if (VarType == GMS_DT_SET) {
        gdxAddSetText(PGX, elemText.c_str(), &txtnr);
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
  if (!rc) stop("error calling gdxDataWriteStr");
  return;
}



// [[Rcpp::export]]
List getSpecialValues(CharacterVector gdxName, CharacterVector sysDir) {
gdxHandle_t PGX = NULL;
char Msg[GMS_SSSIZE];
gdxSVals_t sVals;
int rc;
std::string mysysDir = Rcpp::as<std::string>(sysDir);
rc = gdxCreateD(&PGX, mysysDir.c_str(), Msg, sizeof(Msg));
if (!rc) stop("Error creating GDX object: %s", Msg);

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
List checkAcronyms(CharacterVector gdxName, CharacterVector sysDir) {
  gdxHandle_t PGX = NULL;
  char Msg[GMS_SSSIZE], acrName[GMS_SSSIZE];
  std::string myname = Rcpp::as<std::string>(gdxName);
  std::string mysysDir = Rcpp::as<std::string>(sysDir);
  List L;
  int rc, errCode;

  rc = gdxCreateD(&PGX, mysysDir.c_str(), Msg, sizeof(Msg));
  if (!rc) stop("Error creating GDX object: %s", Msg);

  gdxOpenRead(PGX, myname.c_str(), &errCode);

  // check acronyms
  int nAcronym;
  nAcronym = gdxAcronymCount(PGX);
  if (nAcronym > 0) {
    Rcout << "warning! Acronyms are not supported!" << "\n";
    int acrID;
    std::vector<int> acronyms;
    for (int i=0; i < nAcronym; i++){
      gdxAcronymGetInfo(PGX, i+1, acrName, Msg, &acrID);
      acronyms.push_back(acrID);
    }
    L = List::create(_["nAcronyms"]=nAcronym, _["acronyms"]=acronyms);
  }
  else {
    L = List::create(_["nAcronyms"]=nAcronym);
  }
  if (gdxClose(PGX)) stop("Error in closing GDX file");
  return L;

}

// [[Rcpp::export]]
List getSymbols(CharacterVector gdxName, CharacterVector sysDir) {
  gdxHandle_t PGX = NULL;
  std::vector<std::string> domain;
  int rc, errCode, symCount, UelCount, sym_dimension, sym_type, nrecs, dummy,
  subtype;
  char symbolID[GMS_SSSIZE],aliasForID[GMS_SSSIZE],
   explText[GMS_SSSIZE], Msg[GMS_SSSIZE];
  std::string myname = Rcpp::as<std::string>(gdxName);
  std::string mysysDir = Rcpp::as<std::string>(sysDir);
	gdxStrIndexPtrs_t domains_ptr;
	gdxStrIndex_t domains;

	GDXSTRINDEXPTRS_INIT(domains, domains_ptr);

  rc = gdxCreateD(&PGX, mysysDir.c_str(), Msg, sizeof(Msg));
  if (!rc) stop("Error creating GDX object: %s", Msg);
  gdxOpenRead(PGX, myname.c_str(), &errCode);

  gdxSystemInfo(PGX, &symCount, &UelCount);

  List templist, L1;
	for (int i=0; i <= symCount; i++) {
		gdxSymbolInfo(PGX, i, symbolID, &sym_dimension, &sym_type);
    if (strcmp(symbolID, "*") == 0) {
      continue;
    }
    gdxSymbolInfoX(PGX, i, &nrecs, &subtype, explText);

      rc = gdxSymbolGetDomainX(PGX, i, domains_ptr);
      if (!rc) stop("Error calling gdxSymbolGetDomainX");
      for (int j=0; j < sym_dimension; j++) {
      domain.push_back(domains_ptr[j]);
		  }

      if (sym_type == GMS_DT_ALIAS) {
        gdxSymbolInfo(PGX, subtype, aliasForID, &sym_dimension, &dummy);
        templist = List::create(_["name"] = symbolID, _["type"] = sym_type,
         _["aliasfor"] = aliasForID);
      }
      else {
        templist = List::create(_["name"] = symbolID, _["type"] = sym_type, 
        _["dimensions"]=sym_dimension, _["domain"]=domain, _["subtype"] = subtype,
        _["expltext"]=explText);
      }

    domain.clear();

    L1.push_back(templist);
  }

  return L1;
}

// [[Rcpp::export]]
void gdxWriteSuper(List data, CharacterVector sysDir, 
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
  if (!rc) stop("Error creating GDX object: %s", Msg);

  gdxSVals_t sVals;
  gdxGetSpecialValues(PGX, sVals);

  sVals[GMS_SVIDX_NA] = NA_REAL;
  sVals[GMS_SVIDX_EPS] = -0.0;
  sVals[GMS_SVIDX_UNDEF] = R_NaN;
  sVals[GMS_SVIDX_PINF] = R_PosInf;
  sVals[GMS_SVIDX_MINF] = R_NegInf;

  rc = gdxSetSpecialValues(PGX, sVals);

	gdxGetDLLVersion(PGX, Msg);

	/* Write demand data */
  if (!compress) {
    rc = gdxOpenWrite(PGX, myFileName.c_str(), "GAMS Transfer", &ErrNr);
    if (!rc) stop("Error opening the file %s with error code %i", myFileName, ErrNr);
  }
  else {
    rc = gdxOpenWriteEx(PGX, myFileName.c_str(), "GAMS Transfer", 1, &ErrNr);
    if (!rc) stop("Error opening the file %s with error code %i", myFileName, ErrNr);
  }

  // register UELs
  int UELno;
  if (is_uel_priority) {
    rc = gdxUELRegisterStrStart(PGX);
    if (!rc) stop("Error in gdxUELRegisterStrStart.");
    for (int i = 0; i < uel_priority.length(); i++) {
      myUEL = uel_priority(i);
      rc = gdxUELRegisterStr(PGX, myUEL.c_str(), &UELno);
      if (!rc) stop("Error registering UEL: %s", myUEL);
    }
    gdxUELRegisterDone(PGX);
  }

  DataFrame df;
  List domain;
  int Dim;
  std::vector<double> values;
  std::string elemText;
  StringVector colString, colElemText;
  NumericVector colDouble;

  for (int d=0; d < data.length(); d++){
    Environment symname = data[d];
    std::string mysym = symname["name"];
    varType = symname[".gams_type"];
    varSubType = symname[".gams_subtype"];

    if (varType == GMS_DT_ALIAS) {
      Environment alias_with_env = symname["aliasWith"];
      std::string alias_with = alias_with_env["name"];
      if (!gdxAddAlias(PGX, mysym.c_str(), alias_with.c_str()))
      stop("Error in gdxAddAlias: cannot add alias");
      continue;
    }
    Dim = symname["dimension"];
    StringVector names(Dim);
    df = symname["records"];
    domain = symname["domainstr"];

    domain = symname["domain"];
    List domainstr;
    if (Dim != 0) {
      domainstr = symname["domain_names"];
      std::string blah = domainstr[0];
    }
    std::string expltxt = symname["description"];
    if (!gdxDataWriteStrStart(PGX, mysym.c_str(), 
    expltxt.c_str(), Dim, varType, varSubType))
    stop("Error in gdxDataWriteStrStart");

    for (int D=0; D < Dim; D++) {
      strcpy(domains_ptr[D], domainstr[D]);
    }

    gdxSymbolSetDomain(PGX, (const char **)domains_ptr);
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
        WriteData(PGX, names, values, varType, Dim, elemText);
      }
      else {
        WriteData(PGX, names, values, varType, Dim, elemText);
      }
      elemText.clear();
      values.clear();
    }

    if (!gdxDataWriteDone(PGX)) stop("Error finishing the write operation with gdxDataWriteDone");
  }

  gdxAutoConvert(PGX, 0);
  if (gdxClose(PGX)) stop("Error in closing GDX file");
  return;
}

// [[Rcpp::export]]
List readSymbols(CharacterVector symNames, CharacterVector gdxName,
                CharacterVector sysDir) {
  gdxHandle_t PGX = NULL;
	char        Msg[GMS_SSSIZE], Producer[GMS_SSSIZE];
	int         ErrNr, VarNr, NrRecs, N, Dim, VarType, D, rc, iDummy;
  char symbolID[GMS_SSSIZE];

  std::string mysymName;
  std::string mygdxName = Rcpp::as<std::string>(gdxName);
  std::string mysysDir = Rcpp::as<std::string>(sysDir);
  gdxStrIndexPtrs_t Indx;

  gdxStrIndex_t Indx_labels;
  gdxValues_t       Values;
  GDXSTRINDEXPTRS_INIT(Indx_labels, Indx);

  std::vector<double> levels, marginals, lower, upper, scale;
  std::vector<std::string> domain, index, elemText;
  std::vector<std::vector<std::string>> indices;
  gdxStrIndexPtrs_t domains_ptr;
	gdxStrIndex_t domains;
	GDXSTRINDEXPTRS_INIT(domains, domains_ptr);

  // find symbol in the gdx
  rc = gdxCreateD(&PGX, mysysDir.c_str(), Msg, sizeof(Msg));
  if (!rc) stop("Error creating GDX object: %s", Msg);

  gdxOpenRead(PGX, mygdxName.c_str(), &ErrNr);
  if (ErrNr) stop("Error in gdxOpenRead with error code %i", ErrNr);

  gdxFileVersion(PGX, Msg, Producer);

  gdxSVals_t sVals;
  gdxGetSpecialValues(PGX, sVals);

  sVals[GMS_SVIDX_NA] = NA_REAL;
  sVals[GMS_SVIDX_EPS] = -0.0;
  sVals[GMS_SVIDX_UNDEF] = R_NaN;
  sVals[GMS_SVIDX_PINF] = R_PosInf;
  sVals[GMS_SVIDX_MINF] = R_NegInf;

  rc = gdxSetSpecialValues(PGX, sVals);

  List L = List::create();
  for(int symcount=0; symcount < symNames.size(); symcount++) {
    mysymName = symNames(symcount);

		if (!gdxFindSymbol(PGX, mysymName.c_str(), &VarNr)) {
			stop("Could not find variable %s", mysymName);
		}

    gdxSymbolInfo(PGX, VarNr, symbolID, &Dim, &VarType);
    if (VarType == GMS_DT_ALIAS) continue;

    rc = gdxSymbolGetDomainX(PGX, VarNr, domains_ptr);
    for (int j=0; j < Dim; j++) {
      domain.push_back(domains_ptr[j]);
    }

    if (!gdxDataReadStrStart(PGX, VarNr, &NrRecs)) {
      stop("Error in gdxDataReadStrStart");
    }

    if (NrRecs == 0) {
      List L1 = List::create(Named("names")=mysymName, _["records"]=R_NilValue);
      L.push_back(L1);
    }
    else {
      while (gdxDataReadStr(PGX, Indx, Values, &N)) {
        if (VarType == GMS_DT_SET || VarType == GMS_DT_PAR) {
          if (VarType == GMS_DT_SET){
            rc = gdxGetElemText(PGX, Values[GMS_VAL_LEVEL], Msg, &iDummy);
            if (rc != 0) {
              elemText.push_back(Msg);
            }
            else {
              elemText.push_back("");
            }
          } else {
            levels.push_back(Values[GMS_VAL_LEVEL]);
          }
          for (D = 0; D < Dim; D++) {
            index.push_back(Indx[D]);
          }
        }
        else if (VarType == GMS_DT_VAR || VarType == GMS_DT_EQU) {
          levels.push_back(Values[GMS_VAL_LEVEL]);
          marginals.push_back(Values[GMS_VAL_MARGINAL]);
          upper.push_back(Values[GMS_VAL_UPPER]);
          lower.push_back(Values[GMS_VAL_LOWER]);
          scale.push_back(Values[GMS_VAL_SCALE]);
          for (D = 0; D < Dim; D++) {
            index.push_back(Indx[D]);
          }
        }
        indices.push_back(index);
        index.clear();
      }
      DataFrame df;
      std::vector<std::string> index_columns;
      for (D = 0; D < Dim; D++) {
        for (int i=0; i < NrRecs; i++){
          index_columns.push_back(indices[i][D]);
        }
        if (strcmp(domain[D].c_str(), "*")== 0) {
          df["uni_"+std::to_string(D)] = index_columns;
        }
        else {
          df[domain[D] + "_" + std::to_string(D)] = index_columns;
        }
        index_columns.clear();
      }
      indices.clear();
      if (VarType == GMS_DT_VAR || VarType == GMS_DT_EQU) {
        df["level"] = levels;
        df["marginal"] = marginals;
        df["lower"] = lower;
        df["upper"] = upper;
        df["scale"] = scale;
      }
      else {
        if (VarType == GMS_DT_PAR) {
          df["values"] = levels;
        } else {
          df["element_text"] = elemText;
        }
      }
      elemText.clear();
      levels.clear();
      marginals.clear();
      upper.clear();
      lower.clear();
      scale.clear();
      domain.clear();
      // copy end

      List L1 = List::create(Named("names")=mysymName, _["records"]=df);
      L.push_back(L1);
    }
  }
  gdxDataReadDone(PGX);
  if (gdxClose(PGX)) stop("Error in closing GDX file");
  return L;
}