
#include <Rcpp.h>
#include "gdxcc.h"
#include "gclgms.h"
using namespace Rcpp;

// void WriteDataSet(gdxHandle_t PGX, StringVector s, 
// std::vector<std::string> V, int VarType, int Dim) {
//   gdxStrIndexPtrs_t Indx;
//   gdxStrIndex_t Indx_labels;
//   gdxValues_t       Values;
// 	int rc;
//   GDXSTRINDEXPTRS_INIT(Indx_labels, Indx);
//   for (int D=0; D < Dim; D++) {
//     strcpy(Indx[D], s[D]);
//   }
// 	Values[GMS_VAL_LEVEL] = V[GMS_VAL_LEVEL];

// 	rc = gdxDataWriteStr(PGX, (const char **)Indx, Values);
//   Rcout << "Index: " << Indx[0] << "\n";
//   // Rcout << "values: " << V << "\n";
// 	Rcout << "return code: "<< rc << "\n";
//   // gdxDataWriteStr(PGX, (const char **)Indx, Values);
//   return;
// }
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
  else {
    Values[GMS_VAL_LEVEL] = 1;
    Values[GMS_VAL_MARGINAL] = 0;
    Values[GMS_VAL_UPPER] = 0;
    Values[GMS_VAL_LOWER] = 0;
    Values[GMS_VAL_SCALE] = 0;
  }
	rc = gdxDataWriteStr(PGX, (const char **)Indx, Values);
  
  int txtnr;
  gdxAddSetText(PGX, elemText.c_str(), &txtnr);

  // gdxDataWriteStr(PGX, (const char **)Indx, Values);
  return;
}



// [[Rcpp::export]]
void gdxReadTrial() {
  gdxHandle_t gdxHandle;
  int rc;
  char        Msg[GMS_SSSIZE];
  rc = gdxCreate(&gdxHandle, Msg, sizeof(Msg));
  Rprintf("The return code: %i \n", rc);
}

// [[Rcpp::export]]
StringVector setList(CharacterVector gdxName) {
  gdxHandle_t PGX = NULL;
  StringVector sets;
  int rc, errCode, symCount, UelCount, sym_dimension, sym_type;
  char mygdx[GMS_SSSIZE] , symbolID[GMS_SSSIZE], Msg[GMS_SSSIZE];
  std::string myname = Rcpp::as<std::string>(gdxName);
  Rcout << myname << "\n";

  rc = gdxCreateD(&PGX,"/opt/gams/gams35.1_linux_x64_64_sfx", Msg, sizeof(Msg));
  gdxOpenRead(PGX, myname.c_str(), &errCode);

  rc = gdxSystemInfo(PGX, &symCount, &UelCount);

	for (int i=0; i < symCount; i++){
		gdxSymbolInfo(PGX, i, symbolID, &sym_dimension, &sym_type);
		if (sym_type == GMS_DT_SET){
      sets.push_back(symbolID);
		}
	}

  return sets;
}

// [[Rcpp::export]]
StringVector parameterList(CharacterVector gdxName) {

  gdxHandle_t PGX = NULL;
  StringVector parameters;
  int rc, errCode, symCount, UelCount, sym_dimension, sym_type;
  char mygdx[GMS_SSSIZE] , symbolID[GMS_SSSIZE], Msg[GMS_SSSIZE];
  std::string myname = Rcpp::as<std::string>(gdxName);

  rc = gdxCreateD(&PGX,"/opt/gams/gams35.1_linux_x64_64_sfx", Msg, sizeof(Msg));
  gdxOpenRead(PGX, myname.c_str(), &errCode);

  rc = gdxSystemInfo(PGX, &symCount, &UelCount);

	for (int i=0; i < symCount; i++){
		gdxSymbolInfo(PGX, i, symbolID, &sym_dimension, &sym_type);
		if (sym_type == GMS_DT_PAR){
      parameters.push_back(symbolID);
		}
	}

  return parameters;
}

// [[Rcpp::export]]
List getParameters(CharacterVector gdxName) {
  gdxHandle_t PGX = NULL;
  StringVector parameters;
  List domainslist;
  StringVector domain;
  IntegerVector dimensions;
  int rc, errCode, symCount, UelCount, sym_dimension, sym_type;
  char mygdx[GMS_SSSIZE] , symbolID[GMS_SSSIZE], Msg[GMS_SSSIZE];
  std::string myname = Rcpp::as<std::string>(gdxName);

	gdxStrIndexPtrs_t domains_ptr;
	gdxStrIndex_t domains;

	GDXSTRINDEXPTRS_INIT(domains, domains_ptr);

  rc = gdxCreateD(&PGX,"/opt/gams/gams35.1_linux_x64_64_sfx", Msg, sizeof(Msg));
  gdxOpenRead(PGX, myname.c_str(), &errCode);

  rc = gdxSystemInfo(PGX, &symCount, &UelCount);

	for (int i=0; i < symCount; i++){
		gdxSymbolInfo(PGX, i, symbolID, &sym_dimension, &sym_type);
		if (sym_type == GMS_DT_PAR){
      parameters.push_back(symbolID);
      dimensions.push_back(sym_dimension);

      rc = gdxSymbolGetDomainX(PGX, i, domains_ptr);
      for (int j=0; j < sym_dimension; j++) {
      domain.push_back(domains_ptr[j]);
		  }
      domainslist.push_back(domain);
      domain.erase(domain.begin(), domain.end());
		}

	}
  List L = List::create(Named("names")=parameters, _["dimensions"]=dimensions, _["domain"]=domainslist);

  return L;
}

// void setSpecialValues(gdxHandle_t *PGX, CharacterVector gdxName, CharacterVector sysDir) {
// char Msg[GMS_SSSIZE];
// int rc;
// std::string mysysDir = Rcpp::as<std::string>(sysDir);
// double sVals[5];

// sVals[GMS_SVIDX_NA] = NA_REAL;
// sVals[GMS_SVIDX_EPS] = -0.0;
// sVals[GMS_SVIDX_UNDEF] = R_NaN;
// sVals[GMS_SVIDX_PINF] = R_PosInf;
// sVals[GMS_SVIDX_MINF] = R_NegInf;

// rc = gdxSetSpecialValues(PGX, sVals);
// Rcout << "special values set: "<< rc <<  "\n";
// return;
// }

// void setSpecialValues(CharacterVector gdxName, CharacterVector sysDir) {
// gdxHandle_t PGX = NULL;
// char Msg[GMS_SSSIZE];
// // gdxSVals_t sVals;
// int rc;
// std::string mysysDir = Rcpp::as<std::string>(sysDir);
// double sVals[5];

// sVals[GMS_SVIDX_NA] = NA_REAL;
// sVals[GMS_SVIDX_EPS] = -0.0;
// sVals[GMS_SVIDX_UNDEF] = R_NaN;
// sVals[GMS_SVIDX_PINF] = R_PosInf;
// sVals[GMS_SVIDX_MINF] = R_NegInf;

// rc = gdxCreateD(&PGX, mysysDir.c_str(), Msg, sizeof(Msg));

// rc = gdxSetSpecialValues(PGX, sVals);
// Rcout << "special values set: "<< rc <<  "\n";
// return;
// }

// [[Rcpp::export]]
List getSpecialValues(CharacterVector gdxName, CharacterVector sysDir) {
gdxHandle_t PGX = NULL;
char Msg[GMS_SSSIZE];
gdxSVals_t sVals;
int rc;
std::string mysysDir = Rcpp::as<std::string>(sysDir);
rc = gdxCreateD(&PGX, mysysDir.c_str(), Msg, sizeof(Msg));

gdxGetSpecialValues (PGX, sVals);
List L = List::create(
_["NA"] = sVals[GMS_SVIDX_UNDEF],
_["EPS"] = sVals[GMS_SVIDX_EPS],
_["UNDEF"] = sVals[GMS_SVIDX_UNDEF],
_["POSINF"] = sVals[GMS_SVIDX_PINF],
_["NEGINF"]= sVals[GMS_SVIDX_MINF]
);
return L;
}
// [[Rcpp::export]]
List getSymbols(CharacterVector gdxName, CharacterVector sysDir) {
  gdxHandle_t PGX = NULL;
  std::vector<std::string> domain;
  int rc, errCode, symCount, UelCount, sym_dimension, sym_type, nrecs,
  subtype;
  char symbolID[GMS_SSSIZE], Msg[GMS_SSSIZE], explText[GMS_SSSIZE];
  std::string myname = Rcpp::as<std::string>(gdxName);
  std::string mysysDir = Rcpp::as<std::string>(sysDir);
	gdxStrIndexPtrs_t domains_ptr;
	gdxStrIndex_t domains;

	GDXSTRINDEXPTRS_INIT(domains, domains_ptr);

  rc = gdxCreateD(&PGX, mysysDir.c_str(), Msg, sizeof(Msg));
  gdxOpenRead(PGX, myname.c_str(), &errCode);

  rc = gdxSystemInfo(PGX, &symCount, &UelCount);
  List templist, L1;
	for (int i=0; i < symCount; i++){
		gdxSymbolInfo(PGX, i, symbolID, &sym_dimension, &sym_type);
    gdxSymbolInfoX(PGX, i, &nrecs, &subtype, explText);

      rc = gdxSymbolGetDomainX(PGX, i, domains_ptr);
      for (int j=0; j < sym_dimension; j++) {
      domain.push_back(domains_ptr[j]);
		  }
      templist = List::create(_["name"] = symbolID, _["type"] = sym_type, 
      _["dimensions"]=sym_dimension, _["domain"]=domain, _["subtype"] = subtype,
      _["expltext"]=explText);
      domain.clear();

    if (strcmp(symbolID, "*") != 0) {
    L1.push_back(templist);
    }
  }

  return L1;
}

// [[Rcpp::export]]
void gdxWriteSuper(List data,
CharacterVector sysDir, CharacterVector fileName) {
  std::string mysysDir = Rcpp::as<std::string>(sysDir);
  std::string myFileName = Rcpp::as<std::string>(fileName);

  gdxHandle_t PGX = NULL;
	char        Msg[GMS_SSSIZE];
	int         ErrNr, rc, varType;

	if (!gdxCreateD(&PGX, mysysDir.c_str(), Msg, sizeof(Msg))) {
		Rcout << "**** Could not load GDX library" << "\n" << "**** " << Msg << "\n";
		exit(1);
	}
  gdxSVals_t sVals;
  gdxGetSpecialValues(PGX, sVals);


  sVals[GMS_SVIDX_NA] = NA_REAL;
  sVals[GMS_SVIDX_EPS] = -0.0;
  sVals[GMS_SVIDX_UNDEF] = R_NaN;
  sVals[GMS_SVIDX_PINF] = R_PosInf;
  sVals[GMS_SVIDX_MINF] = R_NegInf;

  rc = gdxSetSpecialValues(PGX, sVals);

	gdxGetDLLVersion(PGX, Msg);
	Rcout << "Using GDX DLL version: " << Msg << "\n";
	/* Write demand data */
	rc = gdxOpenWrite(PGX, myFileName.c_str(), "xp_example1", &ErrNr);

	if (ErrNr) Rcout << "Error1" << "\n";

  for (int d=0; d < data.length(); d++){
    List symname = data[d];
    std::string mysym = symname["gams_name"];
    Rcout << mysym << "\n";
    DataFrame df = symname["records"];
    List domain = symname["domainstr"];

    int Dim = symname["dimension"];

    std::string varTypeStr = symname["type"];


    if (strcmp(varTypeStr.c_str(),"parameter") == 0) {
      varType = GMS_DT_PAR;
    }
    else if (strcmp(varTypeStr.c_str(),"set") == 0) {
      varType = GMS_DT_SET;
    }
    else if (strcmp(varTypeStr.c_str(),"variable") == 0) {
      varType = GMS_DT_VAR;
    }
    else if (strcmp(varTypeStr.c_str(),"equation") == 0) {
      varType = GMS_DT_EQU;
    }
    else {
      Rcout << "unsupported symbol type \n";
    }


    if (!gdxDataWriteStrStart(PGX, mysym.c_str(), 
    "Demand data", Dim, varType, 0))
    Rcout << "Error2" << "\n";

    StringVector colString, colElemText, names(Dim);
    NumericVector colDouble;
    std::vector<double> values;
    std::string elemText;
    gdxStrIndexPtrs_t domains_ptr;
    gdxStrIndex_t domains;
    GDXSTRINDEXPTRS_INIT(domains, domains_ptr);

    for (int D=0; D < Dim; D++) {
      strcpy(domains_ptr[D], domain[D]);
    }

    gdxSymbolSetDomain(PGX, (const char **)domains_ptr);

    for (int i =0; i < df.nrows(); i++) {
      for (int j=0; j < df.size(); j++) {
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
        Rcout << "text: " << elemText << "\n";
        WriteData(PGX, names, values, varType, Dim, elemText);
      }

    }

    if (!gdxDataWriteDone(PGX)) Rcout << "Error3" << "\n";
    Rcout << "Demand data written by example1" << "\n";
  }
  if (gdxClose(PGX)) Rcout << "Error4" << "\n";

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

  gdxOpenRead(PGX, mygdxName.c_str(), &ErrNr);
  if (ErrNr) Rcout << "Error1" << "\n";
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
			Rcout << "**** Could not find variable X" << "\n";
			exit(1);
		}

    gdxSymbolInfo(PGX, VarNr, symbolID, &Dim, &VarType);
    rc = gdxSymbolGetDomainX(PGX, VarNr, domains_ptr);
    for (int j=0; j < Dim; j++) {
      domain.push_back(domains_ptr[j]);
    }
    if (!gdxDataReadStrStart(PGX, VarNr, &NrRecs)) Rcout << "Error2" << "\n";
		while (gdxDataReadStr(PGX, Indx, Values, &N)) {
      if (VarType == GMS_DT_SET || VarType == GMS_DT_PAR){
        if (VarType == GMS_DT_SET){
        gdxGetElemText(PGX, Values[GMS_VAL_LEVEL], Msg, &iDummy);
        elemText.push_back(Msg);
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
        df["uni"] = index_columns;
      }
      else {
        df[domain[D]] = index_columns;
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
  gdxDataReadDone(PGX);
  if (ErrNr = gdxClose(PGX)) Rcout << "Error3" << "\n";
  return L;
}

// [[Rcpp::export]]
DataFrame readSymbol(CharacterVector symName, CharacterVector gdxName,
                CharacterVector sysDir) {
  gdxHandle_t PGX = NULL;
	char        Msg[GMS_SSSIZE], Producer[GMS_SSSIZE];
	int         ErrNr, VarNr, NrRecs, N, Dim, VarType, D, rc, iDummy;
  char symbolID[GMS_SSSIZE];

  std::string mysymName = Rcpp::as<std::string>(symName);
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

  	gdxOpenRead(PGX, mygdxName.c_str(), &ErrNr);
		if (ErrNr) Rcout << "Error1" << "\n";
		gdxFileVersion(PGX, Msg, Producer);

    gdxSVals_t sVals;
    gdxGetSpecialValues(PGX, sVals);


    sVals[GMS_SVIDX_NA] = NA_REAL;
    sVals[GMS_SVIDX_EPS] = -0.0;
    sVals[GMS_SVIDX_UNDEF] = R_NaN;
    sVals[GMS_SVIDX_PINF] = R_PosInf;
    sVals[GMS_SVIDX_MINF] = R_NegInf;

    rc = gdxSetSpecialValues(PGX, sVals);


		if (!gdxFindSymbol(PGX, mysymName.c_str(), &VarNr)) {
			Rcout << "**** Could not find variable X" << "\n";
			exit(1);
		}

    gdxSymbolInfo(PGX, VarNr, symbolID, &Dim, &VarType);
    rc = gdxSymbolGetDomainX(PGX, VarNr, domains_ptr);
    for (int j=0; j < Dim; j++) {
      domain.push_back(domains_ptr[j]);
    }
    if (!gdxDataReadStrStart(PGX, VarNr, &NrRecs)) Rcout << "Error2" << "\n";
		while (gdxDataReadStr(PGX, Indx, Values, &N)) {
      if (VarType == GMS_DT_SET || VarType == GMS_DT_PAR){
        if (VarType == GMS_DT_SET){
        gdxGetElemText(PGX, Values[GMS_VAL_LEVEL], Msg, &iDummy);
        elemText.push_back(Msg);
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
        df["uni"] = index_columns;
      }
      else {
        df[domain[D]] = index_columns;
      }
      index_columns.clear();
    }

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
		gdxDataReadDone(PGX);
    if (ErrNr = gdxClose(PGX)) Rcout << "Error3" << "\n";
    return df;
}
