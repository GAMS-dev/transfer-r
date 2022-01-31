
#include <Rcpp.h>
#include "gdxcc.h"
#include "gclgms.h"
using namespace Rcpp;

void WriteData(gdxHandle_t PGX, StringVector s, 
std::vector<double> V, int VarType, int Dim,
std::string elemText, int &serialNo, std::map<std::string, int> &elemTextToNumber) {
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

      if (elemTextToNumber.count(elemText) == 0) {
        serialNo = serialNo + 1;
        elemTextToNumber.insert(std::pair<std::string, int> (elemText, serialNo));
        Values[GMS_VAL_LEVEL] = serialNo; //zero indexing in c++
      }
      else {
        Values[GMS_VAL_LEVEL] = elemTextToNumber[elemText];
      }

      // Values[GMS_VAL_LEVEL] = 1;
      Values[GMS_VAL_MARGINAL] = 0;
      Values[GMS_VAL_UPPER] = 0;
      Values[GMS_VAL_LOWER] = 0;
      Values[GMS_VAL_SCALE] = 0;

    }
    else {
      Rcout << "empty string\n";
      Values[GMS_VAL_LEVEL]=0;
    }
  }
  else {
    Values[GMS_VAL_LEVEL] = V[GMS_VAL_LEVEL];
  }
  int txtnr;
  if (VarType == GMS_DT_SET) {
    Rcout << "textinside Writedata " << elemText << "\n";
    gdxAddSetText(PGX, elemText.c_str(), &txtnr);
  }

	rc = gdxDataWriteStr(PGX, (const char **)Indx, Values);



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
  int rc;

  rc = gdxCreateD(&PGX, mysysDir.c_str(), Msg, sizeof(Msg));

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
  return L;

}
// [[Rcpp::export]]
List getSymbolNames(CharacterVector gdxName, CharacterVector sysDir) {
  gdxHandle_t PGX = NULL;
  std::vector<std::string> domain;
  int rc, errCode, symCount, UelCount, sym_dimension, sym_type;
  char symbolID[GMS_SSSIZE], Msg[GMS_SSSIZE];
  std::string myname = Rcpp::as<std::string>(gdxName);
  std::string mysysDir = Rcpp::as<std::string>(sysDir);

  rc = gdxCreateD(&PGX, mysysDir.c_str(), Msg, sizeof(Msg));
  gdxOpenRead(PGX, myname.c_str(), &errCode);

  rc = gdxSystemInfo(PGX, &symCount, &UelCount);


  List L1;
	for (int i=0; i <= symCount; i++){
		gdxSymbolInfo(PGX, i, symbolID, &sym_dimension, &sym_type);

    if (strcmp(symbolID, "*") != 0) {
    L1.push_back(symbolID);
    }
  }

  return L1;

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
  Rcout << "gdxcreate return code: " << rc << "\n";
  Rcout << "gdxcreate message: " << Msg << "\n";
  gdxOpenRead(PGX, myname.c_str(), &errCode);
  Rcout << "Filename: " << myname << "\n";
  Rcout << "open read return code: " << errCode << "\n";

  rc = gdxSystemInfo(PGX, &symCount, &UelCount);
  Rcout << "symbol count: " << symCount << "\n";

  List templist, L1;
	for (int i=0; i <= symCount; i++) {
		gdxSymbolInfo(PGX, i, symbolID, &sym_dimension, &sym_type);
    if (strcmp(symbolID, "*") == 0) {
      continue;
    }
    gdxSymbolInfoX(PGX, i, &nrecs, &subtype, explText);

      rc = gdxSymbolGetDomainX(PGX, i, domains_ptr);
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

    // if (strcmp(symbolID, "*") != 0) {
    // L1.push_back(templist);
    // }
  }

  return L1;
}

// [[Rcpp::export]]
void gdxWriteSuper(List data,
CharacterVector sysDir, CharacterVector fileName) {
  Rcout << "here2\n";
  std::string mysysDir = Rcpp::as<std::string>(sysDir);
  std::string myFileName = Rcpp::as<std::string>(fileName);
  gdxHandle_t PGX = NULL;
	char        Msg[GMS_SSSIZE];
	int         ErrNr, rc, varType;
  std::map<std::string, int> elemTextToNumber;
  gdxStrIndexPtrs_t domains_ptr;
  gdxStrIndex_t domains;
  GDXSTRINDEXPTRS_INIT(domains, domains_ptr);
  Rcout << "here1\n";
	if (!gdxCreateD(&PGX, mysysDir.c_str(), Msg, sizeof(Msg))) {
		Rcout << "**** Could not load GDX library" << "\n" << "**** " << Msg << "\n";
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

	/* Write demand data */
	rc = gdxOpenWrite(PGX, myFileName.c_str(), "xp_example1", &ErrNr);
	if (ErrNr) Rcout << "Error1" << "\n";
  // std::string mysym, varTypeStr;
  DataFrame df;
  List domain;
  int Dim;
  std::vector<double> values;
  std::string elemText;
  StringVector colString, colElemText;
  NumericVector colDouble;
  int elemTextCount = 0;
  for (int d=0; d < data.length(); d++){
    Rcout << "inside data loop\n";
    Environment symname = data[d];
    Rcout << "got symname\n";
    std::string mysym = symname["name"];
    Rcout << "here3 " << mysym << "\n";
    varType = symname["type"];

    if (varType == GMS_DT_ALIAS) {
      Rcout << "herehere\n";
      Environment alias_with_env = symname["aliasWith"];
      std::string alias_with = alias_with_env["name"];
      Rcout << "alias_with: " << alias_with << "\n";
      if (!gdxAddAlias(PGX, mysym.c_str(), alias_with.c_str()))
      Rcout << "cannot add alias \n";
      continue;
    }
    Dim = symname["dimension"];
    StringVector names(Dim);
    // only executed if not an alias
    df = symname["records"];
    // domain = symname["domainstr"];

    
    domain = symname["domain"];
    Rcout << "symbol: " << mysym << "\n";
    List domainstr;
    if (Dim != 0) {
      Rcpp::Function  domain_names = symname["domain_names"];
      Rcout << "here3\n";
      domainstr = domain_names();
      Rcout << "here4\n";
      std::string blah = domainstr[0];
      Rcout << "here5\n";
      Rcout << "string domain: " << blah << "\n";

    }
    std::string expltxt = symname["description"];
    Rcout << "dim " << Dim << "\n";
    Rcout << "varType " << varType << "\n";
    Rcout << "mysym " << mysym << "\n";
    if (!gdxDataWriteStrStart(PGX, mysym.c_str(), 
    expltxt.c_str(), Dim, varType, 0))
    Rcout << "Error2" << "\n";

    for (int D=0; D < Dim; D++) {
      strcpy(domains_ptr[D], domainstr[D]);
      // try
      // {
      //   Environment env = domain[D];
      //   if (env.exists("name")) {
      //     List assumedenv;
      //     assumedenv = domain[D];
      //     strcpy(domains_ptr[D], assumedenv["name"]);
      //   }
      // }
      // catch(const std::exception& e)
      // {
      //  strcpy(domains_ptr[D], domain[D]);
      // }
    }

    gdxSymbolSetDomain(PGX, (const char **)domains_ptr);
    int nrows = df.nrows();
    int ncols = df.size();
    Rcout << "rows: " << df.nrows() << "\n";
    Rcout << "columns: " << df.size() << "\n";
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
        WriteData(PGX, names, values, varType, Dim, elemText, elemTextCount, elemTextToNumber);
      }
      else {

        Rcout << "elemTextCount " << elemTextCount << " elementText: " << elemText << "\n";
        WriteData(PGX, names, values, varType, Dim, elemText, elemTextCount, elemTextToNumber);
        Rcout << "elemTextCountAfter " << elemTextCount << "\n";
      }
      elemText.clear();
      values.clear();
    }
    Rcout << "here5\n";

    if (!gdxDataWriteDone(PGX)) Rcout << "Error3" << "\n";
  }
  Rcout << "here4\n";
  if (gdxClose(PGX)) Rcout << "Error4" << "\n";
  return;
}

// [[Rcpp::export]]
List readSymbols(CharacterVector symNames, CharacterVector gdxName,
                CharacterVector sysDir) {
  Rcout << "here1 sysdir " << sysDir << "\n";
  Rcout << "here1 gdxname " << gdxName << "\n";
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
			Rcout << "**** Could not find variable " << mysymName  << " \n";
		}

    gdxSymbolInfo(PGX, VarNr, symbolID, &Dim, &VarType);
    if (VarType == GMS_DT_ALIAS) continue;
    rc = gdxSymbolGetDomainX(PGX, VarNr, domains_ptr);
    for (int j=0; j < Dim; j++) {
      domain.push_back(domains_ptr[j]);
    }
    if (!gdxDataReadStrStart(PGX, VarNr, &NrRecs)) Rcout << "Error2" << "\n";
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
  gdxDataReadDone(PGX);
  if (ErrNr = gdxClose(PGX)) Rcout << "Error3" << "\n";
  return L;
}