.gdxSymbolTypes = function(){
  return(CPP_getGDXSymbolTypes())
}


.VarTypeSubtype = function() {
  return(CPP_getGDXVarTypeSubtype())
}

.EqTypeSubtype = function() {
  return(CPP_getGDXEqTypeSubtype())
}

.SetTypeSubtype = function() {
  return(CPP_getGDXSetTypeSubtype())
}

.EquationTypes = c(
eq = "eq",
E = "eq",
e = "eq",
geq = "geq",
G = "geq",
g = "geq",
leq = "leq",
L = "leq",
l = "leq",
nonbinding = "nonbinding",
N = "nonbinding",
n = "nonbinding",
cone = "cone",
C = "cone",
c = "cone",
external = "external",
X = "external",
x = "external",
boolean = "boolean",
B = "boolean",
b = "boolean"
)

.varTypes = c(
  "binary",
  "integer",
  "positive",
  "negative",
  "free",
  "sos1",
  "sos2",
  "semicont",
  "semiint"
)
