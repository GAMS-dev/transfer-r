library(gamstransfer)

test_that("readwritetest", {

  # tryCatch(
    # expr = {
  gams_text = '
* test sets
Set i_1 "set i_1" / i1, i2 "i2 element text" /;
Set i_2 "set i_2" / 1 "1 element text", 2 /;

Set j_1(i_1,i_2)  "set j_1" / #i_1.#i_2 /;
Set j_2(i_1,*)  "set j_2" / #i_1.#i_2 /;
Set j_3(*,i_2)  "set j_3" / #i_1.#i_2 /;

Singleton Set k_1 "singleton_set k_1" / k1 /;
Singleton Set k_2(i_1,i_2) "singleton_set k_2";
k_2("i1","1") = yes;

Singleton Set k_3(i_1,*) "singleton_set k_3";
k_3("i1","uni") = yes;

Singleton Set k_4(*,i_2) "singleton_set k_4";
k_4("uni","2") = yes;

* test aliases
Alias(i_1, i_1_p);
Alias(i_2, i_2_p);
Alias(j_1, j_1_p);
Alias(j_2, j_2_p);
Alias(j_3, j_3_p);
Alias(k_1, k_1_p);
Alias(k_2, k_2_p);
Alias(k_3, k_3_p);

* test scalar
Scalar s_1 "scalar s_1";
Scalar s_2 "scalar s_2";
Scalar s_3 "scalar s_3";
Scalar s_4 "scalar s_4";
Scalar s_5 "scalar s_5";
Scalar s_6 "scalar s_6";
s_1 = 1;
s_2 = 1/0;
s_3 = NA;
s_4 = Inf;
s_5 = -Inf;
s_6 = eps;

* test parameters
Set sv / sv1, sv2, sv3, sv4, sv5, sv6 /;

Parameter p_1(sv) "parameter p_1";
p_1("sv1") = 1;
p_1("sv2") = 1/0;
p_1("sv3") = NA;
p_1("sv4") = Inf;
p_1("sv5") = -Inf;
p_1("sv6") = eps;

Parameter p_2(sv,sv) "parameter p_2";
p_2("sv1","sv1") = 1;
p_2("sv2","sv2") = 1/0;
p_2("sv3","sv3") = NA;
p_2("sv4","sv4") = Inf;
p_2("sv5","sv5") = -Inf;
p_2("sv6","sv6") = eps;

Parameter p_3(*,sv) "parameter p_3";
p_3("sv1","sv1") = 1;
p_3("sv2","sv2") = 1/0;
p_3("sv3","sv3") = NA;
p_3("sv4","sv4") = Inf;
p_3("sv5","sv5") = -Inf;
p_3("sv6","sv6") = eps;


* test free variables
variable free_1 "scalar variable free_1";
variable free_2 "scalar variable free_2";
variable free_3 "scalar variable free_3";
variable free_4 "scalar variable free_4";
variable free_5 "scalar variable free_5";
variable free_6 "scalar variable free_6";

free_1.L = 1;
free_1.M = 1;
free_1.LO = 1;
free_1.UP = 1;
free_1.SCALE = 1;

free_2.L = 1/0;
free_2.M = 1/0;
free_2.LO = 1/0;
free_2.UP = 1/0;
free_2.SCALE = 1/0;

free_3.L = NA;
free_3.M = NA;
free_3.LO = NA;
free_3.UP = NA;
free_3.SCALE = NA;

free_4.L = Inf;
free_4.M = Inf;
free_4.LO = Inf;
free_4.UP = Inf;
free_4.SCALE = Inf;

free_5.L = -Inf;
free_5.M = -Inf;
free_5.LO = -Inf;
free_5.UP = -Inf;
free_5.SCALE = -Inf;

free_6.L = eps;
free_6.M = eps;
free_6.LO = eps;
free_6.UP = eps;
free_6.SCALE = eps;

variable free_7(sv) "variable free_7";
free_7.L("sv1") = 1;
free_7.L("sv2") = 1/0;
free_7.L("sv3") = NA;
free_7.L("sv4") = +Inf;
free_7.L("sv5") = -Inf;
free_7.L("sv6") = eps;

free_7.M("sv1") = 1;
free_7.M("sv2") = 1/0;
free_7.M("sv3") = NA;
free_7.M("sv4") = +Inf;
free_7.M("sv5") = -Inf;
free_7.M("sv6") = eps;

free_7.LO("sv1") = 1;
free_7.LO("sv2") = 1/0;
free_7.LO("sv3") = NA;
free_7.LO("sv4") = +Inf;
free_7.LO("sv5") = -Inf;
free_7.LO("sv6") = eps;

free_7.UP("sv1") = 1;
free_7.UP("sv2") = 1/0;
free_7.UP("sv3") = NA;
free_7.UP("sv4") = +Inf;
free_7.UP("sv5") = -Inf;
free_7.UP("sv6") = eps;

free_7.SCALE("sv1") = 1;
free_7.SCALE("sv2") = 1/0;
free_7.SCALE("sv3") = NA;
free_7.SCALE("sv4") = +Inf;
free_7.SCALE("sv5") = -Inf;
free_7.SCALE("sv6") = eps;

variable free_8(sv,sv) "variable free_8";
free_8.L("sv1","sv1") = 1;
free_8.L("sv2","sv2") = 1/0;
free_8.L("sv3","sv3") = NA;
free_8.L("sv4","sv4") = +Inf;
free_8.L("sv5","sv5") = -Inf;
free_8.L("sv6","sv6") = eps;

free_8.M("sv1","sv1") = 1;
free_8.M("sv2","sv2") = 1/0;
free_8.M("sv3","sv3") = NA;
free_8.M("sv4","sv4") = +Inf;
free_8.M("sv5","sv5") = -Inf;
free_8.M("sv6","sv6") = eps;

free_8.LO("sv1","sv1") = 1;
free_8.LO("sv2","sv2") = 1/0;
free_8.LO("sv3","sv3") = NA;
free_8.LO("sv4","sv4") = +Inf;
free_8.LO("sv5","sv5") = -Inf;
free_8.LO("sv6","sv6") = eps;

free_8.UP("sv1","sv1") = 1;
free_8.UP("sv2","sv2") = 1/0;
free_8.UP("sv3","sv3") = NA;
free_8.UP("sv4","sv4") = +Inf;
free_8.UP("sv5","sv5") = -Inf;
free_8.UP("sv6","sv6") = eps;

free_8.SCALE("sv1","sv1") = 1;
free_8.SCALE("sv2","sv2") = 1/0;
free_8.SCALE("sv3","sv3") = NA;
free_8.SCALE("sv4","sv4") = +Inf;
free_8.SCALE("sv5","sv5") = -Inf;
free_8.SCALE("sv6","sv6") = eps;

variable free_9(*,sv) "variable free_9";
free_9.L("sv1","sv1") = 1;
free_9.L("sv2","sv2") = 1/0;
free_9.L("sv3","sv3") = NA;
free_9.L("sv4","sv4") = +Inf;
free_9.L("sv5","sv5") = -Inf;
free_9.L("sv6","sv6") = eps;

free_9.M("sv1","sv1") = 1;
free_9.M("sv2","sv2") = 1/0;
free_9.M("sv3","sv3") = NA;
free_9.M("sv4","sv4") = +Inf;
free_9.M("sv5","sv5") = -Inf;
free_9.M("sv6","sv6") = eps;

free_9.LO("sv1","sv1") = 1;
free_9.LO("sv2","sv2") = 1/0;
free_9.LO("sv3","sv3") = NA;
free_9.LO("sv4","sv4") = +Inf;
free_9.LO("sv5","sv5") = -Inf;
free_9.LO("sv6","sv6") = eps;

free_9.UP("sv1","sv1") = 1;
free_9.UP("sv2","sv2") = 1/0;
free_9.UP("sv3","sv3") = NA;
free_9.UP("sv4","sv4") = +Inf;
free_9.UP("sv5","sv5") = -Inf;
free_9.UP("sv6","sv6") = eps;

free_9.SCALE("sv1","sv1") = 1;
free_9.SCALE("sv2","sv2") = 1/0;
free_9.SCALE("sv3","sv3") = NA;
free_9.SCALE("sv4","sv4") = +Inf;
free_9.SCALE("sv5","sv5") = -Inf;
free_9.SCALE("sv6","sv6") = eps;




* test binary variables
binary variable binary_1 "scalar variable binary_1";
binary variable binary_2 "scalar variable binary_2";
binary variable binary_3 "scalar variable binary_3";
binary variable binary_4 "scalar variable binary_4";
binary variable binary_5 "scalar variable binary_5";
binary variable binary_6 "scalar variable binary_6";

binary_1.L = 1;
binary_1.M = 1;
binary_1.LO = 1;
binary_1.UP = 1;

binary_2.L = 1/0;
binary_2.M = 1/0;
binary_2.LO = 1/0;
binary_2.UP = 1/0;

binary_3.L = NA;
binary_3.M = NA;
binary_3.LO = NA;
binary_3.UP = NA;

binary_4.L = Inf;
binary_4.M = Inf;
binary_4.LO = Inf;
binary_4.UP = Inf;

binary_5.L = -Inf;
binary_5.M = -Inf;
binary_5.LO = -Inf;
binary_5.UP = -Inf;

binary_6.L = eps;
binary_6.M = eps;
binary_6.LO = eps;
binary_6.UP = eps;


binary variable binary_7(sv) "variable binary_7";
binary_7.L("sv1") = 1;
binary_7.L("sv2") = 1/0;
binary_7.L("sv3") = NA;
binary_7.L("sv4") = +Inf;
binary_7.L("sv5") = -Inf;
binary_7.L("sv6") = eps;

binary_7.M("sv1") = 1;
binary_7.M("sv2") = 1/0;
binary_7.M("sv3") = NA;
binary_7.M("sv4") = +Inf;
binary_7.M("sv5") = -Inf;
binary_7.M("sv6") = eps;

binary_7.LO("sv1") = 1;
binary_7.LO("sv2") = 1/0;
binary_7.LO("sv3") = NA;
binary_7.LO("sv4") = +Inf;
binary_7.LO("sv5") = -Inf;
binary_7.LO("sv6") = eps;

binary_7.UP("sv1") = 1;
binary_7.UP("sv2") = 1/0;
binary_7.UP("sv3") = NA;
binary_7.UP("sv4") = +Inf;
binary_7.UP("sv5") = -Inf;
binary_7.UP("sv6") = eps;

binary variable binary_8(sv,sv) "variable binary_8";
binary_8.L("sv1","sv1") = 1;
binary_8.L("sv2","sv2") = 1/0;
binary_8.L("sv3","sv3") = NA;
binary_8.L("sv4","sv4") = +Inf;
binary_8.L("sv5","sv5") = -Inf;
binary_8.L("sv6","sv6") = eps;

binary_8.M("sv1","sv1") = 1;
binary_8.M("sv2","sv2") = 1/0;
binary_8.M("sv3","sv3") = NA;
binary_8.M("sv4","sv4") = +Inf;
binary_8.M("sv5","sv5") = -Inf;
binary_8.M("sv6","sv6") = eps;

binary_8.LO("sv1","sv1") = 1;
binary_8.LO("sv2","sv2") = 1/0;
binary_8.LO("sv3","sv3") = NA;
binary_8.LO("sv4","sv4") = +Inf;
binary_8.LO("sv5","sv5") = -Inf;
binary_8.LO("sv6","sv6") = eps;

binary_8.UP("sv1","sv1") = 1;
binary_8.UP("sv2","sv2") = 1/0;
binary_8.UP("sv3","sv3") = NA;
binary_8.UP("sv4","sv4") = +Inf;
binary_8.UP("sv5","sv5") = -Inf;
binary_8.UP("sv6","sv6") = eps;

binary variable binary_9(*,sv) "variable binary_9";
binary_9.L("sv1","sv1") = 1;
binary_9.L("sv2","sv2") = 1/0;
binary_9.L("sv3","sv3") = NA;
binary_9.L("sv4","sv4") = +Inf;
binary_9.L("sv5","sv5") = -Inf;
binary_9.L("sv6","sv6") = eps;

binary_9.M("sv1","sv1") = 1;
binary_9.M("sv2","sv2") = 1/0;
binary_9.M("sv3","sv3") = NA;
binary_9.M("sv4","sv4") = +Inf;
binary_9.M("sv5","sv5") = -Inf;
binary_9.M("sv6","sv6") = eps;

binary_9.LO("sv1","sv1") = 1;
binary_9.LO("sv2","sv2") = 1/0;
binary_9.LO("sv3","sv3") = NA;
binary_9.LO("sv4","sv4") = +Inf;
binary_9.LO("sv5","sv5") = -Inf;
binary_9.LO("sv6","sv6") = eps;

binary_9.UP("sv1","sv1") = 1;
binary_9.UP("sv2","sv2") = 1/0;
binary_9.UP("sv3","sv3") = NA;
binary_9.UP("sv4","sv4") = +Inf;
binary_9.UP("sv5","sv5") = -Inf;
binary_9.UP("sv6","sv6") = eps;


* test integer variables
integer variable integer_1 "scalar variable integer_1";
integer variable integer_2 "scalar variable integer_2";
integer variable integer_3 "scalar variable integer_3";
integer variable integer_4 "scalar variable integer_4";
integer variable integer_5 "scalar variable integer_5";
integer variable integer_6 "scalar variable integer_6";

integer_1.L = 1;
integer_1.M = 1;
integer_1.LO = 1;
integer_1.UP = 1;

integer_2.L = 1/0;
integer_2.M = 1/0;
integer_2.LO = 1/0;
integer_2.UP = 1/0;

integer_3.L = NA;
integer_3.M = NA;
integer_3.LO = NA;
integer_3.UP = NA;

integer_4.L = Inf;
integer_4.M = Inf;
integer_4.LO = Inf;
integer_4.UP = Inf;

integer_5.L = -Inf;
integer_5.M = -Inf;
integer_5.LO = -Inf;
integer_5.UP = -Inf;

integer_6.L = eps;
integer_6.M = eps;
integer_6.LO = eps;
integer_6.UP = eps;


integer variable integer_7(sv) "variable integer_7";
integer_7.L("sv1") = 1;
integer_7.L("sv2") = 1/0;
integer_7.L("sv3") = NA;
integer_7.L("sv4") = +Inf;
integer_7.L("sv5") = -Inf;
integer_7.L("sv6") = eps;

integer_7.M("sv1") = 1;
integer_7.M("sv2") = 1/0;
integer_7.M("sv3") = NA;
integer_7.M("sv4") = +Inf;
integer_7.M("sv5") = -Inf;
integer_7.M("sv6") = eps;

integer_7.LO("sv1") = 1;
integer_7.LO("sv2") = 1/0;
integer_7.LO("sv3") = NA;
integer_7.LO("sv4") = +Inf;
integer_7.LO("sv5") = -Inf;
integer_7.LO("sv6") = eps;

integer_7.UP("sv1") = 1;
integer_7.UP("sv2") = 1/0;
integer_7.UP("sv3") = NA;
integer_7.UP("sv4") = +Inf;
integer_7.UP("sv5") = -Inf;
integer_7.UP("sv6") = eps;

integer variable integer_8(sv,sv) "variable integer_8";
integer_8.L("sv1","sv1") = 1;
integer_8.L("sv2","sv2") = 1/0;
integer_8.L("sv3","sv3") = NA;
integer_8.L("sv4","sv4") = +Inf;
integer_8.L("sv5","sv5") = -Inf;
integer_8.L("sv6","sv6") = eps;

integer_8.M("sv1","sv1") = 1;
integer_8.M("sv2","sv2") = 1/0;
integer_8.M("sv3","sv3") = NA;
integer_8.M("sv4","sv4") = +Inf;
integer_8.M("sv5","sv5") = -Inf;
integer_8.M("sv6","sv6") = eps;

integer_8.LO("sv1","sv1") = 1;
integer_8.LO("sv2","sv2") = 1/0;
integer_8.LO("sv3","sv3") = NA;
integer_8.LO("sv4","sv4") = +Inf;
integer_8.LO("sv5","sv5") = -Inf;
integer_8.LO("sv6","sv6") = eps;

integer_8.UP("sv1","sv1") = 1;
integer_8.UP("sv2","sv2") = 1/0;
integer_8.UP("sv3","sv3") = NA;
integer_8.UP("sv4","sv4") = +Inf;
integer_8.UP("sv5","sv5") = -Inf;
integer_8.UP("sv6","sv6") = eps;

integer variable integer_9(*,sv) "variable integer_9";
integer_9.L("sv1","sv1") = 1;
integer_9.L("sv2","sv2") = 1/0;
integer_9.L("sv3","sv3") = NA;
integer_9.L("sv4","sv4") = +Inf;
integer_9.L("sv5","sv5") = -Inf;
integer_9.L("sv6","sv6") = eps;

integer_9.M("sv1","sv1") = 1;
integer_9.M("sv2","sv2") = 1/0;
integer_9.M("sv3","sv3") = NA;
integer_9.M("sv4","sv4") = +Inf;
integer_9.M("sv5","sv5") = -Inf;
integer_9.M("sv6","sv6") = eps;

integer_9.LO("sv1","sv1") = 1;
integer_9.LO("sv2","sv2") = 1/0;
integer_9.LO("sv3","sv3") = NA;
integer_9.LO("sv4","sv4") = +Inf;
integer_9.LO("sv5","sv5") = -Inf;
integer_9.LO("sv6","sv6") = eps;

integer_9.UP("sv1","sv1") = 1;
integer_9.UP("sv2","sv2") = 1/0;
integer_9.UP("sv3","sv3") = NA;
integer_9.UP("sv4","sv4") = +Inf;
integer_9.UP("sv5","sv5") = -Inf;
integer_9.UP("sv6","sv6") = eps;


* test positive variables
positive variable positive_1 "scalar variable positive_1";
positive variable positive_2 "scalar variable positive_2";
positive variable positive_3 "scalar variable positive_3";
positive variable positive_4 "scalar variable positive_4";
positive variable positive_5 "scalar variable positive_5";
positive variable positive_6 "scalar variable positive_6";

positive_1.L = 1;
positive_1.M = 1;
positive_1.LO = 1;
positive_1.UP = 1;
positive_1.SCALE = 1;

positive_2.L = 1/0;
positive_2.M = 1/0;
positive_2.LO = 1/0;
positive_2.UP = 1/0;
positive_2.SCALE = 1/0;

positive_3.L = NA;
positive_3.M = NA;
positive_3.LO = NA;
positive_3.UP = NA;
positive_3.SCALE = NA;

positive_4.L = Inf;
positive_4.M = Inf;
positive_4.LO = Inf;
positive_4.UP = Inf;
positive_4.SCALE = Inf;

positive_5.L = -Inf;
positive_5.M = -Inf;
positive_5.LO = -Inf;
positive_5.UP = -Inf;
positive_5.SCALE = -Inf;

positive_6.L = eps;
positive_6.M = eps;
positive_6.LO = eps;
positive_6.UP = eps;
positive_6.SCALE = eps;

positive variable positive_7(sv) "variable positive_7";
positive_7.L("sv1") = 1;
positive_7.L("sv2") = 1/0;
positive_7.L("sv3") = NA;
positive_7.L("sv4") = +Inf;
positive_7.L("sv5") = -Inf;
positive_7.L("sv6") = eps;

positive_7.M("sv1") = 1;
positive_7.M("sv2") = 1/0;
positive_7.M("sv3") = NA;
positive_7.M("sv4") = +Inf;
positive_7.M("sv5") = -Inf;
positive_7.M("sv6") = eps;

positive_7.LO("sv1") = 1;
positive_7.LO("sv2") = 1/0;
positive_7.LO("sv3") = NA;
positive_7.LO("sv4") = +Inf;
positive_7.LO("sv5") = -Inf;
positive_7.LO("sv6") = eps;

positive_7.UP("sv1") = 1;
positive_7.UP("sv2") = 1/0;
positive_7.UP("sv3") = NA;
positive_7.UP("sv4") = +Inf;
positive_7.UP("sv5") = -Inf;
positive_7.UP("sv6") = eps;

positive_7.SCALE("sv1") = 1;
positive_7.SCALE("sv2") = 1/0;
positive_7.SCALE("sv3") = NA;
positive_7.SCALE("sv4") = +Inf;
positive_7.SCALE("sv5") = -Inf;
positive_7.SCALE("sv6") = eps;

positive variable positive_8(sv,sv) "variable positive_8";
positive_8.L("sv1","sv1") = 1;
positive_8.L("sv2","sv2") = 1/0;
positive_8.L("sv3","sv3") = NA;
positive_8.L("sv4","sv4") = +Inf;
positive_8.L("sv5","sv5") = -Inf;
positive_8.L("sv6","sv6") = eps;

positive_8.M("sv1","sv1") = 1;
positive_8.M("sv2","sv2") = 1/0;
positive_8.M("sv3","sv3") = NA;
positive_8.M("sv4","sv4") = +Inf;
positive_8.M("sv5","sv5") = -Inf;
positive_8.M("sv6","sv6") = eps;

positive_8.LO("sv1","sv1") = 1;
positive_8.LO("sv2","sv2") = 1/0;
positive_8.LO("sv3","sv3") = NA;
positive_8.LO("sv4","sv4") = +Inf;
positive_8.LO("sv5","sv5") = -Inf;
positive_8.LO("sv6","sv6") = eps;

positive_8.UP("sv1","sv1") = 1;
positive_8.UP("sv2","sv2") = 1/0;
positive_8.UP("sv3","sv3") = NA;
positive_8.UP("sv4","sv4") = +Inf;
positive_8.UP("sv5","sv5") = -Inf;
positive_8.UP("sv6","sv6") = eps;

positive_8.SCALE("sv1","sv1") = 1;
positive_8.SCALE("sv2","sv2") = 1/0;
positive_8.SCALE("sv3","sv3") = NA;
positive_8.SCALE("sv4","sv4") = +Inf;
positive_8.SCALE("sv5","sv5") = -Inf;
positive_8.SCALE("sv6","sv6") = eps;

positive variable positive_9(*,sv) "variable positive_9";
positive_9.L("sv1","sv1") = 1;
positive_9.L("sv2","sv2") = 1/0;
positive_9.L("sv3","sv3") = NA;
positive_9.L("sv4","sv4") = +Inf;
positive_9.L("sv5","sv5") = -Inf;
positive_9.L("sv6","sv6") = eps;

positive_9.M("sv1","sv1") = 1;
positive_9.M("sv2","sv2") = 1/0;
positive_9.M("sv3","sv3") = NA;
positive_9.M("sv4","sv4") = +Inf;
positive_9.M("sv5","sv5") = -Inf;
positive_9.M("sv6","sv6") = eps;

positive_9.LO("sv1","sv1") = 1;
positive_9.LO("sv2","sv2") = 1/0;
positive_9.LO("sv3","sv3") = NA;
positive_9.LO("sv4","sv4") = +Inf;
positive_9.LO("sv5","sv5") = -Inf;
positive_9.LO("sv6","sv6") = eps;

positive_9.UP("sv1","sv1") = 1;
positive_9.UP("sv2","sv2") = 1/0;
positive_9.UP("sv3","sv3") = NA;
positive_9.UP("sv4","sv4") = +Inf;
positive_9.UP("sv5","sv5") = -Inf;
positive_9.UP("sv6","sv6") = eps;

positive_9.SCALE("sv1","sv1") = 1;
positive_9.SCALE("sv2","sv2") = 1/0;
positive_9.SCALE("sv3","sv3") = NA;
positive_9.SCALE("sv4","sv4") = +Inf;
positive_9.SCALE("sv5","sv5") = -Inf;
positive_9.SCALE("sv6","sv6") = eps;



* test negative variables
negative variable negative_1 "scalar variable negative_1";
negative variable negative_2 "scalar variable negative_2";
negative variable negative_3 "scalar variable negative_3";
negative variable negative_4 "scalar variable negative_4";
negative variable negative_5 "scalar variable negative_5";
negative variable negative_6 "scalar variable negative_6";

negative_1.L = 1;
negative_1.M = 1;
negative_1.LO = 1;
negative_1.UP = 1;
negative_1.SCALE = 1;

negative_2.L = 1/0;
negative_2.M = 1/0;
negative_2.LO = 1/0;
negative_2.UP = 1/0;
negative_2.SCALE = 1/0;

negative_3.L = NA;
negative_3.M = NA;
negative_3.LO = NA;
negative_3.UP = NA;
negative_3.SCALE = NA;

negative_4.L = Inf;
negative_4.M = Inf;
negative_4.LO = Inf;
negative_4.UP = Inf;
negative_4.SCALE = Inf;

negative_5.L = -Inf;
negative_5.M = -Inf;
negative_5.LO = -Inf;
negative_5.UP = -Inf;
negative_5.SCALE = -Inf;

negative_6.L = eps;
negative_6.M = eps;
negative_6.LO = eps;
negative_6.UP = eps;
negative_6.SCALE = eps;

negative variable negative_7(sv) "variable negative_7";
negative_7.L("sv1") = 1;
negative_7.L("sv2") = 1/0;
negative_7.L("sv3") = NA;
negative_7.L("sv4") = +Inf;
negative_7.L("sv5") = -Inf;
negative_7.L("sv6") = eps;

negative_7.M("sv1") = 1;
negative_7.M("sv2") = 1/0;
negative_7.M("sv3") = NA;
negative_7.M("sv4") = +Inf;
negative_7.M("sv5") = -Inf;
negative_7.M("sv6") = eps;

negative_7.LO("sv1") = 1;
negative_7.LO("sv2") = 1/0;
negative_7.LO("sv3") = NA;
negative_7.LO("sv4") = +Inf;
negative_7.LO("sv5") = -Inf;
negative_7.LO("sv6") = eps;

negative_7.UP("sv1") = 1;
negative_7.UP("sv2") = 1/0;
negative_7.UP("sv3") = NA;
negative_7.UP("sv4") = +Inf;
negative_7.UP("sv5") = -Inf;
negative_7.UP("sv6") = eps;

negative_7.SCALE("sv1") = 1;
negative_7.SCALE("sv2") = 1/0;
negative_7.SCALE("sv3") = NA;
negative_7.SCALE("sv4") = +Inf;
negative_7.SCALE("sv5") = -Inf;
negative_7.SCALE("sv6") = eps;

negative variable negative_8(sv,sv) "variable negative_8";
negative_8.L("sv1","sv1") = 1;
negative_8.L("sv2","sv2") = 1/0;
negative_8.L("sv3","sv3") = NA;
negative_8.L("sv4","sv4") = +Inf;
negative_8.L("sv5","sv5") = -Inf;
negative_8.L("sv6","sv6") = eps;

negative_8.M("sv1","sv1") = 1;
negative_8.M("sv2","sv2") = 1/0;
negative_8.M("sv3","sv3") = NA;
negative_8.M("sv4","sv4") = +Inf;
negative_8.M("sv5","sv5") = -Inf;
negative_8.M("sv6","sv6") = eps;

negative_8.LO("sv1","sv1") = 1;
negative_8.LO("sv2","sv2") = 1/0;
negative_8.LO("sv3","sv3") = NA;
negative_8.LO("sv4","sv4") = +Inf;
negative_8.LO("sv5","sv5") = -Inf;
negative_8.LO("sv6","sv6") = eps;

negative_8.UP("sv1","sv1") = 1;
negative_8.UP("sv2","sv2") = 1/0;
negative_8.UP("sv3","sv3") = NA;
negative_8.UP("sv4","sv4") = +Inf;
negative_8.UP("sv5","sv5") = -Inf;
negative_8.UP("sv6","sv6") = eps;

negative_8.SCALE("sv1","sv1") = 1;
negative_8.SCALE("sv2","sv2") = 1/0;
negative_8.SCALE("sv3","sv3") = NA;
negative_8.SCALE("sv4","sv4") = +Inf;
negative_8.SCALE("sv5","sv5") = -Inf;
negative_8.SCALE("sv6","sv6") = eps;

negative variable negative_9(*,sv) "variable negative_9";
negative_9.L("sv1","sv1") = 1;
negative_9.L("sv2","sv2") = 1/0;
negative_9.L("sv3","sv3") = NA;
negative_9.L("sv4","sv4") = +Inf;
negative_9.L("sv5","sv5") = -Inf;
negative_9.L("sv6","sv6") = eps;

negative_9.M("sv1","sv1") = 1;
negative_9.M("sv2","sv2") = 1/0;
negative_9.M("sv3","sv3") = NA;
negative_9.M("sv4","sv4") = +Inf;
negative_9.M("sv5","sv5") = -Inf;
negative_9.M("sv6","sv6") = eps;

negative_9.LO("sv1","sv1") = 1;
negative_9.LO("sv2","sv2") = 1/0;
negative_9.LO("sv3","sv3") = NA;
negative_9.LO("sv4","sv4") = +Inf;
negative_9.LO("sv5","sv5") = -Inf;
negative_9.LO("sv6","sv6") = eps;

negative_9.UP("sv1","sv1") = 1;
negative_9.UP("sv2","sv2") = 1/0;
negative_9.UP("sv3","sv3") = NA;
negative_9.UP("sv4","sv4") = +Inf;
negative_9.UP("sv5","sv5") = -Inf;
negative_9.UP("sv6","sv6") = eps;

negative_9.SCALE("sv1","sv1") = 1;
negative_9.SCALE("sv2","sv2") = 1/0;
negative_9.SCALE("sv3","sv3") = NA;
negative_9.SCALE("sv4","sv4") = +Inf;
negative_9.SCALE("sv5","sv5") = -Inf;
negative_9.SCALE("sv6","sv6") = eps;


* test SOS1 variables
sos1 variable sos1_1 "scalar variable sos1_1";
sos1 variable sos1_2 "scalar variable sos1_2";
sos1 variable sos1_3 "scalar variable sos1_3";
sos1 variable sos1_4 "scalar variable sos1_4";
sos1 variable sos1_5 "scalar variable sos1_5";
sos1 variable sos1_6 "scalar variable sos1_6";

sos1_1.L = 1;
sos1_1.M = 1;
sos1_1.LO = 1;
sos1_1.UP = 1;

sos1_2.L = 1/0;
sos1_2.M = 1/0;
sos1_2.LO = 1/0;
sos1_2.UP = 1/0;

sos1_3.L = NA;
sos1_3.M = NA;
sos1_3.LO = NA;
sos1_3.UP = NA;

sos1_4.L = Inf;
sos1_4.M = Inf;
sos1_4.LO = Inf;
sos1_4.UP = Inf;

sos1_5.L = -Inf;
sos1_5.M = -Inf;
sos1_5.LO = -Inf;
sos1_5.UP = -Inf;

sos1_6.L = eps;
sos1_6.M = eps;
sos1_6.LO = eps;
sos1_6.UP = eps;

sos1 variable sos1_7(sv) "variable sos1_7";
sos1_7.L("sv1") = 1;
sos1_7.L("sv2") = 1/0;
sos1_7.L("sv3") = NA;
sos1_7.L("sv4") = +Inf;
sos1_7.L("sv5") = -Inf;
sos1_7.L("sv6") = eps;

sos1_7.M("sv1") = 1;
sos1_7.M("sv2") = 1/0;
sos1_7.M("sv3") = NA;
sos1_7.M("sv4") = +Inf;
sos1_7.M("sv5") = -Inf;
sos1_7.M("sv6") = eps;

sos1_7.LO("sv1") = 1;
sos1_7.LO("sv2") = 1/0;
sos1_7.LO("sv3") = NA;
sos1_7.LO("sv4") = +Inf;
sos1_7.LO("sv5") = -Inf;
sos1_7.LO("sv6") = eps;

sos1_7.UP("sv1") = 1;
sos1_7.UP("sv2") = 1/0;
sos1_7.UP("sv3") = NA;
sos1_7.UP("sv4") = +Inf;
sos1_7.UP("sv5") = -Inf;
sos1_7.UP("sv6") = eps;

sos1 variable sos1_8(sv,sv) "variable sos1_8";
sos1_8.L("sv1","sv1") = 1;
sos1_8.L("sv2","sv2") = 1/0;
sos1_8.L("sv3","sv3") = NA;
sos1_8.L("sv4","sv4") = +Inf;
sos1_8.L("sv5","sv5") = -Inf;
sos1_8.L("sv6","sv6") = eps;

sos1_8.M("sv1","sv1") = 1;
sos1_8.M("sv2","sv2") = 1/0;
sos1_8.M("sv3","sv3") = NA;
sos1_8.M("sv4","sv4") = +Inf;
sos1_8.M("sv5","sv5") = -Inf;
sos1_8.M("sv6","sv6") = eps;

sos1_8.LO("sv1","sv1") = 1;
sos1_8.LO("sv2","sv2") = 1/0;
sos1_8.LO("sv3","sv3") = NA;
sos1_8.LO("sv4","sv4") = +Inf;
sos1_8.LO("sv5","sv5") = -Inf;
sos1_8.LO("sv6","sv6") = eps;

sos1_8.UP("sv1","sv1") = 1;
sos1_8.UP("sv2","sv2") = 1/0;
sos1_8.UP("sv3","sv3") = NA;
sos1_8.UP("sv4","sv4") = +Inf;
sos1_8.UP("sv5","sv5") = -Inf;
sos1_8.UP("sv6","sv6") = eps;

sos1 variable sos1_9(*,sv) "variable sos1_9";
sos1_9.L("sv1","sv1") = 1;
sos1_9.L("sv2","sv2") = 1/0;
sos1_9.L("sv3","sv3") = NA;
sos1_9.L("sv4","sv4") = +Inf;
sos1_9.L("sv5","sv5") = -Inf;
sos1_9.L("sv6","sv6") = eps;

sos1_9.M("sv1","sv1") = 1;
sos1_9.M("sv2","sv2") = 1/0;
sos1_9.M("sv3","sv3") = NA;
sos1_9.M("sv4","sv4") = +Inf;
sos1_9.M("sv5","sv5") = -Inf;
sos1_9.M("sv6","sv6") = eps;

sos1_9.LO("sv1","sv1") = 1;
sos1_9.LO("sv2","sv2") = 1/0;
sos1_9.LO("sv3","sv3") = NA;
sos1_9.LO("sv4","sv4") = +Inf;
sos1_9.LO("sv5","sv5") = -Inf;
sos1_9.LO("sv6","sv6") = eps;

sos1_9.UP("sv1","sv1") = 1;
sos1_9.UP("sv2","sv2") = 1/0;
sos1_9.UP("sv3","sv3") = NA;
sos1_9.UP("sv4","sv4") = +Inf;
sos1_9.UP("sv5","sv5") = -Inf;
sos1_9.UP("sv6","sv6") = eps;



* test SOS2 variables
sos2 variable sos2_1 "scalar variable sos2_1";
sos2 variable sos2_2 "scalar variable sos2_2";
sos2 variable sos2_3 "scalar variable sos2_3";
sos2 variable sos2_4 "scalar variable sos2_4";
sos2 variable sos2_5 "scalar variable sos2_5";
sos2 variable sos2_6 "scalar variable sos2_6";

sos2_1.L = 1;
sos2_1.M = 1;
sos2_1.LO = 1;
sos2_1.UP = 1;

sos2_2.L = 1/0;
sos2_2.M = 1/0;
sos2_2.LO = 1/0;
sos2_2.UP = 1/0;

sos2_3.L = NA;
sos2_3.M = NA;
sos2_3.LO = NA;
sos2_3.UP = NA;

sos2_4.L = Inf;
sos2_4.M = Inf;
sos2_4.LO = Inf;
sos2_4.UP = Inf;

sos2_5.L = -Inf;
sos2_5.M = -Inf;
sos2_5.LO = -Inf;
sos2_5.UP = -Inf;

sos2_6.L = eps;
sos2_6.M = eps;
sos2_6.LO = eps;
sos2_6.UP = eps;

sos2 variable sos2_7(sv) "variable sos2_7";
sos2_7.L("sv1") = 1;
sos2_7.L("sv2") = 1/0;
sos2_7.L("sv3") = NA;
sos2_7.L("sv4") = +Inf;
sos2_7.L("sv5") = -Inf;
sos2_7.L("sv6") = eps;

sos2_7.M("sv1") = 1;
sos2_7.M("sv2") = 1/0;
sos2_7.M("sv3") = NA;
sos2_7.M("sv4") = +Inf;
sos2_7.M("sv5") = -Inf;
sos2_7.M("sv6") = eps;

sos2_7.LO("sv1") = 1;
sos2_7.LO("sv2") = 1/0;
sos2_7.LO("sv3") = NA;
sos2_7.LO("sv4") = +Inf;
sos2_7.LO("sv5") = -Inf;
sos2_7.LO("sv6") = eps;

sos2_7.UP("sv1") = 1;
sos2_7.UP("sv2") = 1/0;
sos2_7.UP("sv3") = NA;
sos2_7.UP("sv4") = +Inf;
sos2_7.UP("sv5") = -Inf;
sos2_7.UP("sv6") = eps;

sos2 variable sos2_8(sv,sv) "variable sos2_8";
sos2_8.L("sv1","sv1") = 1;
sos2_8.L("sv2","sv2") = 1/0;
sos2_8.L("sv3","sv3") = NA;
sos2_8.L("sv4","sv4") = +Inf;
sos2_8.L("sv5","sv5") = -Inf;
sos2_8.L("sv6","sv6") = eps;

sos2_8.M("sv1","sv1") = 1;
sos2_8.M("sv2","sv2") = 1/0;
sos2_8.M("sv3","sv3") = NA;
sos2_8.M("sv4","sv4") = +Inf;
sos2_8.M("sv5","sv5") = -Inf;
sos2_8.M("sv6","sv6") = eps;

sos2_8.LO("sv1","sv1") = 1;
sos2_8.LO("sv2","sv2") = 1/0;
sos2_8.LO("sv3","sv3") = NA;
sos2_8.LO("sv4","sv4") = +Inf;
sos2_8.LO("sv5","sv5") = -Inf;
sos2_8.LO("sv6","sv6") = eps;

sos2_8.UP("sv1","sv1") = 1;
sos2_8.UP("sv2","sv2") = 1/0;
sos2_8.UP("sv3","sv3") = NA;
sos2_8.UP("sv4","sv4") = +Inf;
sos2_8.UP("sv5","sv5") = -Inf;
sos2_8.UP("sv6","sv6") = eps;

sos2 variable sos2_9(*,sv) "variable sos2_9";
sos2_9.L("sv1","sv1") = 1;
sos2_9.L("sv2","sv2") = 1/0;
sos2_9.L("sv3","sv3") = NA;
sos2_9.L("sv4","sv4") = +Inf;
sos2_9.L("sv5","sv5") = -Inf;
sos2_9.L("sv6","sv6") = eps;

sos2_9.M("sv1","sv1") = 1;
sos2_9.M("sv2","sv2") = 1/0;
sos2_9.M("sv3","sv3") = NA;
sos2_9.M("sv4","sv4") = +Inf;
sos2_9.M("sv5","sv5") = -Inf;
sos2_9.M("sv6","sv6") = eps;

sos2_9.LO("sv1","sv1") = 1;
sos2_9.LO("sv2","sv2") = 1/0;
sos2_9.LO("sv3","sv3") = NA;
sos2_9.LO("sv4","sv4") = +Inf;
sos2_9.LO("sv5","sv5") = -Inf;
sos2_9.LO("sv6","sv6") = eps;

sos2_9.UP("sv1","sv1") = 1;
sos2_9.UP("sv2","sv2") = 1/0;
sos2_9.UP("sv3","sv3") = NA;
sos2_9.UP("sv4","sv4") = +Inf;
sos2_9.UP("sv5","sv5") = -Inf;
sos2_9.UP("sv6","sv6") = eps;


* test semiint variables
semiint variable semiint_1 "scalar variable semiint_1";
semiint variable semiint_2 "scalar variable semiint_2";
semiint variable semiint_3 "scalar variable semiint_3";
semiint variable semiint_4 "scalar variable semiint_4";
semiint variable semiint_5 "scalar variable semiint_5";
semiint variable semiint_6 "scalar variable semiint_6";

semiint_1.L = 1;
semiint_1.M = 1;
semiint_1.LO = 1;
semiint_1.UP = 1;

semiint_2.L = 1/0;
semiint_2.M = 1/0;
semiint_2.LO = 1/0;
semiint_2.UP = 1/0;

semiint_3.L = NA;
semiint_3.M = NA;
semiint_3.LO = NA;
semiint_3.UP = NA;

semiint_4.L = Inf;
semiint_4.M = Inf;
semiint_4.LO = Inf;
semiint_4.UP = Inf;

semiint_5.L = -Inf;
semiint_5.M = -Inf;
semiint_5.LO = -Inf;
semiint_5.UP = -Inf;

semiint_6.L = eps;
semiint_6.M = eps;
semiint_6.LO = eps;
semiint_6.UP = eps;

semiint variable semiint_7(sv) "variable semiint_7";
semiint_7.L("sv1") = 1;
semiint_7.L("sv2") = 1/0;
semiint_7.L("sv3") = NA;
semiint_7.L("sv4") = +Inf;
semiint_7.L("sv5") = -Inf;
semiint_7.L("sv6") = eps;

semiint_7.M("sv1") = 1;
semiint_7.M("sv2") = 1/0;
semiint_7.M("sv3") = NA;
semiint_7.M("sv4") = +Inf;
semiint_7.M("sv5") = -Inf;
semiint_7.M("sv6") = eps;

semiint_7.LO("sv1") = 1;
semiint_7.LO("sv2") = 1/0;
semiint_7.LO("sv3") = NA;
semiint_7.LO("sv4") = +Inf;
semiint_7.LO("sv5") = -Inf;
semiint_7.LO("sv6") = eps;

semiint_7.UP("sv1") = 1;
semiint_7.UP("sv2") = 1/0;
semiint_7.UP("sv3") = NA;
semiint_7.UP("sv4") = +Inf;
semiint_7.UP("sv5") = -Inf;
semiint_7.UP("sv6") = eps;

semiint variable semiint_8(sv,sv) "variable semiint_8";
semiint_8.L("sv1","sv1") = 1;
semiint_8.L("sv2","sv2") = 1/0;
semiint_8.L("sv3","sv3") = NA;
semiint_8.L("sv4","sv4") = +Inf;
semiint_8.L("sv5","sv5") = -Inf;
semiint_8.L("sv6","sv6") = eps;

semiint_8.M("sv1","sv1") = 1;
semiint_8.M("sv2","sv2") = 1/0;
semiint_8.M("sv3","sv3") = NA;
semiint_8.M("sv4","sv4") = +Inf;
semiint_8.M("sv5","sv5") = -Inf;
semiint_8.M("sv6","sv6") = eps;

semiint_8.LO("sv1","sv1") = 1;
semiint_8.LO("sv2","sv2") = 1/0;
semiint_8.LO("sv3","sv3") = NA;
semiint_8.LO("sv4","sv4") = +Inf;
semiint_8.LO("sv5","sv5") = -Inf;
semiint_8.LO("sv6","sv6") = eps;

semiint_8.UP("sv1","sv1") = 1;
semiint_8.UP("sv2","sv2") = 1/0;
semiint_8.UP("sv3","sv3") = NA;
semiint_8.UP("sv4","sv4") = +Inf;
semiint_8.UP("sv5","sv5") = -Inf;
semiint_8.UP("sv6","sv6") = eps;

semiint variable semiint_9(*,sv) "variable semiint_9";
semiint_9.L("sv1","sv1") = 1;
semiint_9.L("sv2","sv2") = 1/0;
semiint_9.L("sv3","sv3") = NA;
semiint_9.L("sv4","sv4") = +Inf;
semiint_9.L("sv5","sv5") = -Inf;
semiint_9.L("sv6","sv6") = eps;

semiint_9.M("sv1","sv1") = 1;
semiint_9.M("sv2","sv2") = 1/0;
semiint_9.M("sv3","sv3") = NA;
semiint_9.M("sv4","sv4") = +Inf;
semiint_9.M("sv5","sv5") = -Inf;
semiint_9.M("sv6","sv6") = eps;

semiint_9.LO("sv1","sv1") = 1;
semiint_9.LO("sv2","sv2") = 1/0;
semiint_9.LO("sv3","sv3") = NA;
semiint_9.LO("sv4","sv4") = +Inf;
semiint_9.LO("sv5","sv5") = -Inf;
semiint_9.LO("sv6","sv6") = eps;

semiint_9.UP("sv1","sv1") = 1;
semiint_9.UP("sv2","sv2") = 1/0;
semiint_9.UP("sv3","sv3") = NA;
semiint_9.UP("sv4","sv4") = +Inf;
semiint_9.UP("sv5","sv5") = -Inf;
semiint_9.UP("sv6","sv6") = eps;



* test semicont variables
semicont variable semicont_1 "scalar variable semicont_1";
semicont variable semicont_2 "scalar variable semicont_2";
semicont variable semicont_3 "scalar variable semicont_3";
semicont variable semicont_4 "scalar variable semicont_4";
semicont variable semicont_5 "scalar variable semicont_5";
semicont variable semicont_6 "scalar variable semicont_6";

semicont_1.L = 1;
semicont_1.M = 1;
semicont_1.LO = 1;
semicont_1.UP = 1;

semicont_2.L = 1/0;
semicont_2.M = 1/0;
semicont_2.LO = 1/0;
semicont_2.UP = 1/0;

semicont_3.L = NA;
semicont_3.M = NA;
semicont_3.LO = NA;
semicont_3.UP = NA;

semicont_4.L = Inf;
semicont_4.M = Inf;
semicont_4.LO = Inf;
semicont_4.UP = Inf;

semicont_5.L = -Inf;
semicont_5.M = -Inf;
semicont_5.LO = -Inf;
semicont_5.UP = -Inf;

semicont_6.L = eps;
semicont_6.M = eps;
semicont_6.LO = eps;
semicont_6.UP = eps;

semicont variable semicont_7(sv) "variable semicont_7";
semicont_7.L("sv1") = 1;
semicont_7.L("sv2") = 1/0;
semicont_7.L("sv3") = NA;
semicont_7.L("sv4") = +Inf;
semicont_7.L("sv5") = -Inf;
semicont_7.L("sv6") = eps;

semicont_7.M("sv1") = 1;
semicont_7.M("sv2") = 1/0;
semicont_7.M("sv3") = NA;
semicont_7.M("sv4") = +Inf;
semicont_7.M("sv5") = -Inf;
semicont_7.M("sv6") = eps;

semicont_7.LO("sv1") = 1;
semicont_7.LO("sv2") = 1/0;
semicont_7.LO("sv3") = NA;
semicont_7.LO("sv4") = +Inf;
semicont_7.LO("sv5") = -Inf;
semicont_7.LO("sv6") = eps;

semicont_7.UP("sv1") = 1;
semicont_7.UP("sv2") = 1/0;
semicont_7.UP("sv3") = NA;
semicont_7.UP("sv4") = +Inf;
semicont_7.UP("sv5") = -Inf;
semicont_7.UP("sv6") = eps;

semicont variable semicont_8(sv,sv) "variable semicont_8";
semicont_8.L("sv1","sv1") = 1;
semicont_8.L("sv2","sv2") = 1/0;
semicont_8.L("sv3","sv3") = NA;
semicont_8.L("sv4","sv4") = +Inf;
semicont_8.L("sv5","sv5") = -Inf;
semicont_8.L("sv6","sv6") = eps;

semicont_8.M("sv1","sv1") = 1;
semicont_8.M("sv2","sv2") = 1/0;
semicont_8.M("sv3","sv3") = NA;
semicont_8.M("sv4","sv4") = +Inf;
semicont_8.M("sv5","sv5") = -Inf;
semicont_8.M("sv6","sv6") = eps;

semicont_8.LO("sv1","sv1") = 1;
semicont_8.LO("sv2","sv2") = 1/0;
semicont_8.LO("sv3","sv3") = NA;
semicont_8.LO("sv4","sv4") = +Inf;
semicont_8.LO("sv5","sv5") = -Inf;
semicont_8.LO("sv6","sv6") = eps;

semicont_8.UP("sv1","sv1") = 1;
semicont_8.UP("sv2","sv2") = 1/0;
semicont_8.UP("sv3","sv3") = NA;
semicont_8.UP("sv4","sv4") = +Inf;
semicont_8.UP("sv5","sv5") = -Inf;
semicont_8.UP("sv6","sv6") = eps;

semicont variable semicont_9(*,sv) "variable semicont_9";
semicont_9.L("sv1","sv1") = 1;
semicont_9.L("sv2","sv2") = 1/0;
semicont_9.L("sv3","sv3") = NA;
semicont_9.L("sv4","sv4") = +Inf;
semicont_9.L("sv5","sv5") = -Inf;
semicont_9.L("sv6","sv6") = eps;

semicont_9.M("sv1","sv1") = 1;
semicont_9.M("sv2","sv2") = 1/0;
semicont_9.M("sv3","sv3") = NA;
semicont_9.M("sv4","sv4") = +Inf;
semicont_9.M("sv5","sv5") = -Inf;
semicont_9.M("sv6","sv6") = eps;

semicont_9.LO("sv1","sv1") = 1;
semicont_9.LO("sv2","sv2") = 1/0;
semicont_9.LO("sv3","sv3") = NA;
semicont_9.LO("sv4","sv4") = +Inf;
semicont_9.LO("sv5","sv5") = -Inf;
semicont_9.LO("sv6","sv6") = eps;

semicont_9.UP("sv1","sv1") = 1;
semicont_9.UP("sv2","sv2") = 1/0;
semicont_9.UP("sv3","sv3") = NA;
semicont_9.UP("sv4","sv4") = +Inf;
semicont_9.UP("sv5","sv5") = -Inf;
semicont_9.UP("sv6","sv6") = eps;



* test equality equations
Equation eq_1 "scalar equation eq_1";
Equation eq_2 "scalar equation eq_2";
Equation eq_3 "scalar equation eq_3";
Equation eq_4 "scalar equation eq_4";
Equation eq_5 "scalar equation eq_5";
Equation eq_6 "scalar equation eq_6";

eq_1.. 1 =E= 1;
eq_1.L = 1;
eq_1.M = 1;
eq_1.LO = 1;
eq_1.UP = 1;
eq_1.SCALE = 1;

eq_2.. 1 =E= 1;
eq_2.L = 1/0;
eq_2.M = 1/0;
eq_2.LO = 1/0;
eq_2.UP = 1/0;
eq_2.SCALE = 1/0;

eq_3.. 1 =E= 1;
eq_3.L = NA;
eq_3.M = NA;
eq_3.LO = NA;
eq_3.UP = NA;
eq_3.SCALE = NA;

eq_4.. 1 =E= 1;
eq_4.L = Inf;
eq_4.M = Inf;
eq_4.LO = Inf;
eq_4.UP = Inf;
eq_4.SCALE = Inf;

eq_5.. 1 =E= 1;
eq_5.L = -Inf;
eq_5.M = -Inf;
eq_5.LO = -Inf;
eq_5.UP = -Inf;
eq_5.SCALE = -Inf;

eq_6.. 1 =E= 1;
eq_6.L = eps;
eq_6.M = eps;
eq_6.LO = eps;
eq_6.UP = eps;
eq_6.SCALE = eps;

Equation eq_7(sv) "equation eq_7";

eq_7(sv).. 1 =E= 1;

eq_7.L("sv1") = 1;
eq_7.L("sv2") = 1/0;
eq_7.L("sv3") = NA;
eq_7.L("sv4") = +Inf;
eq_7.L("sv5") = -Inf;
eq_7.L("sv6") = eps;

eq_7.M("sv1") = 1;
eq_7.M("sv2") = 1/0;
eq_7.M("sv3") = NA;
eq_7.M("sv4") = +Inf;
eq_7.M("sv5") = -Inf;
eq_7.M("sv6") = eps;

eq_7.LO("sv1") = 1;
eq_7.LO("sv2") = 1/0;
eq_7.LO("sv3") = NA;
eq_7.LO("sv4") = +Inf;
eq_7.LO("sv5") = -Inf;
eq_7.LO("sv6") = eps;

eq_7.UP("sv1") = 1;
eq_7.UP("sv2") = 1/0;
eq_7.UP("sv3") = NA;
eq_7.UP("sv4") = +Inf;
eq_7.UP("sv5") = -Inf;
eq_7.UP("sv6") = eps;

eq_7.SCALE("sv1") = 1;
eq_7.SCALE("sv2") = 1/0;
eq_7.SCALE("sv3") = NA;
eq_7.SCALE("sv4") = +Inf;
eq_7.SCALE("sv5") = -Inf;
eq_7.SCALE("sv6") = eps;

Equation eq_8(sv,sv) "equation eq_8";
eq_8(sv,sv).. 1 =E= 1;

eq_8.L("sv1","sv1") = 1;
eq_8.L("sv2","sv2") = 1/0;
eq_8.L("sv3","sv3") = NA;
eq_8.L("sv4","sv4") = +Inf;
eq_8.L("sv5","sv5") = -Inf;
eq_8.L("sv6","sv6") = eps;

eq_8.M("sv1","sv1") = 1;
eq_8.M("sv2","sv2") = 1/0;
eq_8.M("sv3","sv3") = NA;
eq_8.M("sv4","sv4") = +Inf;
eq_8.M("sv5","sv5") = -Inf;
eq_8.M("sv6","sv6") = eps;

eq_8.LO("sv1","sv1") = 1;
eq_8.LO("sv2","sv2") = 1/0;
eq_8.LO("sv3","sv3") = NA;
eq_8.LO("sv4","sv4") = +Inf;
eq_8.LO("sv5","sv5") = -Inf;
eq_8.LO("sv6","sv6") = eps;

eq_8.UP("sv1","sv1") = 1;
eq_8.UP("sv2","sv2") = 1/0;
eq_8.UP("sv3","sv3") = NA;
eq_8.UP("sv4","sv4") = +Inf;
eq_8.UP("sv5","sv5") = -Inf;
eq_8.UP("sv6","sv6") = eps;

eq_8.SCALE("sv1","sv1") = 1;
eq_8.SCALE("sv2","sv2") = 1/0;
eq_8.SCALE("sv3","sv3") = NA;
eq_8.SCALE("sv4","sv4") = +Inf;
eq_8.SCALE("sv5","sv5") = -Inf;
eq_8.SCALE("sv6","sv6") = eps;

Equation eq_9(*,sv) "equation eq_9";
eq_9(sv,sv).. 1 =E= 1;

eq_9.L("sv1","sv1") = 1;
eq_9.L("sv2","sv2") = 1/0;
eq_9.L("sv3","sv3") = NA;
eq_9.L("sv4","sv4") = +Inf;
eq_9.L("sv5","sv5") = -Inf;
eq_9.L("sv6","sv6") = eps;

eq_9.M("sv1","sv1") = 1;
eq_9.M("sv2","sv2") = 1/0;
eq_9.M("sv3","sv3") = NA;
eq_9.M("sv4","sv4") = +Inf;
eq_9.M("sv5","sv5") = -Inf;
eq_9.M("sv6","sv6") = eps;

eq_9.LO("sv1","sv1") = 1;
eq_9.LO("sv2","sv2") = 1/0;
eq_9.LO("sv3","sv3") = NA;
eq_9.LO("sv4","sv4") = +Inf;
eq_9.LO("sv5","sv5") = -Inf;
eq_9.LO("sv6","sv6") = eps;

eq_9.UP("sv1","sv1") = 1;
eq_9.UP("sv2","sv2") = 1/0;
eq_9.UP("sv3","sv3") = NA;
eq_9.UP("sv4","sv4") = +Inf;
eq_9.UP("sv5","sv5") = -Inf;
eq_9.UP("sv6","sv6") = eps;

eq_9.SCALE("sv1","sv1") = 1;
eq_9.SCALE("sv2","sv2") = 1/0;
eq_9.SCALE("sv3","sv3") = NA;
eq_9.SCALE("sv4","sv4") = +Inf;
eq_9.SCALE("sv5","sv5") = -Inf;
eq_9.SCALE("sv6","sv6") = eps;



* test less than equations
Equation leq_1 "scalar equation leq_1";
Equation leq_2 "scalar equation leq_2";
Equation leq_3 "scalar equation leq_3";
Equation leq_4 "scalar equation leq_4";
Equation leq_5 "scalar equation leq_5";
Equation leq_6 "scalar equation leq_6";

leq_1.. 1 =L= 1;
leq_1.L = 1;
leq_1.M = 1;
leq_1.LO = 1;
leq_1.UP = 1;
leq_1.SCALE = 1;

leq_2.. 1 =L= 1;
leq_2.L = 1/0;
leq_2.M = 1/0;
leq_2.LO = 1/0;
leq_2.UP = 1/0;
leq_2.SCALE = 1/0;

leq_3.. 1 =L= 1;
leq_3.L = NA;
leq_3.M = NA;
leq_3.LO = NA;
leq_3.UP = NA;
leq_3.SCALE = NA;

leq_4.. 1 =L= 1;
leq_4.L = Inf;
leq_4.M = Inf;
leq_4.LO = Inf;
leq_4.UP = Inf;
leq_4.SCALE = Inf;

leq_5.. 1 =L= 1;
leq_5.L = -Inf;
leq_5.M = -Inf;
leq_5.LO = -Inf;
leq_5.UP = -Inf;
leq_5.SCALE = -Inf;

leq_6.. 1 =L= 1;
leq_6.L = eps;
leq_6.M = eps;
leq_6.LO = eps;
leq_6.UP = eps;
leq_6.SCALE = eps;

Equation leq_7(sv) "equation leq_7";

leq_7(sv).. 1 =L= 1;

leq_7.L("sv1") = 1;
leq_7.L("sv2") = 1/0;
leq_7.L("sv3") = NA;
leq_7.L("sv4") = +Inf;
leq_7.L("sv5") = -Inf;
leq_7.L("sv6") = eps;

leq_7.M("sv1") = 1;
leq_7.M("sv2") = 1/0;
leq_7.M("sv3") = NA;
leq_7.M("sv4") = +Inf;
leq_7.M("sv5") = -Inf;
leq_7.M("sv6") = eps;

leq_7.LO("sv1") = 1;
leq_7.LO("sv2") = 1/0;
leq_7.LO("sv3") = NA;
leq_7.LO("sv4") = +Inf;
leq_7.LO("sv5") = -Inf;
leq_7.LO("sv6") = eps;

leq_7.UP("sv1") = 1;
leq_7.UP("sv2") = 1/0;
leq_7.UP("sv3") = NA;
leq_7.UP("sv4") = +Inf;
leq_7.UP("sv5") = -Inf;
leq_7.UP("sv6") = eps;

leq_7.SCALE("sv1") = 1;
leq_7.SCALE("sv2") = 1/0;
leq_7.SCALE("sv3") = NA;
leq_7.SCALE("sv4") = +Inf;
leq_7.SCALE("sv5") = -Inf;
leq_7.SCALE("sv6") = eps;

Equation leq_8(sv,sv) "equation leq_8";
leq_8(sv,sv).. 1 =L= 1;

leq_8.L("sv1","sv1") = 1;
leq_8.L("sv2","sv2") = 1/0;
leq_8.L("sv3","sv3") = NA;
leq_8.L("sv4","sv4") = +Inf;
leq_8.L("sv5","sv5") = -Inf;
leq_8.L("sv6","sv6") = eps;

leq_8.M("sv1","sv1") = 1;
leq_8.M("sv2","sv2") = 1/0;
leq_8.M("sv3","sv3") = NA;
leq_8.M("sv4","sv4") = +Inf;
leq_8.M("sv5","sv5") = -Inf;
leq_8.M("sv6","sv6") = eps;

leq_8.LO("sv1","sv1") = 1;
leq_8.LO("sv2","sv2") = 1/0;
leq_8.LO("sv3","sv3") = NA;
leq_8.LO("sv4","sv4") = +Inf;
leq_8.LO("sv5","sv5") = -Inf;
leq_8.LO("sv6","sv6") = eps;

leq_8.UP("sv1","sv1") = 1;
leq_8.UP("sv2","sv2") = 1/0;
leq_8.UP("sv3","sv3") = NA;
leq_8.UP("sv4","sv4") = +Inf;
leq_8.UP("sv5","sv5") = -Inf;
leq_8.UP("sv6","sv6") = eps;

leq_8.SCALE("sv1","sv1") = 1;
leq_8.SCALE("sv2","sv2") = 1/0;
leq_8.SCALE("sv3","sv3") = NA;
leq_8.SCALE("sv4","sv4") = +Inf;
leq_8.SCALE("sv5","sv5") = -Inf;
leq_8.SCALE("sv6","sv6") = eps;

Equation leq_9(*,sv) "equation leq_9";
leq_9(sv,sv).. 1 =L= 1;

leq_9.L("sv1","sv1") = 1;
leq_9.L("sv2","sv2") = 1/0;
leq_9.L("sv3","sv3") = NA;
leq_9.L("sv4","sv4") = +Inf;
leq_9.L("sv5","sv5") = -Inf;
leq_9.L("sv6","sv6") = eps;

leq_9.M("sv1","sv1") = 1;
leq_9.M("sv2","sv2") = 1/0;
leq_9.M("sv3","sv3") = NA;
leq_9.M("sv4","sv4") = +Inf;
leq_9.M("sv5","sv5") = -Inf;
leq_9.M("sv6","sv6") = eps;

leq_9.LO("sv1","sv1") = 1;
leq_9.LO("sv2","sv2") = 1/0;
leq_9.LO("sv3","sv3") = NA;
leq_9.LO("sv4","sv4") = +Inf;
leq_9.LO("sv5","sv5") = -Inf;
leq_9.LO("sv6","sv6") = eps;

leq_9.UP("sv1","sv1") = 1;
leq_9.UP("sv2","sv2") = 1/0;
leq_9.UP("sv3","sv3") = NA;
leq_9.UP("sv4","sv4") = +Inf;
leq_9.UP("sv5","sv5") = -Inf;
leq_9.UP("sv6","sv6") = eps;

leq_9.SCALE("sv1","sv1") = 1;
leq_9.SCALE("sv2","sv2") = 1/0;
leq_9.SCALE("sv3","sv3") = NA;
leq_9.SCALE("sv4","sv4") = +Inf;
leq_9.SCALE("sv5","sv5") = -Inf;
leq_9.SCALE("sv6","sv6") = eps;

* test greater than equations
Equation geq_1 "scalar equation geq_1";
Equation geq_2 "scalar equation geq_2";
Equation geq_3 "scalar equation geq_3";
Equation geq_4 "scalar equation geq_4";
Equation geq_5 "scalar equation geq_5";
Equation geq_6 "scalar equation geq_6";

geq_1.. 1 =G= 1;
geq_1.L = 1;
geq_1.M = 1;
geq_1.LO = 1;
geq_1.UP = 1;
geq_1.SCALE = 1;

geq_2.. 1 =G= 1;
geq_2.L = 1/0;
geq_2.M = 1/0;
geq_2.LO = 1/0;
geq_2.UP = 1/0;
geq_2.SCALE = 1/0;

geq_3.. 1 =G= 1;
geq_3.L = NA;
geq_3.M = NA;
geq_3.LO = NA;
geq_3.UP = NA;
geq_3.SCALE = NA;

geq_4.. 1 =G= 1;
geq_4.L = Inf;
geq_4.M = Inf;
geq_4.LO = Inf;
geq_4.UP = Inf;
geq_4.SCALE = Inf;

geq_5.. 1 =G= 1;
geq_5.L = -Inf;
geq_5.M = -Inf;
geq_5.LO = -Inf;
geq_5.UP = -Inf;
geq_5.SCALE = -Inf;

geq_6.. 1 =G= 1;
geq_6.L = eps;
geq_6.M = eps;
geq_6.LO = eps;
geq_6.UP = eps;
geq_6.SCALE = eps;

Equation geq_7(sv) "equation geq_7";

geq_7(sv).. 1 =G= 1;

geq_7.L("sv1") = 1;
geq_7.L("sv2") = 1/0;
geq_7.L("sv3") = NA;
geq_7.L("sv4") = +Inf;
geq_7.L("sv5") = -Inf;
geq_7.L("sv6") = eps;

geq_7.M("sv1") = 1;
geq_7.M("sv2") = 1/0;
geq_7.M("sv3") = NA;
geq_7.M("sv4") = +Inf;
geq_7.M("sv5") = -Inf;
geq_7.M("sv6") = eps;

geq_7.LO("sv1") = 1;
geq_7.LO("sv2") = 1/0;
geq_7.LO("sv3") = NA;
geq_7.LO("sv4") = +Inf;
geq_7.LO("sv5") = -Inf;
geq_7.LO("sv6") = eps;

geq_7.UP("sv1") = 1;
geq_7.UP("sv2") = 1/0;
geq_7.UP("sv3") = NA;
geq_7.UP("sv4") = +Inf;
geq_7.UP("sv5") = -Inf;
geq_7.UP("sv6") = eps;

geq_7.SCALE("sv1") = 1;
geq_7.SCALE("sv2") = 1/0;
geq_7.SCALE("sv3") = NA;
geq_7.SCALE("sv4") = +Inf;
geq_7.SCALE("sv5") = -Inf;
geq_7.SCALE("sv6") = eps;

Equation geq_8(sv,sv) "equation geq_8";
geq_8(sv,sv).. 1 =G= 1;

geq_8.L("sv1","sv1") = 1;
geq_8.L("sv2","sv2") = 1/0;
geq_8.L("sv3","sv3") = NA;
geq_8.L("sv4","sv4") = +Inf;
geq_8.L("sv5","sv5") = -Inf;
geq_8.L("sv6","sv6") = eps;

geq_8.M("sv1","sv1") = 1;
geq_8.M("sv2","sv2") = 1/0;
geq_8.M("sv3","sv3") = NA;
geq_8.M("sv4","sv4") = +Inf;
geq_8.M("sv5","sv5") = -Inf;
geq_8.M("sv6","sv6") = eps;

geq_8.LO("sv1","sv1") = 1;
geq_8.LO("sv2","sv2") = 1/0;
geq_8.LO("sv3","sv3") = NA;
geq_8.LO("sv4","sv4") = +Inf;
geq_8.LO("sv5","sv5") = -Inf;
geq_8.LO("sv6","sv6") = eps;

geq_8.UP("sv1","sv1") = 1;
geq_8.UP("sv2","sv2") = 1/0;
geq_8.UP("sv3","sv3") = NA;
geq_8.UP("sv4","sv4") = +Inf;
geq_8.UP("sv5","sv5") = -Inf;
geq_8.UP("sv6","sv6") = eps;

geq_8.SCALE("sv1","sv1") = 1;
geq_8.SCALE("sv2","sv2") = 1/0;
geq_8.SCALE("sv3","sv3") = NA;
geq_8.SCALE("sv4","sv4") = +Inf;
geq_8.SCALE("sv5","sv5") = -Inf;
geq_8.SCALE("sv6","sv6") = eps;

Equation geq_9(*,sv) "equation geq_9";
geq_9(sv,sv).. 1 =G= 1;

geq_9.L("sv1","sv1") = 1;
geq_9.L("sv2","sv2") = 1/0;
geq_9.L("sv3","sv3") = NA;
geq_9.L("sv4","sv4") = +Inf;
geq_9.L("sv5","sv5") = -Inf;
geq_9.L("sv6","sv6") = eps;

geq_9.M("sv1","sv1") = 1;
geq_9.M("sv2","sv2") = 1/0;
geq_9.M("sv3","sv3") = NA;
geq_9.M("sv4","sv4") = +Inf;
geq_9.M("sv5","sv5") = -Inf;
geq_9.M("sv6","sv6") = eps;

geq_9.LO("sv1","sv1") = 1;
geq_9.LO("sv2","sv2") = 1/0;
geq_9.LO("sv3","sv3") = NA;
geq_9.LO("sv4","sv4") = +Inf;
geq_9.LO("sv5","sv5") = -Inf;
geq_9.LO("sv6","sv6") = eps;

geq_9.UP("sv1","sv1") = 1;
geq_9.UP("sv2","sv2") = 1/0;
geq_9.UP("sv3","sv3") = NA;
geq_9.UP("sv4","sv4") = +Inf;
geq_9.UP("sv5","sv5") = -Inf;
geq_9.UP("sv6","sv6") = eps;

geq_9.SCALE("sv1","sv1") = 1;
geq_9.SCALE("sv2","sv2") = 1/0;
geq_9.SCALE("sv3","sv3") = NA;
geq_9.SCALE("sv4","sv4") = +Inf;
geq_9.SCALE("sv5","sv5") = -Inf;
geq_9.SCALE("sv6","sv6") = eps;


* test nonbinding equations
Equation nb_1 "scalar equation nb_1";
Equation nb_2 "scalar equation nb_2";
Equation nb_3 "scalar equation nb_3";
Equation nb_4 "scalar equation nb_4";
Equation nb_5 "scalar equation nb_5";
Equation nb_6 "scalar equation nb_6";

nb_1.. 1 =N= 1;
nb_1.L = 1;
nb_1.M = 1;
nb_1.LO = 1;
nb_1.UP = 1;
nb_1.SCALE = 1;

nb_2.. 1 =N= 1;
nb_2.L = 1/0;
nb_2.M = 1/0;
nb_2.LO = 1/0;
nb_2.UP = 1/0;
nb_2.SCALE = 1/0;

nb_3.. 1 =N= 1;
nb_3.L = NA;
nb_3.M = NA;
nb_3.LO = NA;
nb_3.UP = NA;
nb_3.SCALE = NA;

nb_4.. 1 =N= 1;
nb_4.L = Inf;
nb_4.M = Inf;
nb_4.LO = Inf;
nb_4.UP = Inf;
nb_4.SCALE = Inf;

nb_5.. 1 =N= 1;
nb_5.L = -Inf;
nb_5.M = -Inf;
nb_5.LO = -Inf;
nb_5.UP = -Inf;
nb_5.SCALE = -Inf;

nb_6.. 1 =N= 1;
nb_6.L = eps;
nb_6.M = eps;
nb_6.LO = eps;
nb_6.UP = eps;
nb_6.SCALE = eps;

Equation nb_7(sv) "equation nb_7";

nb_7(sv).. 1 =N= 1;

nb_7.L("sv1") = 1;
nb_7.L("sv2") = 1/0;
nb_7.L("sv3") = NA;
nb_7.L("sv4") = +Inf;
nb_7.L("sv5") = -Inf;
nb_7.L("sv6") = eps;

nb_7.M("sv1") = 1;
nb_7.M("sv2") = 1/0;
nb_7.M("sv3") = NA;
nb_7.M("sv4") = +Inf;
nb_7.M("sv5") = -Inf;
nb_7.M("sv6") = eps;

nb_7.LO("sv1") = 1;
nb_7.LO("sv2") = 1/0;
nb_7.LO("sv3") = NA;
nb_7.LO("sv4") = +Inf;
nb_7.LO("sv5") = -Inf;
nb_7.LO("sv6") = eps;

nb_7.UP("sv1") = 1;
nb_7.UP("sv2") = 1/0;
nb_7.UP("sv3") = NA;
nb_7.UP("sv4") = +Inf;
nb_7.UP("sv5") = -Inf;
nb_7.UP("sv6") = eps;

nb_7.SCALE("sv1") = 1;
nb_7.SCALE("sv2") = 1/0;
nb_7.SCALE("sv3") = NA;
nb_7.SCALE("sv4") = +Inf;
nb_7.SCALE("sv5") = -Inf;
nb_7.SCALE("sv6") = eps;

Equation nb_8(sv,sv) "equation nb_8";
nb_8(sv,sv).. 1 =N= 1;

nb_8.L("sv1","sv1") = 1;
nb_8.L("sv2","sv2") = 1/0;
nb_8.L("sv3","sv3") = NA;
nb_8.L("sv4","sv4") = +Inf;
nb_8.L("sv5","sv5") = -Inf;
nb_8.L("sv6","sv6") = eps;

nb_8.M("sv1","sv1") = 1;
nb_8.M("sv2","sv2") = 1/0;
nb_8.M("sv3","sv3") = NA;
nb_8.M("sv4","sv4") = +Inf;
nb_8.M("sv5","sv5") = -Inf;
nb_8.M("sv6","sv6") = eps;

nb_8.LO("sv1","sv1") = 1;
nb_8.LO("sv2","sv2") = 1/0;
nb_8.LO("sv3","sv3") = NA;
nb_8.LO("sv4","sv4") = +Inf;
nb_8.LO("sv5","sv5") = -Inf;
nb_8.LO("sv6","sv6") = eps;

nb_8.UP("sv1","sv1") = 1;
nb_8.UP("sv2","sv2") = 1/0;
nb_8.UP("sv3","sv3") = NA;
nb_8.UP("sv4","sv4") = +Inf;
nb_8.UP("sv5","sv5") = -Inf;
nb_8.UP("sv6","sv6") = eps;

nb_8.SCALE("sv1","sv1") = 1;
nb_8.SCALE("sv2","sv2") = 1/0;
nb_8.SCALE("sv3","sv3") = NA;
nb_8.SCALE("sv4","sv4") = +Inf;
nb_8.SCALE("sv5","sv5") = -Inf;
nb_8.SCALE("sv6","sv6") = eps;

Equation nb_9(*,sv) "equation nb_9";
nb_9(sv,sv).. 1 =N= 1;

nb_9.L("sv1","sv1") = 1;
nb_9.L("sv2","sv2") = 1/0;
nb_9.L("sv3","sv3") = NA;
nb_9.L("sv4","sv4") = +Inf;
nb_9.L("sv5","sv5") = -Inf;
nb_9.L("sv6","sv6") = eps;

nb_9.M("sv1","sv1") = 1;
nb_9.M("sv2","sv2") = 1/0;
nb_9.M("sv3","sv3") = NA;
nb_9.M("sv4","sv4") = +Inf;
nb_9.M("sv5","sv5") = -Inf;
nb_9.M("sv6","sv6") = eps;

nb_9.LO("sv1","sv1") = 1;
nb_9.LO("sv2","sv2") = 1/0;
nb_9.LO("sv3","sv3") = NA;
nb_9.LO("sv4","sv4") = +Inf;
nb_9.LO("sv5","sv5") = -Inf;
nb_9.LO("sv6","sv6") = eps;

nb_9.UP("sv1","sv1") = 1;
nb_9.UP("sv2","sv2") = 1/0;
nb_9.UP("sv3","sv3") = NA;
nb_9.UP("sv4","sv4") = +Inf;
nb_9.UP("sv5","sv5") = -Inf;
nb_9.UP("sv6","sv6") = eps;

nb_9.SCALE("sv1","sv1") = 1;
nb_9.SCALE("sv2","sv2") = 1/0;
nb_9.SCALE("sv3","sv3") = NA;
nb_9.SCALE("sv4","sv4") = +Inf;
nb_9.SCALE("sv5","sv5") = -Inf;
nb_9.SCALE("sv6","sv6") = eps;

* test cone equations
Equation cone_1 "scalar equation cone_1";
Equation cone_2 "scalar equation cone_2";
Equation cone_3 "scalar equation cone_3";
Equation cone_4 "scalar equation cone_4";
Equation cone_5 "scalar equation cone_5";
Equation cone_6 "scalar equation cone_6";

cone_1.. 1 =C= 1;
cone_1.L = 1;
cone_1.M = 1;
cone_1.LO = 1;
cone_1.UP = 1;
cone_1.SCALE = 1;

cone_2.. 1 =C= 1;
cone_2.L = 1/0;
cone_2.M = 1/0;
cone_2.LO = 1/0;
cone_2.UP = 1/0;
cone_2.SCALE = 1/0;

cone_3.. 1 =C= 1;
cone_3.L = NA;
cone_3.M = NA;
cone_3.LO = NA;
cone_3.UP = NA;
cone_3.SCALE = NA;

cone_4.. 1 =C= 1;
cone_4.L = Inf;
cone_4.M = Inf;
cone_4.LO = Inf;
cone_4.UP = Inf;
cone_4.SCALE = Inf;

cone_5.. 1 =C= 1;
cone_5.L = -Inf;
cone_5.M = -Inf;
cone_5.LO = -Inf;
cone_5.UP = -Inf;
cone_5.SCALE = -Inf;

cone_6.. 1 =C= 1;
cone_6.L = eps;
cone_6.M = eps;
cone_6.LO = eps;
cone_6.UP = eps;
cone_6.SCALE = eps;

Equation cone_7(sv) "equation cone_7";

cone_7(sv).. 1 =C= 1;

cone_7.L("sv1") = 1;
cone_7.L("sv2") = 1/0;
cone_7.L("sv3") = NA;
cone_7.L("sv4") = +Inf;
cone_7.L("sv5") = -Inf;
cone_7.L("sv6") = eps;

cone_7.M("sv1") = 1;
cone_7.M("sv2") = 1/0;
cone_7.M("sv3") = NA;
cone_7.M("sv4") = +Inf;
cone_7.M("sv5") = -Inf;
cone_7.M("sv6") = eps;

cone_7.LO("sv1") = 1;
cone_7.LO("sv2") = 1/0;
cone_7.LO("sv3") = NA;
cone_7.LO("sv4") = +Inf;
cone_7.LO("sv5") = -Inf;
cone_7.LO("sv6") = eps;

cone_7.UP("sv1") = 1;
cone_7.UP("sv2") = 1/0;
cone_7.UP("sv3") = NA;
cone_7.UP("sv4") = +Inf;
cone_7.UP("sv5") = -Inf;
cone_7.UP("sv6") = eps;

cone_7.SCALE("sv1") = 1;
cone_7.SCALE("sv2") = 1/0;
cone_7.SCALE("sv3") = NA;
cone_7.SCALE("sv4") = +Inf;
cone_7.SCALE("sv5") = -Inf;
cone_7.SCALE("sv6") = eps;

Equation cone_8(sv,sv) "equation cone_8";
cone_8(sv,sv).. 1 =C= 1;

cone_8.L("sv1","sv1") = 1;
cone_8.L("sv2","sv2") = 1/0;
cone_8.L("sv3","sv3") = NA;
cone_8.L("sv4","sv4") = +Inf;
cone_8.L("sv5","sv5") = -Inf;
cone_8.L("sv6","sv6") = eps;

cone_8.M("sv1","sv1") = 1;
cone_8.M("sv2","sv2") = 1/0;
cone_8.M("sv3","sv3") = NA;
cone_8.M("sv4","sv4") = +Inf;
cone_8.M("sv5","sv5") = -Inf;
cone_8.M("sv6","sv6") = eps;

cone_8.LO("sv1","sv1") = 1;
cone_8.LO("sv2","sv2") = 1/0;
cone_8.LO("sv3","sv3") = NA;
cone_8.LO("sv4","sv4") = +Inf;
cone_8.LO("sv5","sv5") = -Inf;
cone_8.LO("sv6","sv6") = eps;

cone_8.UP("sv1","sv1") = 1;
cone_8.UP("sv2","sv2") = 1/0;
cone_8.UP("sv3","sv3") = NA;
cone_8.UP("sv4","sv4") = +Inf;
cone_8.UP("sv5","sv5") = -Inf;
cone_8.UP("sv6","sv6") = eps;

cone_8.SCALE("sv1","sv1") = 1;
cone_8.SCALE("sv2","sv2") = 1/0;
cone_8.SCALE("sv3","sv3") = NA;
cone_8.SCALE("sv4","sv4") = +Inf;
cone_8.SCALE("sv5","sv5") = -Inf;
cone_8.SCALE("sv6","sv6") = eps;

Equation cone_9(*,sv) "equation cone_9";
cone_9(sv,sv).. 1 =C= 1;

cone_9.L("sv1","sv1") = 1;
cone_9.L("sv2","sv2") = 1/0;
cone_9.L("sv3","sv3") = NA;
cone_9.L("sv4","sv4") = +Inf;
cone_9.L("sv5","sv5") = -Inf;
cone_9.L("sv6","sv6") = eps;

cone_9.M("sv1","sv1") = 1;
cone_9.M("sv2","sv2") = 1/0;
cone_9.M("sv3","sv3") = NA;
cone_9.M("sv4","sv4") = +Inf;
cone_9.M("sv5","sv5") = -Inf;
cone_9.M("sv6","sv6") = eps;

cone_9.LO("sv1","sv1") = 1;
cone_9.LO("sv2","sv2") = 1/0;
cone_9.LO("sv3","sv3") = NA;
cone_9.LO("sv4","sv4") = +Inf;
cone_9.LO("sv5","sv5") = -Inf;
cone_9.LO("sv6","sv6") = eps;

cone_9.UP("sv1","sv1") = 1;
cone_9.UP("sv2","sv2") = 1/0;
cone_9.UP("sv3","sv3") = NA;
cone_9.UP("sv4","sv4") = +Inf;
cone_9.UP("sv5","sv5") = -Inf;
cone_9.UP("sv6","sv6") = eps;

cone_9.SCALE("sv1","sv1") = 1;
cone_9.SCALE("sv2","sv2") = 1/0;
cone_9.SCALE("sv3","sv3") = NA;
cone_9.SCALE("sv4","sv4") = +Inf;
cone_9.SCALE("sv5","sv5") = -Inf;
cone_9.SCALE("sv6","sv6") = eps;

* test bool equations
Equation bool_1 "scalar equation bool_1";
Equation bool_2 "scalar equation bool_2";
Equation bool_3 "scalar equation bool_3";
Equation bool_4 "scalar equation bool_4";
Equation bool_5 "scalar equation bool_5";
Equation bool_6 "scalar equation bool_6";

bool_1.. 1 =B= 1;
bool_1.L = 1;
bool_1.M = 1;
bool_1.LO = 1;
bool_1.UP = 1;
bool_1.SCALE = 1;

bool_2.. 1 =B= 1;
bool_2.L = 1/0;
bool_2.M = 1/0;
bool_2.LO = 1/0;
bool_2.UP = 1/0;
bool_2.SCALE = 1/0;

bool_3.. 1 =B= 1;
bool_3.L = NA;
bool_3.M = NA;
bool_3.LO = NA;
bool_3.UP = NA;
bool_3.SCALE = NA;

bool_4.. 1 =B= 1;
bool_4.L = Inf;
bool_4.M = Inf;
bool_4.LO = Inf;
bool_4.UP = Inf;
bool_4.SCALE = Inf;

bool_5.. 1 =B= 1;
bool_5.L = -Inf;
bool_5.M = -Inf;
bool_5.LO = -Inf;
bool_5.UP = -Inf;
bool_5.SCALE = -Inf;

bool_6.. 1 =B= 1;
bool_6.L = eps;
bool_6.M = eps;
bool_6.LO = eps;
bool_6.UP = eps;
bool_6.SCALE = eps;

Equation bool_7(sv) "equation bool_7";

bool_7(sv).. 1 =B= 1;

bool_7.L("sv1") = 1;
bool_7.L("sv2") = 1/0;
bool_7.L("sv3") = NA;
bool_7.L("sv4") = +Inf;
bool_7.L("sv5") = -Inf;
bool_7.L("sv6") = eps;

bool_7.M("sv1") = 1;
bool_7.M("sv2") = 1/0;
bool_7.M("sv3") = NA;
bool_7.M("sv4") = +Inf;
bool_7.M("sv5") = -Inf;
bool_7.M("sv6") = eps;

bool_7.LO("sv1") = 1;
bool_7.LO("sv2") = 1/0;
bool_7.LO("sv3") = NA;
bool_7.LO("sv4") = +Inf;
bool_7.LO("sv5") = -Inf;
bool_7.LO("sv6") = eps;

bool_7.UP("sv1") = 1;
bool_7.UP("sv2") = 1/0;
bool_7.UP("sv3") = NA;
bool_7.UP("sv4") = +Inf;
bool_7.UP("sv5") = -Inf;
bool_7.UP("sv6") = eps;

bool_7.SCALE("sv1") = 1;
bool_7.SCALE("sv2") = 1/0;
bool_7.SCALE("sv3") = NA;
bool_7.SCALE("sv4") = +Inf;
bool_7.SCALE("sv5") = -Inf;
bool_7.SCALE("sv6") = eps;

Equation bool_8(sv,sv) "equation bool_8";
bool_8(sv,sv).. 1 =B= 1;

bool_8.L("sv1","sv1") = 1;
bool_8.L("sv2","sv2") = 1/0;
bool_8.L("sv3","sv3") = NA;
bool_8.L("sv4","sv4") = +Inf;
bool_8.L("sv5","sv5") = -Inf;
bool_8.L("sv6","sv6") = eps;

bool_8.M("sv1","sv1") = 1;
bool_8.M("sv2","sv2") = 1/0;
bool_8.M("sv3","sv3") = NA;
bool_8.M("sv4","sv4") = +Inf;
bool_8.M("sv5","sv5") = -Inf;
bool_8.M("sv6","sv6") = eps;

bool_8.LO("sv1","sv1") = 1;
bool_8.LO("sv2","sv2") = 1/0;
bool_8.LO("sv3","sv3") = NA;
bool_8.LO("sv4","sv4") = +Inf;
bool_8.LO("sv5","sv5") = -Inf;
bool_8.LO("sv6","sv6") = eps;

bool_8.UP("sv1","sv1") = 1;
bool_8.UP("sv2","sv2") = 1/0;
bool_8.UP("sv3","sv3") = NA;
bool_8.UP("sv4","sv4") = +Inf;
bool_8.UP("sv5","sv5") = -Inf;
bool_8.UP("sv6","sv6") = eps;

bool_8.SCALE("sv1","sv1") = 1;
bool_8.SCALE("sv2","sv2") = 1/0;
bool_8.SCALE("sv3","sv3") = NA;
bool_8.SCALE("sv4","sv4") = +Inf;
bool_8.SCALE("sv5","sv5") = -Inf;
bool_8.SCALE("sv6","sv6") = eps;

Equation bool_9(*,sv) "equation bool_9";
bool_9(sv,sv).. 1 =B= 1;

bool_9.L("sv1","sv1") = 1;
bool_9.L("sv2","sv2") = 1/0;
bool_9.L("sv3","sv3") = NA;
bool_9.L("sv4","sv4") = +Inf;
bool_9.L("sv5","sv5") = -Inf;
bool_9.L("sv6","sv6") = eps;

bool_9.M("sv1","sv1") = 1;
bool_9.M("sv2","sv2") = 1/0;
bool_9.M("sv3","sv3") = NA;
bool_9.M("sv4","sv4") = +Inf;
bool_9.M("sv5","sv5") = -Inf;
bool_9.M("sv6","sv6") = eps;

bool_9.LO("sv1","sv1") = 1;
bool_9.LO("sv2","sv2") = 1/0;
bool_9.LO("sv3","sv3") = NA;
bool_9.LO("sv4","sv4") = +Inf;
bool_9.LO("sv5","sv5") = -Inf;
bool_9.LO("sv6","sv6") = eps;

bool_9.UP("sv1","sv1") = 1;
bool_9.UP("sv2","sv2") = 1/0;
bool_9.UP("sv3","sv3") = NA;
bool_9.UP("sv4","sv4") = +Inf;
bool_9.UP("sv5","sv5") = -Inf;
bool_9.UP("sv6","sv6") = eps;

bool_9.SCALE("sv1","sv1") = 1;
bool_9.SCALE("sv2","sv2") = 1/0;
bool_9.SCALE("sv3","sv3") = NA;
bool_9.SCALE("sv4","sv4") = +Inf;
bool_9.SCALE("sv5","sv5") = -Inf;
bool_9.SCALE("sv6","sv6") = eps;

* test external equations
Equation ext_1 "scalar equation ext_1";
Equation ext_2 "scalar equation ext_2";
Equation ext_3 "scalar equation ext_3";
Equation ext_4 "scalar equation ext_4";
Equation ext_5 "scalar equation ext_5";
Equation ext_6 "scalar equation ext_6";

ext_1.. 1 =X= 1;
ext_1.L = 1;
ext_1.M = 1;
ext_1.LO = 1;
ext_1.UP = 1;
ext_1.SCALE = 1;

ext_2.. 1 =X= 1;
ext_2.L = 1/0;
ext_2.M = 1/0;
ext_2.LO = 1/0;
ext_2.UP = 1/0;
ext_2.SCALE = 1/0;

ext_3.. 1 =X= 1;
ext_3.L = NA;
ext_3.M = NA;
ext_3.LO = NA;
ext_3.UP = NA;
ext_3.SCALE = NA;

ext_4.. 1 =X= 1;
ext_4.L = Inf;
ext_4.M = Inf;
ext_4.LO = Inf;
ext_4.UP = Inf;
ext_4.SCALE = Inf;

ext_5.. 1 =X= 1;
ext_5.L = -Inf;
ext_5.M = -Inf;
ext_5.LO = -Inf;
ext_5.UP = -Inf;
ext_5.SCALE = -Inf;

ext_6.. 1 =X= 1;
ext_6.L = eps;
ext_6.M = eps;
ext_6.LO = eps;
ext_6.UP = eps;
ext_6.SCALE = eps;

Equation ext_7(sv) "equation ext_7";

ext_7(sv).. 1 =X= 1;

ext_7.L("sv1") = 1;
ext_7.L("sv2") = 1/0;
ext_7.L("sv3") = NA;
ext_7.L("sv4") = +Inf;
ext_7.L("sv5") = -Inf;
ext_7.L("sv6") = eps;

ext_7.M("sv1") = 1;
ext_7.M("sv2") = 1/0;
ext_7.M("sv3") = NA;
ext_7.M("sv4") = +Inf;
ext_7.M("sv5") = -Inf;
ext_7.M("sv6") = eps;

ext_7.LO("sv1") = 1;
ext_7.LO("sv2") = 1/0;
ext_7.LO("sv3") = NA;
ext_7.LO("sv4") = +Inf;
ext_7.LO("sv5") = -Inf;
ext_7.LO("sv6") = eps;

ext_7.UP("sv1") = 1;
ext_7.UP("sv2") = 1/0;
ext_7.UP("sv3") = NA;
ext_7.UP("sv4") = +Inf;
ext_7.UP("sv5") = -Inf;
ext_7.UP("sv6") = eps;

ext_7.SCALE("sv1") = 1;
ext_7.SCALE("sv2") = 1/0;
ext_7.SCALE("sv3") = NA;
ext_7.SCALE("sv4") = +Inf;
ext_7.SCALE("sv5") = -Inf;
ext_7.SCALE("sv6") = eps;

Equation ext_8(sv,sv) "equation ext_8";
ext_8(sv,sv).. 1 =X= 1;

ext_8.L("sv1","sv1") = 1;
ext_8.L("sv2","sv2") = 1/0;
ext_8.L("sv3","sv3") = NA;
ext_8.L("sv4","sv4") = +Inf;
ext_8.L("sv5","sv5") = -Inf;
ext_8.L("sv6","sv6") = eps;

ext_8.M("sv1","sv1") = 1;
ext_8.M("sv2","sv2") = 1/0;
ext_8.M("sv3","sv3") = NA;
ext_8.M("sv4","sv4") = +Inf;
ext_8.M("sv5","sv5") = -Inf;
ext_8.M("sv6","sv6") = eps;

ext_8.LO("sv1","sv1") = 1;
ext_8.LO("sv2","sv2") = 1/0;
ext_8.LO("sv3","sv3") = NA;
ext_8.LO("sv4","sv4") = +Inf;
ext_8.LO("sv5","sv5") = -Inf;
ext_8.LO("sv6","sv6") = eps;

ext_8.UP("sv1","sv1") = 1;
ext_8.UP("sv2","sv2") = 1/0;
ext_8.UP("sv3","sv3") = NA;
ext_8.UP("sv4","sv4") = +Inf;
ext_8.UP("sv5","sv5") = -Inf;
ext_8.UP("sv6","sv6") = eps;

ext_8.SCALE("sv1","sv1") = 1;
ext_8.SCALE("sv2","sv2") = 1/0;
ext_8.SCALE("sv3","sv3") = NA;
ext_8.SCALE("sv4","sv4") = +Inf;
ext_8.SCALE("sv5","sv5") = -Inf;
ext_8.SCALE("sv6","sv6") = eps;

Equation ext_9(*,sv) "equation ext_9";
ext_9(sv,sv).. 1 =X= 1;

ext_9.L("sv1","sv1") = 1;
ext_9.L("sv2","sv2") = 1/0;
ext_9.L("sv3","sv3") = NA;
ext_9.L("sv4","sv4") = +Inf;
ext_9.L("sv5","sv5") = -Inf;
ext_9.L("sv6","sv6") = eps;

ext_9.M("sv1","sv1") = 1;
ext_9.M("sv2","sv2") = 1/0;
ext_9.M("sv3","sv3") = NA;
ext_9.M("sv4","sv4") = +Inf;
ext_9.M("sv5","sv5") = -Inf;
ext_9.M("sv6","sv6") = eps;

ext_9.LO("sv1","sv1") = 1;
ext_9.LO("sv2","sv2") = 1/0;
ext_9.LO("sv3","sv3") = NA;
ext_9.LO("sv4","sv4") = +Inf;
ext_9.LO("sv5","sv5") = -Inf;
ext_9.LO("sv6","sv6") = eps;

ext_9.UP("sv1","sv1") = 1;
ext_9.UP("sv2","sv2") = 1/0;
ext_9.UP("sv3","sv3") = NA;
ext_9.UP("sv4","sv4") = +Inf;
ext_9.UP("sv5","sv5") = -Inf;
ext_9.UP("sv6","sv6") = eps;

ext_9.SCALE("sv1","sv1") = 1;
ext_9.SCALE("sv2","sv2") = 1/0;
ext_9.SCALE("sv3","sv3") = NA;
ext_9.SCALE("sv4","sv4") = +Inf;
ext_9.SCALE("sv5","sv5") = -Inf;
ext_9.SCALE("sv6","sv6") = eps;

ExecError = 0;

execute_unload "data.gdx";

'
  write(gams_text, "data.gms")
  ret = system2(command="gams", args= 
  paste0(testthat::test_path("data.gms"), " gdx=data.gdx"), 
  stdout = TRUE, stderr = TRUE)

  m = Container$new()

  # read all symbols
  m$read(testthat::test_path("data.gdx"))

  # write everything
  m$write(testthat::test_path("gt.gdx"))

  ret <- system2(command="gdxdiff", args=
  paste0(testthat::test_path("data.gdx"), " ", testthat::test_path("gt.gdx")),
  stdout = FALSE)
  expect_equal(ret, 0)
}
)


test_that("test_num_1", {
  m <- Container$new()
  expect_true(inherits(m, "Container"))

  i <- Set$new(m, "i", records = c("a", "b"))
  expect_true(is.data.frame(i$records))
  expect_equal(nrow(i$records), 2)

  j <- Set$new(m, "j", records = c("c", "d"))
  expect_true(is.data.frame(j$records))
  expect_equal(nrow(j$records), 2)

  recs <- data.frame(list("i" = c("a", "b"), 
  "j" = c("c", "d"), "values" = c(1, 1)))
  a <- Parameter$new(m, "a", c("i", "j"), records = recs)
  expect_true(is.data.frame(a$records))
  expect_equal(nrow(a$records), 2)

  m$write("gt.gdx")


  # gams syntax
  gams_text = '
  set i / a, b /;
  set j / c, d /;

  parameter a(i,j) /
  "a"."c" 1
  "b"."d" 1
  /;
  '


  write(gams_text, "data.gms")
  ret = system2(command="gams", args= 
  paste0(testthat::test_path("data.gms"), " gdx=data.gdx"), 
  stdout = TRUE, stderr = TRUE)

  ret <- system2(command="gdxdiff", args=
  paste0(testthat::test_path("data.gdx"), " ", testthat::test_path("gt.gdx")),
  stdout = FALSE)

  expect_equal(ret, 0)
}
)

test_that("test_num_2", {
  m <- Container$new()
  expect_true(inherits(m, "Container"))

  i <- Set$new(m, "i")
  expect_true(is.null(i$records))

  j <- Set$new(m, "j")
  expect_true(is.null(j$records))

  recs <- data.frame(list("i" = c("a", "b"), 
  "j" = c("c", "d"), "values" = c(1, 1)))
  a <- Parameter$new(m, "a", c(i, j), recs, domainForwarding = TRUE)

  expect_true(is.data.frame(i$records))
  expect_equal(as.character(i$records$uni_1), c("a", "b"))

  expect_true(is.data.frame(j$records))
  expect_equal(as.character(j$records$uni_1), c("c", "d"))

  m$write("gt.gdx")

  # gams syntax
  gams_text = '
  set i;
  set j;

  parameter a(i<,j<) /
  "a"."c" 1
  "b"."d" 1
  /;
  '
  write(gams_text, "data.gms")
  ret = system2(command="gams", args= 
  paste0(testthat::test_path("data.gms"), " gdx=data.gdx"), 
  stdout = TRUE, stderr = TRUE)

  ret <- system2(command="gdxdiff", args=
  paste0(testthat::test_path("data.gdx"), " ", testthat::test_path("gt.gdx")),
  stdout = FALSE)

  expect_equal(ret, 0)
}
)

test_that("test_num_3", {
  m <- Container$new()
  expect_true(inherits(m, "Container"))

  i <- Set$new(m, "i")
  expect_true(is.null(i$records))

  j <- Alias$new(m, "j", i)
  expect_true(is.null(j$records))

  recs <- data.frame(list("i" = c("a", "b"), 
  "j" = c("a", "b"), "values" = c(1, 1)))
  a <- Parameter$new(m, "a", c(i, j), recs, domainForwarding = TRUE)

  expect_true(is.data.frame(i$records))
  expect_equal(as.character(i$records$uni_1), c("a", "b"))

  expect_true(is.data.frame(j$records))
  expect_equal(as.character(j$records$uni_1), c("a", "b"))
  expect_equal(nrow(a$records), 2)

  m$write("gt.gdx")

  # gams syntax
  gams_text = '
  set i / a, b /;
  alias(i, j);

  parameter a(i,j) /
  "a"."a" 1
  "b"."b" 1
  /;
  '
  write(gams_text, "data.gms")
  ret = system2(command="gams", args= 
  paste0(testthat::test_path("data.gms"), " gdx=data.gdx"), 
  stdout = TRUE, stderr = TRUE)

  ret <- system2(command="gdxdiff", args=
  paste0(testthat::test_path("data.gdx"), " ", testthat::test_path("gt.gdx")),
  stdout = FALSE)

  expect_equal(ret, 0)
}
)

test_that("test_num_4", {
  m <- Container$new()

  i <- Set$new(m, "i")
  expect_true(is.null(i$records))

  j <- Set$new(m, "j", i)
  expect_true(is.null(j$records))

  k <- Set$new(m, "k", j)
  expect_true(is.null(k$records))

  l = Set$new(m, "l", k, records = c("a", "b"), domainForwarding = TRUE )
  expect_true(is.data.frame(i$records))
  expect_equal(as.character(i$records$uni_1), c("a", "b"))

  expect_true(is.data.frame(j$records))
  expect_equal(as.character(j$records$i_1), c("a", "b"))

  expect_true(is.data.frame(k$records))
  expect_equal(as.character(k$records$j_1), c("a", "b"))

  expect_true(is.data.frame(l$records))
  expect_equal(as.character(l$records$k_1), c("a", "b"))

  m$write("gt.gdx")

  # gams syntax
  gams_text = '
  set i  / a,b /;
  set j(i) / a,b /;
  set k(j) / a,b /;
  set l(k) / a,b /;
  '

  write(gams_text, "data.gms")
  ret = system2(command="gams", args= 
  paste0(testthat::test_path("data.gms"), " gdx=data.gdx"), 
  stdout = TRUE, stderr = TRUE)

  ret <- system2(command="gdxdiff", args=
  paste0(testthat::test_path("data.gdx"), " ", testthat::test_path("gt.gdx")),
  stdout = FALSE)

  expect_equal(ret, 0)
}
)

test_that("test_num_5", {
  m <- Container$new()
  expect_true(inherits(m, "Container"))

  i <- Set$new(m, "i")
  expect_true(is.null(i$records))

  recs <- data.frame(list("i" = "c", "element_text" = "desc for elem 'c'"))
  j <- Set$new(m, "j", i, records = recs, domainForwarding = TRUE)
  expect_true(is.data.frame(i$records))
  expect_equal(as.character(i$records$uni_1), c("c"))
  expect_true(is.data.frame(j$records))
  expect_equal(as.character(j$records$i_1), c("c"))

  k <- Set$new(m, "k", j)
  expect_true(is.data.frame(i$records))
  expect_equal(as.character(i$records$uni_1), c("c"))
  expect_true(is.data.frame(j$records))
  expect_equal(as.character(j$records$i_1), c("c"))
  expect_true(is.null(k$records))

  l <- Set$new(m, "l", k, records = c("a", "b"), domainForwarding = TRUE)
  expect_true(is.data.frame(i$records))
  expect_equal(as.character(i$records$uni_1), c("c", "a", "b"))
  expect_true(is.data.frame(j$records))
  expect_equal(as.character(j$records$i_1), c("c", "a", "b"))

  expect_true(is.data.frame(k$records))
  expect_equal(as.character(k$records$j_1), c("a", "b"))

  expect_true(is.data.frame(l$records))
  expect_equal(as.character(l$records$k_1), c("a", "b"))

  m$write("gt.gdx")

  # gams syntax
  gams_text = '
  set i  / c, a, b /;
  set j(i) / c "desc for elem \'c\'", a, b /;
  set k(j) / a, b /;
  set l(k) / a, b /;
  '

  write(gams_text, "data.gms")
  ret = system2(command="gams", args= 
  paste0(testthat::test_path("data.gms"), " gdx=data.gdx"), 
  stdout = TRUE, stderr = TRUE)

  ret <- system2(command="gdxdiff", args=
  paste0(testthat::test_path("data.gdx"), " ", testthat::test_path("gt.gdx")),
  stdout = FALSE)

  expect_equal(ret, 0)
}
)

test_that("test_num_6", {
  m <- Container$new()
  expect_true(inherits(m, "Container"))

  i <- Set$new(m, "i", records = c("c", "a", "b"))
  expect_true(is.data.frame(i$records))
  m$write("gt.gdx")

  # gams syntax
  gams_text = '
  set i  / c, a, b /;
  '

  write(gams_text, "data.gms")
  ret = system2(command="gams", args= 
  paste0(testthat::test_path("data.gms"), " gdx=data.gdx"), 
  stdout = TRUE, stderr = TRUE)

  ret <- system2(command="gdxdump", args=
  paste(testthat::test_path("data.gdx"), "output=uels.gms uelTable=foo"),
  stdout = FALSE)

  ret = system2(command="gams", args= 
  paste0(testthat::test_path("uels.gms"), " gdx=uels.gdx"), 
  stdout = TRUE, stderr = TRUE)

  m2 = Container$new(testthat::test_path("uels.gdx"))
  expect_true(inherits(m2, "Container"))
  expect_true(is.data.frame(m2$data$foo$records))

  expect_equal(as.character(m$getUniverseSet()), as.character(m2$data$foo$records$uni_1))
}
)

test_that("test_num_7", {
  m <- Container$new()
  expect_true(inherits(m, "Container"))

  i <- Set$new(m, "i", records = c("c", "a", "b"))
  expect_true(is.data.frame(i$records))

  m$write("gt.gdx", uelPriority = list("a"))

  # gams syntax
  gams_text = '
  set i  / a, c, b /;
  '

  write(gams_text, "data.gms")
  ret = system2(command="gams", args= 
  paste0(testthat::test_path("data.gms"), " gdx=data.gdx"), 
  stdout = TRUE, stderr = TRUE)

  ret <- system2(command="gdxdiff", args=
  paste0(testthat::test_path("data.gdx"), " ", testthat::test_path("gt.gdx")),
  stdout = FALSE)
  expect_equal(ret, 0)

  ret <- system2(command="gdxdump", args=
  paste(testthat::test_path("data.gdx"), "output=uels.gms uelTable=foo"),
  stdout = FALSE)

  ret = system2(command="gams", args= 
  paste0(testthat::test_path("uels.gms"), " gdx=uels.gdx"), 
  stdout = TRUE, stderr = TRUE)

  m2 = Container$new(testthat::test_path("uels.gdx"))
  expect_true(inherits(m2, "Container"))
  expect_true(is.data.frame(m2$data$foo$records))

  expect_equal(c("a", "c", "b"), as.character(m2$data$foo$records$uni_1))
  
}
)

test_that("test_num_8", {
  m <- Container$new()
  expect_true(inherits(m, "Container"))

  i <- Set$new(m, "i", records = c("a", "b"))
  expect_equal(as.character(i$records$uni_1), c("a", "b"))

  j <- Alias$new(m, "j", i)
  expect_equal(as.character(j$records$uni_1), c("a", "b"))
  expect_equal(j$aliasWith$name, "i")

  k <- Alias$new(m, "k", j)
  expect_equal(as.character(k$records$uni_1), c("a", "b"))
  expect_equal(j$aliasWith$name, "i")

  #try writing
  m$write("out.gdx")
}
)

test_that("test_num_9", {
  m <- Container$new()
  expect_true(inherits(m, "Container"))

  i <- Set$new(m, "i", list("*", "*"), records = 
  data.frame(list("col1"=c("a","b"), "col2"=c("c","d"))))

  j <- Parameter$new(m, "j", "*", records = 
  data.frame(list("col1" = c("e", "f"), "col2" = c(1, 1))))
  expect_equal(as.character(i$records$uni_1), c("a", "b"))
  expect_equal(as.character(i$records$uni_2), c("c", "d"))

  expect_equal(as.character(m$getUniverseSet()), c("a", "c", "b", "d", "e", "f"))

  #just try writing to see if there are any errors
  m$write("out.gdx")
}
)

test_that("test_num_10", {
  m <- Container$new()
  expect_true(inherits(m, "Container"))

  i <- Set$new(m, "i")
  expect_true(is.null(i$records))

  j <- Set$new(m, "j")
  expect_true(is.null(j$records))

  a <- Parameter$new(m, "a", list(i, j), domainForwarding=TRUE)

  df <- data.frame(list("i_1"= c("a", "b"),
  "j_2" = c("c", "d"), "value" = c(1, 1)))

  a$records <- df

  expect_equal(as.character(i$records$uni_1), c("a", "b"))
  expect_equal(as.character(j$records$uni_1), c("c", "d"))

  #try writing
  m$write("out.gdx")
}
)

test_that("test_num_11", {
  m <- Container$new()
  expect_true(inherits(m, "Container"))

  i <- Set$new(m, "i", records = c("a", "b", "c"))
  expect_equal(as.character(i$records$uni_1), c("a", "b", "c"))


  j <- Parameter$new(m, "j", i, 
  records = data.frame("j"=c("a", "c"), "val" = c(1, 2)))
  expect_equal(as.character(j$records$i_1), c("a", "c"))
  expect_equal(j$records$value, c(1, 2))

  m$removeSymbols(c("i", "j"))

  i <- Set$new(m, "i", records = c("a", "b", "c", "d"))
  j <- Parameter$new(m, "j", i, 
  records = data.frame("i"= c("a", "c"), "val" = c(1, 2)))
  expect_equal(j$toDense(), array(c(1,0,2,0)))
  m$write("out.gdx")
}
)

test_that("test_num_12", {
  # gams syntax
  gams_text = "
        set i / i1 * i3 /;
        acronym
          small 'baby bear'
          medium 'mama bear'
          large 'papa bear'
          ;
          parameter b(i) /
          i1 1
          i2 medium
          i3 large
          /;
        execute_unload '%system.fn%.gdx';
  "

  write(gams_text, "data.gms")
  ret = system2(command="gams", args= 
  paste0(testthat::test_path("data.gms"), " gdx=data.gdx"), 
  stdout = TRUE, stderr = TRUE)

  testthat::expect_warning(Container$new(testthat::test_path("data.gdx")))
}
)

test_that("test_num_13", {
  m  = Container$new()
  i = Set$new(m, "i", records=c("a", "b", "c"))
  j = Set$new(m, "j", records=c("d", "e", "f"))
  a = Parameter$new(m, "a", i, records=data.frame("i"=c("a","c"),"val"=c(1, 2) ))
  b = Parameter$new(m, "b", j, records=data.frame("j"=c("e","f"),"val"=c(1, 2) ))

  m$removeSymbols("i")
  expect_equal(names(m$data), c("j", "a", "b"))

  m$removeSymbols(c("a", "b"))
  expect_equal(names(m$data), c("j"))

  m$write("out.gdx")
}
)


test_that("test_num_14", {
  m  = Container$new()
  i = Set$new(m, "i", records=c("a", "b", "c"))

  expect_equal(names(m$data), c("i"))

  m$renameSymbol("i", "h")
  expect_equal(names(m$data), c("h"))

  m$write("out.gdx")
}
)

test_that("test_num_15", {
  m  = Container$new()
  i = Set$new(m, "i", records=c("a", "b", "c"))
  a = Parameter$new(m, "a", i, records=data.frame(c("aa", "c"), c(1, 2)))

  expect_equal(a$findDomainViolations(), 1)
    expect_equal(a$isValid(), FALSE)
}
)

test_that("test_num_16", {
  m  = Container$new()
  i = Set$new(m, "i")
  j = Set$new(m, "j", i, records = c("a", "b"), domainForwarding = TRUE)
  k = Set$new(m, "k")
  l = Set$new(m, "l", k, records = c("c"), domainForwarding = TRUE)
  a = Parameter$new(m, "a", l, records = data.frame(c("aa", "c"), c(1, 2)), domainForwarding = TRUE)

  # check container
  expect_equal(m$.requiresStateCheck, TRUE)
  expect_equal(m$isValid(), TRUE)
  expect_equal(m$.requiresStateCheck, FALSE)

  expect_equal(as.character(m$data[["l"]]$records$k_1), c("c", "aa"))
  expect_equal(as.character(m$data[["k"]]$records$uni_1), c("c", "aa"))
  expect_equal(as.character(m$data[["j"]]$records$i_1), c("a", "b"))
  expect_equal(as.character(m$data[["i"]]$records$uni_1), c("a", "b"))

  expect_equal(as.character(m$getUniverseSet()), c("a", "b", "c", "aa"))
  expect_equal(a$isValid(), TRUE)
  expect_equal(l$isValid(), TRUE)
}
)

test_that("test_num_17", {
  m  = Container$new()
  m$addSet("i")
  m$addSet("j", m$data[["i"]], records = c("a", "b"), domainForwarding = TRUE)
  m$addSet("k")
  m$addSet("l", m$data[["k"]], records = c("c"), domainForwarding = TRUE)

  m$addParameter("a", m$data[["l"]], records = data.frame(c("aa", "c"), c(1, 2)), domainForwarding = TRUE)

  # check container
  expect_equal(m$.requiresStateCheck, TRUE)
  expect_equal(m$isValid(), TRUE)
  expect_equal(m$.requiresStateCheck, FALSE)

  expect_equal(as.character(m$data[["l"]]$records$k_1), c("c", "aa"))
  expect_equal(as.character(m$data[["k"]]$records$uni_1), c("c", "aa"))
  expect_equal(as.character(m$data[["j"]]$records$i_1), c("a", "b"))
  expect_equal(as.character(m$data[["i"]]$records$uni_1), c("a", "b"))

  expect_equal(as.character(m$getUniverseSet()), c("a", "b", "c", "aa"))

  expect_equal(m$listSymbols(isValid = FALSE), NULL)
}
)

test_that("test_num_18", {

    default_values = list(
    "binary" = list(
        "level" = 0.0,
        "marginal" = 0.0,
        "lower" = 0.0,
        "upper" = 1.0,
        "scale" = 1.0
    ),
    "integer" = list(
        "level" = 0.0,
        "marginal" = 0.0,
        "lower" = 0.0,
        "upper" = SpecialValues$POSINF,
        "scale" = 1.0
    ),
    "positive" = list(
        "level" = 0.0,
        "marginal" = 0.0,
        "lower" = 0.0,
        "upper" = SpecialValues$POSINF,
        "scale" = 1.0
    ),
    "negative" = list(
        "level" = 0.0,
        "marginal" = 0.0,
        "lower" = SpecialValues$NEGINF,
        "upper" = 0.0,
        "scale" = 1.0
    ),
    "free" = list(
        "level" = 0.0,
        "marginal" = 0.0,
        "lower" = SpecialValues$NEGINF,
        "upper" = SpecialValues$POSINF,
        "scale" = 1.0
    ),
    "sos1" = list(
        "level" = 0.0,
        "marginal" = 0.0,
        "lower" = 0.0,
        "upper" = SpecialValues$POSINF,
        "scale" = 1.0
    ),
    "sos2" = list(
        "level" = 0.0,
        "marginal" = 0.0,
        "lower" = 0.0,
        "upper" = SpecialValues$POSINF,
        "scale" = 1.0
    ),
    "semicont" = list(
        "level" = 0.0,
        "marginal" = 0.0,
        "lower" = 1.0,
        "upper" = SpecialValues$POSINF,
        "scale" = 1.0
    ),
    "semiint" = list(
        "level" = 0.0,
        "marginal" = 0.0,
        "lower" = 1.0,
        "upper" = SpecialValues$POSINF,
        "scale" = 1.0
    )
  )
  m  = Container$new()
  df = data.frame("domain"=c("i0"))

  types = c(
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

  for (i in types) {
    varname = paste0("a_", i)
    m$addVariable(varname, i, "*", records = df)

    expect_equal(colnames(m$data[[varname]]$records),
    c("uni_1", "level", "marginal", "lower", "upper", "scale"))

    expect_equal(m$data[[varname]]$records[1, "level"], 
    default_values[[i]][["level"]])

    expect_equal(m$data[[varname]]$records[1, "marginal"], 
    default_values[[i]][["marginal"]])

    expect_equal(m$data[[varname]]$records[1, "lower"], 
    default_values[[i]][["lower"]])

    expect_equal(m$data[[varname]]$records[1, "upper"], 
    default_values[[i]][["upper"]])

    expect_equal(m$data[[varname]]$records[1, "scale"], 
    default_values[[i]][["scale"]])
  }
}
)

test_that("test_num_19", {

  default_values = list(
      "eq" = list(
          "level" = 0.0,
          "marginal" = 0.0,
          "lower" = 0.0,
          "upper" = 0.0,
          "scale" = 1.0
      ),
      "geq" = list(
          "level" = 0.0,
          "marginal" = 0.0,
          "lower" = 0.0,
          "upper" = SpecialValues$POSINF,
          "scale" = 1.0
      ),
      "leq" = list(
          "level" = 0.0,
          "marginal" = 0.0,
          "lower" = SpecialValues$NEGINF,
          "upper" = 0.0,
          "scale" = 1.0
      ),
      "nonbinding" = list(
          "level" = 0.0,
          "marginal" = 0.0,
          "lower" = SpecialValues$NEGINF,
          "upper" = SpecialValues$POSINF,
          "scale" = 1.0
      ),
      "cone" = list(
          "level" = 0.0,
          "marginal" = 0.0,
          "lower" = 0.0,
          "upper" = SpecialValues$POSINF,
          "scale" = 1.0
      ),
      "external" = list(
          "level" = 0.0,
          "marginal" = 0.0,
          "lower" = 0.0,
          "upper" = 0.0,
          "scale" = 1.0
      ),
      "boolean" = list(
          "level" = 0.0,
          "marginal" = 0.0,
          "lower" = 0.0,
          "upper" = 0.0,
          "scale" = 1.0
      )
  )

  m  = Container$new()
  df = data.frame("domain"= c("i0"))

  types = c(
  "eq", "geq", "leq", "nonbinding", "cone", "external", "boolean"
  )

  for (i in types) {
    eqname = paste0("a_", i)
    m$addEquation(eqname, i, "*", records = df)

    expect_equal(colnames(m$data[[eqname]]$records),
    c("uni_1", "level", "marginal", "lower", "upper", "scale"))

    expect_equal(m$data[[eqname]]$records[1, "level"], 
    default_values[[i]][["level"]])

    expect_equal(m$data[[eqname]]$records[1, "marginal"], 
    default_values[[i]][["marginal"]])

    expect_equal(m$data[[eqname]]$records[1, "lower"], 
    default_values[[i]][["lower"]])

    expect_equal(m$data[[eqname]]$records[1, "upper"], 
    default_values[[i]][["upper"]])

    expect_equal(m$data[[eqname]]$records[1, "scale"], 
    default_values[[i]][["scale"]])
  }
}
)

test_that("test_num_20", {
  m = Container$new()
  df=  data.frame(list("i0", 1, 1, 1, 1, 1))
  colnames(df)= c("domain", "marginal", "lower", "scale", "upper", "level")

  type = c(
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

  for (i in type) {
    varname = paste0("a_", i)
    m$addVariable(varname, i, "*", records = df)
    expect_equal(colnames(m$data[[varname]]$records),
    c("uni_1", "level", "marginal", "lower", "upper", "scale"))
  }
}
)

test_that("test_num_21", {
  m = Container$new()
  df=  data.frame(list("i0", 1, 1, 1, 1, 1))
  colnames(df)= c("domain", "marginal", "lower", "scale", "upper", "level")

  type = c(
    "eq", "geq", "leq", "nonbinding", "cone", "external", "boolean"
  )

  for (i in type) {
    eqname = paste0("a_", i)
    m$addEquation(eqname, i, "*", records = df)
    expect_equal(colnames(m$data[[eqname]]$records),
    c("uni_1", "level", "marginal", "lower", "upper", "scale"))
  }
}
)


test_that("test_num_22", {
  # gams syntax
  gams_text = "
    Set
        i 'canning plants' / seattle,  san-diego /
        j 'markets'        / new-york, chicago, topeka /;

    Parameter
        a(i) 'capacity of plant i in cases'
            / seattle    350
              san-diego  600 /

        b(j) 'demand at market j in cases'
            / new-york   325
              chicago    300
              topeka     275 /;

    Table d(i,j) 'distance in thousands of miles'
                  new-york  chicago  topeka
        seattle         2.5      1.7     1.8
        san-diego       2.5      1.8     1.4;

    Scalar f 'freight in dollars per case per thousand miles' / 90 /;

    Parameter c(i,j) 'transport cost in thousands of dollars per case';
    c(i,j) = f*d(i,j)/1000;

    Variable
        x(i,j) 'shipment quantities in cases'
        z      'total transportation costs in thousands of dollars';

    Positive Variable x;

    Equation
        cost      'define objective function'
        supply(i) 'observe supply limit at plant i'
        demand(j) 'satisfy demand at market j';

    cost..      z =e= sum((i,j), c(i,j)*x(i,j));

    supply(i).. sum(j, x(i,j)) =l= a(i);

    demand(j).. sum(i, x(i,j)) =g= b(j);

    Model transport / all /;

    solve transport using lp minimizing z;
  "

  write(gams_text, "data.gms")
  ret = system2(command="gams", args= 
  paste0(testthat::test_path("data.gms"), " gdx=data.gdx"), 
  stdout = TRUE, stderr = TRUE)

  m = Container$new()
  m$read(testthat::test_path("data.gdx"), records = FALSE)

  for (i in m$data) {
    expect_equal(m$data[[i$name]]$records, NULL)
  }
}
)


test_that("test_num_23", {
  # gams syntax
  gams_text = "
    Set
        i 'canning plants' / seattle,  san-diego /
        j 'markets'        / new-york, chicago, topeka /;

    Parameter
        a(i) 'capacity of plant i in cases'
            / seattle    350
              san-diego  600 /

        b(j) 'demand at market j in cases'
            / new-york   325
              chicago    300
              topeka     275 /;

    Table d(i,j) 'distance in thousands of miles'
                  new-york  chicago  topeka
        seattle         2.5      1.7     1.8
        san-diego       2.5      1.8     1.4;

    Scalar f 'freight in dollars per case per thousand miles' / 90 /;

    Parameter c(i,j) 'transport cost in thousands of dollars per case';
    c(i,j) = f*d(i,j)/1000;

    Variable
        x(i,j) 'shipment quantities in cases'
        z      'total transportation costs in thousands of dollars';

    Positive Variable x;

    Equation
        cost      'define objective function'
        supply(i) 'observe supply limit at plant i'
        demand(j) 'satisfy demand at market j';

    cost..      z =e= sum((i,j), c(i,j)*x(i,j));

    supply(i).. sum(j, x(i,j)) =l= a(i);

    demand(j).. sum(i, x(i,j)) =g= b(j);

    Model transport / all /;

    solve transport using lp minimizing z;
  "

  write(gams_text, "data.gms")
  ret = system2(command="gams", args= 
  paste0(testthat::test_path("data.gms"), " gdx=data.gdx"), 
  stdout = TRUE, stderr = TRUE)

  m = Container$new()
  m$read(testthat::test_path("data.gdx"), c("i", "j", "x"))

  expect_equal(m$data[["i"]]$domainType, "none")
  expect_equal(m$data[["j"]]$domainType, "none")
  expect_equal(m$data[["x"]]$domainType, "regular")
}
)

test_that("test_num_24", {
  # gams syntax
  gams_text = "
    Set
        i 'canning plants' / seattle,  san-diego /
        j 'markets'        / new-york, chicago, topeka /;

    Parameter
        a(i) 'capacity of plant i in cases'
            / seattle    350
              san-diego  600 /

        b(j) 'demand at market j in cases'
            / new-york   325
              chicago    300
              topeka     275 /;

    Table d(i,j) 'distance in thousands of miles'
                  new-york  chicago  topeka
        seattle         2.5      1.7     1.8
        san-diego       2.5      1.8     1.4;

    Scalar f 'freight in dollars per case per thousand miles' / 90 /;

    Parameter c(i,j) 'transport cost in thousands of dollars per case';
    c(i,j) = f*d(i,j)/1000;

    Variable
        x(i,j) 'shipment quantities in cases'
        z      'total transportation costs in thousands of dollars';

    Positive Variable x;

    Equation
        cost      'define objective function'
        supply(i) 'observe supply limit at plant i'
        demand(j) 'satisfy demand at market j';

    cost..      z =e= sum((i,j), c(i,j)*x(i,j));

    supply(i).. sum(j, x(i,j)) =l= a(i);

    demand(j).. sum(i, x(i,j)) =g= b(j);

    Model transport / all /;

    solve transport using lp minimizing z;
  "

  write(gams_text, "data.gms")
  ret = system2(command="gams", args= 
  paste0(testthat::test_path("data.gms"), " gdx=data.gdx"), 
  stdout = TRUE, stderr = TRUE)

  m = Container$new()
  m$read(testthat::test_path("data.gdx"), c("x"))

  expect_equal(names(m$data), "x")
  expect_equal(m$data[["x"]]$domainType, "relaxed")
}
)

test_that("test_num_25", {
  # gams syntax
  gams_text = "
    Set i(i) / a,b,c /;
  "

  write(gams_text, "data.gms")
  ret = system2(command="gams", args= 
  paste0(testthat::test_path("data.gms"), " gdx=data.gdx"), 
  stdout = TRUE, stderr = TRUE)

  m = Container$new(testthat::test_path("data.gdx"))

  expect_equal(names(m$data), "i")
  expect_equal(m$data[["i"]]$domain, list("i"))
  expect_equal(m$data[["i"]]$domainType, "relaxed")
}
)


test_that("test_num_26", {
  m = Container$new()
  i = Set$new(m, "i", "p")
  expect_equal(i$domainType, "relaxed")

  j = Set$new(m, "j", i, records = 
  data.frame(c("c"), c("desc for elem 'c'")), domainForwarding=TRUE)

  df = data.frame("p_1" =c("c"), "element_text" = c(""))
  df$p_1 = factor(df$p_1, ordered = TRUE)
  expect_equal(i$records, df)

  df = data.frame("i_1" =c("c"), "element_text" = c("desc for elem 'c'"))
  df$i_1 = factor(df$i_1, ordered = TRUE)
  expect_equal(j$records, df)

  k = Set$new(m ,"k", "j")
  expect_equal(k$records, NULL)

  l = Set$new(m, "l", k, records = c("a", "b"), domainForwarding = TRUE)

  # test l
  df = data.frame("k_1"=c("a", "b"), "element_text"=c("", ""))
  df$k_1 = factor(df$k_1, ordered=TRUE)
  expect_equal(l$records, df)

  # test k
  df = data.frame("j_1"=c("a", "b"), "element_text"=c("",""))
  df$j_1 = factor(df$j_1, ordered = TRUE)
  expect_equal(k$records, df)

  # test j
  df = data.frame("i_1"=c("c"), "element_text"=c("desc for elem 'c'"))
  df$i_1 = factor(df$i_1, ordered = TRUE)
  expect_equal(j$records, df)

  # test i
  df = data.frame("p_1"=c("c"), "element_text"=c(""))
  df$p_1 = factor(df$p_1, ordered = TRUE)
  expect_equal(i$records, df)

}
)

test_that("test_num_27", {
  # gams syntax
  gams_text = "
    Set i(*);
    Set j(*);
    Set a(a);
    Set k(i);
    Set l(i);
    Set m(j);
    Set n(j);
    Set o(l);
    Set p(m);
    Set b(a);
    Set c(a);
    Set d(b);
    Set e(b);
  "

  write(gams_text, "data.gms")
  ret = system2(command="gams", args= 
  paste0(testthat::test_path("data.gms"), " gdx=data.gdx"), 
  stdout = TRUE, stderr = TRUE)

  m = Container$new()
  m$read(testthat::test_path("data.gdx"))

  # write everything
  m$write(testthat::test_path("gt.gdx"))

  ret <- system2(command="gdxdiff", args=
  paste0(testthat::test_path("data.gdx"), " ", testthat::test_path("gt.gdx")),
  stdout = FALSE)
  expect_equal(ret, 0)
}
)

test_that("test_num_28", {
  m = Container$new()

  i = Set$new(m, "i")
  a_eq = Equation$new(m, "a_eq", "eq", i)
  a_geq = Equation$new(m, "a_geq", "geq", i)
  a_leq = Equation$new(m, "a_leq", "leq", i)
  a_nonbinding = Equation$new(m, "a_nonbinding", "nonbinding", i)
  a_cone = Equation$new(m, "a_cone", "cone", i)
  a_external = Equation$new(m, "a_external", "external", i)
  a_boolean = Equation$new(m, "a_boolean", "boolean", i)
  # try writing

  expect_equal(m$write("gt.gdx"), NULL)
}
)

test_that("test_num_29", {
  m = Container$new()

  i = Set$new(m, "i")
  a_binary = Variable$new(m, "a_binary", "binary", i)
  a_integer = Variable$new(m, "a_integer", "integer", i)
  a_positive = Variable$new(m, "a_positive", "positive", i)
  a_negative = Variable$new(m, "a_negative", "negative", i)
  a_free = Variable$new(m, "a_free", "free", i)
  a_sos1 = Variable$new(m, "a_sos1", "sos1", i)
  a_sos2 = Variable$new(m, "a_sos2", "sos2", i)
  a_semicont = Variable$new(m, "a_semicont", "semicont", i)
  a_semiint = Variable$new(m, "a_semiint", "semiint", i)

  # try writing
  expect_equal(m$write("gt.gdx"), NULL)
}
)

test_that("test_num_30", {
  m = Container$new()
  i = Set$new(m, "i")

  a = Parameter$new(m, "a", i)
  # try writing
  expect_equal(m$write("gt.gdx"), NULL)
}
)

test_that("test_num_31", {
  m = Container$new()
  i = Set$new(m, "i", "j")
  j = Set$new(m, "j", "i")

  a = Parameter$new(m, "a", c(i, j))
  m$removeSymbols("j")
  j = Set$new(m, "j", "i")
  expect_error(a$isValid(verbose=TRUE))

}
)

test_that("test_num_32", {
  m = Container$new()
  i = Set$new(m, "i")
  m$isValid()

  expect_equal(m$.requiresStateCheck, FALSE)

  j = Set$new(m, "j")

  expect_true(m$.requiresStateCheck)
}
)

test_that("test_num_33", {
  m = Container$new()
  i = Set$new(m, "i")
  j = Set$new(m, "j")
  k = Set$new(m, "k")

  i$domain = j
  j$domain = k
  k$domain = i

  expect_error(m$.__enclos_env__$private$validSymbolOrder())
}
)

test_that("test_num_34", {
  m = Container$new()
  i = Set$new(m, "i")
  j = Alias$new(m, "j", i)
  j$setRecords(c("a", "b"))
  df = data.frame("uni_1"=c("a", "b"), "element_text"=c("",""))
  df$uni_1 = factor(df$uni_1, ordered=TRUE)

  expect_equal(i$records, df)
}
)

test_that("test_num_35", {
  m = Container$new()
  i = Set$new(m, "i")
  j = Alias$new(m, "j", i)
  j$description = "just a test"
  expect_equal(i$description, "just a test")
}
)

test_that("test_num_36", {
  m = Container$new()
  i = Set$new(m, "i")
  k = Set$new(m, "k")
  j = Alias$new(m, "j", i)
  j$domain = c(k, k)
  expect_equal(i$domainNames, c("k", "k"))
}
)

test_that("test_num_37", {
  m = Container$new()
  i = Set$new(m, "i")
  j = Alias$new(m, "j", i)
  j$isSingleton = TRUE
  expect_equal(i$isSingleton, TRUE)
}
)

test_that("test_num_38", {
  m = Container$new()
  i = Set$new(m, "i")

  expect_equal(i$dimension, 1)
  expect_equal(i$domain, list("*"))

  i$dimension = 2
  expect_equal(i$dimension, 2)
  expect_equal(i$domain, list("*", "*"))

  i$dimension = 0
  expect_equal(i$dimension, 0)
  expect_equal(i$domain, list())

  a = Parameter$new(m, "a")
  expect_equal(a$dimension, 0)
  expect_equal(a$domain, list())
  expect_equal(a$isScalar, TRUE)

  a$dimension = 2
  expect_equal(a$domain, list("*", "*"))
  expect_equal(a$isScalar, FALSE)

  a$dimension = 0
  expect_equal(a$domain, list())
  expect_equal(a$isScalar, TRUE)

  ip = Alias$new(m, "ip", i)
  ip$dimension = 2
  expect_equal(ip$dimension, 2)
  expect_equal(i$dimension, 2)
  expect_equal(ip$domain, list("*", "*"))
  expect_equal(i$domain, list("*", "*"))
}
)

test_that("test_num_39", {
  m = Container$new()
  i = Set$new(m, "i")
  expect_equal(i$numberRecords, 0)
  m$removeSymbols("i")

  i = Set$new(m, "i", records = c("a", "b"))
  expect_equal(i$numberRecords, 2)

  j = Set$new(m, "j", i, records = c("a", "c"))

  expect_true(is.na(j$numberRecords)) #because NA
}
)

test_that("test_num_40", {
  # gams syntax
  gams_text = "
    set i / a, b /;
    set j / c, d /;

    parameter a(i,j) //;
  "

  write(gams_text, "data.gms")
  ret = system2(command="gams", args= 
  paste0(testthat::test_path("data.gms"), " gdx=data.gdx"), 
  stdout = TRUE, stderr = TRUE)

  m = Container$new(testthat::test_path("data.gdx"))
  expect_true(m$data$a$domainType == "regular")
  expect_true(is.null(m$data$a$records))
}
)

test_that("test_num_41", {
  # test for matrix input of parameter records 2D
  m = Container$new()
  i = Set$new(m, "i", records = c("a","b"))
  j = Set$new(m, "j", records = c("x","y","z"))

  recs = matrix(c(1:6), nrow = 2, ncol=3)
  d = Parameter$new(m, "d", c(i, j), records = recs)

  df = data.frame(i_1 = c("a","a", "a", "b","b","b"), 
  j_2 = c("x", "y", "z", "x", "y", "z"),
  value = c(1,3,5,2,4,6))
  df[,1] = factor(df[,1], ordered = TRUE)
  df[,2] = factor(df[,2], ordered = TRUE)


  expect_equal(d$records, df)

  # test for array input of parameter records 2D
  m$removeSymbols("d")
  recs = array(c(1:6), dim=c(2,3))
  d = Parameter$new(m, "d", c(i, j), records = recs)
  expect_equal(d$records, df)

  #test for array 3D
  recs = array(c(1:12), dim=c(2,3,2))
  k = Set$new(m, "k", records = c("alpha", "beta"))
  d3 = Parameter$new(m, "d3", c(i, j, k), records = recs)

  df = data.frame(i_1 = c("a","a", "a", "a","a","a", "b", "b","b","b", "b", "b"), 
  j_2 = c("x", "x", "y", "y", "z", "z", "x", "x", "y", "y", "z", "z"),
  k_3 = c("alpha","beta","alpha","beta","alpha","beta",
  "alpha","beta","alpha","beta","alpha","beta"),
  value = c(1,7,3,9,5,11,2,8,4,10,6,12))
  df[,1] = factor(df[,1], ordered = TRUE)
  df[,2] = factor(df[,2], ordered = TRUE)
  df[,3] = factor(df[,3], ordered = TRUE)
  expect_equal(d3$records, df)
}
)

test_that("test_num_42", {
  m = Container$new()
  i = Set$new(m, "i", records = paste0("i", c(1:5)))
  j = Set$new(m, "j", records = paste0("j", c(1:5)))
  recs=data.frame("i"=c("i1","i2","i3","i4","i5"), 
  "j"=c("j1","j2","j3","j4","j5"), "val"=c(1,1,1,1,1))
  a = Parameter$new(m, "a", domain=c(i, j), records = recs)
  ap = Parameter$new(m, "ap", domain=c(i, j), records = a$toDense())
  expect_equal(a$records, ap$records)

  v = Variable$new(m, "v", domain=c(i, j), records = list("level"=a$toDense()))

  df = data.frame(i_1=i$records[,1], 
  j_2=j$records[,1],
  level= c(1,1,1,1,1),
  marginal=c(0,0,0,0,0),
  lower = replicate(5, -Inf),
  upper = replicate(5, Inf),
  scale = replicate(5, 1)
  )

  expect_equal(v$records, df)

  v2 = Variable$new(m, "v2", domain=c(i, j),
  records = list(
    "level" = a$toDense(),
    marginal = a$toDense(),
    lower = a$toDense(),
    upper = a$toDense(),
    scale = a$toDense()
  )
  )

  e = Equation$new(m, "e", "eq", domain=c(i, j),
  records = list(
    "level" = a$toDense(),
    marginal = a$toDense(),
    lower = a$toDense(),
    upper = a$toDense(),
    scale = a$toDense()
  )
  )

  df = data.frame(i_1=i$records[,1], 
  j_2=j$records[,1],
  level= c(1,1,1,1,1),
  marginal = replicate(5, 1),
  lower = replicate(5, 1),
  upper = replicate(5, 1),
  scale = replicate(5, 1)
  )

  expect_equal(v2$records, df)
  expect_equal(e$records, df)
}
)

# test converting arrays to records for Parameters, Varaibles, Equations
test_that("test_num_43", {
  m = Container$new()
  i = Set$new(m, "i", records = paste0("i", c(1:5)))
  j = Set$new(m, "j", records = paste0("j", c(1:5)))
  recs=data.frame("i"=c("i1","i2","i3","i4","i5"), 
  "j"=c("j1","j2","j3","j4","j5"), "val"=c(0,0,0,0,0))
  recs[2,"val"] = 1
  recs[4, "val"] = SpecialValues$EPS
  recs = recs[-c(1,3,5),]
  row.names(recs) <- NULL

  recs2 = data.frame("i"=c("i1","i2","i3","i4","i5"), 
  "j"=c("j1","j2","j3","j4","j5"), "val"=c(0,0,0,0,0))
  recs2["val"] = SpecialValues$EPS

  a = Parameter$new(m, "a", domain=c(i, j), records = recs)
  ap = Parameter$new(m, "ap", domain=c(i, j), records = recs2)

  v = Variable$new(m, "v", domain=c(i, j),
  records = list(
    "level"=a$toDense(),
    "marginal"= ap$toDense()
  ))

  e = Equation$new(m, "e", "eq", domain=c(i, j),
  records = list(
    "level"=a$toDense(),
    "marginal"= ap$toDense()
  ))

  cols = v$domainLabels
  cols = append(cols, c("level", "marginal", "lower", "upper", "scale"))

  recs=data.frame("i_1"=c("i1","i2","i3","i4","i5"), 
  "j_2"=c("j1","j2","j3","j4","j5"), "level"=c(0,0,0,0,0),
  "marginal"=replicate(5, SpecialValues$EPS),
  "lower" = replicate(5, SpecialValues$NEGINF),
  "upper" = replicate(5, SpecialValues$POSINF),
  "scale" = replicate(5, 1))
  recs[,1] = factor(recs[,1], ordered=TRUE)
  recs[,2] = factor(recs[,2], ordered=TRUE)
  recs[2,"level"] = 1
  recs[4, "level"] = SpecialValues$EPS

  expect_equal(v$records, recs)

  recs=data.frame("i_1"=c("i1","i2","i3","i4","i5"), 
  "j_2"=c("j1","j2","j3","j4","j5"), "level"=c(0,0,0,0,0),
  "marginal"=replicate(5, SpecialValues$EPS),
  "lower" = replicate(5, 0),
  "upper" = replicate(5, 0),
  "scale" = replicate(5, 1))
  recs[,1] = factor(recs[,1], ordered=TRUE)
  recs[,2] = factor(recs[,2], ordered=TRUE)
  recs[2,"level"] = 1
  recs[4, "level"] = SpecialValues$EPS
  
  expect_equal(e$records, recs)
}
)

test_that("test_num_44", {
  # gams syntax
  gams_text = "
    variable i(*);
  "

  write(gams_text, "data.gms")
  ret = system2(command="gams", args= 
  paste0(testthat::test_path("data.gms"), " gdx=data.gdx"), 
  stdout = TRUE, stderr = TRUE)

  m = Container$new(testthat::test_path("data.gdx"))
  expect_true(m$data$i$type == "free")

  m$write("out.gdx")

  m2 = Container$new(testthat::test_path("out.gdx"))
  expect_true(m2$data$i$type == "free")
}
)

test_that("test_num_45", {
  m = Container$new()
  i = Set$new(m, "i", records = paste0("i", 1:5))
  j = Set$new(m, "j", i, records = paste0("i", 1:5))

  m$removeSymbols("i")

  expect_true(is.null(i$refContainer))
  expect_true(names(m$data) == c("j"))
  expect_true(j$isValid() == FALSE)

  expect_error(Container$new(m))

  m2 = Container$new()
  expect_error(m2$read(m))
}
)

test_that("test_num_46", {
  # gams syntax
  gams_text = '
* test sets
Set i_1 "set i_1" / i1, i2 "i2 element text" /;
Set i_2 "set i_2" / 1 "1 element text", 2 /;

Set j_1(i_1,i_2)  "set j_1" / #i_1.#i_2 /;
Set j_2(i_1,*)  "set j_2" / #i_1.#i_2 /;
Set j_3(*,i_2)  "set j_3" / #i_1.#i_2 /;

Singleton Set k_1 "singleton_set k_1" / k1 /;
Singleton Set k_2(i_1,i_2) "singleton_set k_2";
k_2("i1","1") = yes;

Singleton Set k_3(i_1,*) "singleton_set k_3";
k_3("i1","uni") = yes;

Singleton Set k_4(*,i_2) "singleton_set k_4";
k_4("uni","2") = yes;

* test aliases
Alias(i_1, i_1_p);
Alias(i_2, i_2_p);
Alias(j_1, j_1_p);
Alias(j_2, j_2_p);
Alias(j_3, j_3_p);
Alias(k_1, k_1_p);
Alias(k_2, k_2_p);
Alias(k_3, k_3_p);

* test scalar
Scalar s_1 "scalar s_1";
Scalar s_2 "scalar s_2";
Scalar s_3 "scalar s_3";
Scalar s_4 "scalar s_4";
Scalar s_5 "scalar s_5";
Scalar s_6 "scalar s_6";
s_1 = 1;
s_2 = 1/0;
s_3 = NA;
s_4 = Inf;
s_5 = -Inf;
s_6 = eps;

* test parameters
Set sv / sv1, sv2, sv3, sv4, sv5, sv6 /;

Parameter p_1(sv) "parameter p_1";
p_1("sv1") = 1;
p_1("sv2") = 1/0;
p_1("sv3") = NA;
p_1("sv4") = Inf;
p_1("sv5") = -Inf;
p_1("sv6") = eps;

Parameter p_2(sv,sv) "parameter p_2";
p_2("sv1","sv1") = 1;
p_2("sv2","sv2") = 1/0;
p_2("sv3","sv3") = NA;
p_2("sv4","sv4") = Inf;
p_2("sv5","sv5") = -Inf;
p_2("sv6","sv6") = eps;

Parameter p_3(*,sv) "parameter p_3";
p_3("sv1","sv1") = 1;
p_3("sv2","sv2") = 1/0;
p_3("sv3","sv3") = NA;
p_3("sv4","sv4") = Inf;
p_3("sv5","sv5") = -Inf;
p_3("sv6","sv6") = eps;


* test free variables
variable free_1 "scalar variable free_1";
variable free_2 "scalar variable free_2";
variable free_3 "scalar variable free_3";
variable free_4 "scalar variable free_4";
variable free_5 "scalar variable free_5";
variable free_6 "scalar variable free_6";

free_1.L = 1;
free_1.M = 1;
free_1.LO = 1;
free_1.UP = 1;
free_1.SCALE = 1;

free_2.L = 1/0;
free_2.M = 1/0;
free_2.LO = 1/0;
free_2.UP = 1/0;
free_2.SCALE = 1/0;

free_3.L = NA;
free_3.M = NA;
free_3.LO = NA;
free_3.UP = NA;
free_3.SCALE = NA;

free_4.L = Inf;
free_4.M = Inf;
free_4.LO = Inf;
free_4.UP = Inf;
free_4.SCALE = Inf;

free_5.L = -Inf;
free_5.M = -Inf;
free_5.LO = -Inf;
free_5.UP = -Inf;
free_5.SCALE = -Inf;

free_6.L = eps;
free_6.M = eps;
free_6.LO = eps;
free_6.UP = eps;
free_6.SCALE = eps;

variable free_7(sv) "variable free_7";
free_7.L("sv1") = 1;
free_7.L("sv2") = 1/0;
free_7.L("sv3") = NA;
free_7.L("sv4") = +Inf;
free_7.L("sv5") = -Inf;
free_7.L("sv6") = eps;

free_7.M("sv1") = 1;
free_7.M("sv2") = 1/0;
free_7.M("sv3") = NA;
free_7.M("sv4") = +Inf;
free_7.M("sv5") = -Inf;
free_7.M("sv6") = eps;

free_7.LO("sv1") = 1;
free_7.LO("sv2") = 1/0;
free_7.LO("sv3") = NA;
free_7.LO("sv4") = +Inf;
free_7.LO("sv5") = -Inf;
free_7.LO("sv6") = eps;

free_7.UP("sv1") = 1;
free_7.UP("sv2") = 1/0;
free_7.UP("sv3") = NA;
free_7.UP("sv4") = +Inf;
free_7.UP("sv5") = -Inf;
free_7.UP("sv6") = eps;

free_7.SCALE("sv1") = 1;
free_7.SCALE("sv2") = 1/0;
free_7.SCALE("sv3") = NA;
free_7.SCALE("sv4") = +Inf;
free_7.SCALE("sv5") = -Inf;
free_7.SCALE("sv6") = eps;

variable free_8(sv,sv) "variable free_8";
free_8.L("sv1","sv1") = 1;
free_8.L("sv2","sv2") = 1/0;
free_8.L("sv3","sv3") = NA;
free_8.L("sv4","sv4") = +Inf;
free_8.L("sv5","sv5") = -Inf;
free_8.L("sv6","sv6") = eps;

free_8.M("sv1","sv1") = 1;
free_8.M("sv2","sv2") = 1/0;
free_8.M("sv3","sv3") = NA;
free_8.M("sv4","sv4") = +Inf;
free_8.M("sv5","sv5") = -Inf;
free_8.M("sv6","sv6") = eps;

free_8.LO("sv1","sv1") = 1;
free_8.LO("sv2","sv2") = 1/0;
free_8.LO("sv3","sv3") = NA;
free_8.LO("sv4","sv4") = +Inf;
free_8.LO("sv5","sv5") = -Inf;
free_8.LO("sv6","sv6") = eps;

free_8.UP("sv1","sv1") = 1;
free_8.UP("sv2","sv2") = 1/0;
free_8.UP("sv3","sv3") = NA;
free_8.UP("sv4","sv4") = +Inf;
free_8.UP("sv5","sv5") = -Inf;
free_8.UP("sv6","sv6") = eps;

free_8.SCALE("sv1","sv1") = 1;
free_8.SCALE("sv2","sv2") = 1/0;
free_8.SCALE("sv3","sv3") = NA;
free_8.SCALE("sv4","sv4") = +Inf;
free_8.SCALE("sv5","sv5") = -Inf;
free_8.SCALE("sv6","sv6") = eps;

variable free_9(*,sv) "variable free_9";
free_9.L("sv1","sv1") = 1;
free_9.L("sv2","sv2") = 1/0;
free_9.L("sv3","sv3") = NA;
free_9.L("sv4","sv4") = +Inf;
free_9.L("sv5","sv5") = -Inf;
free_9.L("sv6","sv6") = eps;

free_9.M("sv1","sv1") = 1;
free_9.M("sv2","sv2") = 1/0;
free_9.M("sv3","sv3") = NA;
free_9.M("sv4","sv4") = +Inf;
free_9.M("sv5","sv5") = -Inf;
free_9.M("sv6","sv6") = eps;

free_9.LO("sv1","sv1") = 1;
free_9.LO("sv2","sv2") = 1/0;
free_9.LO("sv3","sv3") = NA;
free_9.LO("sv4","sv4") = +Inf;
free_9.LO("sv5","sv5") = -Inf;
free_9.LO("sv6","sv6") = eps;

free_9.UP("sv1","sv1") = 1;
free_9.UP("sv2","sv2") = 1/0;
free_9.UP("sv3","sv3") = NA;
free_9.UP("sv4","sv4") = +Inf;
free_9.UP("sv5","sv5") = -Inf;
free_9.UP("sv6","sv6") = eps;

free_9.SCALE("sv1","sv1") = 1;
free_9.SCALE("sv2","sv2") = 1/0;
free_9.SCALE("sv3","sv3") = NA;
free_9.SCALE("sv4","sv4") = +Inf;
free_9.SCALE("sv5","sv5") = -Inf;
free_9.SCALE("sv6","sv6") = eps;




* test binary variables
binary variable binary_1 "scalar variable binary_1";
binary variable binary_2 "scalar variable binary_2";
binary variable binary_3 "scalar variable binary_3";
binary variable binary_4 "scalar variable binary_4";
binary variable binary_5 "scalar variable binary_5";
binary variable binary_6 "scalar variable binary_6";

binary_1.L = 1;
binary_1.M = 1;
binary_1.LO = 1;
binary_1.UP = 1;

binary_2.L = 1/0;
binary_2.M = 1/0;
binary_2.LO = 1/0;
binary_2.UP = 1/0;

binary_3.L = NA;
binary_3.M = NA;
binary_3.LO = NA;
binary_3.UP = NA;

binary_4.L = Inf;
binary_4.M = Inf;
binary_4.LO = Inf;
binary_4.UP = Inf;

binary_5.L = -Inf;
binary_5.M = -Inf;
binary_5.LO = -Inf;
binary_5.UP = -Inf;

binary_6.L = eps;
binary_6.M = eps;
binary_6.LO = eps;
binary_6.UP = eps;


binary variable binary_7(sv) "variable binary_7";
binary_7.L("sv1") = 1;
binary_7.L("sv2") = 1/0;
binary_7.L("sv3") = NA;
binary_7.L("sv4") = +Inf;
binary_7.L("sv5") = -Inf;
binary_7.L("sv6") = eps;

binary_7.M("sv1") = 1;
binary_7.M("sv2") = 1/0;
binary_7.M("sv3") = NA;
binary_7.M("sv4") = +Inf;
binary_7.M("sv5") = -Inf;
binary_7.M("sv6") = eps;

binary_7.LO("sv1") = 1;
binary_7.LO("sv2") = 1/0;
binary_7.LO("sv3") = NA;
binary_7.LO("sv4") = +Inf;
binary_7.LO("sv5") = -Inf;
binary_7.LO("sv6") = eps;

binary_7.UP("sv1") = 1;
binary_7.UP("sv2") = 1/0;
binary_7.UP("sv3") = NA;
binary_7.UP("sv4") = +Inf;
binary_7.UP("sv5") = -Inf;
binary_7.UP("sv6") = eps;

binary variable binary_8(sv,sv) "variable binary_8";
binary_8.L("sv1","sv1") = 1;
binary_8.L("sv2","sv2") = 1/0;
binary_8.L("sv3","sv3") = NA;
binary_8.L("sv4","sv4") = +Inf;
binary_8.L("sv5","sv5") = -Inf;
binary_8.L("sv6","sv6") = eps;

binary_8.M("sv1","sv1") = 1;
binary_8.M("sv2","sv2") = 1/0;
binary_8.M("sv3","sv3") = NA;
binary_8.M("sv4","sv4") = +Inf;
binary_8.M("sv5","sv5") = -Inf;
binary_8.M("sv6","sv6") = eps;

binary_8.LO("sv1","sv1") = 1;
binary_8.LO("sv2","sv2") = 1/0;
binary_8.LO("sv3","sv3") = NA;
binary_8.LO("sv4","sv4") = +Inf;
binary_8.LO("sv5","sv5") = -Inf;
binary_8.LO("sv6","sv6") = eps;

binary_8.UP("sv1","sv1") = 1;
binary_8.UP("sv2","sv2") = 1/0;
binary_8.UP("sv3","sv3") = NA;
binary_8.UP("sv4","sv4") = +Inf;
binary_8.UP("sv5","sv5") = -Inf;
binary_8.UP("sv6","sv6") = eps;

binary variable binary_9(*,sv) "variable binary_9";
binary_9.L("sv1","sv1") = 1;
binary_9.L("sv2","sv2") = 1/0;
binary_9.L("sv3","sv3") = NA;
binary_9.L("sv4","sv4") = +Inf;
binary_9.L("sv5","sv5") = -Inf;
binary_9.L("sv6","sv6") = eps;

binary_9.M("sv1","sv1") = 1;
binary_9.M("sv2","sv2") = 1/0;
binary_9.M("sv3","sv3") = NA;
binary_9.M("sv4","sv4") = +Inf;
binary_9.M("sv5","sv5") = -Inf;
binary_9.M("sv6","sv6") = eps;

binary_9.LO("sv1","sv1") = 1;
binary_9.LO("sv2","sv2") = 1/0;
binary_9.LO("sv3","sv3") = NA;
binary_9.LO("sv4","sv4") = +Inf;
binary_9.LO("sv5","sv5") = -Inf;
binary_9.LO("sv6","sv6") = eps;

binary_9.UP("sv1","sv1") = 1;
binary_9.UP("sv2","sv2") = 1/0;
binary_9.UP("sv3","sv3") = NA;
binary_9.UP("sv4","sv4") = +Inf;
binary_9.UP("sv5","sv5") = -Inf;
binary_9.UP("sv6","sv6") = eps;


* test integer variables
integer variable integer_1 "scalar variable integer_1";
integer variable integer_2 "scalar variable integer_2";
integer variable integer_3 "scalar variable integer_3";
integer variable integer_4 "scalar variable integer_4";
integer variable integer_5 "scalar variable integer_5";
integer variable integer_6 "scalar variable integer_6";

integer_1.L = 1;
integer_1.M = 1;
integer_1.LO = 1;
integer_1.UP = 1;

integer_2.L = 1/0;
integer_2.M = 1/0;
integer_2.LO = 1/0;
integer_2.UP = 1/0;

integer_3.L = NA;
integer_3.M = NA;
integer_3.LO = NA;
integer_3.UP = NA;

integer_4.L = Inf;
integer_4.M = Inf;
integer_4.LO = Inf;
integer_4.UP = Inf;

integer_5.L = -Inf;
integer_5.M = -Inf;
integer_5.LO = -Inf;
integer_5.UP = -Inf;

integer_6.L = eps;
integer_6.M = eps;
integer_6.LO = eps;
integer_6.UP = eps;


integer variable integer_7(sv) "variable integer_7";
integer_7.L("sv1") = 1;
integer_7.L("sv2") = 1/0;
integer_7.L("sv3") = NA;
integer_7.L("sv4") = +Inf;
integer_7.L("sv5") = -Inf;
integer_7.L("sv6") = eps;

integer_7.M("sv1") = 1;
integer_7.M("sv2") = 1/0;
integer_7.M("sv3") = NA;
integer_7.M("sv4") = +Inf;
integer_7.M("sv5") = -Inf;
integer_7.M("sv6") = eps;

integer_7.LO("sv1") = 1;
integer_7.LO("sv2") = 1/0;
integer_7.LO("sv3") = NA;
integer_7.LO("sv4") = +Inf;
integer_7.LO("sv5") = -Inf;
integer_7.LO("sv6") = eps;

integer_7.UP("sv1") = 1;
integer_7.UP("sv2") = 1/0;
integer_7.UP("sv3") = NA;
integer_7.UP("sv4") = +Inf;
integer_7.UP("sv5") = -Inf;
integer_7.UP("sv6") = eps;

integer variable integer_8(sv,sv) "variable integer_8";
integer_8.L("sv1","sv1") = 1;
integer_8.L("sv2","sv2") = 1/0;
integer_8.L("sv3","sv3") = NA;
integer_8.L("sv4","sv4") = +Inf;
integer_8.L("sv5","sv5") = -Inf;
integer_8.L("sv6","sv6") = eps;

integer_8.M("sv1","sv1") = 1;
integer_8.M("sv2","sv2") = 1/0;
integer_8.M("sv3","sv3") = NA;
integer_8.M("sv4","sv4") = +Inf;
integer_8.M("sv5","sv5") = -Inf;
integer_8.M("sv6","sv6") = eps;

integer_8.LO("sv1","sv1") = 1;
integer_8.LO("sv2","sv2") = 1/0;
integer_8.LO("sv3","sv3") = NA;
integer_8.LO("sv4","sv4") = +Inf;
integer_8.LO("sv5","sv5") = -Inf;
integer_8.LO("sv6","sv6") = eps;

integer_8.UP("sv1","sv1") = 1;
integer_8.UP("sv2","sv2") = 1/0;
integer_8.UP("sv3","sv3") = NA;
integer_8.UP("sv4","sv4") = +Inf;
integer_8.UP("sv5","sv5") = -Inf;
integer_8.UP("sv6","sv6") = eps;

integer variable integer_9(*,sv) "variable integer_9";
integer_9.L("sv1","sv1") = 1;
integer_9.L("sv2","sv2") = 1/0;
integer_9.L("sv3","sv3") = NA;
integer_9.L("sv4","sv4") = +Inf;
integer_9.L("sv5","sv5") = -Inf;
integer_9.L("sv6","sv6") = eps;

integer_9.M("sv1","sv1") = 1;
integer_9.M("sv2","sv2") = 1/0;
integer_9.M("sv3","sv3") = NA;
integer_9.M("sv4","sv4") = +Inf;
integer_9.M("sv5","sv5") = -Inf;
integer_9.M("sv6","sv6") = eps;

integer_9.LO("sv1","sv1") = 1;
integer_9.LO("sv2","sv2") = 1/0;
integer_9.LO("sv3","sv3") = NA;
integer_9.LO("sv4","sv4") = +Inf;
integer_9.LO("sv5","sv5") = -Inf;
integer_9.LO("sv6","sv6") = eps;

integer_9.UP("sv1","sv1") = 1;
integer_9.UP("sv2","sv2") = 1/0;
integer_9.UP("sv3","sv3") = NA;
integer_9.UP("sv4","sv4") = +Inf;
integer_9.UP("sv5","sv5") = -Inf;
integer_9.UP("sv6","sv6") = eps;


* test positive variables
positive variable positive_1 "scalar variable positive_1";
positive variable positive_2 "scalar variable positive_2";
positive variable positive_3 "scalar variable positive_3";
positive variable positive_4 "scalar variable positive_4";
positive variable positive_5 "scalar variable positive_5";
positive variable positive_6 "scalar variable positive_6";

positive_1.L = 1;
positive_1.M = 1;
positive_1.LO = 1;
positive_1.UP = 1;
positive_1.SCALE = 1;

positive_2.L = 1/0;
positive_2.M = 1/0;
positive_2.LO = 1/0;
positive_2.UP = 1/0;
positive_2.SCALE = 1/0;

positive_3.L = NA;
positive_3.M = NA;
positive_3.LO = NA;
positive_3.UP = NA;
positive_3.SCALE = NA;

positive_4.L = Inf;
positive_4.M = Inf;
positive_4.LO = Inf;
positive_4.UP = Inf;
positive_4.SCALE = Inf;

positive_5.L = -Inf;
positive_5.M = -Inf;
positive_5.LO = -Inf;
positive_5.UP = -Inf;
positive_5.SCALE = -Inf;

positive_6.L = eps;
positive_6.M = eps;
positive_6.LO = eps;
positive_6.UP = eps;
positive_6.SCALE = eps;

positive variable positive_7(sv) "variable positive_7";
positive_7.L("sv1") = 1;
positive_7.L("sv2") = 1/0;
positive_7.L("sv3") = NA;
positive_7.L("sv4") = +Inf;
positive_7.L("sv5") = -Inf;
positive_7.L("sv6") = eps;

positive_7.M("sv1") = 1;
positive_7.M("sv2") = 1/0;
positive_7.M("sv3") = NA;
positive_7.M("sv4") = +Inf;
positive_7.M("sv5") = -Inf;
positive_7.M("sv6") = eps;

positive_7.LO("sv1") = 1;
positive_7.LO("sv2") = 1/0;
positive_7.LO("sv3") = NA;
positive_7.LO("sv4") = +Inf;
positive_7.LO("sv5") = -Inf;
positive_7.LO("sv6") = eps;

positive_7.UP("sv1") = 1;
positive_7.UP("sv2") = 1/0;
positive_7.UP("sv3") = NA;
positive_7.UP("sv4") = +Inf;
positive_7.UP("sv5") = -Inf;
positive_7.UP("sv6") = eps;

positive_7.SCALE("sv1") = 1;
positive_7.SCALE("sv2") = 1/0;
positive_7.SCALE("sv3") = NA;
positive_7.SCALE("sv4") = +Inf;
positive_7.SCALE("sv5") = -Inf;
positive_7.SCALE("sv6") = eps;

positive variable positive_8(sv,sv) "variable positive_8";
positive_8.L("sv1","sv1") = 1;
positive_8.L("sv2","sv2") = 1/0;
positive_8.L("sv3","sv3") = NA;
positive_8.L("sv4","sv4") = +Inf;
positive_8.L("sv5","sv5") = -Inf;
positive_8.L("sv6","sv6") = eps;

positive_8.M("sv1","sv1") = 1;
positive_8.M("sv2","sv2") = 1/0;
positive_8.M("sv3","sv3") = NA;
positive_8.M("sv4","sv4") = +Inf;
positive_8.M("sv5","sv5") = -Inf;
positive_8.M("sv6","sv6") = eps;

positive_8.LO("sv1","sv1") = 1;
positive_8.LO("sv2","sv2") = 1/0;
positive_8.LO("sv3","sv3") = NA;
positive_8.LO("sv4","sv4") = +Inf;
positive_8.LO("sv5","sv5") = -Inf;
positive_8.LO("sv6","sv6") = eps;

positive_8.UP("sv1","sv1") = 1;
positive_8.UP("sv2","sv2") = 1/0;
positive_8.UP("sv3","sv3") = NA;
positive_8.UP("sv4","sv4") = +Inf;
positive_8.UP("sv5","sv5") = -Inf;
positive_8.UP("sv6","sv6") = eps;

positive_8.SCALE("sv1","sv1") = 1;
positive_8.SCALE("sv2","sv2") = 1/0;
positive_8.SCALE("sv3","sv3") = NA;
positive_8.SCALE("sv4","sv4") = +Inf;
positive_8.SCALE("sv5","sv5") = -Inf;
positive_8.SCALE("sv6","sv6") = eps;

positive variable positive_9(*,sv) "variable positive_9";
positive_9.L("sv1","sv1") = 1;
positive_9.L("sv2","sv2") = 1/0;
positive_9.L("sv3","sv3") = NA;
positive_9.L("sv4","sv4") = +Inf;
positive_9.L("sv5","sv5") = -Inf;
positive_9.L("sv6","sv6") = eps;

positive_9.M("sv1","sv1") = 1;
positive_9.M("sv2","sv2") = 1/0;
positive_9.M("sv3","sv3") = NA;
positive_9.M("sv4","sv4") = +Inf;
positive_9.M("sv5","sv5") = -Inf;
positive_9.M("sv6","sv6") = eps;

positive_9.LO("sv1","sv1") = 1;
positive_9.LO("sv2","sv2") = 1/0;
positive_9.LO("sv3","sv3") = NA;
positive_9.LO("sv4","sv4") = +Inf;
positive_9.LO("sv5","sv5") = -Inf;
positive_9.LO("sv6","sv6") = eps;

positive_9.UP("sv1","sv1") = 1;
positive_9.UP("sv2","sv2") = 1/0;
positive_9.UP("sv3","sv3") = NA;
positive_9.UP("sv4","sv4") = +Inf;
positive_9.UP("sv5","sv5") = -Inf;
positive_9.UP("sv6","sv6") = eps;

positive_9.SCALE("sv1","sv1") = 1;
positive_9.SCALE("sv2","sv2") = 1/0;
positive_9.SCALE("sv3","sv3") = NA;
positive_9.SCALE("sv4","sv4") = +Inf;
positive_9.SCALE("sv5","sv5") = -Inf;
positive_9.SCALE("sv6","sv6") = eps;



* test negative variables
negative variable negative_1 "scalar variable negative_1";
negative variable negative_2 "scalar variable negative_2";
negative variable negative_3 "scalar variable negative_3";
negative variable negative_4 "scalar variable negative_4";
negative variable negative_5 "scalar variable negative_5";
negative variable negative_6 "scalar variable negative_6";

negative_1.L = 1;
negative_1.M = 1;
negative_1.LO = 1;
negative_1.UP = 1;
negative_1.SCALE = 1;

negative_2.L = 1/0;
negative_2.M = 1/0;
negative_2.LO = 1/0;
negative_2.UP = 1/0;
negative_2.SCALE = 1/0;

negative_3.L = NA;
negative_3.M = NA;
negative_3.LO = NA;
negative_3.UP = NA;
negative_3.SCALE = NA;

negative_4.L = Inf;
negative_4.M = Inf;
negative_4.LO = Inf;
negative_4.UP = Inf;
negative_4.SCALE = Inf;

negative_5.L = -Inf;
negative_5.M = -Inf;
negative_5.LO = -Inf;
negative_5.UP = -Inf;
negative_5.SCALE = -Inf;

negative_6.L = eps;
negative_6.M = eps;
negative_6.LO = eps;
negative_6.UP = eps;
negative_6.SCALE = eps;

negative variable negative_7(sv) "variable negative_7";
negative_7.L("sv1") = 1;
negative_7.L("sv2") = 1/0;
negative_7.L("sv3") = NA;
negative_7.L("sv4") = +Inf;
negative_7.L("sv5") = -Inf;
negative_7.L("sv6") = eps;

negative_7.M("sv1") = 1;
negative_7.M("sv2") = 1/0;
negative_7.M("sv3") = NA;
negative_7.M("sv4") = +Inf;
negative_7.M("sv5") = -Inf;
negative_7.M("sv6") = eps;

negative_7.LO("sv1") = 1;
negative_7.LO("sv2") = 1/0;
negative_7.LO("sv3") = NA;
negative_7.LO("sv4") = +Inf;
negative_7.LO("sv5") = -Inf;
negative_7.LO("sv6") = eps;

negative_7.UP("sv1") = 1;
negative_7.UP("sv2") = 1/0;
negative_7.UP("sv3") = NA;
negative_7.UP("sv4") = +Inf;
negative_7.UP("sv5") = -Inf;
negative_7.UP("sv6") = eps;

negative_7.SCALE("sv1") = 1;
negative_7.SCALE("sv2") = 1/0;
negative_7.SCALE("sv3") = NA;
negative_7.SCALE("sv4") = +Inf;
negative_7.SCALE("sv5") = -Inf;
negative_7.SCALE("sv6") = eps;

negative variable negative_8(sv,sv) "variable negative_8";
negative_8.L("sv1","sv1") = 1;
negative_8.L("sv2","sv2") = 1/0;
negative_8.L("sv3","sv3") = NA;
negative_8.L("sv4","sv4") = +Inf;
negative_8.L("sv5","sv5") = -Inf;
negative_8.L("sv6","sv6") = eps;

negative_8.M("sv1","sv1") = 1;
negative_8.M("sv2","sv2") = 1/0;
negative_8.M("sv3","sv3") = NA;
negative_8.M("sv4","sv4") = +Inf;
negative_8.M("sv5","sv5") = -Inf;
negative_8.M("sv6","sv6") = eps;

negative_8.LO("sv1","sv1") = 1;
negative_8.LO("sv2","sv2") = 1/0;
negative_8.LO("sv3","sv3") = NA;
negative_8.LO("sv4","sv4") = +Inf;
negative_8.LO("sv5","sv5") = -Inf;
negative_8.LO("sv6","sv6") = eps;

negative_8.UP("sv1","sv1") = 1;
negative_8.UP("sv2","sv2") = 1/0;
negative_8.UP("sv3","sv3") = NA;
negative_8.UP("sv4","sv4") = +Inf;
negative_8.UP("sv5","sv5") = -Inf;
negative_8.UP("sv6","sv6") = eps;

negative_8.SCALE("sv1","sv1") = 1;
negative_8.SCALE("sv2","sv2") = 1/0;
negative_8.SCALE("sv3","sv3") = NA;
negative_8.SCALE("sv4","sv4") = +Inf;
negative_8.SCALE("sv5","sv5") = -Inf;
negative_8.SCALE("sv6","sv6") = eps;

negative variable negative_9(*,sv) "variable negative_9";
negative_9.L("sv1","sv1") = 1;
negative_9.L("sv2","sv2") = 1/0;
negative_9.L("sv3","sv3") = NA;
negative_9.L("sv4","sv4") = +Inf;
negative_9.L("sv5","sv5") = -Inf;
negative_9.L("sv6","sv6") = eps;

negative_9.M("sv1","sv1") = 1;
negative_9.M("sv2","sv2") = 1/0;
negative_9.M("sv3","sv3") = NA;
negative_9.M("sv4","sv4") = +Inf;
negative_9.M("sv5","sv5") = -Inf;
negative_9.M("sv6","sv6") = eps;

negative_9.LO("sv1","sv1") = 1;
negative_9.LO("sv2","sv2") = 1/0;
negative_9.LO("sv3","sv3") = NA;
negative_9.LO("sv4","sv4") = +Inf;
negative_9.LO("sv5","sv5") = -Inf;
negative_9.LO("sv6","sv6") = eps;

negative_9.UP("sv1","sv1") = 1;
negative_9.UP("sv2","sv2") = 1/0;
negative_9.UP("sv3","sv3") = NA;
negative_9.UP("sv4","sv4") = +Inf;
negative_9.UP("sv5","sv5") = -Inf;
negative_9.UP("sv6","sv6") = eps;

negative_9.SCALE("sv1","sv1") = 1;
negative_9.SCALE("sv2","sv2") = 1/0;
negative_9.SCALE("sv3","sv3") = NA;
negative_9.SCALE("sv4","sv4") = +Inf;
negative_9.SCALE("sv5","sv5") = -Inf;
negative_9.SCALE("sv6","sv6") = eps;


* test SOS1 variables
sos1 variable sos1_1 "scalar variable sos1_1";
sos1 variable sos1_2 "scalar variable sos1_2";
sos1 variable sos1_3 "scalar variable sos1_3";
sos1 variable sos1_4 "scalar variable sos1_4";
sos1 variable sos1_5 "scalar variable sos1_5";
sos1 variable sos1_6 "scalar variable sos1_6";

sos1_1.L = 1;
sos1_1.M = 1;
sos1_1.LO = 1;
sos1_1.UP = 1;

sos1_2.L = 1/0;
sos1_2.M = 1/0;
sos1_2.LO = 1/0;
sos1_2.UP = 1/0;

sos1_3.L = NA;
sos1_3.M = NA;
sos1_3.LO = NA;
sos1_3.UP = NA;

sos1_4.L = Inf;
sos1_4.M = Inf;
sos1_4.LO = Inf;
sos1_4.UP = Inf;

sos1_5.L = -Inf;
sos1_5.M = -Inf;
sos1_5.LO = -Inf;
sos1_5.UP = -Inf;

sos1_6.L = eps;
sos1_6.M = eps;
sos1_6.LO = eps;
sos1_6.UP = eps;

sos1 variable sos1_7(sv) "variable sos1_7";
sos1_7.L("sv1") = 1;
sos1_7.L("sv2") = 1/0;
sos1_7.L("sv3") = NA;
sos1_7.L("sv4") = +Inf;
sos1_7.L("sv5") = -Inf;
sos1_7.L("sv6") = eps;

sos1_7.M("sv1") = 1;
sos1_7.M("sv2") = 1/0;
sos1_7.M("sv3") = NA;
sos1_7.M("sv4") = +Inf;
sos1_7.M("sv5") = -Inf;
sos1_7.M("sv6") = eps;

sos1_7.LO("sv1") = 1;
sos1_7.LO("sv2") = 1/0;
sos1_7.LO("sv3") = NA;
sos1_7.LO("sv4") = +Inf;
sos1_7.LO("sv5") = -Inf;
sos1_7.LO("sv6") = eps;

sos1_7.UP("sv1") = 1;
sos1_7.UP("sv2") = 1/0;
sos1_7.UP("sv3") = NA;
sos1_7.UP("sv4") = +Inf;
sos1_7.UP("sv5") = -Inf;
sos1_7.UP("sv6") = eps;

sos1 variable sos1_8(sv,sv) "variable sos1_8";
sos1_8.L("sv1","sv1") = 1;
sos1_8.L("sv2","sv2") = 1/0;
sos1_8.L("sv3","sv3") = NA;
sos1_8.L("sv4","sv4") = +Inf;
sos1_8.L("sv5","sv5") = -Inf;
sos1_8.L("sv6","sv6") = eps;

sos1_8.M("sv1","sv1") = 1;
sos1_8.M("sv2","sv2") = 1/0;
sos1_8.M("sv3","sv3") = NA;
sos1_8.M("sv4","sv4") = +Inf;
sos1_8.M("sv5","sv5") = -Inf;
sos1_8.M("sv6","sv6") = eps;

sos1_8.LO("sv1","sv1") = 1;
sos1_8.LO("sv2","sv2") = 1/0;
sos1_8.LO("sv3","sv3") = NA;
sos1_8.LO("sv4","sv4") = +Inf;
sos1_8.LO("sv5","sv5") = -Inf;
sos1_8.LO("sv6","sv6") = eps;

sos1_8.UP("sv1","sv1") = 1;
sos1_8.UP("sv2","sv2") = 1/0;
sos1_8.UP("sv3","sv3") = NA;
sos1_8.UP("sv4","sv4") = +Inf;
sos1_8.UP("sv5","sv5") = -Inf;
sos1_8.UP("sv6","sv6") = eps;

sos1 variable sos1_9(*,sv) "variable sos1_9";
sos1_9.L("sv1","sv1") = 1;
sos1_9.L("sv2","sv2") = 1/0;
sos1_9.L("sv3","sv3") = NA;
sos1_9.L("sv4","sv4") = +Inf;
sos1_9.L("sv5","sv5") = -Inf;
sos1_9.L("sv6","sv6") = eps;

sos1_9.M("sv1","sv1") = 1;
sos1_9.M("sv2","sv2") = 1/0;
sos1_9.M("sv3","sv3") = NA;
sos1_9.M("sv4","sv4") = +Inf;
sos1_9.M("sv5","sv5") = -Inf;
sos1_9.M("sv6","sv6") = eps;

sos1_9.LO("sv1","sv1") = 1;
sos1_9.LO("sv2","sv2") = 1/0;
sos1_9.LO("sv3","sv3") = NA;
sos1_9.LO("sv4","sv4") = +Inf;
sos1_9.LO("sv5","sv5") = -Inf;
sos1_9.LO("sv6","sv6") = eps;

sos1_9.UP("sv1","sv1") = 1;
sos1_9.UP("sv2","sv2") = 1/0;
sos1_9.UP("sv3","sv3") = NA;
sos1_9.UP("sv4","sv4") = +Inf;
sos1_9.UP("sv5","sv5") = -Inf;
sos1_9.UP("sv6","sv6") = eps;



* test SOS2 variables
sos2 variable sos2_1 "scalar variable sos2_1";
sos2 variable sos2_2 "scalar variable sos2_2";
sos2 variable sos2_3 "scalar variable sos2_3";
sos2 variable sos2_4 "scalar variable sos2_4";
sos2 variable sos2_5 "scalar variable sos2_5";
sos2 variable sos2_6 "scalar variable sos2_6";

sos2_1.L = 1;
sos2_1.M = 1;
sos2_1.LO = 1;
sos2_1.UP = 1;

sos2_2.L = 1/0;
sos2_2.M = 1/0;
sos2_2.LO = 1/0;
sos2_2.UP = 1/0;

sos2_3.L = NA;
sos2_3.M = NA;
sos2_3.LO = NA;
sos2_3.UP = NA;

sos2_4.L = Inf;
sos2_4.M = Inf;
sos2_4.LO = Inf;
sos2_4.UP = Inf;

sos2_5.L = -Inf;
sos2_5.M = -Inf;
sos2_5.LO = -Inf;
sos2_5.UP = -Inf;

sos2_6.L = eps;
sos2_6.M = eps;
sos2_6.LO = eps;
sos2_6.UP = eps;

sos2 variable sos2_7(sv) "variable sos2_7";
sos2_7.L("sv1") = 1;
sos2_7.L("sv2") = 1/0;
sos2_7.L("sv3") = NA;
sos2_7.L("sv4") = +Inf;
sos2_7.L("sv5") = -Inf;
sos2_7.L("sv6") = eps;

sos2_7.M("sv1") = 1;
sos2_7.M("sv2") = 1/0;
sos2_7.M("sv3") = NA;
sos2_7.M("sv4") = +Inf;
sos2_7.M("sv5") = -Inf;
sos2_7.M("sv6") = eps;

sos2_7.LO("sv1") = 1;
sos2_7.LO("sv2") = 1/0;
sos2_7.LO("sv3") = NA;
sos2_7.LO("sv4") = +Inf;
sos2_7.LO("sv5") = -Inf;
sos2_7.LO("sv6") = eps;

sos2_7.UP("sv1") = 1;
sos2_7.UP("sv2") = 1/0;
sos2_7.UP("sv3") = NA;
sos2_7.UP("sv4") = +Inf;
sos2_7.UP("sv5") = -Inf;
sos2_7.UP("sv6") = eps;

sos2 variable sos2_8(sv,sv) "variable sos2_8";
sos2_8.L("sv1","sv1") = 1;
sos2_8.L("sv2","sv2") = 1/0;
sos2_8.L("sv3","sv3") = NA;
sos2_8.L("sv4","sv4") = +Inf;
sos2_8.L("sv5","sv5") = -Inf;
sos2_8.L("sv6","sv6") = eps;

sos2_8.M("sv1","sv1") = 1;
sos2_8.M("sv2","sv2") = 1/0;
sos2_8.M("sv3","sv3") = NA;
sos2_8.M("sv4","sv4") = +Inf;
sos2_8.M("sv5","sv5") = -Inf;
sos2_8.M("sv6","sv6") = eps;

sos2_8.LO("sv1","sv1") = 1;
sos2_8.LO("sv2","sv2") = 1/0;
sos2_8.LO("sv3","sv3") = NA;
sos2_8.LO("sv4","sv4") = +Inf;
sos2_8.LO("sv5","sv5") = -Inf;
sos2_8.LO("sv6","sv6") = eps;

sos2_8.UP("sv1","sv1") = 1;
sos2_8.UP("sv2","sv2") = 1/0;
sos2_8.UP("sv3","sv3") = NA;
sos2_8.UP("sv4","sv4") = +Inf;
sos2_8.UP("sv5","sv5") = -Inf;
sos2_8.UP("sv6","sv6") = eps;

sos2 variable sos2_9(*,sv) "variable sos2_9";
sos2_9.L("sv1","sv1") = 1;
sos2_9.L("sv2","sv2") = 1/0;
sos2_9.L("sv3","sv3") = NA;
sos2_9.L("sv4","sv4") = +Inf;
sos2_9.L("sv5","sv5") = -Inf;
sos2_9.L("sv6","sv6") = eps;

sos2_9.M("sv1","sv1") = 1;
sos2_9.M("sv2","sv2") = 1/0;
sos2_9.M("sv3","sv3") = NA;
sos2_9.M("sv4","sv4") = +Inf;
sos2_9.M("sv5","sv5") = -Inf;
sos2_9.M("sv6","sv6") = eps;

sos2_9.LO("sv1","sv1") = 1;
sos2_9.LO("sv2","sv2") = 1/0;
sos2_9.LO("sv3","sv3") = NA;
sos2_9.LO("sv4","sv4") = +Inf;
sos2_9.LO("sv5","sv5") = -Inf;
sos2_9.LO("sv6","sv6") = eps;

sos2_9.UP("sv1","sv1") = 1;
sos2_9.UP("sv2","sv2") = 1/0;
sos2_9.UP("sv3","sv3") = NA;
sos2_9.UP("sv4","sv4") = +Inf;
sos2_9.UP("sv5","sv5") = -Inf;
sos2_9.UP("sv6","sv6") = eps;


* test semiint variables
semiint variable semiint_1 "scalar variable semiint_1";
semiint variable semiint_2 "scalar variable semiint_2";
semiint variable semiint_3 "scalar variable semiint_3";
semiint variable semiint_4 "scalar variable semiint_4";
semiint variable semiint_5 "scalar variable semiint_5";
semiint variable semiint_6 "scalar variable semiint_6";

semiint_1.L = 1;
semiint_1.M = 1;
semiint_1.LO = 1;
semiint_1.UP = 1;

semiint_2.L = 1/0;
semiint_2.M = 1/0;
semiint_2.LO = 1/0;
semiint_2.UP = 1/0;

semiint_3.L = NA;
semiint_3.M = NA;
semiint_3.LO = NA;
semiint_3.UP = NA;

semiint_4.L = Inf;
semiint_4.M = Inf;
semiint_4.LO = Inf;
semiint_4.UP = Inf;

semiint_5.L = -Inf;
semiint_5.M = -Inf;
semiint_5.LO = -Inf;
semiint_5.UP = -Inf;

semiint_6.L = eps;
semiint_6.M = eps;
semiint_6.LO = eps;
semiint_6.UP = eps;

semiint variable semiint_7(sv) "variable semiint_7";
semiint_7.L("sv1") = 1;
semiint_7.L("sv2") = 1/0;
semiint_7.L("sv3") = NA;
semiint_7.L("sv4") = +Inf;
semiint_7.L("sv5") = -Inf;
semiint_7.L("sv6") = eps;

semiint_7.M("sv1") = 1;
semiint_7.M("sv2") = 1/0;
semiint_7.M("sv3") = NA;
semiint_7.M("sv4") = +Inf;
semiint_7.M("sv5") = -Inf;
semiint_7.M("sv6") = eps;

semiint_7.LO("sv1") = 1;
semiint_7.LO("sv2") = 1/0;
semiint_7.LO("sv3") = NA;
semiint_7.LO("sv4") = +Inf;
semiint_7.LO("sv5") = -Inf;
semiint_7.LO("sv6") = eps;

semiint_7.UP("sv1") = 1;
semiint_7.UP("sv2") = 1/0;
semiint_7.UP("sv3") = NA;
semiint_7.UP("sv4") = +Inf;
semiint_7.UP("sv5") = -Inf;
semiint_7.UP("sv6") = eps;

semiint variable semiint_8(sv,sv) "variable semiint_8";
semiint_8.L("sv1","sv1") = 1;
semiint_8.L("sv2","sv2") = 1/0;
semiint_8.L("sv3","sv3") = NA;
semiint_8.L("sv4","sv4") = +Inf;
semiint_8.L("sv5","sv5") = -Inf;
semiint_8.L("sv6","sv6") = eps;

semiint_8.M("sv1","sv1") = 1;
semiint_8.M("sv2","sv2") = 1/0;
semiint_8.M("sv3","sv3") = NA;
semiint_8.M("sv4","sv4") = +Inf;
semiint_8.M("sv5","sv5") = -Inf;
semiint_8.M("sv6","sv6") = eps;

semiint_8.LO("sv1","sv1") = 1;
semiint_8.LO("sv2","sv2") = 1/0;
semiint_8.LO("sv3","sv3") = NA;
semiint_8.LO("sv4","sv4") = +Inf;
semiint_8.LO("sv5","sv5") = -Inf;
semiint_8.LO("sv6","sv6") = eps;

semiint_8.UP("sv1","sv1") = 1;
semiint_8.UP("sv2","sv2") = 1/0;
semiint_8.UP("sv3","sv3") = NA;
semiint_8.UP("sv4","sv4") = +Inf;
semiint_8.UP("sv5","sv5") = -Inf;
semiint_8.UP("sv6","sv6") = eps;

semiint variable semiint_9(*,sv) "variable semiint_9";
semiint_9.L("sv1","sv1") = 1;
semiint_9.L("sv2","sv2") = 1/0;
semiint_9.L("sv3","sv3") = NA;
semiint_9.L("sv4","sv4") = +Inf;
semiint_9.L("sv5","sv5") = -Inf;
semiint_9.L("sv6","sv6") = eps;

semiint_9.M("sv1","sv1") = 1;
semiint_9.M("sv2","sv2") = 1/0;
semiint_9.M("sv3","sv3") = NA;
semiint_9.M("sv4","sv4") = +Inf;
semiint_9.M("sv5","sv5") = -Inf;
semiint_9.M("sv6","sv6") = eps;

semiint_9.LO("sv1","sv1") = 1;
semiint_9.LO("sv2","sv2") = 1/0;
semiint_9.LO("sv3","sv3") = NA;
semiint_9.LO("sv4","sv4") = +Inf;
semiint_9.LO("sv5","sv5") = -Inf;
semiint_9.LO("sv6","sv6") = eps;

semiint_9.UP("sv1","sv1") = 1;
semiint_9.UP("sv2","sv2") = 1/0;
semiint_9.UP("sv3","sv3") = NA;
semiint_9.UP("sv4","sv4") = +Inf;
semiint_9.UP("sv5","sv5") = -Inf;
semiint_9.UP("sv6","sv6") = eps;



* test semicont variables
semicont variable semicont_1 "scalar variable semicont_1";
semicont variable semicont_2 "scalar variable semicont_2";
semicont variable semicont_3 "scalar variable semicont_3";
semicont variable semicont_4 "scalar variable semicont_4";
semicont variable semicont_5 "scalar variable semicont_5";
semicont variable semicont_6 "scalar variable semicont_6";

semicont_1.L = 1;
semicont_1.M = 1;
semicont_1.LO = 1;
semicont_1.UP = 1;

semicont_2.L = 1/0;
semicont_2.M = 1/0;
semicont_2.LO = 1/0;
semicont_2.UP = 1/0;

semicont_3.L = NA;
semicont_3.M = NA;
semicont_3.LO = NA;
semicont_3.UP = NA;

semicont_4.L = Inf;
semicont_4.M = Inf;
semicont_4.LO = Inf;
semicont_4.UP = Inf;

semicont_5.L = -Inf;
semicont_5.M = -Inf;
semicont_5.LO = -Inf;
semicont_5.UP = -Inf;

semicont_6.L = eps;
semicont_6.M = eps;
semicont_6.LO = eps;
semicont_6.UP = eps;

semicont variable semicont_7(sv) "variable semicont_7";
semicont_7.L("sv1") = 1;
semicont_7.L("sv2") = 1/0;
semicont_7.L("sv3") = NA;
semicont_7.L("sv4") = +Inf;
semicont_7.L("sv5") = -Inf;
semicont_7.L("sv6") = eps;

semicont_7.M("sv1") = 1;
semicont_7.M("sv2") = 1/0;
semicont_7.M("sv3") = NA;
semicont_7.M("sv4") = +Inf;
semicont_7.M("sv5") = -Inf;
semicont_7.M("sv6") = eps;

semicont_7.LO("sv1") = 1;
semicont_7.LO("sv2") = 1/0;
semicont_7.LO("sv3") = NA;
semicont_7.LO("sv4") = +Inf;
semicont_7.LO("sv5") = -Inf;
semicont_7.LO("sv6") = eps;

semicont_7.UP("sv1") = 1;
semicont_7.UP("sv2") = 1/0;
semicont_7.UP("sv3") = NA;
semicont_7.UP("sv4") = +Inf;
semicont_7.UP("sv5") = -Inf;
semicont_7.UP("sv6") = eps;

semicont variable semicont_8(sv,sv) "variable semicont_8";
semicont_8.L("sv1","sv1") = 1;
semicont_8.L("sv2","sv2") = 1/0;
semicont_8.L("sv3","sv3") = NA;
semicont_8.L("sv4","sv4") = +Inf;
semicont_8.L("sv5","sv5") = -Inf;
semicont_8.L("sv6","sv6") = eps;

semicont_8.M("sv1","sv1") = 1;
semicont_8.M("sv2","sv2") = 1/0;
semicont_8.M("sv3","sv3") = NA;
semicont_8.M("sv4","sv4") = +Inf;
semicont_8.M("sv5","sv5") = -Inf;
semicont_8.M("sv6","sv6") = eps;

semicont_8.LO("sv1","sv1") = 1;
semicont_8.LO("sv2","sv2") = 1/0;
semicont_8.LO("sv3","sv3") = NA;
semicont_8.LO("sv4","sv4") = +Inf;
semicont_8.LO("sv5","sv5") = -Inf;
semicont_8.LO("sv6","sv6") = eps;

semicont_8.UP("sv1","sv1") = 1;
semicont_8.UP("sv2","sv2") = 1/0;
semicont_8.UP("sv3","sv3") = NA;
semicont_8.UP("sv4","sv4") = +Inf;
semicont_8.UP("sv5","sv5") = -Inf;
semicont_8.UP("sv6","sv6") = eps;

semicont variable semicont_9(*,sv) "variable semicont_9";
semicont_9.L("sv1","sv1") = 1;
semicont_9.L("sv2","sv2") = 1/0;
semicont_9.L("sv3","sv3") = NA;
semicont_9.L("sv4","sv4") = +Inf;
semicont_9.L("sv5","sv5") = -Inf;
semicont_9.L("sv6","sv6") = eps;

semicont_9.M("sv1","sv1") = 1;
semicont_9.M("sv2","sv2") = 1/0;
semicont_9.M("sv3","sv3") = NA;
semicont_9.M("sv4","sv4") = +Inf;
semicont_9.M("sv5","sv5") = -Inf;
semicont_9.M("sv6","sv6") = eps;

semicont_9.LO("sv1","sv1") = 1;
semicont_9.LO("sv2","sv2") = 1/0;
semicont_9.LO("sv3","sv3") = NA;
semicont_9.LO("sv4","sv4") = +Inf;
semicont_9.LO("sv5","sv5") = -Inf;
semicont_9.LO("sv6","sv6") = eps;

semicont_9.UP("sv1","sv1") = 1;
semicont_9.UP("sv2","sv2") = 1/0;
semicont_9.UP("sv3","sv3") = NA;
semicont_9.UP("sv4","sv4") = +Inf;
semicont_9.UP("sv5","sv5") = -Inf;
semicont_9.UP("sv6","sv6") = eps;



* test equality equations
Equation eq_1 "scalar equation eq_1";
Equation eq_2 "scalar equation eq_2";
Equation eq_3 "scalar equation eq_3";
Equation eq_4 "scalar equation eq_4";
Equation eq_5 "scalar equation eq_5";
Equation eq_6 "scalar equation eq_6";

eq_1.. 1 =E= 1;
eq_1.L = 1;
eq_1.M = 1;
eq_1.LO = 1;
eq_1.UP = 1;
eq_1.SCALE = 1;

eq_2.. 1 =E= 1;
eq_2.L = 1/0;
eq_2.M = 1/0;
eq_2.LO = 1/0;
eq_2.UP = 1/0;
eq_2.SCALE = 1/0;

eq_3.. 1 =E= 1;
eq_3.L = NA;
eq_3.M = NA;
eq_3.LO = NA;
eq_3.UP = NA;
eq_3.SCALE = NA;

eq_4.. 1 =E= 1;
eq_4.L = Inf;
eq_4.M = Inf;
eq_4.LO = Inf;
eq_4.UP = Inf;
eq_4.SCALE = Inf;

eq_5.. 1 =E= 1;
eq_5.L = -Inf;
eq_5.M = -Inf;
eq_5.LO = -Inf;
eq_5.UP = -Inf;
eq_5.SCALE = -Inf;

eq_6.. 1 =E= 1;
eq_6.L = eps;
eq_6.M = eps;
eq_6.LO = eps;
eq_6.UP = eps;
eq_6.SCALE = eps;

Equation eq_7(sv) "equation eq_7";

eq_7(sv).. 1 =E= 1;

eq_7.L("sv1") = 1;
eq_7.L("sv2") = 1/0;
eq_7.L("sv3") = NA;
eq_7.L("sv4") = +Inf;
eq_7.L("sv5") = -Inf;
eq_7.L("sv6") = eps;

eq_7.M("sv1") = 1;
eq_7.M("sv2") = 1/0;
eq_7.M("sv3") = NA;
eq_7.M("sv4") = +Inf;
eq_7.M("sv5") = -Inf;
eq_7.M("sv6") = eps;

eq_7.LO("sv1") = 1;
eq_7.LO("sv2") = 1/0;
eq_7.LO("sv3") = NA;
eq_7.LO("sv4") = +Inf;
eq_7.LO("sv5") = -Inf;
eq_7.LO("sv6") = eps;

eq_7.UP("sv1") = 1;
eq_7.UP("sv2") = 1/0;
eq_7.UP("sv3") = NA;
eq_7.UP("sv4") = +Inf;
eq_7.UP("sv5") = -Inf;
eq_7.UP("sv6") = eps;

eq_7.SCALE("sv1") = 1;
eq_7.SCALE("sv2") = 1/0;
eq_7.SCALE("sv3") = NA;
eq_7.SCALE("sv4") = +Inf;
eq_7.SCALE("sv5") = -Inf;
eq_7.SCALE("sv6") = eps;

Equation eq_8(sv,sv) "equation eq_8";
eq_8(sv,sv).. 1 =E= 1;

eq_8.L("sv1","sv1") = 1;
eq_8.L("sv2","sv2") = 1/0;
eq_8.L("sv3","sv3") = NA;
eq_8.L("sv4","sv4") = +Inf;
eq_8.L("sv5","sv5") = -Inf;
eq_8.L("sv6","sv6") = eps;

eq_8.M("sv1","sv1") = 1;
eq_8.M("sv2","sv2") = 1/0;
eq_8.M("sv3","sv3") = NA;
eq_8.M("sv4","sv4") = +Inf;
eq_8.M("sv5","sv5") = -Inf;
eq_8.M("sv6","sv6") = eps;

eq_8.LO("sv1","sv1") = 1;
eq_8.LO("sv2","sv2") = 1/0;
eq_8.LO("sv3","sv3") = NA;
eq_8.LO("sv4","sv4") = +Inf;
eq_8.LO("sv5","sv5") = -Inf;
eq_8.LO("sv6","sv6") = eps;

eq_8.UP("sv1","sv1") = 1;
eq_8.UP("sv2","sv2") = 1/0;
eq_8.UP("sv3","sv3") = NA;
eq_8.UP("sv4","sv4") = +Inf;
eq_8.UP("sv5","sv5") = -Inf;
eq_8.UP("sv6","sv6") = eps;

eq_8.SCALE("sv1","sv1") = 1;
eq_8.SCALE("sv2","sv2") = 1/0;
eq_8.SCALE("sv3","sv3") = NA;
eq_8.SCALE("sv4","sv4") = +Inf;
eq_8.SCALE("sv5","sv5") = -Inf;
eq_8.SCALE("sv6","sv6") = eps;

Equation eq_9(*,sv) "equation eq_9";
eq_9(sv,sv).. 1 =E= 1;

eq_9.L("sv1","sv1") = 1;
eq_9.L("sv2","sv2") = 1/0;
eq_9.L("sv3","sv3") = NA;
eq_9.L("sv4","sv4") = +Inf;
eq_9.L("sv5","sv5") = -Inf;
eq_9.L("sv6","sv6") = eps;

eq_9.M("sv1","sv1") = 1;
eq_9.M("sv2","sv2") = 1/0;
eq_9.M("sv3","sv3") = NA;
eq_9.M("sv4","sv4") = +Inf;
eq_9.M("sv5","sv5") = -Inf;
eq_9.M("sv6","sv6") = eps;

eq_9.LO("sv1","sv1") = 1;
eq_9.LO("sv2","sv2") = 1/0;
eq_9.LO("sv3","sv3") = NA;
eq_9.LO("sv4","sv4") = +Inf;
eq_9.LO("sv5","sv5") = -Inf;
eq_9.LO("sv6","sv6") = eps;

eq_9.UP("sv1","sv1") = 1;
eq_9.UP("sv2","sv2") = 1/0;
eq_9.UP("sv3","sv3") = NA;
eq_9.UP("sv4","sv4") = +Inf;
eq_9.UP("sv5","sv5") = -Inf;
eq_9.UP("sv6","sv6") = eps;

eq_9.SCALE("sv1","sv1") = 1;
eq_9.SCALE("sv2","sv2") = 1/0;
eq_9.SCALE("sv3","sv3") = NA;
eq_9.SCALE("sv4","sv4") = +Inf;
eq_9.SCALE("sv5","sv5") = -Inf;
eq_9.SCALE("sv6","sv6") = eps;



* test less than equations
Equation leq_1 "scalar equation leq_1";
Equation leq_2 "scalar equation leq_2";
Equation leq_3 "scalar equation leq_3";
Equation leq_4 "scalar equation leq_4";
Equation leq_5 "scalar equation leq_5";
Equation leq_6 "scalar equation leq_6";

leq_1.. 1 =L= 1;
leq_1.L = 1;
leq_1.M = 1;
leq_1.LO = 1;
leq_1.UP = 1;
leq_1.SCALE = 1;

leq_2.. 1 =L= 1;
leq_2.L = 1/0;
leq_2.M = 1/0;
leq_2.LO = 1/0;
leq_2.UP = 1/0;
leq_2.SCALE = 1/0;

leq_3.. 1 =L= 1;
leq_3.L = NA;
leq_3.M = NA;
leq_3.LO = NA;
leq_3.UP = NA;
leq_3.SCALE = NA;

leq_4.. 1 =L= 1;
leq_4.L = Inf;
leq_4.M = Inf;
leq_4.LO = Inf;
leq_4.UP = Inf;
leq_4.SCALE = Inf;

leq_5.. 1 =L= 1;
leq_5.L = -Inf;
leq_5.M = -Inf;
leq_5.LO = -Inf;
leq_5.UP = -Inf;
leq_5.SCALE = -Inf;

leq_6.. 1 =L= 1;
leq_6.L = eps;
leq_6.M = eps;
leq_6.LO = eps;
leq_6.UP = eps;
leq_6.SCALE = eps;

Equation leq_7(sv) "equation leq_7";

leq_7(sv).. 1 =L= 1;

leq_7.L("sv1") = 1;
leq_7.L("sv2") = 1/0;
leq_7.L("sv3") = NA;
leq_7.L("sv4") = +Inf;
leq_7.L("sv5") = -Inf;
leq_7.L("sv6") = eps;

leq_7.M("sv1") = 1;
leq_7.M("sv2") = 1/0;
leq_7.M("sv3") = NA;
leq_7.M("sv4") = +Inf;
leq_7.M("sv5") = -Inf;
leq_7.M("sv6") = eps;

leq_7.LO("sv1") = 1;
leq_7.LO("sv2") = 1/0;
leq_7.LO("sv3") = NA;
leq_7.LO("sv4") = +Inf;
leq_7.LO("sv5") = -Inf;
leq_7.LO("sv6") = eps;

leq_7.UP("sv1") = 1;
leq_7.UP("sv2") = 1/0;
leq_7.UP("sv3") = NA;
leq_7.UP("sv4") = +Inf;
leq_7.UP("sv5") = -Inf;
leq_7.UP("sv6") = eps;

leq_7.SCALE("sv1") = 1;
leq_7.SCALE("sv2") = 1/0;
leq_7.SCALE("sv3") = NA;
leq_7.SCALE("sv4") = +Inf;
leq_7.SCALE("sv5") = -Inf;
leq_7.SCALE("sv6") = eps;

Equation leq_8(sv,sv) "equation leq_8";
leq_8(sv,sv).. 1 =L= 1;

leq_8.L("sv1","sv1") = 1;
leq_8.L("sv2","sv2") = 1/0;
leq_8.L("sv3","sv3") = NA;
leq_8.L("sv4","sv4") = +Inf;
leq_8.L("sv5","sv5") = -Inf;
leq_8.L("sv6","sv6") = eps;

leq_8.M("sv1","sv1") = 1;
leq_8.M("sv2","sv2") = 1/0;
leq_8.M("sv3","sv3") = NA;
leq_8.M("sv4","sv4") = +Inf;
leq_8.M("sv5","sv5") = -Inf;
leq_8.M("sv6","sv6") = eps;

leq_8.LO("sv1","sv1") = 1;
leq_8.LO("sv2","sv2") = 1/0;
leq_8.LO("sv3","sv3") = NA;
leq_8.LO("sv4","sv4") = +Inf;
leq_8.LO("sv5","sv5") = -Inf;
leq_8.LO("sv6","sv6") = eps;

leq_8.UP("sv1","sv1") = 1;
leq_8.UP("sv2","sv2") = 1/0;
leq_8.UP("sv3","sv3") = NA;
leq_8.UP("sv4","sv4") = +Inf;
leq_8.UP("sv5","sv5") = -Inf;
leq_8.UP("sv6","sv6") = eps;

leq_8.SCALE("sv1","sv1") = 1;
leq_8.SCALE("sv2","sv2") = 1/0;
leq_8.SCALE("sv3","sv3") = NA;
leq_8.SCALE("sv4","sv4") = +Inf;
leq_8.SCALE("sv5","sv5") = -Inf;
leq_8.SCALE("sv6","sv6") = eps;

Equation leq_9(*,sv) "equation leq_9";
leq_9(sv,sv).. 1 =L= 1;

leq_9.L("sv1","sv1") = 1;
leq_9.L("sv2","sv2") = 1/0;
leq_9.L("sv3","sv3") = NA;
leq_9.L("sv4","sv4") = +Inf;
leq_9.L("sv5","sv5") = -Inf;
leq_9.L("sv6","sv6") = eps;

leq_9.M("sv1","sv1") = 1;
leq_9.M("sv2","sv2") = 1/0;
leq_9.M("sv3","sv3") = NA;
leq_9.M("sv4","sv4") = +Inf;
leq_9.M("sv5","sv5") = -Inf;
leq_9.M("sv6","sv6") = eps;

leq_9.LO("sv1","sv1") = 1;
leq_9.LO("sv2","sv2") = 1/0;
leq_9.LO("sv3","sv3") = NA;
leq_9.LO("sv4","sv4") = +Inf;
leq_9.LO("sv5","sv5") = -Inf;
leq_9.LO("sv6","sv6") = eps;

leq_9.UP("sv1","sv1") = 1;
leq_9.UP("sv2","sv2") = 1/0;
leq_9.UP("sv3","sv3") = NA;
leq_9.UP("sv4","sv4") = +Inf;
leq_9.UP("sv5","sv5") = -Inf;
leq_9.UP("sv6","sv6") = eps;

leq_9.SCALE("sv1","sv1") = 1;
leq_9.SCALE("sv2","sv2") = 1/0;
leq_9.SCALE("sv3","sv3") = NA;
leq_9.SCALE("sv4","sv4") = +Inf;
leq_9.SCALE("sv5","sv5") = -Inf;
leq_9.SCALE("sv6","sv6") = eps;

* test greater than equations
Equation geq_1 "scalar equation geq_1";
Equation geq_2 "scalar equation geq_2";
Equation geq_3 "scalar equation geq_3";
Equation geq_4 "scalar equation geq_4";
Equation geq_5 "scalar equation geq_5";
Equation geq_6 "scalar equation geq_6";

geq_1.. 1 =G= 1;
geq_1.L = 1;
geq_1.M = 1;
geq_1.LO = 1;
geq_1.UP = 1;
geq_1.SCALE = 1;

geq_2.. 1 =G= 1;
geq_2.L = 1/0;
geq_2.M = 1/0;
geq_2.LO = 1/0;
geq_2.UP = 1/0;
geq_2.SCALE = 1/0;

geq_3.. 1 =G= 1;
geq_3.L = NA;
geq_3.M = NA;
geq_3.LO = NA;
geq_3.UP = NA;
geq_3.SCALE = NA;

geq_4.. 1 =G= 1;
geq_4.L = Inf;
geq_4.M = Inf;
geq_4.LO = Inf;
geq_4.UP = Inf;
geq_4.SCALE = Inf;

geq_5.. 1 =G= 1;
geq_5.L = -Inf;
geq_5.M = -Inf;
geq_5.LO = -Inf;
geq_5.UP = -Inf;
geq_5.SCALE = -Inf;

geq_6.. 1 =G= 1;
geq_6.L = eps;
geq_6.M = eps;
geq_6.LO = eps;
geq_6.UP = eps;
geq_6.SCALE = eps;

Equation geq_7(sv) "equation geq_7";

geq_7(sv).. 1 =G= 1;

geq_7.L("sv1") = 1;
geq_7.L("sv2") = 1/0;
geq_7.L("sv3") = NA;
geq_7.L("sv4") = +Inf;
geq_7.L("sv5") = -Inf;
geq_7.L("sv6") = eps;

geq_7.M("sv1") = 1;
geq_7.M("sv2") = 1/0;
geq_7.M("sv3") = NA;
geq_7.M("sv4") = +Inf;
geq_7.M("sv5") = -Inf;
geq_7.M("sv6") = eps;

geq_7.LO("sv1") = 1;
geq_7.LO("sv2") = 1/0;
geq_7.LO("sv3") = NA;
geq_7.LO("sv4") = +Inf;
geq_7.LO("sv5") = -Inf;
geq_7.LO("sv6") = eps;

geq_7.UP("sv1") = 1;
geq_7.UP("sv2") = 1/0;
geq_7.UP("sv3") = NA;
geq_7.UP("sv4") = +Inf;
geq_7.UP("sv5") = -Inf;
geq_7.UP("sv6") = eps;

geq_7.SCALE("sv1") = 1;
geq_7.SCALE("sv2") = 1/0;
geq_7.SCALE("sv3") = NA;
geq_7.SCALE("sv4") = +Inf;
geq_7.SCALE("sv5") = -Inf;
geq_7.SCALE("sv6") = eps;

Equation geq_8(sv,sv) "equation geq_8";
geq_8(sv,sv).. 1 =G= 1;

geq_8.L("sv1","sv1") = 1;
geq_8.L("sv2","sv2") = 1/0;
geq_8.L("sv3","sv3") = NA;
geq_8.L("sv4","sv4") = +Inf;
geq_8.L("sv5","sv5") = -Inf;
geq_8.L("sv6","sv6") = eps;

geq_8.M("sv1","sv1") = 1;
geq_8.M("sv2","sv2") = 1/0;
geq_8.M("sv3","sv3") = NA;
geq_8.M("sv4","sv4") = +Inf;
geq_8.M("sv5","sv5") = -Inf;
geq_8.M("sv6","sv6") = eps;

geq_8.LO("sv1","sv1") = 1;
geq_8.LO("sv2","sv2") = 1/0;
geq_8.LO("sv3","sv3") = NA;
geq_8.LO("sv4","sv4") = +Inf;
geq_8.LO("sv5","sv5") = -Inf;
geq_8.LO("sv6","sv6") = eps;

geq_8.UP("sv1","sv1") = 1;
geq_8.UP("sv2","sv2") = 1/0;
geq_8.UP("sv3","sv3") = NA;
geq_8.UP("sv4","sv4") = +Inf;
geq_8.UP("sv5","sv5") = -Inf;
geq_8.UP("sv6","sv6") = eps;

geq_8.SCALE("sv1","sv1") = 1;
geq_8.SCALE("sv2","sv2") = 1/0;
geq_8.SCALE("sv3","sv3") = NA;
geq_8.SCALE("sv4","sv4") = +Inf;
geq_8.SCALE("sv5","sv5") = -Inf;
geq_8.SCALE("sv6","sv6") = eps;

Equation geq_9(*,sv) "equation geq_9";
geq_9(sv,sv).. 1 =G= 1;

geq_9.L("sv1","sv1") = 1;
geq_9.L("sv2","sv2") = 1/0;
geq_9.L("sv3","sv3") = NA;
geq_9.L("sv4","sv4") = +Inf;
geq_9.L("sv5","sv5") = -Inf;
geq_9.L("sv6","sv6") = eps;

geq_9.M("sv1","sv1") = 1;
geq_9.M("sv2","sv2") = 1/0;
geq_9.M("sv3","sv3") = NA;
geq_9.M("sv4","sv4") = +Inf;
geq_9.M("sv5","sv5") = -Inf;
geq_9.M("sv6","sv6") = eps;

geq_9.LO("sv1","sv1") = 1;
geq_9.LO("sv2","sv2") = 1/0;
geq_9.LO("sv3","sv3") = NA;
geq_9.LO("sv4","sv4") = +Inf;
geq_9.LO("sv5","sv5") = -Inf;
geq_9.LO("sv6","sv6") = eps;

geq_9.UP("sv1","sv1") = 1;
geq_9.UP("sv2","sv2") = 1/0;
geq_9.UP("sv3","sv3") = NA;
geq_9.UP("sv4","sv4") = +Inf;
geq_9.UP("sv5","sv5") = -Inf;
geq_9.UP("sv6","sv6") = eps;

geq_9.SCALE("sv1","sv1") = 1;
geq_9.SCALE("sv2","sv2") = 1/0;
geq_9.SCALE("sv3","sv3") = NA;
geq_9.SCALE("sv4","sv4") = +Inf;
geq_9.SCALE("sv5","sv5") = -Inf;
geq_9.SCALE("sv6","sv6") = eps;


* test nonbinding equations
Equation nb_1 "scalar equation nb_1";
Equation nb_2 "scalar equation nb_2";
Equation nb_3 "scalar equation nb_3";
Equation nb_4 "scalar equation nb_4";
Equation nb_5 "scalar equation nb_5";
Equation nb_6 "scalar equation nb_6";

nb_1.. 1 =N= 1;
nb_1.L = 1;
nb_1.M = 1;
nb_1.LO = 1;
nb_1.UP = 1;
nb_1.SCALE = 1;

nb_2.. 1 =N= 1;
nb_2.L = 1/0;
nb_2.M = 1/0;
nb_2.LO = 1/0;
nb_2.UP = 1/0;
nb_2.SCALE = 1/0;

nb_3.. 1 =N= 1;
nb_3.L = NA;
nb_3.M = NA;
nb_3.LO = NA;
nb_3.UP = NA;
nb_3.SCALE = NA;

nb_4.. 1 =N= 1;
nb_4.L = Inf;
nb_4.M = Inf;
nb_4.LO = Inf;
nb_4.UP = Inf;
nb_4.SCALE = Inf;

nb_5.. 1 =N= 1;
nb_5.L = -Inf;
nb_5.M = -Inf;
nb_5.LO = -Inf;
nb_5.UP = -Inf;
nb_5.SCALE = -Inf;

nb_6.. 1 =N= 1;
nb_6.L = eps;
nb_6.M = eps;
nb_6.LO = eps;
nb_6.UP = eps;
nb_6.SCALE = eps;

Equation nb_7(sv) "equation nb_7";

nb_7(sv).. 1 =N= 1;

nb_7.L("sv1") = 1;
nb_7.L("sv2") = 1/0;
nb_7.L("sv3") = NA;
nb_7.L("sv4") = +Inf;
nb_7.L("sv5") = -Inf;
nb_7.L("sv6") = eps;

nb_7.M("sv1") = 1;
nb_7.M("sv2") = 1/0;
nb_7.M("sv3") = NA;
nb_7.M("sv4") = +Inf;
nb_7.M("sv5") = -Inf;
nb_7.M("sv6") = eps;

nb_7.LO("sv1") = 1;
nb_7.LO("sv2") = 1/0;
nb_7.LO("sv3") = NA;
nb_7.LO("sv4") = +Inf;
nb_7.LO("sv5") = -Inf;
nb_7.LO("sv6") = eps;

nb_7.UP("sv1") = 1;
nb_7.UP("sv2") = 1/0;
nb_7.UP("sv3") = NA;
nb_7.UP("sv4") = +Inf;
nb_7.UP("sv5") = -Inf;
nb_7.UP("sv6") = eps;

nb_7.SCALE("sv1") = 1;
nb_7.SCALE("sv2") = 1/0;
nb_7.SCALE("sv3") = NA;
nb_7.SCALE("sv4") = +Inf;
nb_7.SCALE("sv5") = -Inf;
nb_7.SCALE("sv6") = eps;

Equation nb_8(sv,sv) "equation nb_8";
nb_8(sv,sv).. 1 =N= 1;

nb_8.L("sv1","sv1") = 1;
nb_8.L("sv2","sv2") = 1/0;
nb_8.L("sv3","sv3") = NA;
nb_8.L("sv4","sv4") = +Inf;
nb_8.L("sv5","sv5") = -Inf;
nb_8.L("sv6","sv6") = eps;

nb_8.M("sv1","sv1") = 1;
nb_8.M("sv2","sv2") = 1/0;
nb_8.M("sv3","sv3") = NA;
nb_8.M("sv4","sv4") = +Inf;
nb_8.M("sv5","sv5") = -Inf;
nb_8.M("sv6","sv6") = eps;

nb_8.LO("sv1","sv1") = 1;
nb_8.LO("sv2","sv2") = 1/0;
nb_8.LO("sv3","sv3") = NA;
nb_8.LO("sv4","sv4") = +Inf;
nb_8.LO("sv5","sv5") = -Inf;
nb_8.LO("sv6","sv6") = eps;

nb_8.UP("sv1","sv1") = 1;
nb_8.UP("sv2","sv2") = 1/0;
nb_8.UP("sv3","sv3") = NA;
nb_8.UP("sv4","sv4") = +Inf;
nb_8.UP("sv5","sv5") = -Inf;
nb_8.UP("sv6","sv6") = eps;

nb_8.SCALE("sv1","sv1") = 1;
nb_8.SCALE("sv2","sv2") = 1/0;
nb_8.SCALE("sv3","sv3") = NA;
nb_8.SCALE("sv4","sv4") = +Inf;
nb_8.SCALE("sv5","sv5") = -Inf;
nb_8.SCALE("sv6","sv6") = eps;

Equation nb_9(*,sv) "equation nb_9";
nb_9(sv,sv).. 1 =N= 1;

nb_9.L("sv1","sv1") = 1;
nb_9.L("sv2","sv2") = 1/0;
nb_9.L("sv3","sv3") = NA;
nb_9.L("sv4","sv4") = +Inf;
nb_9.L("sv5","sv5") = -Inf;
nb_9.L("sv6","sv6") = eps;

nb_9.M("sv1","sv1") = 1;
nb_9.M("sv2","sv2") = 1/0;
nb_9.M("sv3","sv3") = NA;
nb_9.M("sv4","sv4") = +Inf;
nb_9.M("sv5","sv5") = -Inf;
nb_9.M("sv6","sv6") = eps;

nb_9.LO("sv1","sv1") = 1;
nb_9.LO("sv2","sv2") = 1/0;
nb_9.LO("sv3","sv3") = NA;
nb_9.LO("sv4","sv4") = +Inf;
nb_9.LO("sv5","sv5") = -Inf;
nb_9.LO("sv6","sv6") = eps;

nb_9.UP("sv1","sv1") = 1;
nb_9.UP("sv2","sv2") = 1/0;
nb_9.UP("sv3","sv3") = NA;
nb_9.UP("sv4","sv4") = +Inf;
nb_9.UP("sv5","sv5") = -Inf;
nb_9.UP("sv6","sv6") = eps;

nb_9.SCALE("sv1","sv1") = 1;
nb_9.SCALE("sv2","sv2") = 1/0;
nb_9.SCALE("sv3","sv3") = NA;
nb_9.SCALE("sv4","sv4") = +Inf;
nb_9.SCALE("sv5","sv5") = -Inf;
nb_9.SCALE("sv6","sv6") = eps;

* test cone equations
Equation cone_1 "scalar equation cone_1";
Equation cone_2 "scalar equation cone_2";
Equation cone_3 "scalar equation cone_3";
Equation cone_4 "scalar equation cone_4";
Equation cone_5 "scalar equation cone_5";
Equation cone_6 "scalar equation cone_6";

cone_1.. 1 =C= 1;
cone_1.L = 1;
cone_1.M = 1;
cone_1.LO = 1;
cone_1.UP = 1;
cone_1.SCALE = 1;

cone_2.. 1 =C= 1;
cone_2.L = 1/0;
cone_2.M = 1/0;
cone_2.LO = 1/0;
cone_2.UP = 1/0;
cone_2.SCALE = 1/0;

cone_3.. 1 =C= 1;
cone_3.L = NA;
cone_3.M = NA;
cone_3.LO = NA;
cone_3.UP = NA;
cone_3.SCALE = NA;

cone_4.. 1 =C= 1;
cone_4.L = Inf;
cone_4.M = Inf;
cone_4.LO = Inf;
cone_4.UP = Inf;
cone_4.SCALE = Inf;

cone_5.. 1 =C= 1;
cone_5.L = -Inf;
cone_5.M = -Inf;
cone_5.LO = -Inf;
cone_5.UP = -Inf;
cone_5.SCALE = -Inf;

cone_6.. 1 =C= 1;
cone_6.L = eps;
cone_6.M = eps;
cone_6.LO = eps;
cone_6.UP = eps;
cone_6.SCALE = eps;

Equation cone_7(sv) "equation cone_7";

cone_7(sv).. 1 =C= 1;

cone_7.L("sv1") = 1;
cone_7.L("sv2") = 1/0;
cone_7.L("sv3") = NA;
cone_7.L("sv4") = +Inf;
cone_7.L("sv5") = -Inf;
cone_7.L("sv6") = eps;

cone_7.M("sv1") = 1;
cone_7.M("sv2") = 1/0;
cone_7.M("sv3") = NA;
cone_7.M("sv4") = +Inf;
cone_7.M("sv5") = -Inf;
cone_7.M("sv6") = eps;

cone_7.LO("sv1") = 1;
cone_7.LO("sv2") = 1/0;
cone_7.LO("sv3") = NA;
cone_7.LO("sv4") = +Inf;
cone_7.LO("sv5") = -Inf;
cone_7.LO("sv6") = eps;

cone_7.UP("sv1") = 1;
cone_7.UP("sv2") = 1/0;
cone_7.UP("sv3") = NA;
cone_7.UP("sv4") = +Inf;
cone_7.UP("sv5") = -Inf;
cone_7.UP("sv6") = eps;

cone_7.SCALE("sv1") = 1;
cone_7.SCALE("sv2") = 1/0;
cone_7.SCALE("sv3") = NA;
cone_7.SCALE("sv4") = +Inf;
cone_7.SCALE("sv5") = -Inf;
cone_7.SCALE("sv6") = eps;

Equation cone_8(sv,sv) "equation cone_8";
cone_8(sv,sv).. 1 =C= 1;

cone_8.L("sv1","sv1") = 1;
cone_8.L("sv2","sv2") = 1/0;
cone_8.L("sv3","sv3") = NA;
cone_8.L("sv4","sv4") = +Inf;
cone_8.L("sv5","sv5") = -Inf;
cone_8.L("sv6","sv6") = eps;

cone_8.M("sv1","sv1") = 1;
cone_8.M("sv2","sv2") = 1/0;
cone_8.M("sv3","sv3") = NA;
cone_8.M("sv4","sv4") = +Inf;
cone_8.M("sv5","sv5") = -Inf;
cone_8.M("sv6","sv6") = eps;

cone_8.LO("sv1","sv1") = 1;
cone_8.LO("sv2","sv2") = 1/0;
cone_8.LO("sv3","sv3") = NA;
cone_8.LO("sv4","sv4") = +Inf;
cone_8.LO("sv5","sv5") = -Inf;
cone_8.LO("sv6","sv6") = eps;

cone_8.UP("sv1","sv1") = 1;
cone_8.UP("sv2","sv2") = 1/0;
cone_8.UP("sv3","sv3") = NA;
cone_8.UP("sv4","sv4") = +Inf;
cone_8.UP("sv5","sv5") = -Inf;
cone_8.UP("sv6","sv6") = eps;

cone_8.SCALE("sv1","sv1") = 1;
cone_8.SCALE("sv2","sv2") = 1/0;
cone_8.SCALE("sv3","sv3") = NA;
cone_8.SCALE("sv4","sv4") = +Inf;
cone_8.SCALE("sv5","sv5") = -Inf;
cone_8.SCALE("sv6","sv6") = eps;

Equation cone_9(*,sv) "equation cone_9";
cone_9(sv,sv).. 1 =C= 1;

cone_9.L("sv1","sv1") = 1;
cone_9.L("sv2","sv2") = 1/0;
cone_9.L("sv3","sv3") = NA;
cone_9.L("sv4","sv4") = +Inf;
cone_9.L("sv5","sv5") = -Inf;
cone_9.L("sv6","sv6") = eps;

cone_9.M("sv1","sv1") = 1;
cone_9.M("sv2","sv2") = 1/0;
cone_9.M("sv3","sv3") = NA;
cone_9.M("sv4","sv4") = +Inf;
cone_9.M("sv5","sv5") = -Inf;
cone_9.M("sv6","sv6") = eps;

cone_9.LO("sv1","sv1") = 1;
cone_9.LO("sv2","sv2") = 1/0;
cone_9.LO("sv3","sv3") = NA;
cone_9.LO("sv4","sv4") = +Inf;
cone_9.LO("sv5","sv5") = -Inf;
cone_9.LO("sv6","sv6") = eps;

cone_9.UP("sv1","sv1") = 1;
cone_9.UP("sv2","sv2") = 1/0;
cone_9.UP("sv3","sv3") = NA;
cone_9.UP("sv4","sv4") = +Inf;
cone_9.UP("sv5","sv5") = -Inf;
cone_9.UP("sv6","sv6") = eps;

cone_9.SCALE("sv1","sv1") = 1;
cone_9.SCALE("sv2","sv2") = 1/0;
cone_9.SCALE("sv3","sv3") = NA;
cone_9.SCALE("sv4","sv4") = +Inf;
cone_9.SCALE("sv5","sv5") = -Inf;
cone_9.SCALE("sv6","sv6") = eps;

* test bool equations
Equation bool_1 "scalar equation bool_1";
Equation bool_2 "scalar equation bool_2";
Equation bool_3 "scalar equation bool_3";
Equation bool_4 "scalar equation bool_4";
Equation bool_5 "scalar equation bool_5";
Equation bool_6 "scalar equation bool_6";

bool_1.. 1 =B= 1;
bool_1.L = 1;
bool_1.M = 1;
bool_1.LO = 1;
bool_1.UP = 1;
bool_1.SCALE = 1;

bool_2.. 1 =B= 1;
bool_2.L = 1/0;
bool_2.M = 1/0;
bool_2.LO = 1/0;
bool_2.UP = 1/0;
bool_2.SCALE = 1/0;

bool_3.. 1 =B= 1;
bool_3.L = NA;
bool_3.M = NA;
bool_3.LO = NA;
bool_3.UP = NA;
bool_3.SCALE = NA;

bool_4.. 1 =B= 1;
bool_4.L = Inf;
bool_4.M = Inf;
bool_4.LO = Inf;
bool_4.UP = Inf;
bool_4.SCALE = Inf;

bool_5.. 1 =B= 1;
bool_5.L = -Inf;
bool_5.M = -Inf;
bool_5.LO = -Inf;
bool_5.UP = -Inf;
bool_5.SCALE = -Inf;

bool_6.. 1 =B= 1;
bool_6.L = eps;
bool_6.M = eps;
bool_6.LO = eps;
bool_6.UP = eps;
bool_6.SCALE = eps;

Equation bool_7(sv) "equation bool_7";

bool_7(sv).. 1 =B= 1;

bool_7.L("sv1") = 1;
bool_7.L("sv2") = 1/0;
bool_7.L("sv3") = NA;
bool_7.L("sv4") = +Inf;
bool_7.L("sv5") = -Inf;
bool_7.L("sv6") = eps;

bool_7.M("sv1") = 1;
bool_7.M("sv2") = 1/0;
bool_7.M("sv3") = NA;
bool_7.M("sv4") = +Inf;
bool_7.M("sv5") = -Inf;
bool_7.M("sv6") = eps;

bool_7.LO("sv1") = 1;
bool_7.LO("sv2") = 1/0;
bool_7.LO("sv3") = NA;
bool_7.LO("sv4") = +Inf;
bool_7.LO("sv5") = -Inf;
bool_7.LO("sv6") = eps;

bool_7.UP("sv1") = 1;
bool_7.UP("sv2") = 1/0;
bool_7.UP("sv3") = NA;
bool_7.UP("sv4") = +Inf;
bool_7.UP("sv5") = -Inf;
bool_7.UP("sv6") = eps;

bool_7.SCALE("sv1") = 1;
bool_7.SCALE("sv2") = 1/0;
bool_7.SCALE("sv3") = NA;
bool_7.SCALE("sv4") = +Inf;
bool_7.SCALE("sv5") = -Inf;
bool_7.SCALE("sv6") = eps;

Equation bool_8(sv,sv) "equation bool_8";
bool_8(sv,sv).. 1 =B= 1;

bool_8.L("sv1","sv1") = 1;
bool_8.L("sv2","sv2") = 1/0;
bool_8.L("sv3","sv3") = NA;
bool_8.L("sv4","sv4") = +Inf;
bool_8.L("sv5","sv5") = -Inf;
bool_8.L("sv6","sv6") = eps;

bool_8.M("sv1","sv1") = 1;
bool_8.M("sv2","sv2") = 1/0;
bool_8.M("sv3","sv3") = NA;
bool_8.M("sv4","sv4") = +Inf;
bool_8.M("sv5","sv5") = -Inf;
bool_8.M("sv6","sv6") = eps;

bool_8.LO("sv1","sv1") = 1;
bool_8.LO("sv2","sv2") = 1/0;
bool_8.LO("sv3","sv3") = NA;
bool_8.LO("sv4","sv4") = +Inf;
bool_8.LO("sv5","sv5") = -Inf;
bool_8.LO("sv6","sv6") = eps;

bool_8.UP("sv1","sv1") = 1;
bool_8.UP("sv2","sv2") = 1/0;
bool_8.UP("sv3","sv3") = NA;
bool_8.UP("sv4","sv4") = +Inf;
bool_8.UP("sv5","sv5") = -Inf;
bool_8.UP("sv6","sv6") = eps;

bool_8.SCALE("sv1","sv1") = 1;
bool_8.SCALE("sv2","sv2") = 1/0;
bool_8.SCALE("sv3","sv3") = NA;
bool_8.SCALE("sv4","sv4") = +Inf;
bool_8.SCALE("sv5","sv5") = -Inf;
bool_8.SCALE("sv6","sv6") = eps;

Equation bool_9(*,sv) "equation bool_9";
bool_9(sv,sv).. 1 =B= 1;

bool_9.L("sv1","sv1") = 1;
bool_9.L("sv2","sv2") = 1/0;
bool_9.L("sv3","sv3") = NA;
bool_9.L("sv4","sv4") = +Inf;
bool_9.L("sv5","sv5") = -Inf;
bool_9.L("sv6","sv6") = eps;

bool_9.M("sv1","sv1") = 1;
bool_9.M("sv2","sv2") = 1/0;
bool_9.M("sv3","sv3") = NA;
bool_9.M("sv4","sv4") = +Inf;
bool_9.M("sv5","sv5") = -Inf;
bool_9.M("sv6","sv6") = eps;

bool_9.LO("sv1","sv1") = 1;
bool_9.LO("sv2","sv2") = 1/0;
bool_9.LO("sv3","sv3") = NA;
bool_9.LO("sv4","sv4") = +Inf;
bool_9.LO("sv5","sv5") = -Inf;
bool_9.LO("sv6","sv6") = eps;

bool_9.UP("sv1","sv1") = 1;
bool_9.UP("sv2","sv2") = 1/0;
bool_9.UP("sv3","sv3") = NA;
bool_9.UP("sv4","sv4") = +Inf;
bool_9.UP("sv5","sv5") = -Inf;
bool_9.UP("sv6","sv6") = eps;

bool_9.SCALE("sv1","sv1") = 1;
bool_9.SCALE("sv2","sv2") = 1/0;
bool_9.SCALE("sv3","sv3") = NA;
bool_9.SCALE("sv4","sv4") = +Inf;
bool_9.SCALE("sv5","sv5") = -Inf;
bool_9.SCALE("sv6","sv6") = eps;

* test external equations
Equation ext_1 "scalar equation ext_1";
Equation ext_2 "scalar equation ext_2";
Equation ext_3 "scalar equation ext_3";
Equation ext_4 "scalar equation ext_4";
Equation ext_5 "scalar equation ext_5";
Equation ext_6 "scalar equation ext_6";

ext_1.. 1 =X= 1;
ext_1.L = 1;
ext_1.M = 1;
ext_1.LO = 1;
ext_1.UP = 1;
ext_1.SCALE = 1;

ext_2.. 1 =X= 1;
ext_2.L = 1/0;
ext_2.M = 1/0;
ext_2.LO = 1/0;
ext_2.UP = 1/0;
ext_2.SCALE = 1/0;

ext_3.. 1 =X= 1;
ext_3.L = NA;
ext_3.M = NA;
ext_3.LO = NA;
ext_3.UP = NA;
ext_3.SCALE = NA;

ext_4.. 1 =X= 1;
ext_4.L = Inf;
ext_4.M = Inf;
ext_4.LO = Inf;
ext_4.UP = Inf;
ext_4.SCALE = Inf;

ext_5.. 1 =X= 1;
ext_5.L = -Inf;
ext_5.M = -Inf;
ext_5.LO = -Inf;
ext_5.UP = -Inf;
ext_5.SCALE = -Inf;

ext_6.. 1 =X= 1;
ext_6.L = eps;
ext_6.M = eps;
ext_6.LO = eps;
ext_6.UP = eps;
ext_6.SCALE = eps;

Equation ext_7(sv) "equation ext_7";

ext_7(sv).. 1 =X= 1;

ext_7.L("sv1") = 1;
ext_7.L("sv2") = 1/0;
ext_7.L("sv3") = NA;
ext_7.L("sv4") = +Inf;
ext_7.L("sv5") = -Inf;
ext_7.L("sv6") = eps;

ext_7.M("sv1") = 1;
ext_7.M("sv2") = 1/0;
ext_7.M("sv3") = NA;
ext_7.M("sv4") = +Inf;
ext_7.M("sv5") = -Inf;
ext_7.M("sv6") = eps;

ext_7.LO("sv1") = 1;
ext_7.LO("sv2") = 1/0;
ext_7.LO("sv3") = NA;
ext_7.LO("sv4") = +Inf;
ext_7.LO("sv5") = -Inf;
ext_7.LO("sv6") = eps;

ext_7.UP("sv1") = 1;
ext_7.UP("sv2") = 1/0;
ext_7.UP("sv3") = NA;
ext_7.UP("sv4") = +Inf;
ext_7.UP("sv5") = -Inf;
ext_7.UP("sv6") = eps;

ext_7.SCALE("sv1") = 1;
ext_7.SCALE("sv2") = 1/0;
ext_7.SCALE("sv3") = NA;
ext_7.SCALE("sv4") = +Inf;
ext_7.SCALE("sv5") = -Inf;
ext_7.SCALE("sv6") = eps;

Equation ext_8(sv,sv) "equation ext_8";
ext_8(sv,sv).. 1 =X= 1;

ext_8.L("sv1","sv1") = 1;
ext_8.L("sv2","sv2") = 1/0;
ext_8.L("sv3","sv3") = NA;
ext_8.L("sv4","sv4") = +Inf;
ext_8.L("sv5","sv5") = -Inf;
ext_8.L("sv6","sv6") = eps;

ext_8.M("sv1","sv1") = 1;
ext_8.M("sv2","sv2") = 1/0;
ext_8.M("sv3","sv3") = NA;
ext_8.M("sv4","sv4") = +Inf;
ext_8.M("sv5","sv5") = -Inf;
ext_8.M("sv6","sv6") = eps;

ext_8.LO("sv1","sv1") = 1;
ext_8.LO("sv2","sv2") = 1/0;
ext_8.LO("sv3","sv3") = NA;
ext_8.LO("sv4","sv4") = +Inf;
ext_8.LO("sv5","sv5") = -Inf;
ext_8.LO("sv6","sv6") = eps;

ext_8.UP("sv1","sv1") = 1;
ext_8.UP("sv2","sv2") = 1/0;
ext_8.UP("sv3","sv3") = NA;
ext_8.UP("sv4","sv4") = +Inf;
ext_8.UP("sv5","sv5") = -Inf;
ext_8.UP("sv6","sv6") = eps;

ext_8.SCALE("sv1","sv1") = 1;
ext_8.SCALE("sv2","sv2") = 1/0;
ext_8.SCALE("sv3","sv3") = NA;
ext_8.SCALE("sv4","sv4") = +Inf;
ext_8.SCALE("sv5","sv5") = -Inf;
ext_8.SCALE("sv6","sv6") = eps;

Equation ext_9(*,sv) "equation ext_9";
ext_9(sv,sv).. 1 =X= 1;

ext_9.L("sv1","sv1") = 1;
ext_9.L("sv2","sv2") = 1/0;
ext_9.L("sv3","sv3") = NA;
ext_9.L("sv4","sv4") = +Inf;
ext_9.L("sv5","sv5") = -Inf;
ext_9.L("sv6","sv6") = eps;

ext_9.M("sv1","sv1") = 1;
ext_9.M("sv2","sv2") = 1/0;
ext_9.M("sv3","sv3") = NA;
ext_9.M("sv4","sv4") = +Inf;
ext_9.M("sv5","sv5") = -Inf;
ext_9.M("sv6","sv6") = eps;

ext_9.LO("sv1","sv1") = 1;
ext_9.LO("sv2","sv2") = 1/0;
ext_9.LO("sv3","sv3") = NA;
ext_9.LO("sv4","sv4") = +Inf;
ext_9.LO("sv5","sv5") = -Inf;
ext_9.LO("sv6","sv6") = eps;

ext_9.UP("sv1","sv1") = 1;
ext_9.UP("sv2","sv2") = 1/0;
ext_9.UP("sv3","sv3") = NA;
ext_9.UP("sv4","sv4") = +Inf;
ext_9.UP("sv5","sv5") = -Inf;
ext_9.UP("sv6","sv6") = eps;

ext_9.SCALE("sv1","sv1") = 1;
ext_9.SCALE("sv2","sv2") = 1/0;
ext_9.SCALE("sv3","sv3") = NA;
ext_9.SCALE("sv4","sv4") = +Inf;
ext_9.SCALE("sv5","sv5") = -Inf;
ext_9.SCALE("sv6","sv6") = eps;

ExecError = 0;

execute_unload "data.gdx";

  '
  write(gams_text, "data.gms")
  ret = system2(command="gams", args= 
  paste0(testthat::test_path("data.gms"), " gdx=data.gdx"), 
  stdout = TRUE, stderr = TRUE)

  h = ConstContainer$new()
  h$read("data.gdx")

  m = Container$new(h)
  # write everything
  m$write(testthat::test_path("gt.gdx"))

  ret <- system2(command="gdxdiff", args=
  paste0(testthat::test_path("data.gdx"), " ", testthat::test_path("gt.gdx")),
  stdout = FALSE)
  expect_equal(ret, 0)
}
)

test_that("test_num_47", {
  # gams syntax
  gams_text = "
set i / a, b /;
set j / c, d /;
parameter a(i,j) //;
  "

  write(gams_text, "data.gms")
  ret = system2(command="gams", args= 
  paste0(testthat::test_path("data.gms"), " gdx=data.gdx"), 
  stdout = TRUE, stderr = TRUE)

  m = ConstContainer$new(testthat::test_path("data.gdx"))
  expect_true(m$data$a$domainType == "regular")
  expect_true(is.null(m$data$a$records))
}
)

test_that("test_num_48", {
  m = Container$new()
  i = Set$new(m, "i", domain=c("k"), records = paste0("i", 1:5))
  j = Set$new(m, "j", domain=c("*"), records = paste0("j", 1:5))
  k = Set$new(m, "k", domain = c("*", "l"), records = data.frame(paste0("k", 1:5), paste0("l", 1:5)))
  l = Set$new(m, "l", i, records=paste0("i", 1:2))
  m$write("data.gdx")

  m = ConstContainer$new("data.gdx")

  expect_true(m$data$i$domainType == "relaxed")
  expect_true(m$data$j$domainType == "none")
  expect_true(m$data$k$domainType == "relaxed")
  expect_true(m$data$l$domainType == "regular")

  expect_true(is.null(m$data$i$records))
  expect_true(is.null(m$data$j$records))
  expect_true(is.null(m$data$k$records))
  expect_true(is.null(m$data$l$records))
}
)


test_that("test_num_49", {
  # gams syntax
  gams_text = "
set i(i) / i1, i2 /;
  "

  write(gams_text, "data.gms")
  ret = system2(command="gams", args= 
  paste0(testthat::test_path("data.gms"), " gdx=data.gdx"), 
  stdout = TRUE, stderr = TRUE)

  m = ConstContainer$new(testthat::test_path("data.gdx"))
  expect_equal(m$data$i$domainType, "relaxed")
}
)

test_that("test_num_50", {
  m = Container$new()
  i = Set$new(m, "i", domain=c("k"), records = paste0("i", 1:5))
  j = Set$new(m, "j", domain=c("*"), records = paste0("j", 1:5))  
  k = Set$new(m, "k", domain = c("*", "l"), records = data.frame(paste0("k", 1:5), paste0("l", 1:5)))
  l = Set$new(m, "l", i, records=paste0("i", 1:2))
  m$write("data.gdx")


  m = ConstContainer$new()
  m$read(testthat::test_path("data.gdx"), symbols="i")
  expect_true(!is.null(m$data$i))

  m = ConstContainer$new()
  m$read(testthat::test_path("data.gdx"), symbols= c("i", "j"))
  expect_true(!is.null(m$data$i))
  expect_true(!is.null(m$data$j))

  m = ConstContainer$new()
  m$read(testthat::test_path("data.gdx"), symbols= c("i", "j", "dummy"))
  expect_true(!is.null(m$data$i))
  expect_true(!is.null(m$data$j))
}
)

test_that("test_num_51", {
  expect_error(ConstContainer$new("dummy.gdx"))
  expect_error(Container$new("dummy.gdx"))

  m = Container$new()
  expect_error(m$read("dummy.gdx"))
}
)

test_that("test_num_52", {
  expect_error(ConstContainer$new(data.frame()))
  expect_error(Container$new(data.frame()))

  m = Container$new()
  expect_error(m$read(data.frame()))
}
)

test_that("test_num_53", {
  # gams syntax
  gams_text = "
set i / i1 * i3 /;
acronym
  small 'baby bear'
  medium 'mama bear'
  large 'papa bear'
  ;
  parameter b(i) /
  i1 1
  i2 medium
  i3 large
  /;
execute_unload '%system.fn%.gdx';
  "

  write(gams_text, "data.gms")
  ret = system2(command="gams", args= 
  paste0(testthat::test_path("data.gms"), " gdx=data.gdx"), 
  stdout = TRUE, stderr = TRUE)

  expect_warning(ConstContainer$new(testthat::test_path("data.gdx")))
}
)

test_that("test_num_54", {
  # gams syntax
  gams_text = "
Set
    i 'canning plants' / seattle,  san-diego /
    j 'markets'        / new-york, chicago, topeka /;

Parameter
    a(i) 'capacity of plant i in cases'
        / seattle    350
          san-diego  600 /

    b(j) 'demand at market j in cases'
        / new-york   325
          chicago    300
          topeka     275 /;

Table d(i,j) 'distance in thousands of miles'
              new-york  chicago  topeka
    seattle         2.5      1.7     1.8
    san-diego       2.5      1.8     1.4;

Scalar f 'freight in dollars per case per thousand miles' / 90 /;

Parameter c(i,j) 'transport cost in thousands of dollars per case';
c(i,j) = f*d(i,j)/1000;

Variable
    x(i,j) 'shipment quantities in cases'
    z      'total transportation costs in thousands of dollars';

Positive Variable x;

Equation
    cost      'define objective function'
    supply(i) 'observe supply limit at plant i'
    demand(j) 'satisfy demand at market j';

cost..      z =e= sum((i,j), c(i,j)*x(i,j));

supply(i).. sum(j, x(i,j)) =l= a(i);

demand(j).. sum(i, x(i,j)) =g= b(j);

Model transport / all /;

solve transport using lp minimizing z;
  "

  write(gams_text, "data.gms")
  ret = system2(command="gams", args= 
  paste0(testthat::test_path("data.gms"), " gdx=data.gdx"), 
  stdout = TRUE, stderr = TRUE)

  m = Container$new(testthat::test_path("data.gdx"))

  for (i in names(m$data)) {
    expect_true(is.list(m$data[[i]]$summary))
  }

  expect_true(is.vector(m$listSymbols()))
  expect_true(is.vector(m$listParameters()))
  expect_true(is.vector(m$listSets()))
  expect_true(is.null(m$listAliases()))
  expect_true(is.vector(m$listVariables()))
  expect_true(is.vector(m$listEquations()))

  expect_equal(length(m$listVariables(types="free")), 1)
  expect_equal(length(m$listVariables(types="positive")), 1)

  expect_equal(length(m$listEquations(types="geq")), 1)
  expect_equal(length(m$listEquations(types="eq")), 1)
  expect_equal(length(m$listEquations(types="leq")), 1)
  expect_equal(length(m$listEquations(types=c("geq", "leq"))), 2)

  expect_true(is.data.frame(m$describeSets()))
  expect_true(is.data.frame(m$describeParameters()))
  expect_true(is.data.frame(m$describeVariables()))
  expect_true(is.data.frame(m$describeEquations()))

  expect_equal(nrow(m$describeEquations(m$listEquations(types = "geq"))), 1)
  expect_equal(nrow(m$describeEquations(m$listEquations(types = "eq"))), 1)
  expect_equal(nrow(m$describeEquations(m$listEquations(types = "leq"))), 1)
  
}
)

test_that("test_num_55", {
  # gams syntax
  gams_text = "
Set
    i 'canning plants' / seattle,  san-diego /
    j 'markets'        / new-york, chicago, topeka /;

Parameter
    a(i) 'capacity of plant i in cases'
        / seattle    350
          san-diego  600 /

    b(j) 'demand at market j in cases'
        / new-york   325
          chicago    300
          topeka     275 /;

Table d(i,j) 'distance in thousands of miles'
              new-york  chicago  topeka
    seattle         2.5      1.7     1.8
    san-diego       2.5      1.8     1.4;

Scalar f 'freight in dollars per case per thousand miles' / 90 /;

Parameter c(i,j) 'transport cost in thousands of dollars per case';
c(i,j) = f*d(i,j)/1000;

Variable
    x(i,j) 'shipment quantities in cases'
    z      'total transportation costs in thousands of dollars';

Positive Variable x;

Equation
    cost      'define objective function'
    supply(i) 'observe supply limit at plant i'
    demand(j) 'satisfy demand at market j';

cost..      z =e= sum((i,j), c(i,j)*x(i,j));

supply(i).. sum(j, x(i,j)) =l= a(i);

demand(j).. sum(i, x(i,j)) =g= b(j);

Model transport / all /;

solve transport using lp minimizing z;
  "

  write(gams_text, "data.gms")
  ret = system2(command="gams", args= 
  paste0(testthat::test_path("data.gms"), " gdx=data.gdx"), 
  stdout = TRUE, stderr = TRUE)

  m = ConstContainer$new("data.gdx")
  old_names = names(m$data)
  for (i in names(m$data)) {
    expect_true(is.null(m$data[[i]]$records))
  }

  m = ConstContainer$new()
  m$read("data.gdx")
  for (i in names(m$data)) {
    expect_true(is.data.frame(m$data[[i]]$records))
  }

   gams_text = "
        Set
           alloy 'products on the market' / a*i /
           elem  'required elements'      / lead, zinc, tin /;

        Table compdat(*,alloy) 'composition data (pct and price)'
                    a    b    c    d    e    f    g    h    i
           lead    10   10   40   60   30   30   30   50   20
           zinc    10   30   50   30   30   40   20   40   30
           tin     80   60   10   10   40   30   50   10   50
           price  4.1  4.3  5.8  6.0  7.6  7.5  7.3  6.9  7.3;

        Parameter
           rb(elem)  'required blend' / lead 30, zinc 30, tin 40 /
           ce(alloy) 'composition error (pct-100)';

        ce(alloy) = sum(elem, compdat(elem,alloy)) - 100;
        display ce;

        Variable
           v(alloy) 'purchase of alloy (pounds)'
           phi      'total cost';

        Positive Variable v;

        Equation
           pc(elem) 'purchase constraint'
           mb       'material balance'
           ac       'accounting: total cost';

        pc(elem).. sum(alloy, compdat(elem,alloy)*v(alloy)) =e= rb(elem);

        mb..       sum(alloy, v(alloy)) =e= 1;

        ac..       phi =e= sum(alloy, compdat('price',alloy)*v(alloy));

        Model
           b1 'problem without mb' / pc,     ac /
           b2 'problem with mb'    / pc, mb, ac /;

        Parameter report(alloy,*) 'comparison of model 1 and 2';

        solve b1 minimizing phi using lp;
        report(alloy,'blend-1') = v.l(alloy);

        solve b2 minimizing phi using lp;
        report(alloy,'blend-2') = v.l(alloy);

        display report;
  "
  write(gams_text, "data.gms")
  ret = system2(command="gams", args= 
  paste0(testthat::test_path("data.gms"), " gdx=data.gdx"), 
  stdout = TRUE, stderr = TRUE)

  m = ConstContainer$new("data.gdx")
  new_names = names(m$data)
  for (i in names(m$data)) {
    expect_true(is.null(m$data[[i]]$records))
  }

  expect_true(!identical(old_names, new_names))
}
)

test_that("test_num_56", {
  # gams syntax
  gams_text = "
Set
    i 'canning plants' / seattle,  san-diego /
    j 'markets'        / new-york, chicago, topeka /;

Parameter
    a(i) 'capacity of plant i in cases'
        / seattle    350
          san-diego  600 /

    b(j) 'demand at market j in cases'
        / new-york   325
          chicago    300
          topeka     275 /;

Table d(i,j) 'distance in thousands of miles'
              new-york  chicago  topeka
    seattle         2.5      1.7     1.8
    san-diego       2.5      1.8     1.4;

Scalar f 'freight in dollars per case per thousand miles' / 90 /;

Parameter c(i,j) 'transport cost in thousands of dollars per case';
c(i,j) = f*d(i,j)/1000;

Variable
    x(i,j) 'shipment quantities in cases'
    z      'total transportation costs in thousands of dollars';

Positive Variable x;

Equation
    cost      'define objective function'
    supply(i) 'observe supply limit at plant i'
    demand(j) 'satisfy demand at market j';

cost..      z =e= sum((i,j), c(i,j)*x(i,j));

supply(i).. sum(j, x(i,j)) =l= a(i);

demand(j).. sum(i, x(i,j)) =g= b(j);

Model transport / all /;

solve transport using lp minimizing z;
  "

  write(gams_text, "data.gms")
  ret = system2(command="gams", args= 
  paste0(testthat::test_path("data.gms"), " gdx=data.gdx"), 
  stdout = TRUE, stderr = TRUE)

  m = ConstContainer$new()
  m$read("data.gdx")

  for (i in names(m$data)) {
    expect_true(is.data.frame(m$data$i$records))
  }
}
)

test_that("test_num_57", {
df = data.frame(expand.grid(c("a1","a2"), 
c("b1","b2"), c("c1","c2"), c("d1","d2")), 
stringsAsFactors=TRUE)

df$value = 1
m = Container$new()
t = Parameter$new(m, "t", domain=replicate(4, "*"))
t$records = df

expect_true(!t$isValid())
}
)

# test make sure describe* are consistent between Container and ConstContainer
test_that("test_num_58", {
  # gams syntax
  gams_text = "
Set
    i 'canning plants' / seattle,  san-diego /
    j 'markets'        / new-york, chicago, topeka /;
Alias(i,ip);
Parameter
    a(i) 'capacity of plant i in cases'
        / seattle    350
          san-diego  600 /

    b(j) 'demand at market j in cases'
        / new-york   325
          chicago    300
          topeka     275 /;

Table d(i,j) 'distance in thousands of miles'
              new-york  chicago  topeka
    seattle         2.5      1.7     1.8
    san-diego       2.5      1.8     1.4;

Scalar f 'freight in dollars per case per thousand miles' / 90 /;

Parameter c(i,j) 'transport cost in thousands of dollars per case';
c(i,j) = f*d(i,j)/1000;

Variable
    x(i,j) 'shipment quantities in cases'
    z      'total transportation costs in thousands of dollars';

Positive Variable x;

Equation
    cost      'define objective function'
    supply(i) 'observe supply limit at plant i'
    demand(j) 'satisfy demand at market j';

cost..      z =e= sum((i,j), c(i,j)*x(i,j));

supply(i).. sum(j, x(i,j)) =l= a(i);

demand(j).. sum(i, x(i,j)) =g= b(j);

Model transport / all /;

solve transport using lp minimizing z;
  "

  write(gams_text, "data.gms")
  ret = system2(command="gams", args= 
  paste0(testthat::test_path("data.gms"), " gdx=data.gdx"), 
  stdout = TRUE, stderr = TRUE)

  m = Container$new(testthat::test_path("data.gdx"))

  m2 = ConstContainer$new()
  m2$read(testthat::test_path("data.gdx"))

  expect_true(identical(m2$describeSets(), m$describeSets()))
  expect_true(identical(m2$describeSets(append(m2$listSets(), m2$listAliases())),
  m$describeSets(append(m$listSets(), m$listAliases()))))

  expect_equal(m2$describeParameters(), m$describeParameters())
  expect_equal(m2$describeVariables(), m$describeVariables())
  expect_equal(m2$describeEquations(), m$describeEquations())

}
)

# test read from another Container
test_that("test_num_59", {
  # gams syntax
  gams_text = "
Set
    i 'canning plants' / seattle,  san-diego /
    j 'markets'        / new-york, chicago, topeka /;
Alias(i,ip);
Parameter
    a(i) 'capacity of plant i in cases'
        / seattle    350
          san-diego  600 /

    b(j) 'demand at market j in cases'
        / new-york   325
          chicago    300
          topeka     275 /;

Table d(i,j) 'distance in thousands of miles'
              new-york  chicago  topeka
    seattle         2.5      1.7     1.8
    san-diego       2.5      1.8     1.4;

Scalar f 'freight in dollars per case per thousand miles' / 90 /;

Parameter c(i,j) 'transport cost in thousands of dollars per case';
c(i,j) = f*d(i,j)/1000;

Variable
    x(i,j) 'shipment quantities in cases'
    z      'total transportation costs in thousands of dollars';

Positive Variable x;

Equation
    cost      'define objective function'
    supply(i) 'observe supply limit at plant i'
    demand(j) 'satisfy demand at market j';

cost..      z =e= sum((i,j), c(i,j)*x(i,j));

supply(i).. sum(j, x(i,j)) =l= a(i);

demand(j).. sum(i, x(i,j)) =g= b(j);

Model transport / all /;

solve transport using lp minimizing z;
  "

  write(gams_text, "data.gms")
  ret = system2(command="gams", args= 
  paste0(testthat::test_path("data.gms"), " gdx=data.gdx"), 
  stdout = TRUE, stderr = TRUE)

  m = Container$new(testthat::test_path("data.gdx"))
  expect_true(m$isValid())

  m2 = Container$new(m)
  expect_true(m2$isValid())
}
)

# test read from another Container with invalid symbols
test_that("test_num_60", {
m = Container$new()
i = Set$new(m, "i", records = paste0("i",1:10))
j = Set$new(m, "j", records = paste0("j",1:10))

a = Parameter$new(m, "a", c(i, j))
a$.__enclos_env__$private$.records = "ham"
expect_true(!a$isValid())
expect_error(Container$new(m))

m2 = Container$new()
m2$read(m, c("i", "j"))
expect_equal(names(m2$data), c("i", "j"))
expect_error(m2$read(m, "a"))
}
)


# test converting arrays with EPS (across several columns) values for Parameters, Variables, Equations
test_that("test_num_61", {
  m = Container$new()
  i = Set$new(m, "i", records = paste0("i", c(1:5)))
  j = Set$new(m, "j", records = paste0("j", c(1:5)))
  recs=data.frame("i"=c("i1","i2","i3","i4","i5"), 
  "j"=c("j1","j2","j3","j4","j5"), "val"=c(0,1,0,SpecialValues$EPS,0))
  recs <- recs[-c(1,3,5),]
  a = Parameter$new(m, "a", domain=c(i, j), records = recs)

  recs=data.frame("i"=c("i1","i2","i3","i4","i5"), 
  "j"=c("j1","j2","j3","j4","j5"), "val"=replicate(5,SpecialValues$EPS))

  ap = Parameter$new(m, "ap", domain=c(i, j), records = recs)

  v = Variable$new(m, "v", domain=c(i, j), records = 
  list("level"=a$toDense(), "marginal"=ap$toDense()))

  e = Equation$new(m, "e", "eq", domain=c(i, j),
  records = list(
    "level" = a$toDense(),
    marginal = ap$toDense()
  )
  )

  df = data.frame(i_1=i$records[,1], 
  j_2=j$records[,1],
  level= c(0,1,0,SpecialValues$EPS,0),
  marginal=replicate(5, SpecialValues$EPS),
  lower = replicate(5, -Inf),
  upper = replicate(5, Inf),
  scale = replicate(5, 1)
  )

  expect_equal(v$records, df)

  df = data.frame(i_1=i$records[,1], 
  j_2=j$records[,1],
  level= c(0,1,0,SpecialValues$EPS,0),
  marginal = replicate(5, SpecialValues$EPS),
  lower = replicate(5, 0),
  upper = replicate(5, 0),
  scale = replicate(5, 1)
  )
  expect_equal(e$records, df)
}
)

# test symbol isValid if categories are not set properly (directly)
test_that("test_num_62", {
df = data.frame(rev(expand.grid(rev(list(paste0("h", 1:10), 
paste0("m", 1:10),paste0("s", 1:10))))), stringsAsFactors = TRUE)

colnames(df) = c("h_1", "m_2", "s_3")

df[["value"]] = runif(nrow(df), 0, 100)

m = Container$new()
hrs = Set$new(m, "h", records = unique(df$h_1))
mins = Set$new(m, "m", records = unique(df$m_2))
secs = Set$new(m, "s", records = unique(df$s_3))

a = Parameter$new(m, "a", c(hrs, mins, secs))

# set records directly
a$records = df

expect_true(!a$isValid())
}
)

# test symbol isValid if categories are set properly (directly)
test_that("test_num_63", {
df = data.frame(rev(expand.grid(rev(list(paste0("h", 1:10), 
paste0("m", 1:10),paste0("s", 1:10))))), stringsAsFactors = TRUE)

colnames(df) = c("h_1", "m_2", "s_3")

df[["value"]] = runif(nrow(df), 0, 100)

m = Container$new()
hrs = Set$new(m, "h", records = unique(df$h_1))
mins = Set$new(m, "m", records = unique(df$m_2))
secs = Set$new(m, "s", records = unique(df$s_3))

a = Parameter$new(m, "a", c(hrs, mins, secs))

df$h_1 = factor(df$h_1, levels=levels(hrs$records$uni_1), ordered = TRUE)
df$m_2 = factor(df$m_2, levels=levels(mins$records$uni_1), ordered = TRUE)
df$s_3 = factor(df$s_3, levels=levels(secs$records$uni_1), ordered = TRUE)
# set records directly
a$records = df

expect_true(a$isValid())
}
)

# test symbol isValid if categories are not linked to the proper set (directly)
test_that("test_num_64", {
df = data.frame(rev(expand.grid(rev(list(paste0("h", 1:10), 
paste0("m", 1:10),paste0("s", 1:10))))), stringsAsFactors = TRUE)

colnames(df) = c("h_1", "m_2", "s_3")

df[["value"]] = runif(nrow(df), 0, 100)

m = Container$new()
hrs = Set$new(m, "h", records = unique(df$h_1))
mins = Set$new(m, "m", records = unique(df$m_2))
secs = Set$new(m, "s", records = unique(df$s_3))

a = Parameter$new(m, "a", c(hrs, mins, secs))

dumb_set = append(levels(df$h_1), "new_element")

df$h_1 = factor(df$h_1, levels=dumb_set, ordered = TRUE)
df$m_2 = factor(df$m_2, levels=levels(mins$records$uni_1), ordered = TRUE)
df$s_3 = factor(df$s_3, levels=levels(secs$records$uni_1), ordered = TRUE)
# set records directly
a$records = df

expect_true(!a$isValid())
}
)

# test Exception when attempting to set a symbol name with invalid characters
test_that("test_num_65", {
m = Container$new()
expect_error(Set$new(m, "milk&meat"))
expect_error(Set$new(m, "_milk&meat"))
}
)

# test that name.setter (symbols and aliases) cannot set name if it already exists in container
test_that("test_num_66", {
m = Container$new()
i = Set$new(m, "i")
j = Set$new(m, "j")

ip = Alias$new(m, "ip", i)
jp = Alias$new(m, "jp", j)
expr <- function() j$name = "i"
expect_error(expr())
expr <- function() jp$name = "ip"
expect_error(expr())
}
)

# test utility functions
test_that("test_num_67", {
m = Container$new()
i = Set$new(m, "i", records = paste0("i", 1:10))
a = Parameter$new(m, "a", i, records=data.frame(i$records[,1], 1:10))
expect_equal(a$getMaxValue(), 10.0)
expect_equal(a$getMinValue(), 1.0)
expect_equal(a$getMeanValue(), 5.5)
expect_equal(a$getMaxAbsValue(), 10.0)
expect_equal(a$whereMax(), 10)
expect_equal(a$whereMaxAbs(), 10)
expect_equal(a$whereMin(), 1)
expect_equal(a$countNA(), 0)
expect_equal(a$countEps(), 0)
expect_equal(a$countUndef(), 0)
expect_equal(a$countPosInf(), 0)
expect_equal(a$countNegInf(), 0)
}
)

# test passing .toDense() of multiple dimensions to setRecords
test_that("test_num_68", {
m = Container$new()
i = Set$new(m, "i", records = paste0("i", 1:10))
j = Set$new(m, "j", records = paste0("j", 1:10))

a0 = Parameter$new(m, "a0", records=10)
a1 = Parameter$new(m, "a1", i, records=data.frame(paste0("i", 1:10), 1:10))


df = data.frame(rev(expand.grid(rev(list(paste0("i", 1:10), 
paste0("j", 1:5))))), stringsAsFactors = TRUE)
values = replicate(50, 0)
count = 1
for (i1 in 1:10) {
  for (j1 in 1:5) {
    values[count] = i1 + j1
    count = count + 1
  }
}
df$values = values
a2 = Parameter$new(m, "a2", c(i, j), records=df)


df = data.frame(rev(expand.grid(rev(list(paste0("i", 1:10), 
paste0("j", 1:5), paste0("i", 1:10))))), stringsAsFactors = TRUE)

values = replicate(500, 0)
count = 1
for (i1 in 1:10) {
  for (j1 in 1:5) {
    for (ip1 in 1:10) {
    values[count] = i1 + j1 + ip1
    count = count + 1
    }
  }
}
df$values = values

a3 = Parameter$new(m, "a3", c(i, j, i), records=df)

a0p = Parameter$new(m, "a0p", records=a0$toDense())
a1p = Parameter$new(m, "a1p", i, records = a1$toDense())
a2p = Parameter$new(m, "a2p", c(i, j), records = a2$toDense())
a3p = Parameter$new(m, "a3p", c(i, j, i), records = a3$toDense())

v0 = Variable$new(m, "v0", records = a0$toDense())
v1 = Variable$new(m, "v1", domain=i, records=a1$toDense())
v2 = Variable$new(m, "v2", domain=c(i, j), records= a2$toDense())
v3 = Variable$new(m, "v3", domain=c(i, j, i), records=a3$toDense())

v0p = Variable$new(m, "v0p", records = v0$toDense())
v1p = Variable$new(m, "v1p", domain=i, records=v1$toDense())
v2p = Variable$new(m, "v2p", domain=c(i, j), records= v2$toDense())
v3p = Variable$new(m, "v3p", domain=c(i, j, i), records=v3$toDense())

e0 = Equation$new(m, "e0", "eq", records = a0$toDense())
e1 = Equation$new(m, "e1", "eq", domain=i, records=a1$toDense())
e2 = Equation$new(m, "e2", "eq", domain=c(i, j), records= a2$toDense())
e3 = Equation$new(m, "e3", "eq", domain=c(i, j, i), records=a3$toDense())

e0p = Equation$new(m, "e0p", "eq", records = e0$toDense())
e1p = Equation$new(m, "e1p", "eq", domain=i, records=e1$toDense())
e2p = Equation$new(m, "e2p", "eq", domain=c(i, j), records= e2$toDense())
e3p = Equation$new(m, "e3p", "eq", domain=c(i, j, i), records=e3$toDense())

expect_equal(a0$records, a0p$records)
expect_equal(a1$records, a1p$records)
expect_equal(a2$records, a2p$records)
expect_equal(a3$records, a3p$records)

expect_equal(v0$records, v0p$records)
expect_equal(v1$records, v1p$records)
expect_equal(v2$records, v2p$records)
expect_equal(v3$records, v3p$records)

expect_equal(e0$records, e0p$records)
expect_equal(e1$records, e1p$records)
expect_equal(e2$records, e2p$records)
expect_equal(e3$records, e3p$records)}
)

# test passing .toDense() of multiple dimensions to setRecords (named list structure)
test_that("test_num_69", {
m = Container$new()
i = Set$new(m, "i", records = paste0("i", 1:10))
j = Set$new(m, "j", records = paste0("j", 1:10))

a0 = Parameter$new(m, "a0", records=10)
a1 = Parameter$new(m, "a1", i, records=data.frame(paste0("i", 1:10), 1:10))


df = data.frame(rev(expand.grid(rev(list(paste0("i", 1:10), 
paste0("j", 1:5))))), stringsAsFactors = TRUE)
values = replicate(50, 0)
count = 1
for (i1 in 1:10) {
  for (j1 in 1:5) {
    values[count] = i1 + j1
    count = count + 1
  }
}
df$values = values
a2 = Parameter$new(m, "a2", c(i, j), records=df)


df = data.frame(rev(expand.grid(rev(list(paste0("i", 1:10), 
paste0("j", 1:5), paste0("i", 1:10))))), stringsAsFactors = TRUE)

values = replicate(500, 0)
count = 1
for (i1 in 1:10) {
  for (j1 in 1:5) {
    for (ip1 in 1:10) {
    values[count] = i1 + j1 + ip1
    count = count + 1
    }
  }
}
df$values = values

a3 = Parameter$new(m, "a3", c(i, j, i), records=df)

a0dict = list(
  level = a0$toDense(),
  marginal = a0$toDense(),
  lower = a0$toDense(),
  upper = a0$toDense(),
  scale = a0$toDense()
)

a1dict = list(
  level = a1$toDense(),
  marginal = a1$toDense(),
  lower = a1$toDense(),
  upper = a1$toDense(),
  scale = a1$toDense()
)

a2dict = list(
  level = a2$toDense(),
  marginal = a2$toDense(),
  lower = a2$toDense(),
  upper = a2$toDense(),
  scale = a2$toDense()
)

a3dict = list(
  level = a3$toDense(),
  marginal = a3$toDense(),
  lower = a3$toDense(),
  upper = a3$toDense(),
  scale = a3$toDense()
)

a0p = Parameter$new(m, "a0p", records=a0$toDense())
a1p = Parameter$new(m, "a1p", i, records = a1$toDense())
a2p = Parameter$new(m, "a2p", c(i, j), records = a2$toDense())
a3p = Parameter$new(m, "a3p", c(i, j, i), records = a3$toDense())

v0 = Variable$new(m, "v0", records = a0dict)
v1 = Variable$new(m, "v1", domain=i, records=a1dict)
v2 = Variable$new(m, "v2", domain=c(i, j), records= a2dict)
v3 = Variable$new(m, "v3", domain=c(i, j, i), records=a3dict)

v0p = Variable$new(m, "v0p", records = a0dict)
v1p = Variable$new(m, "v1p", domain=i, records=a1dict)
v2p = Variable$new(m, "v2p", domain=c(i, j), records= a2dict)
v3p = Variable$new(m, "v3p", domain=c(i, j, i), records=a3dict)

e0 = Equation$new(m, "e0", "eq", records = a0dict)
e1 = Equation$new(m, "e1", "eq", domain=i, records=a1dict)
e2 = Equation$new(m, "e2", "eq", domain=c(i, j), records= a2dict)
e3 = Equation$new(m, "e3", "eq", domain=c(i, j, i), records=a3dict)

e0p = Equation$new(m, "e0p", "eq", records = a0dict)
e1p = Equation$new(m, "e1p", "eq", domain=i, records=a1dict)
e2p = Equation$new(m, "e2p", "eq", domain=c(i, j), records= a2dict)
e3p = Equation$new(m, "e3p", "eq", domain=c(i, j, i), records=a3dict)

expect_equal(a0$records, a0p$records)
expect_equal(a1$records, a1p$records)
expect_equal(a2$records, a2p$records)
expect_equal(a3$records, a3p$records)

expect_equal(v0$records, v0p$records)
expect_equal(v1$records, v1p$records)
expect_equal(v2$records, v2p$records)
expect_equal(v3$records, v3p$records)

expect_equal(e0$records, e0p$records)
expect_equal(e1$records, e1p$records)
expect_equal(e2$records, e2p$records)
expect_equal(e3$records, e3p$records)}
)

# shape test by passing eigen values and eigen vectors
test_that("test_num_70", {
  # gams syntax
  gams_text = "
  Set i / i1*i5 /;

  Alias (i,j);

  Table a(i,j)
            i1   i2   i3   i4   i5
      i1    1    2    4    7   11
      i2    2    3    5    8   12
      i3    4    5    6    9   13
      i4    7    8    9   10   14
      i5   11   12   13   14   15;
  "

  write(gams_text, "data.gms")
  ret = system2(command="gams", args= 
  paste0(testthat::test_path("data.gms"), " gdx=data.gdx"), 
  stdout = TRUE, stderr = TRUE)

  m = Container$new(testthat::test_path("data.gdx"))

  e = eigen(m$data$a$toDense())
  val = e$values
  vec = e$vectors
  eval = Parameter$new(m, "eval", m$data$i, records = val)
  evec = Parameter$new(m, "evec", c(m$data$i, m$data$j), records = vec)

  expect_true(m$isValid())
}
)

test_that("test_num_71", {
arr0 = array(c(1:270), dim=c(5,6,9))
m = Container$new()
i = Set$new(m, "i", records=paste0("i",1:5))
j = Set$new(m, "j", records=paste0("j",1:6))
k = Set$new(m, "k", records=paste0("i",1:9))

h = Parameter$new(m, "h", c(i, j, k), records = arr0)
hp = Parameter$new(m, "hp", c(i, j, k), records = h$toDense())

expect_equal(h$shape(), c(5,6,9))
expect_equal(hp$shape(), c(5,6,9))
expect_equal(dim(h$toDense()), c(5, 6, 9))
expect_equal(dim(hp$toDense()), c(5, 6, 9))

expect_equal(h$records, hp$records)
expect_equal(arr0, h$toDense())
expect_equal(arr0, hp$toDense())

v = Variable$new(m, "v", "free", c(i, j, k), records=arr0)
vp = Variable$new(m, "vp", "free", c(i, j, k), records=v$toDense())


expect_equal(v$shape(), c(5,6, 9))
expect_equal(vp$shape(), c(5,6, 9))
expect_equal(dim(v$toDense()), c(5,6, 9))
expect_equal(dim(vp$toDense()), c(5,6, 9))
expect_equal(v$records, vp$records)
expect_equal(arr0, v$toDense())
expect_equal(arr0, vp$toDense())

e = Equation$new(m, "e", "eq", c(i, j, k), records = arr0)
ep = Equation$new(m, "ep", "eq", c(i, j, k), records = e$toDense())

expect_equal(e$shape(), c(5,6, 9))
expect_equal(ep$shape(), c(5,6, 9))
expect_equal(dim(e$toDense()), c(5,6, 9))
expect_equal(dim(ep$toDense()), c(5,6, 9))
expect_equal(e$records, ep$records)
expect_equal(arr0, e$toDense())
expect_equal(arr0, ep$toDense())

}
)

test_that("test_num_72", {
  # gams syntax
  gams_text = "
    set i /i1*i3/, j /j1*j3/, k /k1*k3/;
    parameter
    dim0 / 1 /
    dim1(i) / i1 1, i2 2, i3 3 /
    dim2(i,j) / i1.j1 1, i2.j2 2, i3.j3 3 /
    dim3(i,j,k) / i1.j1.k1 1, i2.j2.k2 2, i3.j3.k3 3 /;
  "

  write(gams_text, "data.gms")
  ret = system2(command="gams", args= 
  paste0(testthat::test_path("data.gms"), " gdx=data.gdx"), 
  stdout = TRUE, stderr = TRUE)

  m = Container$new(testthat::test_path("data.gdx"))

  expect_equal(m$data$dim0$shape(), dim(m$data$dim0$toDense()))
  expect_equal(m$data$dim1$shape(), dim(m$data$dim1$toDense()))
  expect_equal(m$data$dim2$shape(), dim(m$data$dim2$toDense()))
  expect_equal(m$data$dim3$shape(), dim(m$data$dim3$toDense()))
}
)

test_that("test_num_73", {
  # gams syntax
  gams_text = "
set t /t1*t139776/;
parameter ProfitM(t), ProfitA(t);
ProfitM(t) = uniformInt(1,10);
  "

  write(gams_text, "data.gms")
  ret = system2(command="gams", args= 
  paste0(testthat::test_path("data.gms"), " gdx=data.gdx"), 
  stdout = TRUE, stderr = TRUE)

  m = Container$new(testthat::test_path("data.gdx"))
  m$data$ProfitA$setRecords(cumsum(m$data$ProfitM$toDense()))

  expect_equal(m$data$ProfitA$numberRecords, m$data$t$numberRecords)

}
)
