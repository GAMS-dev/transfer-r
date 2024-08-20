GAMS Special Values
========================

The GAMS system contains five 
`special values <https://www.gams.com/47/docs/UG_Parameters.html#UG_Parameters_ExtendedRangeArithmeticAndErrorHandling>`_:
``UNDEF`` (undefined), ``NA`` (not available), ``EPS`` (epsilon), 
``+INF`` (positive infinity), ``-INF`` (negative infinity). These special 
values must be mapped to their R equivalents. GAMS Transfer R follows 
the following convention to generate the ``1:1`` mapping:

- ``+INF`` is mapped to ``Inf``
- ``-INF`` is mapped to ``-Inf``
- ``EPS`` is mapped to ``-0.0`` (mathematically identical to zero)
- ``NA`` is mapped to a ``NA``
- ``UNDEF`` is mapped to ``NaN``

GAMS Transfer R syntax is designed to quickly get data into a form that 
is usable in further analyses or visualization. The user does not need 
to remember these constants as they are provided within the class 
``SpecialValues`` as ``SpecialValues$POSINF``, ``SpecialValues$NEGINF``, 
``SpecialValues$EPS``, ``SpecialValues[["NA"]]``, and ``SpecialValues$UNDEF``. 
Some examples are shown below.

.. code-block:: R

    library(gamstransfer)
    m <- Container$new()
    x <- Parameter$new(
    m, "x", c("*"),
    records = data.frame(uni = paste0("i", 1:6), c(
        1, SpecialValues[["POSINF"]],
        SpecialValues[["NEGINF"]], SpecialValues[["EPS"]], SpecialValues[["NA"]],
        SpecialValues[["UNDEF"]]
    )),
    description = "special values"
    )

The following data frame for ``x`` would look like:

.. code-block:: R

    > x$records
        uni value
    1    i1     1
    2    i2   Inf
    3    i3  -Inf
    4    i4     0
    5    i5    NA
    6    i6   NaN


The user can now easily test for specific special values in 
the ``value`` column of the DataFrame (returns a logical vector):

.. code-block:: R

    > SpecialValues$isNA(x$records$value)
    [1] FALSE FALSE FALSE FALSE  TRUE FALSE


.. code-block:: R

    > SpecialValues$isNA(x$records$value)
    [1] FALSE FALSE FALSE FALSE  TRUE FALSE

    > SpecialValues$isEps(SpecialValues$EPS)
    [1] TRUE

    > SpecialValues$isPosInf(SpecialValues$POSINF)
    [1] TRUE

    > SpecialValues$isNegInf(SpecialValues$NEGINF)
    [1] TRUE

    > SpecialValues$isNA(SpecialValues$NEGINF)
    SpecialValues$isNA

    > SpecialValues$isNA(SpecialValues[["NA"]])
    [1] TRUE

    > SpecialValues$isUndef(SpecialValues$UNDEF)
    [1] TRUE

    > SpecialValues$isUndef(SpecialValues[["NA"]])
    [1] FALSE

    > SpecialValues$isNA(SpecialValues$UNDEF)
    [1] FALSE

.. note:: 
    The syntax SpecialValues$NA is not allowed in R. Therefore, to access NA, 
    one has to use ``SpecialValues[["NA"]]``. As shown in the example above, double 
    bracket syntax works for other special values too.

