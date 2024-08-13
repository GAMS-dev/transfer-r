Equation
============

Adding Equation Records
------------------------------

:ref:`Three possibilities <add_symbol_records>` exist to 
assign symbol records to an equation: We show a few examples of ways 
to create differently structured equations:
 
Example \#1 - Create a GAMS scalar equation
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. code-block:: R

    library(gamstransfer)
    m = Container$new()
    pi = Equation$new(m, "pi", type="eq", records = data.frame(level = 3.14159))

    # NOTE: the above syntax is equivalent to -
    # pi = Equation$new(m, "pi", type="eq")
    # pi$setRecords(data.frame(level = 3.14159))

    # NOTE: the above syntax is also equivalent to -
    # m$addEquation("pi", type="eq", records=data.frame(level = 3.14159))

.. code-block:: R

    > pi$records
        level
    1 3.14159

Example \#2 - Create a 2D positive equation, specifying no numerical data
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. code-block:: R

    library(gamstransfer)
    m = Container$new()
    e = Equation$new(m, "e", "eq", c("*", "*"), records = 
    data.frame(from=c("seattle", "chicago"), to=c("san-diego", c("madison"))))

.. code-block:: R

    > e$records
        from        to
    1 seattle san-diego
    2 chicago   madison

Example \#3 - Create a 2D equation (defined over a set) from a matrix
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. code-block:: R

    library(gamstransfer)
    m = Container$new()
    i = Set$new(m, "i", "*", records = paste0("i", 1:5))
    j = Set$new(m, "j", "*", records = paste0("j", 1:5))

    # creating records for parameter a
    ij = list(i_1 = paste0("i", 1:5), j_2 = paste0("j", 1:5))
    df = rev(expand.grid(rev(ij)))
    df$value = 1:25
    a = Parameter$new(m, "a", c(i, j), records = df)

    # create a free variable and set the level and marginal attributes from matrices
    e = Equation$new(m, "e", "nonbinding", domain = c(i, j), records = list(level = a$toDense(), marginal = a$toDense()))

.. code-block:: R

    > e$records
        i  j level marginal
    1  i1 j1     1        1
    2  i2 j1     6        6
    3  i3 j1    11       11
    4  i4 j1    16       16
    5  i5 j1    21       21
    6  i1 j2     2        2
    7  i2 j2     7        7
    8  i3 j2    12       12
    9  i4 j2    17       17
    10 i5 j2    22       22
    11 i1 j3     3        3
    12 i2 j3     8        8
    13 i3 j3    13       13
    14 i4 j3    18       18
    15 i5 j3    23       23
    16 i1 j4     4        4
    17 i2 j4     9        9
    18 i3 j4    14       14
    19 i4 j4    19       19
    20 i5 j4    24       24
    21 i1 j5     5        5
    22 i2 j5    10       10
    23 i3 j5    15       15
    24 i4 j5    20       20
    25 i5 j5    25       25

    # if not specified, the toDense() method will convert the level values to a matrix
    > e$toDense()
        [,1] [,2] [,3] [,4] [,5]
    [1,]    1    2    3    4    5
    [2,]    6    7    8    9   10
    [3,]   11   12   13   14   15
    [4,]   16   17   18   19   20
    [5,]   21   22   23   24   25


As with sets, parameters, and variables the primary advantage of the 
``setRecords`` method is that GAMS 
Transfer will convert many different (and convenient) data types into the 
standard data format (data frame). Users that require higher performance 
will want to directly pass the :doc:`Container <../../api_reference/Container>` 
a reference to a valid data frame, 
thereby skipping some of these computational steps. This places more burden on 
the user to pass the data in a valid standard form, but it speeds the records 
setting process. In this section, we walk the user through an example of how 
to set records directly.

Example \#4 - Correctly set records (directly)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. code-block:: R

    library(gamstransfer)
    df = data.frame(h_1 = paste0("h", 1:8760), m_2 = paste0("m", 1:60), 
    s_3 = paste0("s", 1:60))
    df$level = runif(nrow(df), min = 0, max = 100)
    df$marginal = 0.0
    df$lower = SpecialValues$NEGINF
    df$upper = SpecialValues$POSINF
    df$scale = 1.0

    m = Container$new()
    hrs = Set$new(m, "h", records = unique(df$h_1))
    mins = Set$new(m, "m", records = unique(df$m_2))
    secs = Set$new(m, "s", records = unique(df$s_3))

    df$h_1 = factor(df$h_1, ordered = TRUE)
    df$m_2 = factor(df$m_2, ordered = TRUE)
    df$s_3 = factor(df$s_3, ordered = TRUE)

    a = Equation$new(m, "a", "eq", domain = c(hrs, mins, secs))

    # set records
    a$records = df

.. code-block:: R

    > a$isValid()
    [1] TRUE

In this example, we create a large equation (31,536,000 records and
8880 unique domain elements. We mimic data that is labeled for 
every second in one year) and assign it to an equation with ``a$records``. 
GAMS Transfer R requires that all domain columns must be ordered factors. 
The ``records`` setter function does very little work other than checking 
if the object being set is a data frame. This places more responsibility 
on the user to create a data frame that complies with the standard format. 
In Example \#1, we take care to properly reference the factor from the 
domain sets and ensure that the symbol ``a`` is valid in the end with 
``a$isValid() = TRUE``.

As with sets, parameters, and variables, users can use the ``$isValid(verbose=TRUE)`` 
method to debug any structural issues.

.. note:: 
    One can also use the :doc:`generateRecords() <../additional_features/generate_records>`
    method to automatically populate randomly generated symbol records in the standard format.
