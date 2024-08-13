Parameter
============

Adding Parameter Records
-------------------------------

:ref:`Three possibilities <add_symbol_records>`
exist to assign symbol records to a parameter: We show a 
few examples of ways to create differently structured parameters:

Example \#1 - Create a GAMS scalar
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. code-block:: R

    library(gamstransfer)
    m = Container$new()
    pi = Parameter$new(m, "pi", records = 3.14159)

    # NOTE: the above syntax is equivalent to -
    # pi = Parameter$new(m, "pi")
    # pi$setRecords(3.14159)

    # NOTE: the above syntax is also equivalent to -
    # m$addParameter("pi", records=3.14159)

    # NOTE: the above syntax is also equivalent to -
    # pi = m$addParameter("pi")
    # pi$setRecords(3.14159)

    # NOTE: the above syntax is also equivalent to -
    # m$addParameter("pi")
    # m["pi"]$setRecords(3.14159)

.. code-block:: R

    > pi$records
        value
    1 3.14159


.. note:: 
    GAMS Transfer R will still convert scalar values to a standard format 
    (i.e., a data frame with a single row and column).

Example \#2 - Create a 2D parameter (defined over a set) from a data frame slice
-------------------------------------------------------------------------------------

.. code-block:: R

    library(gamstransfer)
    dist = data.frame(
        from = c("seattle", "seattle", "seattle", 
        "san-diego", "san-diego", "san-diego"),
        to = c("new-york", "chicago", "topeka",
        "new-york", "chicago", "topeka"),
        thousand_miles = c(2.5, 1.7, 1.8, 2.5, 1.8, 1.4)
    )

    m = Container$new()
    i = Set$new(m, "i", "*", records = unique(dist$from))
    j = Set$new(m, "j", "*", records = unique(dist$to))
    a = Parameter$new(m, "a", c(i, j), records = dist)

.. code-block:: R

    > a$toDense()
        [,1] [,2] [,3]
    [1,]  2.5  1.7  1.8
    [2,]  2.5  1.8  1.4

    # use a$toDense() to create a new (and identicial) parameter a2
    a2 = Parameter$new(m, "a2", c(i, j), records = a$toDense())

    > a2$records
            i        j value
    1   seattle new-york   2.5
    2   seattle  chicago   1.7
    3   seattle   topeka   1.8
    4 san-diego new-york   2.5
    5 san-diego  chicago   1.8
    6 san-diego   topeka   1.4

Example \#3 - Create a 2D parameter from an array using setRecords
------------------------------------------------------------------------

.. code-block:: R

    library(gamstransfer)
    m = Container$new()
    i = Set$new(m, "i", records=paste0("i_", 1:5))
    j = Set$new(m, "j", records=paste0("j_", 1:5))

    # create the parameter with linked domains (these will control the 
    # $shape of the symbol)
    a = Parameter$new(m, "a", c(i, j))

    # here we use the $shape field to easily generate a dense random array
    a$setRecords(array(runif(prod(a$shape), min = 1, max = 10), 
    dim = a$shape ))

.. code-block:: R

    > a$toDense()
            [,1]     [,2]     [,3]     [,4]     [,5]
    [1,] 3.837345 3.632743 9.003275 4.097475 8.608477
    [2,] 7.217257 2.465452 3.286330 2.366017 8.822535
    [3,] 8.421044 8.546226 5.403918 2.286660 6.319740
    [4,] 3.960100 8.538932 2.210829 2.437113 5.324722
    [5,] 1.333846 4.508688 7.411279 5.653044 7.248775

As with Sets, the primary advantage of the ``setRecords`` method is that GAMS 
Transfer will convert many different (and convenient) data types into the 
standard data format (data frame). Users that require higher performance 
will want to directly pass the :doc:`Container <../../api_reference/Container>`
a reference to a valid data frame,
thereby skipping some of these computational steps. This places more burden on 
the user to pass the data in a valid standard form, but it speeds the records 
setting process. In this section, we walk the user through an example of how 
to set records directly.

Example \#4 - Correctly set records (directly)
-----------------------------------------------------

.. code-block:: R

    library(gamstransfer)
    df = data.frame(h_1 = paste0("h", 1:8760), m_2 = paste0("m", 1:60), 
    s_3 = paste0("s", 1:60))
    df$value = runif(nrow(df), min = 0, max = 100)

    m = Container$new()
    hrs = Set$new(m, "h", records = unique(df$h_1))
    mins = Set$new(m, "m", records = unique(df$m_2))
    secs = Set$new(m, "s", records = unique(df$s_3))

    df$h_1 = factor(df$h_1, ordered = TRUE)
    df$m_2 = factor(df$m_2, ordered = TRUE)
    df$s_3 = factor(df$s_3, ordered = TRUE)

    a = Parameter$new(m, "a", c(hrs, mins, secs))

    # set records
    a$records = df

.. code-block:: R

    > a$isValid()
    [1] TRUE

In this example, we create a large parameter (31,536,000 records and
8880 unique domain elements. We mimic data that is labeled for 
every second in one year) and assign it to a parameter with ``a$records``. 
GAMS Transfer R requires that all domain columns must be ordered factors. 
The ``records`` setter function does very little work other than checking 
if the object being set is a data frame. This places more responsibility 
on the user to create a data frame that complies with the standard format. 
In Example \#1, we take care to properly reference the factor from the 
domain sets, and ensure that the symbol ``a`` is valid with ``a$isValid() = TRUE``.

Users will need to use the ``$isValid(verbose=TRUE)`` method to debug any 
structural issues. As an example, we incorrectly generate categorical 
data types by passing the data frame constructor the generic ``factor`` 
argument. This creates factor columns, but they are not 
ordered and they do not reference the underlying domain set. 
These errors result in ``a`` being invalid.

Example \#5 - Incorrectly set records (directly)
----------------------------------------------------

.. code-block:: R

    library(gamstransfer)
    df = data.frame(h_1 = paste0("h", 1:8760), m_2 = paste0("m", 1:60), 
    s_3 = paste0("s", 1:60))
    df$value = runif(nrow(df), min = 0, max = 100)

    m = Container$new()
    hrs = Set$new(m, "h", records = unique(df$h_1))
    mins = Set$new(m, "m", records = unique(df$m_2))
    secs = Set$new(m, "s", records = unique(df$s_3))

    df$h_1 = factor(df$h_1)
    df$m_2 = factor(df$m_2)
    df$s_3 = factor(df$s_3)

    a = Parameter$new(m, "a", c(hrs, mins, secs))

    # set records
    a$records = df

.. code-block:: R

    > a$isValid()
    [1] FALSE

    > a$isValid(verbose=TRUE)
    Domain information in column h_1 must be an ORDERED factor
    [1] FALSE

.. note:: 
    One can also use the :doc:`generateRecords() <../additional_features/generate_records>`
    method to automatically populate randomly generated symbol records in the standard format.

