Generate Records
=======================

Container symbol records in :doc:`standard format <standard_formats>`
can also be generated using the convenience 
method ``generateRecords()``. This method 
generates records with the cartesian product of domain all sets. 
If the argument ``density`` is less than 1, randomly selected records 
are removed. For symbols that are not scalar, using this method requires 
that the symbol domain type is "regular" (i.e., 
``<symbol_name>$domainType = "regular"``). A few examples using the method 
``generateRecords()`` for each type of Container symbol are provided below.

Set
------

Example \#1 Create a large (dense) 4D set
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Generating the initial ``data.frame`` could be difficult for ``Set`` symbols 
that have a large number of records and a small number of UELs. These 
higher dimensional symbols will benefit from the generateRecords 
convenience method.

.. code-block:: R

    library(gamstransfer)
    m <- Container$new()
    i <- Set$new(m, "i", records = paste0("i", 1:50))
    j <- Set$new(m, "j", records = paste0("j", 1:50))
    k <- Set$new(m, "k", records = paste0("k", 1:50))
    l <- Set$new(m, "l", records = paste0("l", 1:50))

    # create and define the symbol ``a`` with ``regular`` domains
    a <- Set$new(m, "a", c(i, j, k, l))

    # generate the records
    a$generateRecords()

.. code-block:: R

    > a$isValid()
    [1] TRUE

    > head(a$records)
    i  j  k  l element_text
    1 i1 j1 k1 l1
    2 i2 j1 k1 l1
    3 i3 j1 k1 l1
    4 i4 j1 k1 l1
    5 i5 j1 k1 l1
    6 i6 j1 k1 l1

    > tail(a$records)
            i   j   k   l element_text
    6249995 i45 j50 k50 l50
    6249996 i46 j50 k50 l50
    6249997 i47 j50 k50 l50
    6249998 i48 j50 k50 l50
    6249999 i49 j50 k50 l50
    6250000 i50 j50 k50 l50

Example \#2 Create a large (sparse) 4D set
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

It is also possible to generate a sparse set (randomly selected rows are removed from the dense dataframe) with the ``density`` argument to ``generateRecords``.

.. code-block:: R

    library(gamstransfer)
    m <- Container$new()
    i <- Set$new(m, "i", records = paste0("i", 1:50))
    j <- Set$new(m, "j", records = paste0("j", 1:50))
    k <- Set$new(m, "k", records = paste0("k", 1:50))
    l <- Set$new(m, "l", records = paste0("l", 1:50))

    # create and define the symbol ``a`` with ``regular`` domains
    a <- Set$new(m, "a", c(i, j, k, l))

    # generate the records
    a$generateRecords(density = 0.05)

.. code-block:: R

    > a$isValid()
    [1] TRUE

    > head(a$records)
        i   j   k   l element_text
    1 i15  j1  k1  l1
    2 i41  j1  k1  l1
    3 i37  j2  k1  l1
    4 i17  j3  k1  l1
    5 i21  j3  k1  l1
    6 i37  j3  k1  l1

    > tail(a$records)
            i   j   k   l element_text
    312495  i6 j48 k50 l50
    312496  i9 j49 k50 l50
    312497 i14 j49 k50 l50
    312498 i41 j49 k50 l50
    312499 i44 j49 k50 l50
    312500 i35 j50 k50 l50

Example \#3 Create a large 4D set with 1 sparse dimension
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. code-block:: R

    library(gamstransfer)
    m <- Container$new()
    i <- Set$new(m, "i", records = paste0("i", 1:50))
    j <- Set$new(m, "j", records = paste0("j", 1:50))
    k <- Set$new(m, "k", records = paste0("k", 1:50))
    l <- Set$new(m, "l", records = paste0("l", 1:50))

    # create and define the symbol ``a`` with ``regular`` domains
    a <- Set$new(m, "a", c(i, j, k, l))

    # generate the records
    a$generateRecords(density = c(1, 0.05, 1, 1))

.. code-block:: R

    > a$isValid()
    [1] TRUE

    > head(a$records)
        i   j   k   l element_text
    1  i1 j29  k1  l1
    2  i2 j29  k1  l1
    3  i3 j29  k1  l1
    4  i4 j29  k1  l1
    5  i5 j29  k1  l1
    6  i6 j29  k1  l1

    > tail(a$records)
            i   j   k   l element_text
    249995 i45 j45 k50 l50
    249996 i46 j45 k50 l50
    249997 i47 j45 k50 l50
    249998 i48 j45 k50 l50
    249999 i49 j45 k50 l50
    250000 i50 j45 k50 l50


Parameter
------------------

Example \#1 Create a large (dense) 4D Parameter
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Generating the initial ``data.frame`` could be difficult for ``Parameter`` symbols 
that have a large number of records and a small number of UELs. 
These higher dimensional symbols will benefit from the ``generateRecords`` 
convenience method.

.. code-block:: R

    library(gamstransfer)
    m <- Container$new()
    i <- Set$new(m, "i", records = paste0("i", 1:50))
    j <- Set$new(m, "j", records = paste0("j", 1:50))
    k <- Set$new(m, "k", records = paste0("k", 1:50))
    l <- Set$new(m, "l", records = paste0("l", 1:50))

    # create and define the symbol ``a`` with ``regular`` domains
    a <- Parameter$new(m, "a", c(i, j, k, l))

    # generate the records
    a$generateRecords()

.. code-block:: R

    > a$isValid()
    [1] TRUE

    > head(a$records)
        i   j   k   l      value
    1  i1  j1  k1  l1 0.47998665
    2  i2  j1  k1  l1 0.20015289
    3  i3  j1  k1  l1 0.57701174
    4  i4  j1  k1  l1 0.73032070
    5  i5  j1  k1  l1 0.08637669
    6  i6  j1  k1  l1 0.45913994

    > tail(a$records)
            i   j   k   l      value
    6249995 i45 j50 k50 l50 0.91182978
    6249996 i46 j50 k50 l50 0.79016549
    6249997 i47 j50 k50 l50 0.77912069
    6249998 i48 j50 k50 l50 0.63232201
    6249999 i49 j50 k50 l50 0.04274219
    6250000 i50 j50 k50 l50 0.71523280

.. note:: 
    In Example \#1 a large 4D parameter was generated. by default, 
    the value of these records are randomly drawn numbers from the interval 
    ``[0,1]`` (uniform distribution).

Example \#2 - Create a large (sparse) 4D parameter with normally distributed values
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. code-block:: R

    library(gamstransfer)
    m <- Container$new()
    i <- Set$new(m, "i", records = paste0("i", 1:50))
    j <- Set$new(m, "j", records = paste0("j", 1:50))
    k <- Set$new(m, "k", records = paste0("k", 1:50))
    l <- Set$new(m, "l", records = paste0("l", 1:50))

    # create and define the symbol ``a`` with ``regular`` domains
    a <- Parameter$new(m, "a", c(i, j, k, l))

    # create a custom function to pass to ``generateRecords``
    value_dist <- function(size) {
    return(rnorm(n = size, mean = 10, sd = 2.3))
    }
    # generate the records
    a$generateRecords(density = 0.05, func = value_dist)

.. code-block:: R

    > a$isValid()
    [1] TRUE

    > head(a$records)
        i   j   k   l     value
    1 i50  j1  k1  l1 12.499060
    2  i6  j2  k1  l1 12.009952
    3 i14  j2  k1  l1  9.931126
    4 i49  j2  k1  l1 13.073977
    5  i7  j3  k1  l1  5.330898
    6 i22  j3  k1  l1  7.887725

    > tail(a$records)
            i   j   k   l     value
    312495 i14 j48 k50 l50 10.213841
    312496 i20 j48 k50 l50  4.831503
    312497 i26 j48 k50 l50  8.129577
    312498 i17 j49 k50 l50 11.570570
    312499 i48 j49 k50 l50 11.321228
    312500 i35 j50 k50 l50  1.714614

    > mean(a$records$value)
    [1] 10.00273

    > sd(a$records$value)
    [1] 2.303193

.. note:: 
    The custom function passed to the argument ``func`` must expose a ``size`` argument. 
    It might be tedious to know the exact number of the records that will be 
    generated, especially if a fractional density is specified; therefore, the 
    ``generateRecords`` method will pass in the correct size automatically.

Example \#3 - Create a large 4D parameter with a random number seed
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. code-block:: R

    library(gamstransfer)
    m <- Container$new()
    i <- Set$new(m, "i", records = paste0("i", 1:50))
    j <- Set$new(m, "j", records = paste0("j", 1:50))
    k <- Set$new(m, "k", records = paste0("k", 1:50))
    l <- Set$new(m, "l", records = paste0("l", 1:50))

    # create and define the symbol ``a`` with ``regular`` domains
    a <- Parameter$new(m, "a", c(i, j, k, l))
    a2 <- Parameter$new(m, "a2", c(i, j, k, l))

    # generate the records
    a$generateRecords(density = 0.05, seed = 123)
    a2$generateRecords(density = 0.05)

.. code-block:: R

    > a$equals(a2, checkMetaData = FALSE)
    [1] FALSE

    a2$generateRecords(density = 0.05, seed = 123)

    > a$equals(a2, checkMetaData = FALSE)
    [1] TRUE

.. note:: 
    The ``seed`` is an ``int`` that will set the random number generator state 
    (enables reproducible sequences of random numbers).

Variable and Equation
--------------------------

Generating records for the symbol types ``Variable`` and ``Equation`` is similar to 
that of previously shown examples of parameters and sets. However, since there are 
more than one attributes to variables and equations, there are a few differences. 
By default, the random sampling is done is only for the ``level`` attribute with 
default values being passed to the other attributes. To randomly generate other 
attributes, one can use the custom ``func`` argument. This is shown in the following example.

Example \#1 Create a large (sparse) 4D variable and Equation
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. code-block:: R

    library(gamstransfer)
    m <- Container$new()
    i <- Set$new(m, "i", records = paste0("i", 1:50))
    j <- Set$new(m, "j", records = paste0("j", 1:50))
    k <- Set$new(m, "k", records = paste0("k", 1:50))
    l <- Set$new(m, "l", records = paste0("l", 1:50))

    # create and define the symbol ``a`` with ``regular`` domains
    av <- Variable$new(m, "av", "free", c(i, j, k, l))
    ae <- Equation$new(m, "ae", "eq", c(i, j, k, l))

    # user can pass the function in-line as follows
    av$generateRecords(
    density = 0.05,
    func = list(
        level = function(size) rnorm(n = size, mean = 10, sd = 2.3),
        marginal = function(size) rnorm(n = size, mean = 0.5, sd = 0.1)
    )
    )

    # functions can also be defined first and then passed
    uniform_distr <- function(size) {
    return(runif(size))
    }
    normal_distr <- function(size) {
    return(rnorm(n = size))
    }

    ae$generateRecords(density = 0.05, func = list(level = uniform_distr, marginal = normal_distr))

.. code-block:: R

    > head(av$records)
        i   j   k   l     level  marginal lower upper scale
    1 i23  j1  k1  l1 12.244702 0.5587150  -Inf   Inf     1
    2 i29  j1  k1  l1  8.265612 0.4242353  -Inf   Inf     1
    3  i2  j2  k1  l1 14.164058 0.4166124  -Inf   Inf     1
    4 i17  j2  k1  l1 13.786874 0.5993234  -Inf   Inf     1
    5 i22  j2  k1  l1  8.489724 0.4924503  -Inf   Inf     1
    6 i36  j2  k1  l1  7.962292 0.4757125  -Inf   Inf     1

    > tail(av$records)
            i   j   k   l     level  marginal lower upper scale
    312495 i33 j48 k50 l50  6.648296 0.4870270  -Inf   Inf     1
    312496 i37 j48 k50 l50 10.012486 0.5478388  -Inf   Inf     1
    312497 i20 j49 k50 l50  7.931512 0.4221189  -Inf   Inf     1
    312498 i41 j49 k50 l50 10.869332 0.5191488  -Inf   Inf     1
    312499 i42 j49 k50 l50  9.316445 0.4263974  -Inf   Inf     1
    312500 i44 j49 k50 l50  8.153729 0.6101864  -Inf   Inf     1

    > head(ae$records)
        i   j   k   l      level   marginal lower upper scale
    1  i5  j1  k1  l1 0.74525909  0.8910060     0     0     1
    2 i10  j1  k1  l1 0.72308699  1.6090443     0     0     1
    3 i22  j1  k1  l1 0.70425801 -1.2204379     0     0     1
    4 i47  j1  k1  l1 0.06490871  0.7270846     0     0     1
    5 i24  j2  k1  l1 0.94752455  0.7864338     0     0     1
    6 i35  j2  k1  l1 0.08555602 -0.2912885     0     0     1

    > tail(ae$records)
            i   j   k   l     level     marginal lower upper scale
    312495  i5 j49 k50 l50 0.7844452 -0.569529636     0     0     1
    312496 i46 j49 k50 l50 0.2224596 -0.182441937     0     0     1
    312497 i11 j50 k50 l50 0.9291730 -0.474982758     0     0     1
    312498 i16 j50 k50 l50 0.3347919  0.009303616     0     0     1
    312499 i20 j50 k50 l50 0.3590295 -0.533782269     0     0     1
    312500 i27 j50 k50 l50 0.7681852 -1.126704380     0     0     1

Alias
--------------

The method ``generateRecords`` for an alias simply calls the corresponding 
method for its referenced set.

Example \#1 Create a large (dense) 4D set from an Alias
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. code-block:: R

    library(gamstransfer)
    m <- Container$new()
    i <- Set$new(m, "i", records = paste0("i", 1:50))
    j <- Set$new(m, "j", records = paste0("j", 1:50))
    k <- Set$new(m, "k", records = paste0("k", 1:50))
    l <- Set$new(m, "l", records = paste0("l", 1:50))

    # create and define the symbol ``a`` with ``regular`` domains
    a <- Set$new(m, "a", c(i, j, k, l))

    # create an Alias ap for the set a
    ap <- Alias$new(m, "ap", a)

    # generate the records
    ap$generateRecords()

.. code-block:: R

    > ap$isValid()
    [1] TRUE

    > head(ap$records)
        i   j   k   l element_text
    1  i1  j1  k1  l1
    2  i2  j1  k1  l1
    3  i3  j1  k1  l1
    4  i4  j1  k1  l1
    5  i5  j1  k1  l1
    6  i6  j1  k1  l1

    > tail(ap$records)
            i   j   k   l element_text
    6249995 i45 j50 k50 l50
    6249996 i46 j50 k50 l50
    6249997 i47 j50 k50 l50
    6249998 i48 j50 k50 l50
    6249999 i49 j50 k50 l50
    6250000 i50 j50 k50 l50

