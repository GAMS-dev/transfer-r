Variable
============

Adding Variable Records
-------------------------------

:ref:`Three possibilities <add_symbol_records>` exist 
to assign symbol records to a variable: We show a few examples of 
ways to create differently structured variables:
 
Example \#1 - Create a GAMS scalar variable
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. code-block:: R

    library(gamstransfer)
    m <- Container$new()
    pi <- Variable$new(m, "pi", records = data.frame(level = 3.14159))

    # NOTE: the above syntax is equivalent to -
    # pi = Variable$new(m, "pi", "free")
    # pi$setRecords(data.frame(level = 3.14159))

    # NOTE: the above syntax is also equivalent to -
    # m$addVariable("pi", "free", records=data.frame(level = 3.14159))

.. code-block:: R

    > pi$records
        level
    1 3.14159

Example \#2 - Create a 2D positive variable, specifying no numerical data
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. code-block:: R

    library(gamstransfer)
    m <- Container$new()
    v <- Variable$new(m, "v", "positive", c("*", "*"),
    records =
        data.frame(from = c("seattle", "chicago"), to = c("san-diego", c("madison")))
    )

.. code-block:: R

    > v$records
        from        to
    1 seattle san-diego
    2 chicago   madison

Example \#3 - Create a 2D variable (defined over a set) from a matrix
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. code-block:: R

    library(gamstransfer)
    m <- Container$new()
    i <- Set$new(m, "i", "*", records = paste0("i", 1:5))
    j <- Set$new(m, "j", "*", records = paste0("j", 1:5))

    # creating records for parameter a
    ij <- list(i_1 = paste0("i", 1:5), j_2 = paste0("j", 1:5))
    df <- rev(expand.grid(rev(ij)))
    df$value <- 1:25
    a <- Parameter$new(m, "a", c(i, j), records = df)

    # create a free variable and set the level and marginal attributes from matricies
    v <- Variable$new(m, "v", domain = c(i, j), records = list(level = a$toDense(), marginal = a$toDense()))

.. code-block:: R

    # if not specified, the toDense() method will convert the level values to a matrix
    > v$toDense()
        [,1] [,2] [,3] [,4] [,5]
    [1,]    1    2    3    4    5
    [2,]    6    7    8    9   10
    [3,]   11   12   13   14   15
    [4,]   16   17   18   19   20
    [5,]   21   22   23   24   25

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
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. code-block:: R

    library(gamstransfer)
    df <- data.frame(
    h_1 = paste0("h", 1:8760), m_2 = paste0("m", 1:60),
    s_3 = paste0("s", 1:60)
    )
    df$level <- runif(nrow(df), min = 0, max = 100)
    df$marginal <- 0.0
    df$lower <- SpecialValues$NEGINF
    df$upper <- SpecialValues$POSINF
    df$scale <- 1.0

    m <- Container$new()
    hrs <- Set$new(m, "h", records = unique(df$h_1))
    mins <- Set$new(m, "m", records = unique(df$m_2))
    secs <- Set$new(m, "s", records = unique(df$s_3))

    df$h_1 <- factor(df$h_1, ordered = TRUE)
    df$m_2 <- factor(df$m_2, ordered = TRUE)
    df$s_3 <- factor(df$s_3, ordered = TRUE)

    a <- Variable$new(m, "a", domain = c(hrs, mins, secs))

    # set records
    a$records <- df

.. code-block:: R

    > a$isValid()
    [1] TRUE

In this example, we create a large variable (31,536,000 records and
8880 unique domain elements. We mimic data that is labeled for 
every second in one year) and assign it to a variable with ``a$records``. 
GAMS Transfer R requires that all domain columns must be ordered factors. 
The ``records`` setter function does very little work other than checking 
if the object being set is a data frame. This places more responsibility 
on the user to create a data frame that complies with the standard format. 
In Example \#1, we take care to properly reference the factor from the 
domain sets and ensure that the symbol ``a`` is valid with ``a$isValid() = TRUE``.

As with Set and Parameters, users can use the ``$isValid(verbose=TRUE)`` 
method to debug any structural issues.

.. note:: 
    One can also use the :doc:`generateRecords() <../additional_features/generate_records>`
    method to automatically populate randomly generated symbol records in the standard format.
