Set
============

Adding Set Records
-------------------------

:ref:`Three possibilities <add_symbol_records>` exist to assign
symbol records to a set: We show a few examples of ways to create
differently structured sets.

Example \#1 - Create a 1D set from a vector
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. code-block:: R

    library(gamstransfer)
    m <- Container$new()
    i <- Set$new(m, "i", records = c("seattle", "san-diego"))

    # NOTE: the above syntax is equivalent to -
    # i = Set$new(m, "i")
    # i$setRecords(c("seattle", "san-diego"))

    # NOTE: the above syntax is also equivalent to -
    # m$addSet("i", records= c("seattle", "san-diego"))

    # NOTE: the above syntax is also equivalent to -
    # i = m$addSet("i")
    # i$setRecords(c("seattle", "san-diego"))

    # NOTE: the above syntax is also equivalent to -
    # m$addSet("i")
    # m["i"]$setRecords(c("seattle", "san-diego"))

.. code-block:: R

    > i$records
            uni
    1   seattle
    2 san-diego

Example \#2 - Create a 2D set from a list
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. code-block:: R

    library(gamstransfer)
    m <- Container$new()
    k <- Set$new(m, "k", c("*", "*"), records = list("seattle", "san-diego"))

    # NOTE: the above syntax is equivalent to -
    # k = Set$new(m, "k", c("*", "*"))
    # k$setRecords(list("seattle", "san-diego"))

    # NOTE: the above syntax is also equivalent to -
    # m$addSet("k", c("*","*"), records=list("seattle", "san-diego"))

    # NOTE: the above syntax is also equivalent to -
    # k = m$addSet("k", c("*","*"))
    # k$setRecords(list("seattle", "san-diego"))

    # NOTE: the above syntax is also equivalent to -
    # m$addSet("k", c("*", "*"))
    # m["k"]$setRecords(list("seattle", "san-diego"))

.. code-block:: R

    > k$records
        uni_1     uni_2
    1 seattle san-diego

Example \#3 - Create a 1D set from a data frame slice
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. code-block:: R

    library(gamstransfer)
    m <- Container$new()

    dist <- data.frame(
    from = c(
        "seattle", "seattle", "seattle",
        "san-diego", "san-diego", "san-diego"
    ),
    to = c(
        "new-york", "chicago", "topeka",
        "new-york", "chicago", "topeka"
    ),
    thousand_miles = c(2.5, 1.7, 1.8, 2.5, 1.8, 1.4)
    )

    l <- Set$new(m, "l", records = unique(dist[["from"]]))

    # NOTE: the above syntax is equivalent to -
    # l = Set$new(m, "l")
    # l$setRecords(unique(dist[["from"]]))

    # NOTE: the above syntax is also equivalent to -
    # m$addSet("l", records=unique(dist[["from"]]))

    # NOTE: the above syntax is also equivalent to -
    # l = m$addSet("l")
    # l$setRecords(unique(dist[["from"]]))

    # NOTE: the above syntax is also equivalent to -
    # m$addSet("l")
    # m["l"]$setRecords(unique(dist[["from"]]))

.. code-block:: R

    > l$records
        uni
    1   seattle
    2 san-diego

Set element text is very handy when labeling specific
set elements within a set. A user can add a set element
text directly with a set element. Note that it is not 
required to label all set elements, as can be seen in 
the following example.

Example \#4 - Add set element text
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. code-block:: R

    library(gamstransfer)
    m <- Container$new()
    i <- Set$new(m, "i",
    records <- data.frame(
        city = c("seattle", "san-diego", "washington_dc"),
        text = c("home of sub pop records", "", "former gams hq")
    )
    )

    # NOTE: the above syntax is equivalent to -
    #
    # i = Set$new(m, "i")
    # i_recs = data.frame(city=c("seattle", "san-diego", "washington_dc"),
    # text=c("home of sub pop records", "", "former gams hq"))
    #
    # i$setRecords(i_recs)

    # NOTE: the above syntax is also equivalent to -
    # m$addSet("i", records=i_recs)

    # NOTE: the above syntax is also equivalent to -
    # i = m$addSet("i")
    # i$setRecords(i_recs)

    # NOTE: the above syntax is also equivalent to -
    # m$addSet("i")
    # m["i"]$setRecords(i_recs)

.. code-block:: R

    > i$records
            city            element_text
    1       seattle home of sub pop records
    2     san-diego
    3 washington_dc          former gams hq

The primary advantage of the ``setRecords`` method is that GAMS Transfer R 
will convert many different (and convenient) data types into the 
standard data format (a data frame). Users that require higher 
performance will want to directly pass the 
:doc:`Container <../../api_reference/Container>` a reference to 
a valid data frame, thereby skipping some of these computational 
steps. This places more burden on the user to pass the data in a valid 
standard form, but it speeds the records setting process.  
In this section, we walk the user 
through an example of how to set records directly.

Example \#5 - Directly set records (1D set)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. code-block:: R

    library(gamstransfer)
    m <- Container$new()
    i <- Set$new(m, "i", description = "supply")

    # create a standard format data frame
    df_i <- data.frame(
    uni_1 <- c("seattle", "san-diego"),
    element_text = c("", "")
    )

    # need to create categorical column type, referencing elements already in df_i
    df_i$uni_1 <- factor(df_i$uni_1, ordered = TRUE)

    # set the records directly
    i$records <- df_i

.. code-block:: R

    > i$isValid()
    [1] TRUE

Stepping through this example we take the following steps:

1. Create an empty :doc:`Container <../../api_reference/Container>`
2. Create a GAMS set ``i`` in the Container, but do not set the ``records``
3. Create a data frame (manually, in this example) taking care 
   to follow the :doc:`standard format <../additional_features/standard_formats>`
4. The data frame has the right shape and column labels so 
   we can proceed to set the records.
5. We need to cast the ``uni_1`` column as a ``factor``, 
   so we create a custom ordered category type using ``factor``
6. Finally, we set the records directly by passing a reference to 
   ``df_i`` into the symbol records attribute.  The setter function of 
   ``records`` checks that a data frame is being set, but does not check 
   validity. Thus, as a final step, we call the ``$isValid()`` method to 
   verify that the symbol is valid.

.. note:: 
    Users can debug their data frames by running 
    `<symbol_name>$isValid(verbose=TRUE)` to get feedback about their data.

Example \#6 - Directly set records (1D subset)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. code-block:: R

    library(gamstransfer)
    m <- Container$new()
    i <- Set$new(m, "i", records = c("seattle", "san-diego"), description = "supply")
    j <- Set$new(m, "j", i, description = "supply")

    # create a standard format data frame
    df_j <- data.frame(i_1 = c("seattle"), "element_text" = c(""))

    # create the categorical column type
    df_j$i_1 <- factor(df_j$i_1, levels = i$records[, 1], ordered = TRUE)

    # set the records
    j$records <- df_j

.. code-block:: R

    > j$isValid()
    [1] TRUE

This example is more subtle in that we want to create a set ``j`` 
that is a subset of ``i``. We create the set ``i`` using the ``setRecords`` 
method but then set the records directly for ``j``. There are two 
important details to note: 1) the column labels in ``df_j`` now reflect 
the standard format for a symbol with a domain set (as opposed to 
the universe) and 2) we create the factors by referencing the parent 
set (``i``) for the ``levels`` (instead of referencing itself).

.. note:: 
    One can also use the :doc:`generateRecords() <../additional_features/generate_records>`
    method to automatically populate randomly generated 
    symbol records in the standard format.
