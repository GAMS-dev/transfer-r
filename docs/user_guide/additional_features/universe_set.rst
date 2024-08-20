Universe Set
==================

A Unique Element (UEL) is
an ``(i,s)`` pair where ``i`` is an identification number for a string ``s``. 
A list of all UELs is also known as 'the universe' or 'the universe set'.
GAMS uses UELs to efficiently store domain entries of a record by 
storing the UEL ID ``i`` of a domain entry instead of the actual string ``s``.
This avoids storing the same string multiple times. The concept of UELs 
also exists in R and is called a "factor". GAMS Transfer R leverages these 
types in order to efficiently store strings and enable domain checking 
within the R environment.

Each domain column in a data frame can be a factor, the effect is that each 
symbol maintains its own list of UELs per dimension. R lets the user choose
if levels in a factor should be regarded as ordered. GAMS Transfer R relies 
exclusively on ``ordered`` factors. By using ordered 
factors, GAMS Transfer R will order the UELs such that elements appear 
in the order in which they appeared in the data (which is how GAMS 
defines the UELs). ``gamstransfer`` allows the user to reorder the UELs with 
the ``uelPriority`` argument in the ``$write()`` method.

GAMS Transfer R does not keep track of the UELs separately from 
other symbols in the :ref:`Container <Container Constructor>`, 
it will be created internal to the 
``$write()`` method and is based on the order in which data is added 
to the container. The user can access the current state of UELs 
with the ``$getUELs()`` :ref:`container method <Container Methods>`. For example, we set a 
two dimensional set:

.. code-block:: R

    library(gamstransfer)
    m <- Container$new()
    j <- Set$new(m, "j", c("*", "*"), records = data.frame(i = c("i1", "i2"), j = c("j1", "j2")))

.. code-block:: R

    > j$records
        i     j
    1    i1    j1
    2    i2    j2

    > m$getUELs()
    [1] "i1" "i2" "j1" "j2"

Customize The Universe Set
------------------------------

GAMS Transfer R allows the users to customize the Universe Set with the help of Symbol
UELs methods shown in the following table.

.. list-table::
   :widths: 10 50 30 10
   :header-rows: 1

   * - Method
     - Description
     - Arguments/Defaults
     - returns
   * - ``addUELs``
     -  adds UELs to the symbol
     - ``uels`` (character), ``dimension=NULL``
     - ``NULL``
   * - ``getUELs``
     - returns the UELs used in the Symbol
     - ``dimension = NULL``, ``codes = NULL``, ``ignoreUnused=FALSE``
     - ``character vector``
   * - ``removeUELs``
     -  removes UELs from the symbol
     - ``uels=NULL`` (character), ``dimension=NULL``
     - ``NULL``
   * - ``renameUELs``
     - renames UELs in the symbol
     - ``uels``(character of same length as current UELs or named character vector), ``dimension=NULL``, ``allowMerge=FALSE`` (logical). If ``allowMerge = TRUE``, the underlying integer mapping of a factor is allowed to change to offer additional data flexibility
     - ``NULL``
   * - ``reorderUELs``
     - reorders UELs in the symbol that appear in the symbol ``dimensions``. If ``uels`` is ``NULL``, the UELs are reordered based on symbol records, unused UELs are moved to the end. If ``dimensions`` is ``NULL`` then reorder UELs in all dimensions of the symbol
     - ``uels=NULL``(``NULL`` or character of same length as current UELs or named character vector), ``dimension=NULL`` (numeric)
     - ``NULL``
   * - ``setUELs``
     - sets UELs for the Symbol
     - ``uels``(character), ``dimension``, ``rename=FALSE``
     - ``NULL``

.. note::
    Symbols should be valid in order to use these methods.

.. note::
    when ``dimension`` argument is optional, the method is applied to all dimensions

Some of these methods are also available as Container methods as shown in the following table. 
Container methods internally call the corresponding Symbol methods.

.. list-table::
   :widths: 10 50 30 10
   :header-rows: 1

   * - Method
     - Description
     - Arguments/Defaults
     - returns
   * - ``getUELs``
     - returns the UELs used in the Container
     - ``symbols=NULL`` (character), ``ignoreUnused = FALSE``
     - ``character vector``
   * - ``removeUELs``
     - removes ``uels`` from all symbols the Container
     - ``uels=NULL`` (character)
     - ``NULL``
   * - ``renameUELs``
     - renames ``uels`` in the Container
     - ``uels`` (named character), ``allowMerge=FALSE`` (logical). If ``allowMerge = TRUE``, the underlying integer mapping of a factor is allowed to change to offer additional data flexibility
     - ``NULL``

.. _getuels_examples:

getUELs Examples
~~~~~~~~~~~~~~~~~~~~~~~~~

.. code-block:: R

library(gamstransfer)
m <- Container$new()
i <- Set$new(m, "i", records = c("i1", "i2", "i3"))
j <- Set$new(m, "j", records = c("j1", "j2", "j3"))
a <- Parameter$new(m, "a", c(i, j), records = data.frame(paste0("i", 1:4), paste0("j", 1:4), 1:4))

.. code-block:: R

    > i$getUELs()
    [1] "i1" "i2" "i3"

    > m$getUELs()
    [1] "i1" "i2" "i3" "j1" "j2" "j3" "i4" "j4"

    > m$getUELs("j")
    [1] "j1" "j2" "j3"

.. _adduels_examples:

addUELs Examples
~~~~~~~~~~~~~~~~~~~~~~~~

.. code-block:: R

    library(gamstransfer)
    m <- Container$new()
    i <- Set$new(m, "i", records = c("i1", "i2", "i3"))
    j <- Set$new(m, "j", records = c("j1", "j2", "j3"))
    a <- Parameter$new(m, "a", c(i, j), records = data.frame(paste0("i", 1:3), paste0("j", 1:3), 1:3))

    i$addUELs("ham")
    a$addUELs("and", 1)
    a$addUELs("cheese", 2)

.. code-block:: R

    > i$getUELs()
    [1] "i1"  "i2"  "i3"  "ham"

    > a$getUELs()
    [1] "i1"     "i2"     "i3"     "and"    "j1"     "j2"     "j3"     "cheese"

In this example we have added three new (unused) UELs: ``ham``, ``and``, 
``cheese``. These three UELs will now appear in the GAMS universe set 
(accessible with ``m$getUELs()``).  The addition of unused UELs does not 
impact the validity of the symbols (i.e., unused UELs will not 
trigger domain violations).

.. _removeuels_examples:

removeUELs Examples
~~~~~~~~~~~~~~~~~~~~~~~~~

.. code-block:: R

    library(gamstransfer)
    m <- Container$new()
    i <- Set$new(m, "i", records = c("i1", "i2", "i3"))
    j <- Set$new(m, "j", records = c("j1", "j2", "j3"))
    a <- Parameter$new(m, "a", c(i, j), records = data.frame(paste0("i", 1:3), paste0("j", 1:3), 1:3))

    i$addUELs("ham")
    a$addUELs("and", 1)
    a$addUELs("cheese", 2)

    # remove symbol UELs explicitly by dimension
    i$removeUELs("ham", 1)
    a$removeUELs("and", 1)
    a$removeUELs(c("and", "cheese"), 2)

    # remove symbol UELs for the entire symbol
    i$removeUELs("ham")
    a$removeUELs(c("and", "cheese"))

    # remove ONLY unused UELs from each symbol, independently
    i$removeUELs()
    a$removeUELs()

    # remove ONLY unused UELs from the entire container (all symbols)
    m$removeUELs()

.. code-block:: R

    > m$getUELs()
    [1] "i1" "i2" "i3" "j1" "j2" "j3"

If a user removes a UEL that appears in data, that data will be lost perminately. 
The domain label will be transformed into an NA as seen in this example:

.. code-block:: R

    library(gamstransfer)
    m <- Container$new()
    i <- Set$new(m, "i", records = c("i1", "i2", "i3"))
    j <- Set$new(m, "j", records = c("j1", "j2", "j3"))
    a <- Parameter$new(m, "a", c(i, j), records = data.frame(i = paste0("i", 1:3), j = paste0("j", 1:3), 1:3))

    m$removeUELs("i1")

.. code-block:: R

    > i$records
    uni
    1 <NA>
    2   i2
    3   i3

    > a$records
        i  j value
    1 <NA> j1     1
    2   i2 j2     2
    3   i3 j3     3

.. note:: 
    A container cannot be written if there are NA entries in any of the 
    domain columns (in any symbol). An error is thrown if there are missing domain labels.

.. _renameuels_examples:

renameUELs Examples
~~~~~~~~~~~~~~~~~~~~~~~~

``renameUELs`` is a method of all GAMS ``Symbol`` classes as well as the ``Container`` class. 
This method allows the user to rename UELs in a symbol dimension(s), over several symbols, 
or over the entire container. This method is handy when attempting to harmonize labeling 
schemes between data structures that originated from different sources. 
For example: 

.. code-block:: R

    library(gamstransfer)
    m <- Container$new()
    a <- Parameter$new(m, "a", c("*", "*"),
    records = data.frame(
        from = c("WI", "IL", "WI"),
        to = c("IL", "IN", "IN"), quantity = c(10, 12.5, 8.7)
    ),
    description = "shipment quantities"
    )

    b <- Parameter$new(m, "b", c("*"),
    records = data.frame(
        state = c("wisconsin", "illinois", "indiana"),
        c(1.2, 1.7, 1.2)
    ), description = "multipliers"
    )

results in the following records:

.. code-block:: R

    > a$records
    from    to value
    1    WI    IL  10.0
    2    IL    IN  12.5
    3    WI    IN   8.7

    > b$records
        state value
    1 wisconsin   1.2
    2  illinois   1.7
    3   indiana   1.2

However, two different data sources were used to generate the 
parameters ``a`` and ``b`` -- one data source used the uppercase 
postal abbreviation of the state name and the other source used a 
lowercase full state name as the uniqe identifier. With the 
following syntax the user can harmonize to a mixed case postal 
code labeling scheme (without losing any of the original UEL ordering).

.. code-block:: R

    m$renameUELs(c(
    "WI" = "Wi", "IL" = "Il", "IN" = "In",
    "wisconsin" = "Wi", "illinois" = "Il", "indiana" = "In"
    ))

This results in the following records and the universe set:

.. code-block:: R

    > a$records
    from    to value
    1    Wi    Il  10.0
    2    Il    In  12.5
    3    Wi    In   8.7

    > b$records
    state value
    1    Wi   1.2
    2    Il   1.7

    > m$getUELs()
    [1] "Wi" "Il" "In"

.. _reorderuels_examples:

reorderUELs Examples
~~~~~~~~~~~~~~~~~~~~~~~~~~

``reorderUELs`` is a method of all GAMS symbol classes. This method allows the 
user to reorder UELs of a specific symbol dimension. ``reorderUELs`` will not 
add/remove any UELs. For example:

.. code-block:: R

    library(gamstransfer)
    m <- Container$new()
    i <- Set$new(m, "i", records = c("i1", "i2", "i3"))

.. code-block:: R

    > i$getUELs()
    [1] "i1" "i2" "i3"

    > m$getUELs()
    [1] "i1" "i2" "i3" "j1" "j2" "j3"

But perhaps we want to reorder the UELs ``i1``, ``i2``, ``i3`` to ``i3``, ``i2``, ``i1``.

.. code-block:: R

    i$reorderUELs(c("i3", "i2", "i1"))

.. code-block:: R

    > i$getUELs()
    [1] "i3" "i2" "i1"

    > i$records
        uni
    1    i1
    2    i2
    3    i3

.. note:: 
    This example does not change the indexing scheme of the data frame. 
    It only changes the underlying integer numbering scheme for the factor levels.  
    We can see this by looking at the ``levels``:

.. code-block:: R

    > as.integer(i$records$uni)
    [1] 3 2 1

When ``reorderUELs()`` is used without the ``uels`` argument, the UELs are rearranged 
based on the records order as illustrated in the following example.

.. code-block:: R

    library(gamstransfer)
    m <- Container$new()
    i <- Set$new(m, "i", records = c("i1", "i2", "i3"))
    i$setUELs(c("i2", "i3", "i1"))

.. code-block:: R

    > i$getUELs()
    [1] "i2" "i3" "i1"

    > i$reorderUELs()

    > i$getUELs()
    [1] "i1" "i2" "i3"

Moreover, if there are unused UELs, they are moved to the end as shown below. 
Here, ``i4`` is the unused UELs that gets moved to the end after using ``reorderUELs``.

.. code-block:: R

    > i$setUELs(c("i2","i3","i4","i1"))

    > i$getUELs()
    [1] "i2" "i3" "i4" "i1"

    > i$reorderUELs()

    > i$getUELs()
    [1] "i1" "i2" "i3" "i4"

.. _setuels_examples:

setUELs Examples
~~~~~~~~~~~~~~~~~~

``setUELs`` is a method of all GAMS symbol classes. This method allows the user to 
create new UELs, rename UELs, and reorder UELs all in one method. For example:

.. code-block:: R

    library(gamstransfer)
    m <- Container$new()
    i <- Set$new(m, "i", records = c("i1", "i2", "i3"))

A user could accomplish a UEL reorder operation with ``setUELs``:

.. code-block:: R

    > i$setUELs(c("i3","i2","i1"))

    > i$getUELs()
    [1] "i3" "i2" "i1"

    > i$records
        uni
    1    i1
    2    i2
    3    i3

A user could accomplish a UEL reorder + add UELs operation with ``setUELs``:

.. code-block:: R

    > i$setUELs(c("i3", "i2", "i1", "j1", "j2"))
    > i$getUELs()
    [1] "i3" "i2" "i1" "j1" "j2"

    > i$records
        uni
    1    i1
    2    i2
    3    i3

    > as.integer(i$records$uni)
    [1] 3 2 1


A user could accomplish a UEL reorder + add + rename with ``setUELs``:

.. code-block:: R

    > i$setUELs(c("j3", "j2", "j1", "ham", "cheese"), rename=TRUE)

    > i$setUELs(c("j3", "j2", "j1", "ham", "cheese"), rename=TRUE)

    > i$getUELs()
    [1] "j3"     "j2"     "j1"     "ham"    "cheese"

    > i$records
        uni
    1    j1
    2    j2
    3    j3

    > as.integer(i$records$uni)
    [1] 3 2 1

.. note:: 
    This example does not change the indexing scheme of the data frame, 
    but the ``rename=TRUE`` flag means that the records will get updated as if 
    a ``renameUELs`` call had been made.

If a user wanted to set new UELs on top of this data, without renaming, 
they would need to be careful to include the current UELs in the UELs being set. 
It is possible to lose these labels if they are not included 
(which will prevent the data from being written to GDX).

.. code-block:: R

    library(gamstransfer)
    m <- Container$new()
    i <- Set$new(m, "i", records = c("i1", "i2", "i3"))
    i$setUELs(c("j1", "i2", "j3", "ham", "cheese"))

.. code-block:: R

    > i$getUELs()
    [1] "j1"     "i2"     "j3"     "ham"    "cheese"

    > i$records
    uni_1
    1  <NA>
    2    i2
    3  <NA>

