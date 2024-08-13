Data Exchange with GDX
======================================

Up until now, we have been focused on using GAMS Transfer R to create 
symbols in an empty :doc:`Container <../../api_reference/Container>` using the symbol constructors (or 
their corresponding container methods). These tools will enable users 
to ingest data from many different formats and add them to a ``Container``. 
However, it is also possible to read in symbol data directly from 
GDX files using the ``read`` container method. In the following sections, 
we will discuss this method in detail as well as the ``write`` method, 
which allows users to write out to new GDX files.

Reading from GDX
-------------------

There are two main ways to read in GDX based data.

-
    Pass the file path directly to the :ref:`Container constructor <Container Constructor>` 
    (will read all symbols and records)
-
    Pass the file path directly to the ``read`` method (default read all 
    symbols, but can read partial files)

The first option here is provided for convenience and will, internally, 
call the ``read`` method. For the following 
examples, we leverage the GDX output generated from the 
`trnsport.gms <https://www.gams.com/latest/gamslib_ml/libhtml/gamslib_trnsport.html>`_ model file.

Example (reading full data into a Container using the constructor)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. code-block:: R

    library(gamstransfer)
    m = Container$new("trnsport.gdx")


.. code-block:: R

    > m$listSymbols()
    [1] "i"      "j"      "a"      "b"      "d"      "f"      "c"      "x"
    [9] "z"      "cost"   "supply" "demand"


.. code-block:: R

    > m$describeParameters()
    name domain domainType dimension numberRecords     min     mean     max
    1    a      i    regular         1             2 350.000 475.0000 600.000
    2    b      j    regular         1             3 275.000 300.0000 325.000
    5    c    i j    regular         2             6   0.126   0.1755   0.225
    3    d    i j    regular         2             6   1.400   1.9500   2.500
    4    f     NA       none         0             1  90.000  90.0000  90.000
    whereMin whereMax sparsity
    1        1        2        0
    2        3        1        0
    5        6        1        0
    3        6        1        0
    4        1        1       NA


A user could also read in data with the :ref:`read method <Container Methods>` 
as shown in the following example.

Example (reading full data into a Container with ``read`` method)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. code-block:: R

    library(gamstransfer)
    m = Container$new()
    m$read("trnsport.gdx")

.. code-block:: R

    > m$listSymbols()
    [1] "i"      "j"      "a"      "b"      "d"      "f"      "c"      "x"
    [9] "z"      "cost"   "supply" "demand"

It is also possible to read in a partial GDX file with the :ref:`read method <Container Methods>`,
as shown in the following example:

.. code-block:: R

    library(gamstransfer)
    m = Container$new()
    m$read("trnsport.gdx", "x")

.. code-block:: R

    > m$listSymbols()
    [1] "x"

    > m["x"]$records
            i        j level marginal lower upper scale
    1   seattle new-york    50    0.000     0   Inf     1
    2   seattle  chicago   300    0.000     0   Inf     1
    3   seattle   topeka     0    0.036     0   Inf     1
    4 san-diego new-york   275    0.000     0   Inf     1
    5 san-diego  chicago     0    0.009     0   Inf     1
    6 san-diego   topeka   275    0.000     0   Inf     1

This syntax assumes that the user will always want to read in 
both the metadata as well as the actual data records, but it 
is possible to skip the reading of the records by passing the 
argument ``records=FALSE``.

.. code-block:: R

    library(gamstransfer)
    m = Container$new()
    m$read("trnsport.gdx", "x", records = FALSE)

.. code-block:: R

    > m$listSymbols()
    [1] "x"

    > m["x"]$summary
    $name
    [1] "x"

    $description
    [1] "shipment quantities in cases"

    $type
    [1] "positive"

    $domain
    [1] "i" "j"

    $domainType
    [1] "relaxed"

    $dimension
    [1] 2

    $numberRecords
    [1] 0

    > m["x"]$records
    NULL

.. note:: 
    The ``read`` method attempts to preserve the symbol domain type 
    from the source but if domain sets are not part of the read operation 
    there is no choice but to default to a "relaxed" 
    ``domainType``. This can be seen in the last example where we only read 
    in the variable ``x`` and not the domain sets (``i`` and ``j``) that the variable 
    is defined over. All the data will be available to the user, but domain 
    checking is no longer possible. The symbol ``x`` will remain with "relaxed" 
    domain type even if the user were to read in sets ``i`` and ``j`` in a second 
    ``read`` call.

Writing to GDX
--------------------

A user can write data to a GDX file by simply passing a file path (as a 
string). The ``write`` method will then create the GDX and write all data 
in the ``Container``.

.. note:: 
    It is not possible to write the :doc:`Container <../../api_reference/Container>` 
    when any of its symbols 
    are invalid. If any symbols are invalid an error will be raised and the 
    user will need to inspect the problematic symbols (perhaps using a 
    combination of the ``listSymbols(isValid=FALSE)`` and ``isValid(verbose=TRUE)`` 
    methods).

Example
~~~~~~~~~~

.. code-block:: R

    m$write("path/to/file.gdx")

Example (write a compressed GDX file)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. code-block:: R

    m$write("path/to/file.gdx", compress = TRUE)

Advanced users might want to specify an order to their UELs (i.e., 
the :doc:`universe set <universe_set>`); recall that the 
UEL ordering follows that dictated by the data. 
As a convenience, it is possible to prepend the list of UELs with 
a user specified order using the ``uelPriority`` argument.

Example (change the order of the UEL)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. code-block:: R

    library(gamstransfer)
    m = Container$new()
    i = Set$new(m, "i", records=c("a", "b", "c"))
    m$write("foo.gdx", uelPriority=c("a", "c"))

The original UEL order for this GDX file would have been 
``c("a", "b", "c")``, but since this example reorders the UELs with 
``uelPriority``, the positions of ``b`` and ``c`` have been swapped. 
This can be verified with the `gdxdump utility <https://www.gams.com/latest/docs/T_GDXDUMP.html>`_ 
(using the ``uelTable`` argument):

.. code-block:: sh

    gdxdump foo.gdx ueltable=foo

    Set foo /
    'a' ,
    'c' ,
    'b' /;
    $onEmpty

    Set i(*) /
    'a',
    'c',
    'b' /;

    $offEmpty
