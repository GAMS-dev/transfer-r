Data Exchange with Containers
===================================

GAMS Transfer R allows data exchange between two Containers with the help of 
read and copy methods.

Data exchange with read
-----------------------------

Similar to reading from a GDX file, a Container can read from another 
Container object. Following examples demonstrate this with
the help of the GDX output generated from
`trnsport.gms <https://www.gams.com/latest/gamslib_ml/libhtml/gamslib_trnsport.html>`_ model file.

Example (reading data from Container)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. code-block:: R

    library(gamstransfer)
    c <- Container$new("trnsport.gdx")

.. code-block:: R

    > c$listSymbols()
    [1] "i"      "j"      "a"      "b"      "d"      "f"      "c"      "x"
    [9] "z"      "cost"   "supply" "demand"

.. code-block:: R

    m <- Container$new(c)

.. code-block:: R

    > m$listSymbols()
    [1] "i"      "j"      "a"      "b"      "d"      "f"      "c"      "x"
    [9] "z"      "cost"   "supply" "demand"

.. code-block:: R

    m <- Container$new()
    m$read(c)

.. code-block:: R

    > m$listSymbols()
    [1] "i"      "j"      "a"      "b"      "d"      "f"      "c"      "x"
    [9] "z"      "cost"   "supply" "demand"

.. code-block:: R

    m <- Container$new()
    m$read(c, symbols=c("d","f"))

.. code-block:: R

    > m$listSymbols()
        [1] "d" "f"

Example (reading data when symbol with the same name already exists)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. code-block:: R

    library(gamstransfer)
    c <- Container$new()
    i <- Set$new(c, "i")
    p <- Parameter$new(m, "p")

    m <- Container$new()
    i <- Set$new(m, "i")

.. code-block:: R

    > m$read(c, symbols="i")
    Error in private$.containerRead(loadFrom, symbols, records) :
    Attempting to add symbol i, however, one already exists in the Container. Symbol replacement is only possible if the symbol is first removed from theContainer with the removeSymbols() method.

The container read method does not allow reading from another source (Container, or 
a GDX file) when a symbol with the same name already exists. The existing symbol must be removed or renamed.

Data exchange with copy
--------------------------------

Symbol ``copy`` method provides an alternative way to exchange data between Containers. Following 
examples demonstrate this starting from a Container containing data from GDX output 
generated from `trnsport.gms <https://www.gams.com/latest/gamslib_ml/libhtml/gamslib_trnsport.html>`_ model file.

Example (copy symbol from one container to another)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. code-block:: R

    library(gamstransfer)
    c = Container$new("trnsport.gdx")

.. code-block:: R

    > c$listSymbols()
    [1] "i"      "j"      "a"      "b"      "d"      "f"      "c"      "x"
    [9] "z"      "cost"   "supply" "demand"

.. code-block:: R

    m <- Container$new()

    c["f"]$copy(m)

.. code-block:: R

    > m$listSymbols()
    [1] "f"

The above example copies symbol ``f`` from Container ``c`` to Container ``m``. If one copies 
a symbol with domain that does not exist in the destination Container, domain is relaxed as shown 
in the following example.

Example (copy symbol to another container without domain symbols)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. code-block:: R

    library(gamstransfer)
    c <- Container$new("trnsport.gdx")

.. code-block:: R

    > c$listSymbols()
    [1] "i"      "j"      "a"      "b"      "d"      "f"      "c"      "x"
    [9] "z"      "cost"   "supply" "demand"

.. code-block:: R

    m <- Container$new()
    c["d"]$copy(m)

.. code-block:: R

    > m$listSymbols()
    [1] "d"

    > m["d"]$domain
    [1] "i" "j"

    > m["d"]$domainType
    [1] "relaxed"

Example (copy symbol to another container with overwrite)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. code-block:: R

    library(gamstransfer)
    c <- Container$new()
    i <- Set$new(c, "i", records=c("i1","i2"))

.. code-block:: R

    > i$records
        uni
    1    i1
    2    i2

.. code-block:: R

    m <- Container$new()
    i <- Set$new(m, "i", records = c("i3", "i4"))

.. code-block:: R

    > i$records
        uni
    1    i3
    2    i4

    # the following command throws an error
    > c["i"]$copy(m)
    Error in private$.copy(destination, overwrite) :
    Symbol i already exists in ``destination``


    > c["i"]$copy(m, overwrite = TRUE)
    > m["i"]$records
        uni
    1    i1
    2    i2

Example (bulk copy operation via Container ``copy`` method)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

A bulk operation is also possible via Container ``copy`` method as shown in the following example.

.. code-block:: R

    library(gamstransfer)
    c <- Container$new("trnsport.gdx")

.. code-block:: R

    > c$listSymbols()
    [1] "i"      "j"      "a"      "b"      "d"      "f"      "c"      "x"
    [9] "z"      "cost"   "supply" "demand"

.. code-block:: R

    m <- Container$new()
    c$copy(m) # copy all symbols

.. code-block:: R

    > c$listSymbols()
    [1] "i"      "j"      "a"      "b"      "d"      "f"      "c"      "x"
    [9] "z"      "cost"   "supply" "demand"

.. code-block:: R

    m <- Container$new()
    c$copy(m, symbols = c("a", "b", "d")) # copy a subset of symbols

.. code-block:: R

    > m$listSymbols()
    [1] "a" "b" "d"

.. code-block:: R

    c$copy(m, symbols="a", overwrite = TRUE) # copy symbols with overwrite

.. code-block:: R

    > m$listSymbols()
    [1] "a" "b" "d"

