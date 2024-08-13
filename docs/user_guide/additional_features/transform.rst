Matrix Generation
=====================

GAMS Transfer R stores data in a "flat" format, that is, one record entry 
per data frame row. However, it is often necessary to convert this data 
format into a matrix/array format. GAMS Transfer R enables users to do this 
with relative ease using the ``toDense`` symbol
methods This method will return a dense ``N``-dimensional 
array (matrix for 2-Dimensions) with each dimension corresponding to 
the GAMS symbol dimension; it is possible to output an array up 
to 20 dimensions (a GAMS limit). 

Example (1D data w/o domain linking (i.e., a relaxed domain))
----------------------------------------------------------------

.. code-block:: R

    library(gamstransfer)
    m = Container$new()
    a = Parameter$new(m, "a", "i", records = data.frame(uni=c("a","c"), element_text=c(1,3)))

.. code-block:: R

    > a$toDense()
    [1] 1 3

Note that the parameter ``a`` is not linked to another symbol, so when 
converting to a matrix, the indexing is referenced to the data structure 
in ``a$records``. Defining a sparse parameter ``a`` over a set ``i`` allows us 
to extract information from the ``i`` domain and construct a very different 
dense matrix, as the following example shows:

.. code-block:: R

    library(gamstransfer)
    m = Container$new()
    i = Set$new(m, "i", records = c("a", "b", "c", "d"))
    a = Parameter$new(m, "a", i, records = data.frame(c("a","c"), c(1,3)))

.. code-block:: R

    > a$toDense()
    [1] 1 0 3 0

Example (2D data w/ domain linking)
-----------------------------------------

.. code-block:: R

    library(gamstransfer)
    m = Container$new()
    i = Set$new(m, "i", records = c("a", "b", "c", "d"))
    a = Parameter$new(m, "a", c(i, i), records = 
    data.frame(i=c("a","c"), i=c("a","c"), c(1,3)))

.. code-block:: R

    > i$records
        uni
    1     a
    2     b
    3     c
    4     d

    > a$records
        i i.1 value
    1   a   a     1
    2   c   c     3

    > a$toDense()
        [,1] [,2] [,3] [,4]
    [1,]    1    0    0    0
    [2,]    0    0    0    0
    [3,]    0    0    3    0
    [4,]    0    0    0    0

.. note:: 
    If there are unused UELs in the domain symbol, ``toDense()`` requires 
    that the unused UELs are at the end of UEL list. One can achieve this by 
    calling the ``reorderUELs()`` method for the domain symbol. Similarly, if the 
    symbol records are in a different order than that of domain symbol UEL, UELs 
    should be reordered to follow the record order. This can also be achived 
    using ``reorderUELs()`` symbol method.


