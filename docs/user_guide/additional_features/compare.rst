Comparing gamstransfer Objects
======================================

Equivalence between two symbols or between two containers can be tested using 
``equals`` method. Since the order of records in the ``records`` data frame is not 
important from a GDX point of view, ``equals`` method compares symbol ``records`` 
independently from the order in which they are stored in the records data frame.
As this requires a merge operation over the domain columns, ``equals`` is a 
computationally expensive call.

.. note::
    We do not recommend using ``equals`` method inside large loops or 
    when performance is critical. It is, however, very useful for data debugging.

A quick example shows the syntax of ``equals``:

.. code-block:: R

    library(gamstransfer)
    m <- Container$new()
    i <- Set$new(m, "i", records = paste0("i", 1:5), description = "set i")
    j <- Set$new(m, "j", records = paste0("i", 1:5), description = "set j")

.. code-block:: R

    > i$equals(j)
    [1] FALSE

One can debug the reason for inequality using the option ``verbose``.

.. code-block:: R

    > i$equals(j, verbose=TRUE)
    Symbol names do not match i != j
    [1] FALSE

By default, ``equals`` takes the strictest view of symbol "equality", i.e., 
everything must be equal. In this case, the symbol names and descriptions 
differ between the two sets ``i`` and ``j``. We can relax this with a combination 
of argument flags. Comparing the two symbols again, but ignoring the meta 
data (i.e., ignoring the symbol name, description and type (if a Variable 
or Equation)):

.. code-block:: R

    > i$equals(j, checkMetaData=FALSE)
    [1] TRUE

The ``checkUELs`` argument will ensure that the symbol "universe" is the same 
(in order and content) between two symbols, as illustrated in the following example:

.. code-block:: R

library(gamstransfer)
m <- Container$new()
i <- Set$new(m, "i", records = c("i1", "i2", "i3"))
ip <- Set$new(m, "ip", records = c("i1", "i3", "i2"))

.. code-block:: R

    > i$equals(ip, checkMetaData=FALSE)
    [1] FALSE

    > i$equals(ip, checkMetaData=FALSE, checkUELs=FALSE)
    [1] TRUE

Numerical comparisons are enabled for ``Parameters``, ``Variables`` and ``Equations``. 
Equality can be flexibly defined through the ``equals`` method arguments. Again, 
the strictest view of equality is taken as the default behavior of ``equals``.

.. code-block:: R

    library(gamstransfer)
    m <- Container$new()
    i <- Set$new(m, "i", records = c("i1", "i2", "i3"))
    a <- Parameter$new(m, "a", i, records = data.frame(c("i1", "i2", "i3"), c(1, 2, 3)))
    ap <- Parameter$new(m, "ap", i, records = data.frame(c("i1", "i2", "i3"), c(1 + 1e-9, 2, 3)))

.. code-block:: R

    > a$equals(ap, checkMetaData=FALSE)
    [1] FALSE

    > a$equals(ap, checkMetaData=FALSE, verbose=TRUE)
    Symbol records contain numeric differences in the value attribute that are outside the specified tolerances
                rtol=0, atol=0

    > a$equals(ap, checkMetaData=FALSE, atol=1e-8)
    [1] TRUE

In the case of variables and equations, it is possible for the user to confine the numerical comparison to 
certain attributes (``level``, ``marginal``, ``lower``, ``upper`` and ``scale``) by specifying the ``columns`` argument 
as the following example illustrates:

.. code-block:: R

    library(gamstransfer)
    m <- Container$new()
    a <- Variable$new(m, "a", "free", records = data.frame(level = 100))
    ap <- Variable$new(m, "ap", "free", records = data.frame(level = 101))

.. code-block:: R

    > a$records
    level
    1   100

    > ap$records
    level
    1   101

    > a$equals(ap, checkMetaData=FALSE)
    [1] FALSE

    > a$equals(ap, checkMetaData=FALSE, columns="level")
    [1] FALSE

    > a$equals(ap, checkMetaData=FALSE, columns="marginal")
    [1] TRUE

Similar to symbols, one can compare two ``Container`` objects 
using the ``equals`` method. When comparing 
``Containers``, the ``data`` fields are compared and if the same symbol keys 
exist in the Containers under comparison, symbol ``equals`` method is used 
to compare the symbols. Here is a brief example:

.. code-block:: R

    library(gamstransfer)
    m <- Container$new()
    i <- Set$new(m, "i")

    m1 <- Container$new()
    i1 <- Set$new(m1, "i")

.. code-block:: R

    > m$equals(m1)
    [1] TRUE

    > j = Set$new(m1, "j")

    > m$equals(m1)
    [1] FALSE

    > m$equals(m1, verbose=TRUE)
    Error in m$equals(m1, verbose = TRUE) :
    Containers contain different number of symbols.
    self: 1
    other :2

    > k = Set$new(m, "k")

    > m$equals(m1)
    [1] FALSE

    > m$equals(m1,verbose=TRUE)
    Error in m$equals(m1, verbose = TRUE) :
    Container ``data`` field keys do not match. Keys not present in ``other`` :k

