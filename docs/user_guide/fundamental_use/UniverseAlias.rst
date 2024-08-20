UniverseAlias
====================

Example - Creating an alias for the Universe
------------------------------------------------------

In GAMS, it is possible to create aliases to the universe (i.e., the entire list of UELs) with the syntax:

.. code-block:: GAMS

    set i / i1, i2 /;
    alias(h,*);
    set j / j1, j2 /;


GAMS Transfer R allows creating aliases to the :doc:`universe set <../additional_features/universe_set>`.

In this small example, ``h`` would be associated with all four UELs (``i1``, ``i2``, ``j1`` and ``j2``) 
even though set ``j`` was defined after the alias declaration. GAMS Transfer mimics this behavior 
with the ``UniverseAlias`` class. Internally, the ``records`` attribute will always call the 
``<Container>$getUELs()``. The ``UniverseAlias`` class is fundamentally different from the 
``Alias`` class because it does not point to a parent set object. It is not possible to perform operations 
(like ``setRecords`` or ``findDomainViolations``) on the parent set through a ``UniverseAlias`` 
(because there is no parent set object). This means that a ``UniverseAlias`` can be created by only 
defining the symbol name. We can see this behavior in the following example:

.. code-block:: R

    library(gamstransfer)
    m <- Container$new()
    i <- Set$new(m, "i", records = c("i1", "i2"))
    h <- UniverseAlias$new(m, "h")
    j <- Set$new(m, "j", records = c("j1", "j2"))

.. code-block:: R

    > m$listSymbols()
    [1] "i" "h" "j"

    > m["h"]
    GAMS Transfer: R6 object of class UniverseAlias.
        Use h$summary for details

    > h$records
    uni
    1 i1
    2 i2
    3 j1
    4 j2

.. note:: 
    Unlike other sets, the universe does not hold on to set ``element_text``, 
    thus the returned ``data frame`` for the ``UniverseAlias`` will only have one column.
