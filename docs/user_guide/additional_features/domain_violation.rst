Domain Violations
======================

Domain violations occur when a symbol uses other 
:doc:`Sets <../../api_reference/Symbols/Set>` as domain(s) – 
and is thus of domain type regular, see Symbol Domain – and uses 
a domain entry in its records that is not present in the corresponding 
referenced domain set. Such a domain violation will lead to a GDX error 
when writing the data.

For example, the symbol ``j`` in the following example
contains domain violations because ``j`` is defined over domain set ``i`` 
and the records entry "grayslake" is not present in ``i``.

.. code-block:: R

    library(gamstransfer)
    m <- Container$new()
    i <- Set$new(m, "i", records = c("seattle", "san-diego", "washington_dc"))
    j <- Set$new(m, "j", i)
    records <- data.frame(cities = c("grayslake", "washington_dc"))
    j$setRecords(records)

Trying to write this container to a GDX file will fail. To ask for domain 
violations, call the method ``Symbol$getDomainViolations()``. It returns a list of 
``DomainViolation`` objects with respect to each dimension of the symbol. This 
list can then be used to resolve the domain violations. 

.. code-block:: R

    > dv <- j$getDomainViolations()
    > dv
    [[1]]
    GAMS Transfer: DomainViolation with properties:
    Symbol: j
    dimension:
    domain: i
    violations: grayslake

The GAMS Transfer R 
feature of :doc:`domain forwarding <implicit_set_growth>` offers 
an automatic expansion of the domain set with the violated entries in order 
to eliminate domain violations.

.. note::
    Checking for domain violations is not a part of Symbol$isValid() for 
    performance reasons.

.. note:: 
    The method for automatically resolving the domain violations can be 
    convenient, but it effectively disables domain checking, which is a valuable 
    tool for error detection. We encourage to use 
    :doc:`domain forwarding <implicit_set_growth>` as rarely as 
    possible. The same holds for using ``relaxed`` domain informaiton when 
    ``regular`` domain information would be possible.

