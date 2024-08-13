Alias
============

Adding Alias Records
------------------------

The linked structure of Aliases offers some unique opportunities to access 
some of the setter functionality of the parent set.  Specifically, GAMS 
Transfer allows the user to change the ``domain``, ``description``, ``dimension``, 
and ``records`` of the underlying parent set as a shorthand notation. 
We can see this behavior if we look at a modified Example \#1 from
:doc:`adding set records <Set>`.

Example - Creating set records through an alias link
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. code-block:: R

    library(gamstransfer)
    m = Container$new()
    i = Set$new(m, "i")
    ip = Alias$new(m, "ip", i)

    ip$description = "adding new descriptive set text"
    ip$domain = c("*", "*")

    ij = list(paste0("i", 1:3), paste0("j", 1:3))
    ip_recs = rev(expand.grid(rev(ij)))
    colnames(ip_recs) = c("i", "j")
    ip$setRecords(ip_recs)

.. code-block:: R

    > i$domain
    [1] "*" "*"

    > i$records
    i  j
    1 i1 j1
    2 i1 j2
    3 i1 j3
    4 i2 j1
    5 i2 j2
    6 i2 j3
    7 i3 j1
    8 i3 j2
    9 i3 j3

.. note::  
    An alias ``$isValid()=TRUE`` when the underlying parent set 
    is also valid. If the parent set is removed from the Container 
    the alias will no longer be valid.

.. note:: 
    One can also use the :doc:`generateRecords() <../additional_features/generate_records>`
    method to automatically populate randomly generated symbol records in the standard format.
