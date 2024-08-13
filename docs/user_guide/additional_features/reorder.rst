Reordering Symbols
======================

In order to write the contents of the :doc:`Container <../../api_reference/Container>`, 
it is required that the symbols are sorted 
such that, for example, a :doc:`Set <../../api_reference/Symbols/Set>` used as a domain 
of another symbol appears before that symbol. The Container will try to 
establish a valid ordering when writing the data. This type of situation
could be encountered if the user is adding and removing many 
symbols (and perhaps rewriting symbols with the same name). 
Users should attempt to only add symbols to a ``Container`` 
once, and care must be taken when naming. 
The method ``reorderSymbols`` attempts to fix symbol ordering 
problems. The following example shows how this can occur:

Example Symbol reordering
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. code-block:: R

    library(gamstransfer)
    m = Container$new()
    i = Set$new(m, "i", records = paste0("i", 1:5))
    j = Set$new(m, "j", i, records = paste0("i", 1:3))

.. code-block:: R

    > m$listSymbols()
    [1] "i" "j"

    # now we remove the set i and recreate the data
    m$removeSymbols("i")
    i = Set$new(m, "i", records = paste0("i", 1:5))

    > m$isValid()
    [1] TRUE

Since the link to ``i`` is broken, Set ``j`` is now invalid. The 
user has to manually set the domain again.

.. code-block:: R

    # fix the domain reference in the set j
    j$domain = i

.. code-block:: R

    > m$listSymbols()
    [1] "j" "i"

Now even though ``j`` is valid, the symbols are out of order. The 
order can be fixed as follows.

.. code-block:: R

    # calling reorderSymbols() will order the list properly, 
    # but the domain reference in j is now broken

    m$reorderSymbols()

.. code-block:: R

    > m$listSymbols()
    [1] "i" "j"

    > m$isValid()
    [1] TRUE


