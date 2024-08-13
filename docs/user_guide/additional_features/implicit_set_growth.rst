Domain Forwarding
========================================

GAMS includes the ability to define sets directly from data using the 
implicit set notation (see: `implicit set definition <https://www.gams.com/latest/docs/UG_SetDefinition.html#INDEX_set_22_implicit_21_set_21_definition>`_ ).
This notation has an analogue in GAMS Transfer R called ``domainForwarding``.

.. note::
    It is possible to recursively update a subset tree in GAMS Transfer R.

Domain forwarding is available as an argument to all symbol object 
constructors; the user would simply need to pass ``domainForwarding=TRUE`` to 
forward domain across all dimensions or as a logical vector 
domainForwarding=c(TRUE, FALSE,....) to forward domain across selected dimensions.

In this example, we have raw data that is in the ``dist`` data frame, and we 
want to send the domain information into the ``i`` and ``j`` sets. We take 
care to pass the set objects as the domain for parameter ``c``.

.. code-block:: R

    library(gamstransfer)
    m = Container$new()
    i = Set$new(m, "i")
    j = Set$new(m, "j")

    dist = data.frame(
        from = c("seattle", "seattle", "seattle", 
        "san-diego", "san-diego", "san-diego"),
        to = c("new-york", "chicago", "topeka",
        "new-york", "chicago", "topeka"),
        thousand_miles = c(2.5, 1.7, 1.8, 2.5, 1.8, 1.4)
    )

    c = Parameter$new(m, "c", c(i, j), records = dist, domainForwarding = TRUE)

.. code-block:: R

    > i$records
            uni
    1   seattle
    2 san-diego

    > j$records
        uni
    1 new-york
    2  chicago
    3   topeka

    > c$records
        from       to value
    1   seattle new-york   2.5
    2   seattle  chicago   1.7
    3   seattle   topeka   1.8
    4 san-diego new-york   2.5
    5 san-diego  chicago   1.8
    6 san-diego   topeka   1.4

.. note:: 
    The element order in the sets ``i`` and ``j`` mirrors that in the raw data.

We can also selectively use domainForwarding for part of the domain by passing a 
logical vector to the ``domainForwarding`` argument as shown in the following example.
The domain records are forwarded only to the domain set ``i`` but not to the domain set ``j``.

.. code-block:: R

    library(gamstransfer)
    m = Container$new()
    i = Set$new(m, "i")
    j = Set$new(m, "j")

    dist = data.frame(
        from = c("seattle", "seattle", "seattle", 
        "san-diego", "san-diego", "san-diego"),
        to = c("new-york", "chicago", "topeka",
        "new-york", "chicago", "topeka"),
        thousand_miles = c(2.5, 1.7, 1.8, 2.5, 1.8, 1.4)
    )

    c = Parameter$new(m, "c", c(i, j), records = dist, domainForwarding = c(TRUE, FALSE))

.. code-block:: R

    > i$records
            uni
    1   seattle
    4 san-diego

    > j$records
    NULL

    > c$records
        from       to value
    1   seattle new-york   2.5
    2   seattle  chicago   1.7
    3   seattle   topeka   1.8
    4 san-diego new-york   2.5
    5 san-diego  chicago   1.8
    6 san-diego   topeka   1.4

In this example, we show that domain forwarding will also work recursively 
to update the entire set lineage. The domain forwarding occurs at the 
creation of every symbol object. The correct order of elements in set ``i`` 
is ``(z, a, b, c)`` because the records from ``j`` are forwarded first, and 
then the records from ``k`` are propagated through (back to ``i``).

.. code-block:: R

    library(gamstransfer)
    m = Container$new()
    i = Set$new(m, "i")
    j = Set$new(m, "j", i, records = c("z"), domainForwarding = TRUE)
    k = Set$new(m, "k", j, records = c("a", "b", "c"), domainForwarding = TRUE)

.. code-block:: R

    > i$records
        uni
    1     z
    2     a
    3     b
    4     c

    > j$records
        i
    1   z
    2   a
    3   b
    4   c

    > k$records
        j
    1   a
    2   b
    3   c
