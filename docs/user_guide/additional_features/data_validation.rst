Validating Data
========================================

GAMS Transfer R requires that the records for all symbols exist in a
(:doc:`standard format <standard_formats>`) 
in order for them to be understood by the ``Container``. It is possible that the data could end up 
in a state that is inconsistent with the standard format (especially 
if setting symbol attributes directly). GAMS Transfer R includes the 
``$isValid()`` method in order to determine if a symbol is valid. 
This method returns a ``logical``. This method does not guarantee that a 
symbol will be successfully written to GDX. Other data errors (duplicate records, 
long UEL names, or domain violations) could exist that are not tested in ``$isValid()``.
For example, we create two valid sets and then check them with ``$isValid()`` to be sure.

It is possible to run ``$isValid()`` on both the ``Container`` as well 
as the symbol object. The Container method ``$isValid()`` will also 
return ``FALSE`` if there are any invalid symbols in the ``Container`` object. 
In addition, the ``Container$isValid()`` method also detects broken Container 
references in symbols and inconsistent symbol naming between the ``Container$data`` 
field and the symbol objects.

Example (valid data)
-----------------------

.. code-block:: R

    library(gamstransfer)
    m = Container$new()
    i = Set$new(m, "i", records = c("seattle", "san-diego", "washington_dc"))
    j = Set$new(m, "j", i, records = c("san-diego", "washington_dc"))

.. code-block:: R

    > i$isValid()
    [1] TRUE

    > j$isValid()
    [1] TRUE

    > m$isValid()
    [1] TRUE

.. _invalid_example:

Now we create some data that is invalid due to incorrect column names 
in the records for set ``j``.

.. code-block:: R

    library(gamstransfer)
    m = Container$new()
    i = Set$new(m, "i", records = c("seattle", "san-diego", "washington_dc"))
    j = Set$new(m, "j", i)

    j$records = data.frame(cities=c("grayslake", "washington_dc"))

.. code-block:: R

    > i$isValid()
    [1] TRUE

    > j$isValid()
    [1] FALSE

    > m$isValid()
    [1] FALSE

In this example, we use ``records`` field of the symbol ``j`` to set the records. 
As mentioned in setting the records, when setting records directly, users must 
adhere to :doc:`standard formats <standard_formats>`. In this example, symbol ``j`` 
does not have the correct number of columns. Moreover, the column headings do 
not follow the convention (should be "i_1"). The user can get more detailed 
error reporting if the ``verbose`` argument is set to ``TRUE``. For example:

.. code-block:: R

    > j$isValid(verbose=TRUE)
    Symbol 'records' does not have the correct number of columns (<symbol dimension> + 1)
    [1] FALSE

The ``$isValid()`` method checks:
- If the symbol belongs to a :doc:`Container <../../api_reference/Container>`
- If all domain set symbols exist in the Container
- If all domain set symbols objects are valid
- If all domain sets are one dimensional and not singleton sets
- If records are a data frame (or ``NULL``)
- If the records ``domainLabels`` are unique
- The shape of the records is congruent with the dimensionality of the symbol
- That all data columns are type ``numeric``
- To make sure that all domain categories are type ``string``

.. note:: 
    Calling ``$isValid()`` too often may have a significant impact on the performance.
