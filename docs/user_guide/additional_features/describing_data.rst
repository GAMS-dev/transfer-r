Describing Data
===================

The methods :ref:`describeSets <describeSets>`, :ref:`describeParameters <describeParameters>`,
:ref:`describeVariables <describeVariables>`, and :ref:`describeEquations <describeEquations>`
allow the user to get a summary view of key data 
statistics. The returned data frame aggregates the output for a number 
of other methods (depending on symbol type). A description of each :ref:`Container method <Container Methods>`
is provided in the following subsections.

.. _describeSets:

describeSets
---------------

.. list-table:: Definition
   :widths: 10 10 60 10 10
   :header-rows: 1

   * - Argument
     - Type
     - Description
     - Required
     - Default
   * - ``symbols``
     - ``list``, ``string``
     - A list of sets in the :doc:`Container <../../api_reference/Container>` to include in the output. describeSets will include aliases if they are explicitly passed by the user.
     - No
     - ``NULL`` (if ``NULL`` specified, will assume all sets)

Returns: ``data frame``

The following table includes a short description of the column headings in the return.

.. list-table:: Output
   :widths: 30 70
   :header-rows: 1

   * - Field / Statistic
     - Description
   * - ``name``
     - name of the symbol
   * - ``isSingleton``
     - ``logical`` if the set is a singleton set
   * - ``domain``
     - domain labels for the symbol
   * - ``domainType``
     - ``none``, ``relaxed`` or ``regular`` depending on the symbol state
   * - ``dimension``
     - dimension
   * - ``numberRecords``
     - number of records in the symbol
   * - ``sparsity``
     - ``1 - numberRecs/dense`` where ``dense`` is the size of full cartesian product of the domain set

Example \#1
~~~~~~~~~~~~~~~~

.. code-block:: R

  library(gamstransfer)
  m <- Container$new("trnsport.gdx")

.. code-block:: R

    > m$describeSets()
    name isSingleton domain domainType dimension numberRecords sparsity
    1    i       FALSE      *       none         1             2       NA
    2    j       FALSE      *       none         1             3       NA

Example \#2 -- with aliases
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. code-block:: R

  library(gamstransfer)
  m <- Container$new()
  i <- Set$new(m, "i", records = paste0("i", 1:10))
  j <- Set$new(m, "j", records = paste0("j", 1:10))

  ip <- Alias$new(m, "ip", i)
  jp <- Alias$new(m, "jp", j)


.. code-block:: R

    > m$describeSets()
    name isSingleton domain domainType dimension numberRecords sparsity
    1    i       FALSE      *       none         1            10       NA
    2    j       FALSE      *       none         1            10       NA

    > m$describeSets(append(m$listSets(), m$listAliases()))
    name isSingleton domain domainType dimension numberRecords sparsity
    1    i       FALSE      *       none         1            10       NA
    3   ip       FALSE      *       none         1            10       NA
    2    j       FALSE      *       none         1            10       NA
    4   jp       FALSE      *       none         1            10       NA

.. _describeParameters:

describeParameters
--------------------

.. list-table:: Definition
   :widths: 10 10 60 10 10
   :header-rows: 1

   * - Argument
     - Type
     - Description
     - Required
     - Default
   * - ``symbols``
     - ``list``, ``string``
     - A list of parameters in the :doc:`Container <../../api_reference/Container>` to include in the output
     - No
     - ``NULL`` (if ``NULL`` specified, will assume all parameters)

Returns: ``data frame``

The following table includes a short description of the column headings in the return.

.. list-table:: Output
   :widths: 30 70
   :header-rows: 1

   * - Field / Statistic
     - Description
   * - ``name``
     - name of the symbol
   * - ``domain``
     - domain labels for the symbol
   * - ``domainType``
     - ``none``, ``relaxed`` or ``regular`` depending on the symbol state
   * - ``dimension``
     - dimension
   * - ``numberRecords``
     - number of records in the symbol
   * - ``min``
     - min value in data
   * - ``mean``
     - mean value in data
   * - ``max``
     - max value in data
   * - ``whereMin``
     - row number min value (if multiple, returns only first occurrence)
   * - ``sparsity``
     - ``1 - numberRecs/dense`` where ``dense`` is the size of full cartesian product of the domain set

.. code-block:: R

    library(gamstransfer)
    m <- Container$new("trnsport.gdx")

.. code-block:: R

    > m$describeParameters()
    >  name domain domainType dimension numberRecords     min     mean     max
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

.. _describeVariables:

describeVariables
-------------------------

.. list-table:: Definition
   :widths: 10 10 60 10 10
   :header-rows: 1

   * - Argument
     - Type
     - Description
     - Required
     - Default
   * - ``symbols``
     - ``list``, ``string``
     - A list of variables in the :doc:`Container <../../api_reference/Container>` to include in the output
     - No
     - ``NULL`` (if ``NULL`` specified, will assume all variables)

Returns: data frame

The following table includes a short description of the column headings in the return.

.. list-table:: Output
   :widths: 30 70
   :header-rows: 1

   * - Field / Statistic
     - Description
   * - ``name``
     - name of the symbol
   * - ``type``
     - type of variable (i.e., ``binary``, ``integer``, ``positive``, ``negative``, ``free``, ``sos1``, ``sos2``, ``semicont``, ``semiint``)
   * - ``domain``
     - domain labels for the symbol
   * - ``domainType``
     - ``none``, ``relaxed`` or ``regular`` depending on the symbol state
   * - ``dimension``
     - dimension
   * - ``numberRecords``
     - number of records in the symbol
   * - ``sparsity``
     - ``1 - numberRecs/dense`` where ``dense`` is the size of full cartesian product of the domain set
   * - ``minLevel``
     - min value in the ``level`` column
   * - ``meanLevel``
     - mean value in the ``level`` column
   * - ``maxLevel``
     - max value in the ``level`` column
   * - ``whereMaxAbsLevel``
     - max absolute value in the ``level`` column

.. code-block:: R

    library(gamstransfer)
    m <- Container$new("trnsport.gdx")

.. code-block:: R

    > m$describeVariables()
    name     type domain domainType dimension numberRecords sparsity minLevel
    1    x positive    i j    regular         2             6        0    0.000
    2    z     free     NA       none         0             1       NA  153.675
    meanLevel maxLevel whereMaxAbsLevel
    1   150.000  300.000                2
    2   153.675  153.675                1

.. _describeEquations:

describeEquations
-------------------------

.. list-table:: Definition
   :widths: 10 10 60 10 10
   :header-rows: 1

   * - Argument
     - Type
     - Description
     - Required
     - Default
   * - ``symbols``
     - ``list``, ``string``
     - A list of equations in the :doc:`Container <../../api_reference/Container>` to include in the output
     - No
     - ``NULL`` (if ``NULL`` specified, will assume all equations)

Returns: data frame

The following table includes a short description of the column headings in the return.

.. list-table:: Output
   :widths: 30 70
   :header-rows: 1

   * - Field / Statistic
     - Description
   * - ``name``
     - name of the symbol
   * - ``type``
     - type of variable (i.e., ``binary``, ``integer``, ``positive``, ``negative``, ``free``, ``sos1``, ``sos2``, ``semicont``, ``semiint``)
   * - ``domain``
     - domain labels for the symbol
   * - ``domainType``
     - ``none``, ``relaxed`` or ``regular`` depending on the symbol state
   * - ``dimension``
     - dimension
   * - ``numberRecords``
     - number of records in the symbol
   * - ``sparsity``
     - ``1 - numberRecs/dense`` where ``dense`` is the size of full cartesian product of the domain set
   * - ``minLevel``
     - min value in the ``level`` column
   * - ``meanLevel``
     - mean value in the ``level`` column
   * - ``maxLevel``
     - max value in the ``level`` column
   * - ``whereMaxAbsLevel``
     - max absolute value in the ``level`` column


.. code-block:: R

    library(gamstransfer)
    m <- Container$new("trnsport.gdx")

.. code-block:: R

    > m$describeEquations()
        name type domain domainType dimension numberRecords sparsity minLevel
    1   cost   eq     NA       none         0             1       NA        0
    3 demand  geq      j    regular         1             3        0      275
    2 supply  leq      i    regular         1             2        0      350
    meanLevel maxLevel whereMaxAbsLevel
    1         0        0                1
    3       300      325                1
    2       450      550                2
