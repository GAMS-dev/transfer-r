.. documentation master file, created by sphinx-quickstart 
   You can adapt this file completely to your liking, but it should at least
   contain the root `toctree` directive.

gamstransfer
================================

.. raw:: html

    <style> .red {color:red} </style>

.. role:: red

GAMS Transfer is a package to maintain GAMS data outside a GAMS
script in a programming language like Python, Matlab, or R. It
allows the user to add GAMS symbols (Sets, Aliases, Parameters,
Variables and Equations), to manipulate GAMS symbols, as well as
read/write symbols to different data endpoints. GAMS Transfer's
main focus is the highly efficient transfer of data between GAMS
and the target programming language, while keeping those
operations as simple as possible for the user. In order to achieve
this, symbol records - the actual and potentially large-scale data
sets - are stored in native data structures of the corresponding
programming languages.  The benefits of this approach are
threefold: (1) The user is usually very familiar with these data
structures, (2) these data structures come with a large tool box
for various data operations, and (3) optimized methods for reading
from and writing to GAMS can transfer the data as a bulk -
resulting in the high performance of this package.


Design Philosophy
----------------------------

Container
~~~~~~~~~~~~~~~~~~

Storing, manipulating, and transforming sparse data requires that it lives
within an environment. This data can then be linked together to enable 
various operations. In GAMS Transfer R, we refer to this "environment" as 
the ``Container``, it is the main repository for storing and linking our data.
Linking data enables data operations such as
:doc:`implicit set growth <user_guide/additional_features/implicit_set_growth>`, 
:doc:`domain checking <user_guide/additional_features/domain_violation>`, and
:doc:`data format <user_guide/additional_features/transform>` transformations
(to dense/sparse matrix formats). A Container also enables different 
read/write operations.

Symbols
~~~~~~~~~~~~~~~~~~

In GAMS Transfer R, a symbol is either a GAMS :doc:`Set <./api_reference/Symbols/Set>`,
:doc:`Parameter <./api_reference/Symbols/Parameter>`,
:doc:`Variable <./api_reference/Symbols/Variable>`, 
:doc:`Equation <./api_reference/Symbols/Equation>`, 
:doc:`Alias <./api_reference/Symbols/Alias>`.
In GAMS Transfer R, a symbol cannot live 
on its own, but is always part of a :doc:`Container <./api_reference/Container>`.

.. _create_symbol:

Create a symbol
#########################

There are two different ways to create a GAMS set and add it to a :doc:`Container <api_reference/Container>`.

1. Use symbol constructor for :ref:`Set <Set Constructor>`, :ref:`Parameter <Parameter Constructor>`,
:ref:`Variable <Variable Constructor>`, :ref:`Equation <Equation Constructor>`, :ref:`Alias <Alias Constructor>`

.. code-block:: R

    library(gamstransfer)
    m = Container$new()
    p = Parameter$new(m, "p")

2. Use the :ref:`Container methods <Container Methods>` "addSet", "addParameter", 
"addVariable", "addEquation", "addAlias" (which internally calls the symbol constructor)

.. code-block:: R

    library(gamstransfer)
    m = Container$new()
    p = m$addParameter("p")

.. _add_symbol_records:

Add Symbol Records
#########################

Three possibilities exist to assign symbol records:
1. Setting the argument ``records`` in the symbol constructor (internally calls ``setRecords``)
2. Using the symbol method ``setRecords``
3. Setting the field ``records`` directly

If the data is in a convenient format, a user may want to pass 
the records directly within the set constructor. This is an optional
keyword argument and internally the symbol constructor will simply call 
the ``setRecords`` method. The symbol method ``setRecords`` is a convenience 
method that transforms the given data into an approved data frame
format (see :doc:`user_guide/additional_features/standard_formats`). Many native 
R data types can be easily transformed into data frames, so the 
``setRecords`` method will accept a number of different 
types for input. The ``setRecords`` method is called internally on 
any data structure that is passed through the ``records`` argument. 

The examples of setting records using the above-mentioned 
methods can be found in the respective documentation for each symbol (
:doc:`adding set records <user_guide/fundamental_use/Set>`, 
:doc:`adding parameter records <user_guide/fundamental_use/Parameter>`,
:doc:`adding variable records <user_guide/fundamental_use/Variable>`,
:doc:`adding vquation records <user_guide/fundamental_use/Equation>`, and
:doc:`adding alias records <user_guide/fundamental_use/Alias>`
).

.. toctree::
   :maxdepth: 1
   :hidden:

   user_guide/index
   api_reference
   release_notes



