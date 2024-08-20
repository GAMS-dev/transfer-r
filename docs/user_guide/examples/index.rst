Examples
=========================

GDX Read
---------------
Reading in all symbols can be accomplished with one line of code 
(we reference data from the `trnsport.gms <https://www.gams.com/latest/gamslib_ml/libhtml/gamslib_trnsport.html>`_ example).

.. code-block:: R

  library(gamstransfer)
  m <- Container$new("trnsport.gdx")

All symbols are internally stored in the Container :ref:`data field <Container Fields>` (dictionary). The users should 
never have to access or modify the ``data`` field. The symbols can be accessed via 
``m[<symbol_name>]`` and the records can be accessed via ``m[<symbol_name>]$records``. Symbol records 
are stored in the data frame format.

GDX Write
---------------
There are five symbol classes within GAMS Transfer R: 
:doc:`../../api_reference/Symbols/Set`, :doc:`../../api_reference/Symbols/Parameter`, :doc:`../../api_reference/Symbols/Variable`, 
:doc:`../../api_reference/Symbols/Equation`, and :doc:`../../api_reference/Symbols/Alias`. For purposes of this 
quick start, we show how to recreate the ``distance`` data structure 
from the `trnsport <https://www.gams.com/latest/gamslib_ml/libhtml/gamslib_trnsport.html>`_ model (the parameter 
`d`). This brief example shows how users can achieve "GAMS-like" 
functionality, but within an R environment. GAMS Transfer R leverages
the object oriented programming to simplify the syntax.

.. code-block:: R

  library(gamstransfer)
  m <- Container$new()

  # create the sets i, j
  i <- Set$new(m, "i", records = c("seattle", "san-diego"), description = "supply")
  j <- Set$new(m, "j", records = c("new-york", "chicago", "topeka"), description = "markets")

  # add "d" parameter -- domain linked to set objects i and j
  d <- Parameter$new(m, "d", c(i, j), description = "distance in thousands of miles")

  # create some data as a generic data frame
  dist <- data.frame(
    from = c(
      "seattle", "seattle", "seattle",
      "san-diego", "san-diego", "san-diego"
    ),
    to = c(
      "new-york", "chicago", "topeka",
      "new-york", "chicago", "topeka"
    ),
    thousand_miles = c(2.5, 1.7, 1.8, 2.5, 1.8, 1.4)
  )

  # setRecords will automatically convert the dist data frame into
  # a standard data frame format
  d$setRecords(dist)

  # write the GDX
  m$write("out.gdx")

This example shows a few fundamental features of GAMS Transfer R:

* A :doc:`../../api_reference/Container` is analogous to a GDX file
* `Symbols` will always be linked to a Container (notice that we always
  pass the Container reference `m` to the symbol constructor)
* `Records` can be added to a 
  symbol with the ``setRecords()`` method,
  through the ``records`` constructor argument (internally 
  calls ``setRecords()``), or through directly setting the ``records`` field. 
  GAMS Transfer R will convert many common 
  R data structures into a standard format.
* Domain linking is possible by passing domain set objects 
  to other symbols.
* Writing a GDX file can be accomplished in one line with the 
  :ref:`write() <Container Methods>` method.


Full Example
---------------
It is possible to use GAMS Transfer R 
to recreate the `trnsport.gms <https://www.gams.com/latest/gamslib_ml/libhtml/gamslib_trnsport.html>`_ results in 
GDX form. As part of this example, we also introduce the :ref:`write() <Container Methods>`
method (and generate ``new.gdx``).  We will discuss it in more detail 
in the :doc:`../additional_features/gdx_exchange` section.

.. code-block:: R

  library(gamstransfer)
  # create an empty Container object
  m <- Container$new()

  # add sets
  i <- Set$new(m, "i", records = c("seattle", "san-diego"), description = "supply")
  j <- Set$new(m, "j", records = c("new-york", "chicago", "topeka"), description = "markets")

  # add parameters
  a <- Parameter$new(m, "a", c("*"), description = "capacity of plant i in cases")
  b <- Parameter$new(m, "b", j, description = "demand at market j in cases")
  d <- Parameter$new(m, "d", c(i, j), description = "distance in thousands of miles")
  f <- Parameter$new(
    m, "f",
    records = 90, description = "freight in dollars per case per thousand miles"
  )
  c <- Parameter$new(
    m, "c", c(i, j),
    description = "transport cost in thousands of dollars per case"
  )

  # set parameter records
  cap <- data.frame(plant = c("seattle", "san-diego"), n_cases = c(350, 600))
  a$setRecords(cap)

  dem <- data.frame(market = c("new-york", "chicago", "topeka"), n_cases = c(325, 300, 275))
  b$setRecords(dem)

  dist <- data.frame(
    from = c(
      "seattle", "seattle", "seattle",
      "san-diego", "san-diego", "san-diego"
    ),
    to = c(
      "new-york", "chicago", "topeka",
      "new-york", "chicago", "topeka"
    ),
    thousand_miles = c(2.5, 1.7, 1.8, 2.5, 1.8, 1.4)
  )
  d$setRecords(dist)

  # c(i,j) = f * d(i,j) / 1000;
  cost <- d$records
  cost$value <- f$records$value * cost$value / 1000
  c$setRecords(cost)

  # add variables
  q <- data.frame(
    from = c(
      "seattle", "seattle", "seattle",
      "san-diego", "san-diego", "san-diego"
    ),
    to = c(
      "new-york", "chicago", "topeka",
      "new-york", "chicago", "topeka"
    ),
    level = c(50, 300, 0, 275, 0, 275),
    marginal = c(0, 0, 0.036, 0, 0.009, 0)
  )
  x <- Variable$new(m, "x", "positive", c(i, j), records = q, description = "shipment quantities in cases")

  z <- Variable$new(
    m,
    "z",
    records = data.frame(level = 153.675),
    description = "total transportation costs in thousands of dollars"
  )

  # add equations
  cost <- Equation$new(m, "cost", "eq", description = "define objective function")
  supply <- Equation$new(m, "supply", "leq", i, description = "observe supply limit at plant i")
  demand <- Equation$new(m, "demand", "geq", j, description = "satisfy demand at market j")

  # set equation records
  cost$setRecords(data.frame(level = 0, marginal = 1, lower = 0, upper = 0))

  supplies <- data.frame(
    from = c("seattle", "san-diego"),
    level = c(350, 550),
    marginal = c(SpecialValues$EPS, 0),
    lower = c(SpecialValues$NEGINF, SpecialValues$NEGINF),
    upper = c(350, 600)
  )

  supply$setRecords(supplies)

  demands <- data.frame(
    from = c("new-york", "chicago", "topeka"),
    level = c(325, 300, 275),
    marginal = c(0.225, 0.153, 0.126),
    lower = c(325, 300, 275)
  )

  demand$setRecords(demands)
  m$write("new.gdx")


It can be observed from the above example that a typical work flow for 
writing using GAMS Transfer R is creating a container, filling it with 
symbols (:doc:`../../api_reference/Symbols/Set`, :doc:`../../api_reference/Symbols/Parameter`, :doc:`../../api_reference/Symbols/Variable`, 
:doc:`../../api_reference/Symbols/Equation`, and :doc:`../../api_reference/Symbols/Alias`), and write it 
to a GDX file. To read a GDX file, a :doc:`../../api_reference/Container`
can simply be initialized with the GDX file name as an argument.

These examples introduced the reader to the GAMS Transfer R syntax. In 
the remaining sections, we will present details about the core functionality 
and dig further into the syntax.

