Install
=============================

GAMS Transfer R is available on
`CRAN <https://cran.r-project.org/web/packages/gamstransfer/index.html>`_
and can be installed as follows from the R console.

.. code-block:: R

  install.packages("gamstransfer")

GAMS Transfer R depends on packages ``R6``, ``R.utils``, and ``Rcpp``, ``collections``.

When building from source, GAMS Transfer R also requires the library ``zlib``. The user
can point to ``zlib`` by adding the directory containing ``zlib`` to the environment variable ``PATH`` or
by using ``configure.vars`` argument of ``install.packages()``.
