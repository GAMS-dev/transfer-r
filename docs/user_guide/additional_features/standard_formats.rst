Standard Data Formats
===========================

This section is meant to introduce the standard format that GAMS Transfer R 
expects for symbol records. It has already been mentioned that we store 
data as a data frame, but there is an assumed structure to the column 
headings and column types that will be important to understand. 
GAMS Transfer R includes convenience functions in order to ease the burden 
of converting data from a user-centric format to one that is understood 
by GAMS Transfer R. However, advanced users will want to convert their data 
first and add it directly to the :doc:`Container <../../api_reference/Container>`.

Set Records Standard Format
-------------------------------

All set records (including singleton sets) are stored as a data frame 
with ``n`` or ``n+1`` number of columns, where ``n`` is the dimensionality of the 
symbol. The first ``n`` columns include the domain elements while 
the last column, if present, includes the set element text. 
Records are organized such that there is one record per row.

The names of the domain columns (domain labels) are unique. If the domain 
labels are non-unique, they are converted to follow a pattern of 
``<user_domain_label>_<index_position>`` by ``domainLabels`` setter. If the records 
are passed in a non-data frame format (vector, matrix etc.), domain labels 
follow a pattern of ``<domain_name>_<index_position>``.
A symbol dimension that is referenced 
to the :doc:`universe <universe_set>` is labeled 
``uni``. The explanatory 
text column is called ``element_text`` and must take the last position 
in the data frame.

All domain columns must be ``factors`` and the ``element_text`` column 
must be a ``string`` type.

Some examples:

.. code-block:: R

    library(gamstransfer)
    m = Container$new()
    i = Set$new(m, "i", records=c("seattle", "san-diego"))
    j = Set$new(m, "j", c(i, "*"), records=data.frame(i=c("seattle", "san-diego"), uni=c("new-york", "st-louis")))
    k = Set$new(m, "k", i, isSingleton=TRUE, records=c("seattle"))

.. code-block:: R

    > i$records
            uni
    1   seattle
    2 san-diego

    > j$records
            i      uni
    1   seattle new-york
    2 san-diego st-louis

    > k$records
            i
    1 seattle

Parameter Records Standard Format
------------------------------------

All parameter records (including scalars) are stored as a data frame 
with ``n`` or ``n + 1`` number of columns, where ``n`` is the dimensionality of the 
symbol. The first ``n`` columns include the domain elements while 
the last column, if present, includes the numerical value of the records. 
Records are organized such that there is one record per row. Scalar parameters 
have zero dimension, therefore they only have one column and one row. In cases 
where the ``value`` columns is not present for a parameter, it is assumed that the 
value is the default parameter value (i.e., 0). Scalar parameters with an empty 
data frame as records is assumed to contain the default value of 0.

The names of the domain columns (domain labels) are unique. If the domain 
labels are non-unique, they are converted to follow a pattern of 
``<user_domain_label>_<index_position>`` by ``domainLabels`` setter. If the records 
are passed in a non-data frame format (vector, matrix etc.), domain labels 
follow a pattern of ``<domain_name>_<index_position>``.
A symbol dimension that is referenced 
to the :doc:`universe <universe_set>` is labeled 
``uni_<index_position>``. The value column 
is called ``value`` and must take the last position in the data frame.

All domain columns must be ``factors`` and the ``value`` column must 
be a ``numeric`` type. GAMS Transfer R requires that all the factor 
levels are of type ``string``.

Some examples:

.. code-block:: R

    library(gamstransfer)
    m = Container$new()
    i = Set$new(m, "i", records=c("seattle", "san-diego"))
    a = Parameter$new(
    m, "a", "*", records=data.frame(c("seattle", "san-diego"), c(50, 100))
    )

    b = Parameter$new(
        m,
        "b",
        c(i, "*"),
        records= data.frame(i=c("seattle", "san-diego"), 
        uni=c("new-york", "st-louis"), c(32.2, 123))
    )

    c = Parameter$new(m, "c", records=90)

.. code-block:: R

    > a$records
            uni value
    1   seattle    50
    2 san-diego   100

    > b$records
            i      uni value
    1   seattle new-york  32.2
    2 san-diego st-louis 123.0

    > c$records
    value
    1    90

Variable/Equation Records Standard Format
----------------------------------------------

Variables and equations share the same standard data format. 
All records (including scalar variables/equations) are stored 
as a data frame with ``n`` to ``n`` + 5 number of columns, where ``n`` is the 
dimensionality of the symbol. The first ``n`` columns 
include the domain elements while the remaining columns include 
the numerical values for different attributes of the records. Records 
are organized such that there is one record per row. Scalar 
variables/equations have zero dimension, therefore they have one row. In 
cases where the records data frame has missing attribute columns, it is 
assumed that the column contains the default value. Similarly, if the records 
data frame for a scalar is empty, it is assumed that all the attribute values 
are at their default. Default values for variables and equations depend on 
their type.

The names of the domain columns (domain labels) are unique. If the domain 
labels are non-unique, they are converted to follow a pattern of 
``<user_domain_label>_<index_position>`` by ``domainLabels`` setter. If the records 
are passed in a non-data frame format (vector, matrix etc.), domain labels 
follow a pattern of ``<domain_name>_<index_position>``. 
A symbol dimension that is referenced 
to the :doc:`universe <universe_set>` is labeled 
``uni_<index_position>``. The attribute columns 
are called ``level``, ``marginal``, ``lower``, ``upper``, and ``scale``. These 
attribute columns must appear in this order. Attributes that are not 
supplied by the user will be assigned the default GAMS values for that 
variable/equation type. It is possible to not pass any attributes, 
GAMS Transfer R would then simply assign default values to all attributes.

All domain columns must be ``factors`` and the attribute columns must 
be a ``numeric`` type. GAMS Transfer R requires that all the factor 
levels are of type ``string``.

Some examples:

.. code-block:: R

    library(gamstransfer)
    m = Container$new()
    i = Set$new(m, "i", records=c("seattle", "san-diego"))
    a = Variable$new(
        m,
        "a",
        "free",
        domain= i,
        records=data.frame(
        city = c("seattle", "san-diego"),
        level = c(50, 100)
        )
    )

.. code-block:: R

    > a$records
        city level
    1   seattle    50
    2 san-diego   100
