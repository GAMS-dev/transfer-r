# gamstransfer 3.0.6

* Do not raise an error anymore when a Singleton Set is used as a domain for another symbol

# gamstransfer 3.0.6

* Removed pre-check for .gdx extension. Now rely on GDX API to validate files, allowing case-insensitive and extension-independent handling
* When reading, if a file without extension is not found, extension .gdx is tried before throwing an error
* New GDX API version 7.11.17

# gamstransfer 3.0.5

* New GDX API version 7.11.16

# gamstransfer 3.0.4

* New GDX API version 7.11.7
* Bug fix: fixed errors when reading symbols with duplicate column names
* Package documentation is now moved to https://transfer-r.readthedocs.io/en/latest/

# gamstransfer 3.0.3

* New GDX API version 7.11.4
* bug fix: Symbol `shape` fixed from a method to a field. Users can access `<Symbol_name>$shape`
* Performance improvements when reading a GDX file containing many symbols into a container
* Improved error check for GDX API calls when reading a GDX file
* Bug fix in corner case equation types from GDX files
* Fixed reading GDX files with records that use UELs without a string representation. These will result in <NA> in the domain column

# gamstransfer 3.0.2

* Fix issues associated with memory checks and uninitialized variables

# gamstransfer 3.0.1

* Initial CRAN submission.
* C++ GDX API version 7.11.1
