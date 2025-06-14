.. toctree::
   :maxdepth: 1
   :caption: Release Notes:

v3.0.6
=======================
- Removed pre-check for .gdx extension. Now rely on GDX API to validate files, allowing case-insensitive and extension-independent handling
- When reading, if a file without extension is not found, extension .gdx is tried before throwing an error
- New GDX API version 7.11.17

v3.0.5
=======================
- New GDX API version 7.11.16

v3.0.4
=======================
- New GDX API version 7.11.7
- Bug fix: fixed errors when reading symbols with duplicate column names
- Package documentation is now moved to https://transfer-r.readthedocs.io/en/latest/

v3.0.3
==============================

- New GDX API version 7.11.4
- bug fix: Symbol `shape` fixed from a method to a field. Users can access `<Symbol_name>$shape`
- Performance improvements when reading a GDX file containing many symbols into a container
- Improved error check for GDX API calls when reading a GDX file
- Bug fix in corner case equation types from GDX files
- Fixed reading GDX files with records that use UELs without a string representation. These will result in <NA> in the domain column

v3.0.2
==============================

- Fix issues associated with memory checks and uninitialized variables

v3.0.1
==============================

- C++ GDX API version 7.11.2

v3.0.0
==============================

- C++ GDX API version 7.10.1
- New three digit versioning scheme

v2.9.0
==============================

- GDX API changed to the new GDX C++ API - significant performance improvements and no dependency on the GAMS system directory
- `systemDirectory` argument in the `Container` constructor is deprecated and will be removed in the future
- Bug fix: `Container` property `summary` changed from a method to active binding
- Fixed overriding PKG_CXXFLAGS in Makevars
- Performance improvements in read
- Bug fix: `Symbol` method `equals` incorrectly returned `FALSE` when the symbol records contain `NA`

v2.8.0
==============================

- bug fix in reading symbols with unused UELs
- bug fix in `getUELs` method with `unusedUELs` argument when the symbol has `NULL` records
- squashed a build warning

v2.6.0
==============================

- bug fix in registering unused UELs when not all symbols from the container are written to a GDX file
- Breaking: Following `Container` methods do not support `list` input for `symbols` argument: `describeSets`, `describeAliases`, `describeParameters`, `describeVariables`, `describeEquations`, `read`, `removeSymbols`, `getSymbols`. Use `vector` inputs instead.
- Breaking: `types` argument in the `Container` methods `listVariables` and `listEquations` does not support the input of type `list`. Use `vector` inputs instead.
- Breaking: `uelPriority` argument in the `Container` method `write` does not support the input of type `list`. Use `vector` inputs instead.
- `Container` method `write` now supports `mapped` write mode with the help of new argument `mode`
- Breaking: removed `getUniverseSet` method from `Container`. Use `getUELs` instead.
- Symbol `field` `refContainer` is renamed to `container`.
- bug fix in failed `Symbol` constructor call resulting in symbol being added to the `Container`
- The output of `describe*` `Container` methods has been improved for clarity.
- `summary` field for `Symbol` objects is refined for clarity.
- `Container` objects now have a `summary` field.
- New convenience methods for `Container` objects: `getSets`, `getAliases`, `getParameters`, `getVariables`, and `getEquations`.
- Breaking: removed all `Const*` classes. `ConstContainer` is no longer supported. Use `Container` instead.
- bug fix in read for `Variable` and `Equation` classes where the `lower` and `upper` attributes were interchanged.
- Records columns that are not specified by the user in a dataframe are not auto completed to save memory. For example, a set with only the domain columns will now not have the `element_text` columns. A default valuer is assumed for the missing attributes.
- bug fix in the Symbol method `equals` where identical domain symbols exist in different containers
- `uelPriority` argument in write does not have to be a subset of the universe set. Users can register any UELs using the `uelPriority` argument.
- new field `defaultValues` for symbols of type `Parameter`, `Variable`, and `Equation`.
- bug fix in symbol `reorderUELs` and `toDense` method for symbols with `relaxed domain`.

v2.4.0
==============================

- bug fix in read for `Variable` and `Equation` classes where the `lower` and `upper` attributes were interchanged.

v2.2.0
==============================

- fixed bug with library unload upon read or write
- performance improvement in setting records for symbols

v1.18.0
==============================

- Symbol method `isValid` now checks also for scalars with more than one record entries
- bug fix in symbol `description`
- added `isScalar` property for the symbols of type `Variable` and `Equation`
- Symbol method `getCardinality` is removed
- Updated symbol method `reorderUELs()`. If the argument `uels` is not passed UELs are reordered based on the records
- Symbol method `toDense()` now requires domain UELs and domain records to be in the same order and unused UELs in the domain (if any) at the end of the UEL list
- Fixed the display of long error messages
- bug fix in accessing Container symbols in a case-insensitive manner
- bug fix in the Container method `describeAliases`
- Container `read` preserves the domain type from the source and avoids domain linking by symbol name

v1.16.0
==============================

- added checks for singleton and multidimensional checksin `Symbol` method `isValid()`
- changed the default domain labels. User-specified domain labels are preserved if unique
- performance improvement in `Container` method `read`

v1.14.0
==============================

- Significant performance improvements to Container write method
- bug fix in Container read when reading a `Symbol` with unused UELs
- bug fix in `todense` method for `Symbol`

v1.12.0
==============================

- released for MacOS ARM64
- bug fix in writing empty container
- added `Symbol` method `copy` to copy symbol from one `Container` or `ConstContainer` to another `Container`
- added `Container` and `ConstContainer` method `copy` to copy symbols from to another
- bug fix in using ConstContainer Alias methods when the aliased parent set is absent

v1.10.0
==============================

- performance improvement in read
- Partial domain forwarding is now allowed by passing a logical vector as the `domainForwarding` argument
- `generateRecords` method for `Symbols` to automatically generate records

v1.8.0
==============================

- `removeSymbols` removes symbol links in other symbols
- added `symbols` argument to Container methods `renameUELs` and `removeUELs`
- `findDuplicateRecords` now returns a data frame instead of row indices
- Breaking: `Container` `data` field is now an ordered dictionary from `collections` package instead of named list. Instead of `m$data$<symbolname>`, use `m[<symbolname>]`.
- performance improvement to `Container` method `hasSymbols`. This results in significant speed-ups when adding multiple symbols to the Container.
- added `symbols` argument to `Container` methods `getDomainViolations`, `hasDomainViolations`, `countDomainViolations`, `dropDomainViolations`, `hasDuplicateRecords`, `countDuplicateRecords`, `dropDuplicateRecords`, `isValid`, and `write` for partial operation.
- `Container` method `getSymbols` now always returns a list
- added `equals` method to `Symbols` to check if symbols are equal
- bug fix in `SpecialValues$isNA`
- bug fix in `getUELs` for scalar symbols
- bug fix in `Symbol` method `isValid` for symbols containing only NA
- bug fix in `Variable` and `Equation` set records for numeric inputs
- added `equals` method for `Container` and `ConstContainer`

v1.6.0
==============================

- domain forwarding bug fix when the records are NULL
- describe* method bug fix when the type of symbol is not present in the Container
- added UniverseAlias symbol type for alias to the Universe
- renameUELs methods now have `allowMerge` argument for more data flexibility

v1.4.0
==============================

- Breaking: Symbol name uniqueness is now checked case insensitively. For
  example, it is not possible to have two different symbols named
  `symbol` and `Symbol` or `SYMBOL` anymore
- Added a method `Container$getSymbolNames` to return the original symbol names
  for a list of symbol names of any case
- Added a method `Container$hasSymbols` to check if symbol name (case insensitive)
  exists
- Changed `Container$getSymbols`, `Container$removeSymbols`,
  `Container$renameSymbol`, `Container$describe*` and others that use
  `Container$getSymbols` to accept symbol names case insensitively
- New Symbol methods `hasDuplicateRecords`, `countDuplicateRecords`, `findDuplicateRecords`, and `dropDuplicateRecords` to help debug and resolve errors with duplicate records
- New Container methods `hasDuplicateRecords`, `countDuplicateRecords`, and `dropDuplicateRecords` to help find symbols that contain duplicate records that cause the Symbol to be invalid
- Raises an exception if attempting to read a symbol that does not exist in the data source (previously symbol names were silently ignored)
- New tests within `Container$isValid()` method to detect broken container references in symbols and inconsistent symbol naming between the `<Container>$data` field and the symbol objects
- Breaking: Symbol records and symbol names are treated in a case insensitive manner. Symbols
  domain is not automaitcally checked
- isValid method does not check for symbol record domain columns being factors, for duplicates, and for domain violations
- New Symbol methods `hasDomainViolations`, `countDomainViolations`, `findDomainViolations`, `dropDomainViolations`, `getDomainViolations`
- New Container methods `hasDomainViolations`, `countDomainViolations`, `dropDomainViolations`
- bug fix in Alias record setter
- bug fix in `toDense()` method for Symbols with domaintype other than `regular`
- allowed symbol overwriting with `addSet/addParameter/addVariable/addEquation/addAlias` methods when everything other than records and description is unchanged
- Added methods `SpecialValues$isNA`, `SpecialValues$isEps`, `SpecialValues$isUndef`, `SpecialValues$isPosInf`, `SpcialValues$isNegInf` to test for SpecialValues
- bug fix in count* methods for multiple columns
- The argument for where* methods changed from `columns` to `column`
- Changing the symbol domain now changes column names of records data.frame accordingly
- removed dependence on the package `assertthat`
- automatically remove trailing whitespaces from UELs in `setRecords` and `*UEL` methods

v1.0.0
==============================

- added a new class ConstContainer for efficient data transfer
- allow read from Container/ConstContainer into another Container
- bug fix in listSets function that used to also list Aliases
- updated default_values in the Equation class
- updated Symbol isValid() method to check for levels of the linked domain set and symbol records
- bug fix in setting factor levels for subsets of subsets
- bug fix in countEPS method
- fixed record setter using non-dataframe object types for variables, equations, and parameters

v0.2.0
==============================

- released binary packages on Windows and MacOS
- bug fixes and improvements

v0.1.0
==============================

- first release

