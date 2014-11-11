#MicroPlate

this package is a set off tools to easily import and work with data from microplate readers

it contains a set of parsers:
- NovaSTAR -- and other *STAR microplate reader
- Spectramax -- and other *** readers
- ****

a standard way to add extra labels to this data using a layoutfile that is written in any of the standard spreadsheet formats (.xls, .xlsx, .ods)

the data is imported in a native data format with 3 levels
- Plate
- Well
- Measurement
to save memory this object stores its data in its own environment, and is thus not copied if assigned to an other variable (a copy function does exist).

it has an interface to the grofit package to easily determine growth rates.

it has some build in plot functions

the data is easily converted into a dataframe

for the full documentation, ask for the manaual.... which is still under construction :)





