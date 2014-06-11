<html>
<head>
<title>MicroPlate</title>
</head>
<body>
this package is a set off tools to easily import and work with data from microplate readers

it contains a set of parsers:
- NovaSTAR -- and other *STAR microplate reader
- Spectramax -- and other *** readers
- ****

a standard way to add extra labels to this data using an .XLS (XLSX files (ODS support could be added if there would be a good package available))
- what is in each well

the data is imported in a native data format with 3 level 
- Plate
- Well
- Measurement
to save memory this object stores its data in its own environment, and is thus not copied if assigned to an other variable (a copy function does exist).

it also contains microplatereader bias removal tools
- plate bias (due to reader inaccuracies)
- time bias (due to evaporation)

it has an interface to the grofit package to easily determine growth rates.

it has some build in plot functions
and apply per plate/well/something you specified functions

the data is easily converted into a dataframe





</body>
</html>

