# GIS-in-R
Using ArcGIS tools in R

This project merges spatial, census, and election data to identify urban centers in southern Benin and their political party affiliation. 

Spatial data comes from shapefiles of Benin's districts (arrondissements). 

Census data was obtained from the National Statistical Office in Benin, in Excel format. 

Election data was obtained from Benin's national Electoral Commission, in PDF format converted to CSV. 

Datasets were merged using district name and district IDs. 

The code excerpt in this repository was written to create a sample pool of districts meeting the following qualifications: a population density of 100/sq. km, mayor voteshares within a 15-point distance from the average municipal voteshare, and located in southern regions. 

The main output is a map of qualifying districts with their buffer zones and visual scales to indicate pop density values. 
