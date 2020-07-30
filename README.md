# Visual-Analytics-5122-Final-Project
Created by Jacob Martin and Robert Bintu

This repository was created to house our final project for the class Visual Analytics 5122 at the University of North Carolina at Charlotte.  It consists of an R Shiny App to visualize the dataset we selected.  

The Dataset comes from https://fatalencounters.org/

Below is a comment provided by the website to provide context on it's creation and contents: 

"We try to document all deaths that happen when police are present or that are caused by police: on-duty, off-duty, criminal, line-of-duty, local, federal, intentional, accidental–all of them. We enable people to filter out the deaths that they aren’t interested in examining. If you want intentional use of force and intentional use of deadly force, sort by Column V. There is at least one team that is +only+ analyzing the police-related suicides. There’s another group that only studies teenagers who die during police pursuits. The idea is that the data is there for anyone who needs it for whatever reason they want it. "

Files:
- server.R - Server file for the R Shiny App 
- ui.R - UI File for the R Shiny App 
- Archive
  - R Markdown - shows data cleaning and some of the work used to create the visualizations
- documents 
  - Our paper - PDF detailing our work and thought process for the project 
- data
  - ChlorData.geojson - a Json file that includes US state geomotry and additional data to coorespond to those
  - Fatal.csv - The Fatal dataset after the removal of some columns


Credits: 
Dataset provided by: https://fatalencounters.org/
State Geometry file provided by Mike Bostock (https://bost.ocks.org/mike/) and pulled from the Leaflet.js team's example (https://leafletjs.com/examples/choropleth/)
Inspiration for the Point's tab visualization from the Fatal encounters visualization team at https://github.com/adv-datasci/fatalencounters and https://jhubiostatistics.shinyapps.io/policeviolence/ 



