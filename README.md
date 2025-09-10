<p align="center">
<img width="330" height="243" alt="Road_Salt_Logo_no_background" src="https://github.com/user-attachments/assets/ce0bc499-e2bb-44fa-aa2b-e190f03c90b6" />
</p>
<h3 align="center">The Road Salt and Pacific Salmon Success Project</h3>
<h1 align="center">
  <a>Water Quality Data Explorer RShiny App</a>
  <br><br>
  </h1>
<p align="center">
  <a href="https://psecengagement.shinyapps.io/WaterQualityRShinyApp/">
    See the app here
  </a>
</p>

<h2>About</h2>
<p>
The code for this RShiny app was written by Patrick Schaefer (<a href="https://datastream.org/en-ca/">DataStream</a>) and Clare Kilgour (UBC / Azimuth Consulting Group Inc.), and was developed as part of the <a href = "https://www.theroadsaltproject.com">Road Salt Project</a> (NSERC Alliance).

The Road Salt Project is a collaboration between academic institutions (UBC, SFU & BCIT), government (Department of Fisheries and Oceans Canada), and local stewardship groups that aims to understand the potential impacts of road salt pollution on the developing salmonids and benthic invertebrates which inhabit the freshwater streams of Vancouver's Lower Mainland.

This app is used to visualize high-frequency conductivity data collected by water quality loggers installed across Vancouver's Lower Mainland. Volunteers from <a href="https://www.theroadsaltproject.com/volunteers">14 stream stewardship organizations</a> download the data from 30 loggers quarterly. That data is stored on DataStream, and accessed by the app via the API. In the app, users can plot the data as specific conductance (μS/cm) or as calculated chloride concentrations (mg/L Cl<sup>-</sup>). When plotted as calculated chloride concentrations, BC's water quality guidelines for chloride are also plotted. Plots generated using this app can then be easily downloaded as a .png file. Community partners can then use these plots to better understand and advocate for their local streams using the water quality data that they have helped to collect. 

The water quality data loggers for this project were installed in partnership with the DFO Pacific Science Enterprise Center. 
</p>

<h2>Components</h2>
<p>
In order to run this app (or create one that is similar), there are two components: 

  1. [Database Creation](https://github.com/clarekilgour/RSP-Water-Quality-Explorer-RShiny-App/tree/main/Data)
     
  3. [App](https://github.com/clarekilgour/RSP-Water-Quality-Explorer-RShiny-App/tree/main/App)

First run the code to create an SQLite database, and then run the app. See the README files in each folder for further details and instruction.
</p>
