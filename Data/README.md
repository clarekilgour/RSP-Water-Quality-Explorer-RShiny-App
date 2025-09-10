<p align="center">
<img width="330" height="243" alt="Road_Salt_Logo_no_background" src="https://github.com/user-attachments/assets/ce0bc499-e2bb-44fa-aa2b-e190f03c90b6" />
</p>
<h3 align="center">The Road Salt and Pacific Salmon Success Project</h3>
<h2 align="center">
  <a>Water Quality Data Explorer RShiny App</a>
  <br><br>
</h2>
<h1 align="center">
  <a>Part 1: Database Creation</a>
  <br><br>
</h1>
<h2>Overview</h2>
<p>
This code uses the <a href="https://github.com/datastreamapp/api-docs">DataStream API</a> to access and compile conductivity data for the monitoring locations of interest for all complete years of data (e.g., in 2025, the database would include all data from 2021-2024). The app can then pull data directly from the SQLite database for all historical years, and can make an API call if data from the current year is requested. This strategy minimizes the number of API calls the app will need to make, improving speed and overall performance. 
</p>
<h2>Getting Started</h2>
<p>
Before running this code you will need to:

  1. <a href="https://docs.google.com/forms/d/1SjPVeblz2QFaghpiBZPZKOVNKXgw5UMnAtJLJS1tQYI/viewform?edit_requested=true">Request a DataStream API Key</a>
  2. Store that API key as an environmental variable (in an .Renviron file), using the format: DATASTREAM_API_KEY= your API key.

If you would like to visualize data from a different dataset that is hosted on DataStream:

  1. Identify the DOI of the dataset you would like to access.
  2. Input this new DOI on line 15.
  3. Update the list of IDs in lines 18-21 to those of your Monitoring Locations of interest. 
</p>
<h2>Outputs</h2>
<p>
This code generates an SQLite database called raw_data.sqlite. This file needs to be updated once annually to keep the app current. 

