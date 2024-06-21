# cmhc-scaling-tool

### Introduction

Main Streets – an urban ‘form’ which exist in every community in Canada, regardless of size or population – are the critical organizing unit of every healthy community. Whether the scale of a city block or a four-corner intersection in a small town, everyone knows where their ‘main street’ meeting place is. These units of social and economic organization are seriously under-leveraged as the nexus of civic life and innovation of all kinds and are an under-resourced asset underpinning community cohesion. Main Streets are the essential building blocks of every place and require a provocative reassessment of their importance and potential to drive resilient city and community building.


ReMaking Main Street will take a fresh look at Canada’s main streets to envision planning and design solutions to put main streets at the centre of Canada’s approach to multi-solving for more resilience in housing, mental health, community safety and local economic development. Building on existing assets, ReMaking Main Street will explore the options of intensifying and repurposing existing street facing buildings, small scale housing, under-utilized or vacant sites as well as repurposing under-utilized 
spaces for sites such as libraries, faith places, post offices, recreation centres, legion halls, police stations and other civic buildings


## Project Goals

1. Analyze the feasibility of intensification on Canada’s main streets through adaptation of existing ground-facing buildings, under-utilized land and buildings with consideration of infrastructure capacity, population trends, civic infrastructure and essential services

2. Explore opportunities for intensification along main streets – gentle density – that enhances neigborhood character while increasing housing options, particularly affordable, multi- unit/multi-generational choices

3. Identify barriers and opportunities associated with these typologies including land use policies, timelines for construction, GHG reductions, local economic development and increased housing choice, including home ownership and rental stock.

4. Develop policy, program and design prototypes to support resilience through this approach to 
intensification


## Project Repository

This repository will help support the goals of this project in three specific ways

1. Create a data-driven methodology to identify similar streets nationally relative to a baseline main street segment or list of segments.

2. For a defined main streets segment or list of main street segments, gather the spatial location and unit count of addresses within a certain distance threshold.

3. Given a list of similar streets, provide an adjusted total of units per street based on a qualitative evaluation of units that can be added to the baseline main street segment


## Data

#### Main Street Base Network
 - Created as part of the Measuring Main Street platform, the Main Street Base Network attaches infrastructure and demographic data to street that have been identified as main street by meeting business and civic density thresholds.
 
#### Housing Data
 - Housing Construction Year and Housing Type Data per Dissemination Area based on the 2021 Canadian Census

#### Surface Parking
 - Surface parking data comprised from Open Street Map using Geofabrink

#### Unit Location Data
 - The National Address Register (NAR) sets up a standardized address structure and provides a list of valid georeferenced civic addresses in Canada. The addresses are extracted from Statistics Canada's Building Register and were validated by a minimum of two independent data sources. The Unit Location Data, due to its size, is split into provincal files
 

## Tools and Methodology

#### housing_data_setup.r
This script attaches the project specific Housing Data and Surface Parking data sets to the Main Street Base Network.

The housing construction data was aggregated into 3 types
  - percentage of housing built pre 1960
  - percentage of housing built from 1961 - 2000
  - percentage of housing built from 2001 - 2023

The housing type data was aggregated into 3 types
  - percentage of single and semi detached housing
  - mid density: percentage of duplex, row, and low-rise apartment housing
  - percentage of high-rise apartments
  
Housing Variables were attached to main streets within 250 metres of the DA. Which was the extent of our qualitative case study boundaries

Similarly the area of surface parking was aggregated and attached to all main streets within 250 metres


#### similar_streets
This tool takes an input list of baseline streets using their respective road segment id and returns a data frame of similar streets within a pre-defined threshold based using a series a similarity scores that model the urban and demographic makeup of the area.

Method

1. Calculate the average values of the baseline streets that will be used for the similarity calculation

2. Calculate a distance matrix between the average of the baseline streets and every street in the Main Street Base Network and convert that into a score of similarity where 0 is not similar and 1 is identical.
      Urban Form - population density / employment density / population change
      Street Content - main street business_density / civic density / business independence / surface parking
      Housing Stock - pre-1960 / construction year 61-00 / construction year 00-23 / Detached Housing / Mid Density / High-rise Apt
      Demographics - indigenous / visible minorities / immigrants and non-permanent residents / average employment income
      
3. Take the average of all four metrics and return within a defined threshold (default is 0.75)


#### get_casestudy_units
This tool takes a input list of baseline streets and the provincial data location file, and return the location and count of all units within a predefined distance of the baseline streets

Method

1. Turn the location csv file into a spatial object using it's respective coordinate data

2. For the baseline of streets create a buffered and dissolve polygon of a predefined distance, default value = 250 metres

3. Preform a spatial intersection between the road buffer and the location data

4. Calculate the sum of units by counting the total units for a given road segment id and address



#### scale_housing
This tool take the previously created data frame of similar streets and provides to total number of units along each road segment within 250 metres, but also provides the adjusted number based on an inputted percentage

Method

1. Create a for loop that iterates through each unique province within the similar street data frame

2. For each province; filter the similar street road network and create a list of road segment id's, load in the location data for that province and run the case study units to get the count of units for each segment.

3. Combine all roads for each province and create the adjusted total












