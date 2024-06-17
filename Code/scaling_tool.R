## Title: housing on main streets - scaling tool
## Author: Alex Tabascio
## Date: 2024-06-17
## Summary: This script takes the cleaned address and location data from the National Address
## Registery to create a realistic estimate of new housing stock across the country based on the
## proposed changes of the defined case studies


## Load Libraries
library(tidyverse)
library(sf)


setwd("~/cmhc-scaling")
test = read_csv("./Data/address_data/joined_addresses_60.csv")
