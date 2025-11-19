# DC Historic district analysis

This repository has the code and data for a project that tries to see if historic districts (HDs) lead to neighborhood socioeconomic changes.

See report for details of the analysis: https://pete-rodrigue.github.io/hd_analysis/

(also see the main script; I commented it a lot)

The main script is an Rmarkdown file called analysis.Rmd. You can open that in R studio and see all my analysis. Here's what the other files/folders do:

* .github/workflows: this folder has a yaml file that describes a github action. The github action just takes the output of analysis.Rmd, which is a nicely-formatted webpage called analysis.html, and renames that html file index.html. This action is automatically triggered any time analysis.html changes. I do this because github pages only recognizes "index.html" as the file that supports the github page. So rather than having to rename that file manually over and over, I have a github action do it. It's just a time saver.
* Affordable housing: a shapefile with the locations of different affordable housing projects in DC. Come from here: https://opendata.dc.gov/datasets/DCGIS::affordable-housing/about
* Historic_Districts: a shapefile with the HD boundaries; from here: https://opendata.dc.gov/datasets/DCGIS::historic-districts/about
* Literature: some of the papers from the "literature review" section of the report.
* Wards_from_2022: shapefile with the ward boundaries. I don't think I actually used this in the report. But it might be helpful if you want to fork this repo! Data from here: https://opendata.dc.gov/datasets/DCGIS::wards-from-2022/about
* block_data: this has 1970 to 2020 block-level data (in CSV format). All of this data comes from IPUMS NHGIS. This folder also has:
    * a folder containing 1990-2020 housing unit data (also in CSV format)
    * a folder containing block crosswalks from NHGIS from 1990 to 2020. I ended up not using these crosswalks, but if you want to remix this analysis, these could be helpful.
* block_group_data: contains 2006-2010 and 2019-2023 5-year ACS block group data on a range of variables. I use this in the descriptive statistics portion of the report. Also comes from IPUMS NHGIS.
* block_group_shapes: these are the shapefiles for the block groups. Also comes from IPUMS NHGIS.
* block_shapes: these are the block shapefiles for the years 1940 - 2020. The shapefiles all come from IPUMS NHGIS. Note that the boundaries are not totally consistent across time (thus the need for some sort of crosswalk). 
* discriminatory_policies: a 1930s Federal Housing Administration map that was basically just redlining, plus a shapefile of discriminatory covenants created from [Mapping Segregation data](https://mappingsegregationdc.org/).
* hd_data: data i put together on historic districts.
* images: images the Rmarkdown file pulls on
* temp_data: you can ignore this (just used this for debugging so i didn't have to pull data from google; my home router blocks google after 9pm lol)
* timeseries: saved versions of the time series data, so you don't have to recreate them every time you run the Rmarkdown file (it takes a bit)
* tract_data: (not used) tract-level data
* xwalks: saved versions of the block crosswalk files that the Rmarkdown file uses.
* zoning: a shapefile of DC's 2016 zoning boundaries, from here: https://opendata.dc.gov/datasets/DCGIS::zoning-boundaries-zoning-regulations-of-2016/about
* .gitattributes: you can sort of ignore this file
* .gitignore: the files you don't want to commit (i don't commit the block data from 1970 for the whole US, for example)
* LICENSE: the MIT license
* README.md: this readme lol
* analysis.Rmd: the main rmarkdown file where the report is created
* index.html: the html file for the github page
* scrape_urban_turf.py: a python script I used to scape all the apartment/condo projects in DC from Urban Turf.
* supporting_functions.R: the supporting functions that analysis.Rmd relies on to do the analysis. I just put them in another script to make all the code easier to read. 
