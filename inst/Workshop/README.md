# Working with Survey Datasets in R

This directory contains materials developed for the workshop "Working with Survey Datasets in R," held on June 3 at the 2019 TRB Planning Applications Conference. It demonstrates through example:  
  
- Setting up an R analysis environment
- Retreiving and manipulating CTPP tables
- Visualizing CTPP interval estimates at any confidence with tables, charts, and maps
- Observing and testing change in CTPP estimates over time 
- Merging estimates from the National Household Travel Survey

View the Workshop presentation [here](https://raw.githack.com/Westat-Transportation/CTPPr/master/inst/Workshop/Workshop.html)

## Steps to reproduce

1. [Install R, Rtools and RStudio](https://github.com/Westat-Transportation/CTPPr/tree/master/inst/install)
2. Download the contents of this directory
3. In addition to [CTPPr](https://github.com/Westat-Transportation/CTPPr), install the [summarizeNHTS](https://github.com/Westat-Transportation/summarizeNHTS) package
4. Place NHTS data csv files in /resources/nhts_data/csv/2017 ([data](https://nhts.ornl.gov/assets/2016/download/Csv.zip) and [weights](https://nhts.ornl.gov/assets/2016/download/ReplicatesCSV.zip))
5. Open Workshop.Rmd
    - Run code chunks in chronological order (recommended) or
    - use RStudio's "Knit" button to render the entire document
6. "Take chances, make mistakes, and get messy!"