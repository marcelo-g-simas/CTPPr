This R package provides direct access to the latest Census Transportation Planning Products (CTPP) datasets. The estimates provided in CTPP tables are some of the only sources available from large-sample survey data that are related to transportation behavior. Learn more about the CTPP and the types of tables available by browsing [here](https://ctpp.transportation.org/) and [here](https://www.fhwa.dot.gov/planning/census_issues/ctpp/). 

# Data

The download function works with two datasets, and our ability to fetch their tables from R depends on the following pages:

[2006-2010 5-Year CTPP](http://data5.ctpp.transportation.org/ctpp)

[2012-2016 5-Year CTPP](http://data5.ctpp.transportation.org/ctpp1216)

# Install

Reference the [install readme](https://github.com/Westat-Transportation/CTPPr/tree/master/inst/install) for specific instructions. Most use [RStudio Desktop](https://www.rstudio.com/products/rstudio/download/) to develop analyses with R.

```R
install.packages('devtools')
devtools::install_github('Westat-Transportation/CTPPr')
```

# Example 1

```R
# A110101 - HOUSING UNITS SAMPLED [TOTAL] (1) (ALL HOUSING UNITS)
A110101 <- download_ctpp(A110101)
```

# Example 2

```R
# A102106 -	MEANS OF TRANSPORTATION (18) (WORKERS 16 YEARS AND OVER)
A102106 <- download_ctpp(
  id = "A102106",
  dataset = "2016",
  geography = "County",
  state = "Maryland",
  output = "FIPS Code"
)
```

# Table Reference

```R
ctpp_tables()
```
[View this list on Github now](https://raw.githack.com/Westat-Transportation/CTPPr/master/inst/ctpp_tables.html). Click on a table row to copy an example download request to your clipboard.

