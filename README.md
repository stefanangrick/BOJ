BOJ
================

[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/BOJ)](http://cran.r-project.org/package=BOJ)
[![Cranlogs
Downloads](http://cranlogs.r-pkg.org/badges/grand-total/BOJ)](http://cran.r-project.org/package=BOJ)

The `BOJ` package provides an `R` interface to [Bank of
Japan](https://www.boj.or.jp/) statistics, specifically the [flat
files](https://www.stat-search.boj.or.jp/info/dload_en.html) available
on the [BOJ Time-Series Data](https://www.stat-search.boj.or.jp/)
portal.

## Installing the package

You can install the package from CRAN or GitHub.

``` r
library(devtools)
install_github("stefanangrick/BOJ")  # GitHub
install.packages("BOJ")              # CRAN
```

## Example usage

To start using the package, load it into your R session.

``` r
library("BOJ")
```

Next, retrieve a list of available data sets using the
`get_boj_datasets()` function.

``` r
ds <- get_boj_datasets()
head(ds)
```

    ## # A tibble: 6 × 3
    ##   desc                                                               name  url  
    ##   <chr>                                                              <chr> <chr>
    ## 1 Corporate Goods Price Index (CGPI)                                 cgpi… http…
    ## 2 Producer Price Index using chain-weighted index formula            cgpi… http…
    ## 3 Services Producer Price Index (SPPI)                               sppi… http…
    ## 4 Wholesale Services Price Index, and Research and development Serv… sppi… http…
    ## 5 Flow of Funds                                                      fof   http…
    ## 6 Flow of Funds (with name of time-series, etc.)                     fof2… http…

The `get_boj_datasets()` function returns a
[tibble](https://tibble.tidyverse.org/) data frame listing available
data sets. Use the `url` column as input for the `get_boj()` function to
download, parse, and import the corresponding data set.

For example, to import monthly-frequency data on Japan’s [Services
Producer Price
Index](https://www.boj.or.jp/en/statistics/pi/sppi_2015/index.htm), use
the following code:

``` r
sppi <- get_boj(ds$url[(ds$name == "sppi_m_en")])
head(sppi)
```

    ## # A tibble: 6 × 5
    ##   code              desc                                   struc date  obs_value
    ##   <chr>             <chr>                                  <chr> <chr>     <dbl>
    ## 1 PRCS20_5200000000 Services Producer Price Index (2020 b… [Bas… 2020…     101. 
    ## 2 PRCS20_5200000000 Services Producer Price Index (2020 b… [Bas… 2020…     101. 
    ## 3 PRCS20_5200000000 Services Producer Price Index (2020 b… [Bas… 2020…     101. 
    ## 4 PRCS20_5200000000 Services Producer Price Index (2020 b… [Bas… 2020…      99.4
    ## 5 PRCS20_5200000000 Services Producer Price Index (2020 b… [Bas… 2020…      98.8
    ## 6 PRCS20_5200000000 Services Producer Price Index (2020 b… [Bas… 2020…      99.3

To plot the data with [ggplot2](https://ggplot2.tidyverse.org), run the
following:

``` r
library("dplyr")
library("ggplot2")
library("zoo")

sppi_plot <- subset(sppi, code %in% c("PRCS20_5200000000", "PRCS20_5200010001",
                                      "PRCS20_5200010002", "PRCS20_5200010003",
                                      "PRCS20_5200010004", "PRCS20_5200010005",
                                      "PRCS20_5200010006", "PRCS20_5200010007"))
sppi_plot <- mutate(sppi_plot, date = as.Date(as.yearmon(date, format = "%Y%m")))
sppi_plot <- mutate(sppi_plot, struc = gsub("^Major group/ ", "", struc))
sppi_plot <- subset(sppi_plot, !is.na(obs_value))

ggplot(sppi_plot, aes(x = date, y = obs_value)) +
  geom_line(aes(colour = struc)) +
  labs(title = "Services Producer Price Index", x = "Date", y = "Index") +
  theme(legend.title = element_blank())
```

![](README_files/figure-gfm/plot-1.png)<!-- -->

Note that BOJ data sets use various time formats. The
[zoo](https://cran.r-project.org/package=zoo) package (e.g.,
`as.yearmon()`) can handle most of these formats.

## Retrieving individual data series

To retrieve individual data series instead of full data sets, consider
using the [BOJfame](https://github.com/stefanangrick/BOJfame) package.

## Note

This package is neither officially related to nor endorsed by the [Bank
of Japan](https://www.boj.or.jp/). Please avoid overloading the BOJ
servers with unnecessary requests.
