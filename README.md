BOJ
================

[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/BOJ)](http://cran.r-project.org/package=BOJ)
[![Cranlogs
Downloads](http://cranlogs.r-pkg.org/badges/grand-total/BOJ)](http://cran.r-project.org/package=BOJ)

The `BOJ` package provides an `R` interface to [Bank of
Japan](https://www.boj.or.jp/) statistics, specifically the [flat
files](https://www.stat-search.boj.or.jp/info/dload_en.html) available
on the [BOJ Time-Series Data](https://www.stat-search.boj.or.jp/)
portal.

## Installation

Install the package using the below commands:

``` r
library("devtools")
install_github("stefanangrick/BOJ")  # From GitHub
install.packages("BOJ")              # From CRAN
```

## Import data

To import data, first load the package:

``` r
library("BOJ")
```

Next, run the `get_boj_datasets()` function to obtain a list of
available data sets:

``` r
datasets <- get_boj_datasets()
datasets
```

    ## # A tibble: 13 x 3
    ##    desc                                    name     url                         
    ##    <chr>                                   <chr>    <chr>                       
    ##  1 "Corporate Goods Price Index (CGPI)"    cgpi_m_… https://www.stat-search.boj…
    ##  2 "Producer Price Index using chain-weig… cgpiren… https://www.stat-search.boj…
    ##  3 "Services Producer Price Index (SPPI)"  sppi_m_… https://www.stat-search.boj…
    ##  4 "Wholesale Services Price Index"        sppi_q_… https://www.stat-search.boj…
    ##  5 "Input-Output Price Index of the Manuf… iopi_m_… https://www.stat-search.boj…
    ##  6 "Flow of Funds"                         fof      https://www.stat-search.boj…
    ##  7 "Flow of Funds (with name of time-seri… fof2_en  https://www.stat-search.boj…
    ##  8 "TANKAN"                                co       https://www.stat-search.boj…
    ##  9 "TANKAN (Fixed Investment and Software… colease  https://www.stat-search.boj…
    ## 10 "Balance of Payments "                  bp_m_en  https://www.stat-search.boj…
    ## 11 "Regional Balance of Payments (quarter… regbp_q… https://www.stat-search.boj…
    ## 12 "International Investment Position (Qu… qiip_q_… https://www.stat-search.boj…
    ## 13 "International Investment Position (Ca… iip_cy_… https://www.stat-search.boj…

The function returns a [tibble](https://tibble.tidyverse.org/) data
frame listing the available data sets. The column `url` can be used as
input for the function `get_boj()` which downloads, parses and imports
the corresponding data.

To import monthly-frequency data on Japan’s [Balance of
Payments](https://www.boj.or.jp/en/statistics/br/index.htm/), run:

``` r
bop <- get_boj(datasets$url[(datasets$name == "bp_m_en")])
bop
```

    ## # A tibble: 822,600 x 6
    ##    code     desc                      struc            unit      date  obs_value
    ##    <chr>    <chr>                     <chr>            <chr>     <chr>     <dbl>
    ##  1 BPBP6JY… Balance of Payments (Dat… Current account… 100 mill… 1996…      342.
    ##  2 BPBP6JY… Balance of Payments (Dat… Current account… 100 mill… 1996…     8609.
    ##  3 BPBP6JY… Balance of Payments (Dat… Current account… 100 mill… 1996…    13154.
    ##  4 BPBP6JY… Balance of Payments (Dat… Current account… 100 mill… 1996…     6176.
    ##  5 BPBP6JY… Balance of Payments (Dat… Current account… 100 mill… 1996…     4080.
    ##  6 BPBP6JY… Balance of Payments (Dat… Current account… 100 mill… 1996…     6361.
    ##  7 BPBP6JY… Balance of Payments (Dat… Current account… 100 mill… 1996…     5450.
    ##  8 BPBP6JY… Balance of Payments (Dat… Current account… 100 mill… 1996…     4778.
    ##  9 BPBP6JY… Balance of Payments (Dat… Current account… 100 mill… 1996…     7049.
    ## 10 BPBP6JY… Balance of Payments (Dat… Current account… 100 mill… 1996…     3758.
    ## # … with 822,590 more rows

To plot the data using [ggplot2](https://ggplot2.tidyverse.org), run the
following:

``` r
library("dplyr")
library("ggplot2")
library("zoo")

bop_plot <- subset(bop, code %in% c("BPBP6JYNTB", "BPBP6JYNSN", "BPBP6JYNPIN",
                                    "BPBP6JYNSIN"))
bop_plot <- mutate(bop_plot, date = as.Date(as.yearmon(date, format = "%Y%m")))
bop_plot <- subset(bop_plot, date > as.Date("2000-01-01"))
bop_plot <- subset(bop_plot, !is.na(obs_value))

ggplot(bop_plot, aes(x = date, y = obs_value)) +
  geom_bar(aes(fill = struc), stat = "identity") +
  labs(x = "Date", y = "100 million yen") +
  theme(legend.title = element_blank())
```

![](README_files/figure-gfm/plot-1.png)<!-- -->

Note that BOJ data sets come with a number of different time formats.
The [zoo](https://cran.r-project.org/package=zoo) package
(e.g. `as.yearmon()`) should be able to parse most formats.

## Note

This package is in no way officially related to or endorsed by the [Bank
of Japan](https://www.boj.or.jp/). It was inspired by the [BIS R
package](https://github.com/expersso/BIS). Please don’t abuse the BOJ’s
servers with unnecessary calls.
