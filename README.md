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

    ## # A tibble: 13 × 3
    ##    desc                                    name     url                         
    ##    <chr>                                   <chr>    <chr>                       
    ##  1 "Corporate Goods Price Index (CGPI)"    cgpi_m_… https://www.stat-search.boj…
    ##  2 "Producer Price Index using chain-weig… cgpiren… https://www.stat-search.boj…
    ##  3 "Services Producer Price Index (SPPI)"  sppi_m_… https://www.stat-search.boj…
    ##  4 "Wholesale Services Price Index"        sppi_q_… https://www.stat-search.boj…
    ##  5 "Input-Output Price Index of the Manuf… iopi_m_… https://www.stat-search.boj…
    ##  6 "Flow of Funds, "                       fof      https://www.stat-search.boj…
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

To import monthly-frequency data on Japan’s [Services Producer Price
Index](https://www.boj.or.jp/en/statistics/pi/sppi_2015/index.htm/),
run:

``` r
sppi <- get_boj(datasets$url[(datasets$name == "sppi_m_en")])
sppi
```

    ## # A tibble: 41,738 × 5
    ##    code              desc                  struc                 date  obs_value
    ##    <chr>             <chr>                 <chr>                 <chr>     <dbl>
    ##  1 PRCS15_5200000000 Services Producer Pr… [Services Producer P… 2015…      99.6
    ##  2 PRCS15_5200000000 Services Producer Pr… [Services Producer P… 2015…      99.7
    ##  3 PRCS15_5200000000 Services Producer Pr… [Services Producer P… 2015…     100. 
    ##  4 PRCS15_5200000000 Services Producer Pr… [Services Producer P… 2015…     100  
    ##  5 PRCS15_5200000000 Services Producer Pr… [Services Producer P… 2015…     100. 
    ##  6 PRCS15_5200000000 Services Producer Pr… [Services Producer P… 2015…     100  
    ##  7 PRCS15_5200000000 Services Producer Pr… [Services Producer P… 2015…     100. 
    ##  8 PRCS15_5200000000 Services Producer Pr… [Services Producer P… 2015…     100. 
    ##  9 PRCS15_5200000000 Services Producer Pr… [Services Producer P… 2015…      99.9
    ## 10 PRCS15_5200000000 Services Producer Pr… [Services Producer P… 2015…      99.9
    ## # … with 41,728 more rows

To plot the data using [ggplot2](https://ggplot2.tidyverse.org), run the
following:

``` r
library("dplyr")
library("ggplot2")
library("zoo")

sppi_plot <- subset(sppi, code %in% c("PRCS15_5200000000", "PRCS15_5200010001",
                                      "PRCS15_5200010002", "PRCS15_5200010003",
                                      "PRCS15_5200010004", "PRCS15_5200010005",
                                      "PRCS15_5200010006", "PRCS15_5200010007"))
sppi_plot <- mutate(sppi_plot, date = as.Date(as.yearmon(date, format = "%Y%m")))
sppi_plot <- mutate(sppi_plot, struc = gsub("^Major group/ ", "", struc))
sppi_plot <- subset(sppi_plot, !is.na(obs_value))

ggplot(sppi_plot, aes(x = date, y = obs_value)) +
  geom_line(aes(colour = struc)) +
  labs(x = "Date", y = "Services Producer Price Index (2015 base)") +
  theme(legend.title = element_blank())
```

![](README_files/figure-gfm/plot-1.png?)<!-- -->

Note that BOJ data sets come with a number of different time formats.
The [zoo](https://cran.r-project.org/package=zoo) package
(e.g. `as.yearmon()`) should be able to parse most formats.

## Note

This package is in no way officially related to or endorsed by the [Bank
of Japan](https://www.boj.or.jp/). It was inspired by the [BIS R
package](https://github.com/expersso/BIS). Please don’t abuse the BOJ’s
servers with unnecessary calls.
