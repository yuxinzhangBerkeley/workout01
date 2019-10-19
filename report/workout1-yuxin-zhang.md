workout1-yuxin-zhang
================
yuxinzhang
10/18/2019

The dataset from IBTrACs includes tropical cyclone best track data from
2010 to 2015 in a centralized location to aid our understanding of the
distribution, frequency, and intensity of tropical cyclones worldwide.

``` r
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(readr)
library(ggplot2)
library(readr)
library(lubridate)
```

    ## 
    ## Attaching package: 'lubridate'

    ## The following object is masked from 'package:base':
    ## 
    ##     date

``` r
df =read_csv("/Users/yuxinzhang/Downloads/STAT133/workout01/data/ibtracs-2010-2015.csv",
             col_names = TRUE, col_types = cols(), 
             na = c("-999.","-1.0","0.0"))
#Specify strings of missing values with "-999.", "-1.0", and "0.0".
df$Basin = as.factor(df$Basin)
df$ISO_time = as.character(df$ISO_time)
colnames(df)[colnames(df)=="Wind(WMO)"]<-"Wind"
colnames(df)[colnames(df)=="Pres(WMO)"]<-"Pres"
```

``` r
tc_per_year<-summarise(
  group_by(df, Season),
  number_tc=n_distinct(Name)
)

ggplot(tc_per_year, aes(x=Season, y=number_tc))+geom_bar(stat='identity')
```

![](workout1-yuxin-zhang_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

``` r
df['Month'] = month(df$ISO_time, label = TRUE)

tc_per_month<-summarise(
  group_by(df, Month),
  number_tc=n()
)

tc_per_month
```

    ## # A tibble: 12 x 2
    ##    Month number_tc
    ##    <ord>     <int>
    ##  1 Jan        1424
    ##  2 Feb        1354
    ##  3 Mar        1021
    ##  4 Apr         579
    ##  5 May         558
    ##  6 Jun        1019
    ##  7 Jul        1269
    ##  8 Aug        2344
    ##  9 Sep        2314
    ## 10 Oct        2158
    ## 11 Nov         945
    ## 12 Dec        1200

Obviously, the numbers of unique storms in 2015 and 2013 are
significantly higher than the others. With respect to month, the number
of unique storms is at the trough in April, begins to increase gradually
and reach the peak in summer, then decreases significantly. In another
word, storms are more active in Aug and
Sep.

``` r
knitr::include_graphics('/Users/yuxinzhang/Downloads/STAT133/workout01/images/map-all-storms.png')
```

<img src="/Users/yuxinzhang/Downloads/STAT133/workout01/images/map-all-storms.png" style="display: block; margin: auto;" />

The map about worldwide storms shows that most storms concentrate around
the equator. Now we move to compare storm frequency, wind and pressure
in each hemisphere.

``` r
df['hemisphere']=NA
df$hemisphere[df$Latitude >0&df$Longitude >0]="NE"
df$hemisphere[df$Latitude >0&df$Longitude <0]="NW"
df$hemisphere[df$Latitude <0&df$Longitude >0]="SE"
df$hemisphere[df$Latitude <0&df$Longitude <0]="SW"
hemi_summary<-summarise(
  group_by(na.omit(df), hemisphere),
  number_tc=n_distinct(Name),
  mean_wind=mean(Wind),
  mean_pres=mean(Pres)
)
hemi_summary
```

    ## # A tibble: 4 x 4
    ##   hemisphere number_tc mean_wind mean_pres
    ##   <chr>          <int>     <dbl>     <dbl>
    ## 1 NE               138      53.8      979.
    ## 2 NW               165      45.4      996.
    ## 3 SE               105      39.8      990.
    ## 4 SW                23      44.3      987.

The summary table show that storms in NW are most frequent, while
significant least frequent in SW, and average wind is the most strong in
NE. On the other hand, average pressure of storms in each hemisphere do
not deviate a lot.

``` r
basin_wind_pres<-(summarise(
  group_by(na.omit(df), Basin),
  number_tc=n_distinct(Name),
  wind=mean(Wind, na.rm = TRUE),
  pres=mean(Pres, na.rm = TRUE)
))
basin_wind_pres
```

    ## # A tibble: 7 x 4
    ##   Basin number_tc  wind  pres
    ##   <fct>     <int> <dbl> <dbl>
    ## 1 EP           86  43.8  998.
    ## 2 NA           84  46.9  995.
    ## 3 NI           24  39.4  992.
    ## 4 SA            1  32.8 1002.
    ## 5 SI           71  39.5  991.
    ## 6 SP           46  42.4  988.
    ## 7 WP          113  58.8  974.

For more details, storms are most active in WP than in the others, while
the storms in SA are least active. Additionally, we compute the average
wind and pressure in each basin and find out that wind in WP is
strongest, while weakes in SA. And devaition among pressure of storms
are not great, concentrating around 1000, while pressure is lowest in
WP.

``` r
knitr::include_graphics('/Users/yuxinzhang/Downloads/STAT133/workout01/images/map-ep-na-storms-by-year.png')
```

<img src="/Users/yuxinzhang/Downloads/STAT133/workout01/images/map-ep-na-storms-by-year.png" style="display: block; margin: auto;" />

``` r
knitr::include_graphics('/Users/yuxinzhang/Downloads/STAT133/workout01/images/map-ep-na-storms-by-month.png')
```

<img src="/Users/yuxinzhang/Downloads/STAT133/workout01/images/map-ep-na-storms-by-month.png" style="display: block; margin: auto;" />

Focusing on EP and NA basins each year and month, storms in NA are more
active in the last three years than before, while storms in EP are more
active in the first three years and became less active. Storms in both
region are active in later summer, Aug and Sep, and inactive in winnter,
Dec and Jan.
