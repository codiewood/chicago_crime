
<!-- README.md is generated from README.Rmd. Please edit that file -->

# chicri

<!-- badges: start -->

[![R-CMD-check](https://github.com/codiewood/chicago_crime/actions/workflows/check-standard.yaml/badge.svg)](https://github.com/codiewood/chicago_crime/actions/workflows/check-standard.yaml)
<!-- badges: end -->

The goal of chicri is to provide functions for the analysis of the
[Chicago crime data
set](https://data.cityofchicago.org/Public-Safety/Crimes-2001-to-Present/ijzp-q8t2).
Namely, the functions are part of the SC1/SM1 group project for the
Compass PhD, and aid in performing regressions and classifications.

## Installation

You can install the development version of chicri from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("codiewood/chicago_crime/chicri")
```

## Example

This is a basic example which shows you how to load the Chicago crime
data from the year 2019:

``` r
library(chicri)
data <- load_crimes_API(year="2019")
```

We can then also process the data using the data processing
functionality, and investigate the outcome.

``` r
summary(data)
#>        ID           Case Number             Date                       
#>  Min.   :   24368   Length:261187      Min.   :2019-01-01 00:00:00.00  
#>  1st Qu.:11654490   Class :character   1st Qu.:2019-04-10 13:45:00.00  
#>  Median :11751963   Mode  :character   Median :2019-07-05 11:00:00.00  
#>  Mean   :11731161                      Mean   :2019-07-04 03:53:13.02  
#>  3rd Qu.:11847628                      3rd Qu.:2019-09-27 00:11:00.00  
#>  Max.   :12959329                      Max.   :2019-12-31 23:55:00.00  
#>                                                                        
#>     Block               IUCR           Primary Type       Description       
#>  Length:261187      Length:261187      Length:261187      Length:261187     
#>  Class :character   Class :character   Class :character   Class :character  
#>  Mode  :character   Mode  :character   Mode  :character   Mode  :character  
#>                                                                             
#>                                                                             
#>                                                                             
#>                                                                             
#>  Location Description    Arrest            Domestic              Beat     
#>  Length:261187        Length:261187      Length:261187      Min.   : 111  
#>  Class :character     Class :character   Class :character   1st Qu.: 611  
#>  Mode  :character     Mode  :character   Mode  :character   Median :1024  
#>                                                             Mean   :1137  
#>                                                             3rd Qu.:1713  
#>                                                             Max.   :2535  
#>                                                                           
#>     District          Ward       Community Area    FBI Code        
#>  Min.   : 1.00   Min.   : 1.00   Min.   : 1.00   Length:261187     
#>  1st Qu.: 6.00   1st Qu.:10.00   1st Qu.:23.00   Class :character  
#>  Median :10.00   Median :24.00   Median :32.00   Mode  :character  
#>  Mean   :11.15   Mean   :23.33   Mean   :36.61                     
#>  3rd Qu.:17.00   3rd Qu.:34.00   3rd Qu.:53.00                     
#>  Max.   :31.00   Max.   :50.00   Max.   :77.00                     
#>                  NA's   :15                                        
#>   X Coordinate      Y Coordinate          Year     
#>  Min.   :      0   Min.   :      0   Min.   :2019  
#>  1st Qu.:1153429   1st Qu.:1859114   1st Qu.:2019  
#>  Median :1167013   Median :1893812   Median :2019  
#>  Mean   :1165111   Mean   :1886303   Mean   :2019  
#>  3rd Qu.:1176561   3rd Qu.:1908243   3rd Qu.:2019  
#>  Max.   :1205116   Max.   :1951520   Max.   :2019  
#>  NA's   :2026      NA's   :2026                    
#>    Updated On                        Latitude       Longitude     
#>  Min.   :2019-01-10 15:16:50.00   Min.   :36.62   Min.   :-91.69  
#>  1st Qu.:2019-04-23 16:22:49.00   1st Qu.:41.77   1st Qu.:-87.71  
#>  Median :2019-07-20 16:13:10.00   Median :41.86   Median :-87.66  
#>  Mean   :2019-07-28 21:25:23.27   Mean   :41.84   Mean   :-87.67  
#>  3rd Qu.:2019-10-14 16:14:03.00   3rd Qu.:41.90   3rd Qu.:-87.63  
#>  Max.   :2023-01-22 15:50:37.00   Max.   :42.02   Max.   :-87.52  
#>                                   NA's   :2026    NA's   :2026    
#>    Location        
#>  Length:261187     
#>  Class :character  
#>  Mode  :character  
#>                    
#>                    
#>                    
#> 
data <- process_data(data)
summary(data)
#>        ID           Case Number             Date                       
#>  Min.   :   24368   Length:258162      Min.   :2019-01-01 00:00:00.00  
#>  1st Qu.:11654076   Class :character   1st Qu.:2019-04-10 14:00:00.00  
#>  Median :11751056   Mode  :character   Median :2019-07-05 07:30:00.00  
#>  Mean   :11728346                      Mean   :2019-07-04 01:55:18.08  
#>  3rd Qu.:11846413                      3rd Qu.:2019-09-26 19:41:30.00  
#>  Max.   :12799972                      Max.   :2019-12-31 23:55:00.00  
#>                                                                        
#>     Block                IUCR                    Primary Type  
#>  Length:258162      0486   : 23587   THEFT             :61675  
#>  Class :character   0820   : 22859   BATTERY           :49473  
#>  Mode  :character   0460   : 16435   CRIMINAL DAMAGE   :26611  
#>                     0810   : 14487   ASSAULT           :20599  
#>                     0560   : 13875   DECEPTIVE PRACTICE:17256  
#>                     1310   : 12851   OTHER OFFENSE     :16725  
#>                     (Other):154068   (Other)           :65823  
#>  Description                            Location Description   Arrest       
#>  Length:258162      STREET                        :56358     Mode :logical  
#>  Class :character   RESIDENCE                     :42998     FALSE:202091   
#>  Mode  :character   APARTMENT                     :34642     TRUE :56071    
#>                     SIDEWALK                      :20310                    
#>                     OTHER                         :10408                    
#>                     PARKING LOT/GARAGE(NON.RESID.): 7438                    
#>                     (Other)                       :86008                    
#>   Domestic            Beat           District           Ward       
#>  Mode :logical   1834   :  3321   11     : 18598   42     : 16389  
#>  FALSE:214982    111    :  2449   6      : 16754   28     : 12931  
#>  TRUE :43180     1112   :  2291   8      : 15606   27     : 12360  
#>                  1831   :  2224   1      : 15003   24     : 11153  
#>                  112    :  2137   18     : 14920   6      :  9721  
#>                  421    :  2043   4      : 13926   17     :  8363  
#>                  (Other):243697   (Other):163355   (Other):187245  
#>  Community Area      FBI Code      X Coordinate      Y Coordinate    
#>  25     : 14671   06     :61675   Min.   :      0   Min.   :      0  
#>  8      : 12399   08B    :42352   1st Qu.:1153410   1st Qu.:1859114  
#>  32     : 10401   14     :26611   Median :1167001   Median :1893770  
#>  28     :  9239   26     :20669   Mean   :1165101   Mean   :1886284  
#>  29     :  8936   08A    :18352   3rd Qu.:1176558   3rd Qu.:1908199  
#>  43     :  8617   11     :15577   Max.   :1205116   Max.   :1951520  
#>  (Other):193899   (Other):72926                                      
#>       Year        Updated On                        Latitude    
#>  Min.   :2019   Min.   :2019-01-10 15:16:50.00   Min.   :36.62  
#>  1st Qu.:2019   1st Qu.:2019-04-23 16:22:49.00   1st Qu.:41.77  
#>  Median :2019   Median :2019-07-19 16:22:44.00   Median :41.86  
#>  Mean   :2019   Mean   :2019-07-26 04:02:07.92   Mean   :41.84  
#>  3rd Qu.:2019   3rd Qu.:2019-10-12 16:05:42.00   3rd Qu.:41.90  
#>  Max.   :2019   Max.   :2023-01-18 15:50:26.00   Max.   :42.02  
#>                                                                 
#>    Longitude        Location        
#>  Min.   :-91.69   Length:258162     
#>  1st Qu.:-87.71   Class :character  
#>  Median :-87.66   Mode  :character  
#>  Mean   :-87.67                     
#>  3rd Qu.:-87.63                     
#>  Max.   :-87.52                     
#> 
```
