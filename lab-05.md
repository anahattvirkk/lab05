Lab 05 - Wrangling Spatial Data
================
Anahatt Virk
06/28/25

### Load packages and data

``` r
library(tidyverse) 
library(dsbox) 
```

``` r
states <- read_csv("data/states.csv")
```

### Exercise 1

``` r
dn_ak <- dennys %>%
  filter(state == "AK")
  nrow(dn_ak)
```

    ## [1] 3

There are 3 Denny’s locations in Alaska.

``` r
lq_ak <- laquinta %>%
  filter(state == "AK")
  nrow(lq_ak)
```

    ## [1] 2

There are 2 La Quinta hotels in Alaska.

### Exercise 2

``` r
num_dennys <- nrow(dn_ak)
num_lq <- nrow(lq_ak)

num_pairings <- num_dennys * num_lq
```

There are 6 distances that we need to calculate.

### Exercise 3

…

### Exercise 4

…

### Exercise 5

…

### Exercise 6

…

Add exercise headings as needed.
