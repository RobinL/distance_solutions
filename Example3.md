Dplyr example 1
---------------

We have data that looks like this:

``` r
knitr::kable(df)
```

| id       |    x|    y|  cluster|  expected\_distance|
|:---------|----:|----:|--------:|-------------------:|
| 1        |    3|    4|        1|            5.000000|
| 2        |    0|    0|        1|            0.000000|
| centroid |    0|    0|        1|                  NA|
| 3        |    2|    2|        2|            1.414214|
| 4        |    1|    3|        2|            2.000000|
| centroid |    1|    1|        2|                  NA|

I have included an 'expected distance' column so we can later do some unit tests

We can convert this into a tidy format as follows:

``` r
df_c <- df %>% filter(id == "centroid")
df_p <- df %>% filter(id != "centroid")

suffixes = c(point="_p", centroid="_c")

df_d <- dplyr::left_join(df_p, df_c, by="cluster", suffix = suffixes) %>% 
  dplyr::select(-expected_distance_c, -cluster, -id_p, -id_c) %>% 
  dplyr::select(-dplyr::one_of("expected_distance_p"), dplyr::one_of("expected_distance_p")) %>% 
  dplyr::rename(expected_distance =expected_distance_p)
  
knitr::kable(df_d)
```

|  x\_p|  y\_p|  x\_c|  y\_c|  expected\_distance|
|-----:|-----:|-----:|-----:|-------------------:|
|     3|     4|     0|     0|            5.000000|
|     0|     0|     0|     0|            0.000000|
|     2|     2|     1|     1|            1.414214|
|     1|     3|     1|     1|            2.000000|

To compute a distance, we want pairs of vectors to put in a distance function like `distance(v1, v2)`. We want v1 and v2 to be vectors of arbitrary length, corresponding to a set of columns that the user has selected

``` r
distance_cols <- c("x", "y")
point_cols <- paste0(distance_cols, suffixes["point"])
centroid_cols <- paste0(distance_cols, suffixes["centroid"])
```

``` r
myfn <- function(row, cols) {
  distance_list <- as.list(row)[cols]
  unlist(distance_list)
}

df_d <- df_d %>% 
  purrrlyr::by_row(myfn, point_cols, .to="v_p") %>% 
  purrrlyr::by_row(myfn, centroid_cols, .to="v_c")

get_dist <- function(v1, v2) {
  as.double(dist(rbind(v1, v2)))
}

df_d <- df_d %>% 
  mutate(distance = purrr::map2_dbl(v_p, v_c, get_dist))

kable(df_d)
```

|  x\_p|  y\_p|  x\_c|  y\_c|  expected\_distance| v\_p | v\_c |  distance|
|-----:|-----:|-----:|-----:|-------------------:|:-----|:-----|---------:|
|     3|     4|     0|     0|            5.000000| 3, 4 | 0, 0 |  5.000000|
|     0|     0|     0|     0|            0.000000| 0, 0 | 0, 0 |  0.000000|
|     2|     2|     1|     1|            1.414214| 2, 2 | 1, 1 |  1.414214|
|     1|     3|     1|     1|            2.000000| 1, 3 | 1, 1 |  2.000000|

Check that it gives the right result:

``` r
testthat::expect_true(all.equal(df_d$expected_distance, df_d$distance))
```
