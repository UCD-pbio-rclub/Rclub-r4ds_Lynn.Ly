# May 10



# Data transformation

### 5.2.4 Exercises


```r
one <- filter(flights, arr_delay > 120)
two <- filter(flights, dest %in% c("IAH", "HOU"))
three <- filter(flights, carrier %in% c("UA", "AA", "DL"))
four <- filter(flights, month %in% c(7, 8, 9))
five <- filter(flights, arr_delay > 120 & dep_delay <= 0)
six <- filter(flights, dep_delay > 60 & arr_delay < 30)
seven <- filter(flights, dep_time > 0 & dep_time < 360)
sevenb <- filter(flights, between(dep_time, 0, 360))
```

between is a shortcut for x >= left and x >= right. Usage: between(x, left, right)


```r
canceled <- filter(flights, is.na(dep_time))
```

Rows missing dep_time are also missing delays, arrival times. They probably are canceled flights

Anything ^0 is one. |TRUE is always true not matter what the other clauses are. &FALSE is always false. 

### 5.3.1


```r
arrange(flights, desc(is.na(dep_time)))
```

```
## # A tibble: 336,776 × 19
##     year month   day dep_time sched_dep_time dep_delay arr_time
##    <int> <int> <int>    <int>          <int>     <dbl>    <int>
## 1   2013     1     1       NA           1630        NA       NA
## 2   2013     1     1       NA           1935        NA       NA
## 3   2013     1     1       NA           1500        NA       NA
## 4   2013     1     1       NA            600        NA       NA
## 5   2013     1     2       NA           1540        NA       NA
## 6   2013     1     2       NA           1620        NA       NA
## 7   2013     1     2       NA           1355        NA       NA
## 8   2013     1     2       NA           1420        NA       NA
## 9   2013     1     2       NA           1321        NA       NA
## 10  2013     1     2       NA           1545        NA       NA
## # ... with 336,766 more rows, and 12 more variables: sched_arr_time <int>,
## #   arr_delay <dbl>, carrier <chr>, flight <int>, tailnum <chr>,
## #   origin <chr>, dest <chr>, air_time <dbl>, distance <dbl>, hour <dbl>,
## #   minute <dbl>, time_hour <dttm>
```

```r
arrange(flights, dep_delay) #earliest. For most delayed, put desc(dep_delay)
```

```
## # A tibble: 336,776 × 19
##     year month   day dep_time sched_dep_time dep_delay arr_time
##    <int> <int> <int>    <int>          <int>     <dbl>    <int>
## 1   2013    12     7     2040           2123       -43       40
## 2   2013     2     3     2022           2055       -33     2240
## 3   2013    11    10     1408           1440       -32     1549
## 4   2013     1    11     1900           1930       -30     2233
## 5   2013     1    29     1703           1730       -27     1947
## 6   2013     8     9      729            755       -26     1002
## 7   2013    10    23     1907           1932       -25     2143
## 8   2013     3    30     2030           2055       -25     2213
## 9   2013     3     2     1431           1455       -24     1601
## 10  2013     5     5      934            958       -24     1225
## # ... with 336,766 more rows, and 12 more variables: sched_arr_time <int>,
## #   arr_delay <dbl>, carrier <chr>, flight <int>, tailnum <chr>,
## #   origin <chr>, dest <chr>, air_time <dbl>, distance <dbl>, hour <dbl>,
## #   minute <dbl>, time_hour <dttm>
```

```r
arrange(flights, air_time)
```

```
## # A tibble: 336,776 × 19
##     year month   day dep_time sched_dep_time dep_delay arr_time
##    <int> <int> <int>    <int>          <int>     <dbl>    <int>
## 1   2013     1    16     1355           1315        40     1442
## 2   2013     4    13      537            527        10      622
## 3   2013    12     6      922            851        31     1021
## 4   2013     2     3     2153           2129        24     2247
## 5   2013     2     5     1303           1315       -12     1342
## 6   2013     2    12     2123           2130        -7     2211
## 7   2013     3     2     1450           1500       -10     1547
## 8   2013     3     8     2026           1935        51     2131
## 9   2013     3    18     1456           1329        87     1533
## 10  2013     3    19     2226           2145        41     2305
## # ... with 336,766 more rows, and 12 more variables: sched_arr_time <int>,
## #   arr_delay <dbl>, carrier <chr>, flight <int>, tailnum <chr>,
## #   origin <chr>, dest <chr>, air_time <dbl>, distance <dbl>, hour <dbl>,
## #   minute <dbl>, time_hour <dttm>
```

```r
arrange(flights, distance) #shortest. For longest, put desc(distance)
```

```
## # A tibble: 336,776 × 19
##     year month   day dep_time sched_dep_time dep_delay arr_time
##    <int> <int> <int>    <int>          <int>     <dbl>    <int>
## 1   2013     7    27       NA            106        NA       NA
## 2   2013     1     3     2127           2129        -2     2222
## 3   2013     1     4     1240           1200        40     1333
## 4   2013     1     4     1829           1615       134     1937
## 5   2013     1     4     2128           2129        -1     2218
## 6   2013     1     5     1155           1200        -5     1241
## 7   2013     1     6     2125           2129        -4     2224
## 8   2013     1     7     2124           2129        -5     2212
## 9   2013     1     8     2127           2130        -3     2304
## 10  2013     1     9     2126           2129        -3     2217
## # ... with 336,766 more rows, and 12 more variables: sched_arr_time <int>,
## #   arr_delay <dbl>, carrier <chr>, flight <int>, tailnum <chr>,
## #   origin <chr>, dest <chr>, air_time <dbl>, distance <dbl>, hour <dbl>,
## #   minute <dbl>, time_hour <dttm>
```

### 5.3.1


```r
#1
select(flights, dep_time, dep_delay, arr_time, arr_delay)
```

```
## # A tibble: 336,776 × 4
##    dep_time dep_delay arr_time arr_delay
##       <int>     <dbl>    <int>     <dbl>
## 1       517         2      830        11
## 2       533         4      850        20
## 3       542         2      923        33
## 4       544        -1     1004       -18
## 5       554        -6      812       -25
## 6       554        -4      740        12
## 7       555        -5      913        19
## 8       557        -3      709       -14
## 9       557        -3      838        -8
## 10      558        -2      753         8
## # ... with 336,766 more rows
```

```r
select(flights, starts_with("dep"), starts_with("arr"))
```

```
## # A tibble: 336,776 × 4
##    dep_time dep_delay arr_time arr_delay
##       <int>     <dbl>    <int>     <dbl>
## 1       517         2      830        11
## 2       533         4      850        20
## 3       542         2      923        33
## 4       544        -1     1004       -18
## 5       554        -6      812       -25
## 6       554        -4      740        12
## 7       555        -5      913        19
## 8       557        -3      709       -14
## 9       557        -3      838        -8
## 10      558        -2      753         8
## # ... with 336,766 more rows
```

```r
select(flights, matches("^(dep|arr)_(time|delay)"))
```

```
## # A tibble: 336,776 × 4
##    dep_time dep_delay arr_time arr_delay
##       <int>     <dbl>    <int>     <dbl>
## 1       517         2      830        11
## 2       533         4      850        20
## 3       542         2      923        33
## 4       544        -1     1004       -18
## 5       554        -6      812       -25
## 6       554        -4      740        12
## 7       555        -5      913        19
## 8       557        -3      709       -14
## 9       557        -3      838        -8
## 10      558        -2      753         8
## # ... with 336,766 more rows
```

```r
#2
select(flights, dep_time, dep_time, dep_delay) #includes repeated variables only once
```

```
## # A tibble: 336,776 × 2
##    dep_time dep_delay
##       <int>     <dbl>
## 1       517         2
## 2       533         4
## 3       542         2
## 4       544        -1
## 5       554        -6
## 6       554        -4
## 7       555        -5
## 8       557        -3
## 9       557        -3
## 10      558        -2
## # ... with 336,766 more rows
```

```r
#3
vars <- c("year", "month", "day", "dep_delay", "arr_delay")
select(flights, one_of(vars)) # behaves like %in%, but for select
```

```
## # A tibble: 336,776 × 5
##     year month   day dep_delay arr_delay
##    <int> <int> <int>     <dbl>     <dbl>
## 1   2013     1     1         2        11
## 2   2013     1     1         4        20
## 3   2013     1     1         2        33
## 4   2013     1     1        -1       -18
## 5   2013     1     1        -6       -25
## 6   2013     1     1        -4        12
## 7   2013     1     1        -5        19
## 8   2013     1     1        -3       -14
## 9   2013     1     1        -3        -8
## 10  2013     1     1        -2         8
## # ... with 336,766 more rows
```

```r
#4
select(flights, contains("TIME")) #You can include ignore.case=FALSE
```

```
## # A tibble: 336,776 × 6
##    dep_time sched_dep_time arr_time sched_arr_time air_time
##       <int>          <int>    <int>          <int>    <dbl>
## 1       517            515      830            819      227
## 2       533            529      850            830      227
## 3       542            540      923            850      160
## 4       544            545     1004           1022      183
## 5       554            600      812            837      116
## 6       554            558      740            728      150
## 7       555            600      913            854      158
## 8       557            600      709            723       53
## 9       557            600      838            846      140
## 10      558            600      753            745      138
## # ... with 336,766 more rows, and 1 more variables: time_hour <dttm>
```

### 5.5.2 Exercises


```r
flights_sml <- select(flights, year:day, ends_with("delay"), distance, air_time)
#1 
transmute(flights, dep_time, minutes_since_midnight = (dep_time %/% 100) * 60 + dep_time %% 100)
```

```
## # A tibble: 336,776 × 2
##    dep_time minutes_since_midnight
##       <int>                  <dbl>
## 1       517                    317
## 2       533                    333
## 3       542                    342
## 4       544                    344
## 5       554                    354
## 6       554                    354
## 7       555                    355
## 8       557                    357
## 9       557                    357
## 10      558                    358
## # ... with 336,766 more rows
```

```r
#2
transmute(flights, air_time, arr_time - dep_time) # The numbers are different. Different time zones?
```

```
## # A tibble: 336,776 × 2
##    air_time `arr_time - dep_time`
##       <dbl>                 <int>
## 1       227                   313
## 2       227                   317
## 3       160                   381
## 4       183                   460
## 5       116                   258
## 6       150                   186
## 7       158                   358
## 8        53                   152
## 9       140                   281
## 10      138                   195
## # ... with 336,766 more rows
```

```r
transmute(flights, air_time2 = arr_time - dep_time, diff = air_time2 - air_time)
```

```
## # A tibble: 336,776 × 2
##    air_time2  diff
##        <int> <dbl>
## 1        313    86
## 2        317    90
## 3        381   221
## 4        460   277
## 5        258   142
## 6        186    36
## 7        358   200
## 8        152    99
## 9        281   141
## 10       195    57
## # ... with 336,766 more rows
```

```r
#3
transmute(flights, dep_time, sched_dep_time, dep_delay, expect = dep_time - sched_dep_time)
```

```
## # A tibble: 336,776 × 4
##    dep_time sched_dep_time dep_delay expect
##       <int>          <int>     <dbl>  <int>
## 1       517            515         2      2
## 2       533            529         4      4
## 3       542            540         2      2
## 4       544            545        -1     -1
## 5       554            600        -6    -46
## 6       554            558        -4     -4
## 7       555            600        -5    -45
## 8       557            600        -3    -43
## 9       557            600        -3    -43
## 10      558            600        -2    -42
## # ... with 336,766 more rows
```

```r
#We need to convert into minutes, rather than the easily visible time version


#4
test <- transmute(flights, dep_delay, dep_delay_ranked = min_rank(-dep_delay))
arrange(filter(test, dep_delay_ranked < 10), dep_delay)
```

```
## # A tibble: 9 × 2
##   dep_delay dep_delay_ranked
##       <dbl>            <int>
## 1       898                9
## 2       899                8
## 3       911                7
## 4       960                6
## 5      1005                5
## 6      1014                4
## 7      1126                3
## 8      1137                2
## 9      1301                1
```

```r
#5
1:3 + 1:10 #It makes 1:3 repeat until there are 10 units, then adds the two vectors
```

```
## Warning in 1:3 + 1:10: longer object length is not a multiple of shorter
## object length
```

```
##  [1]  2  4  6  5  7  9  8 10 12 11
```

```r
#6
#R includes all of them that I know of (like cos, sin, tan etc)
```

