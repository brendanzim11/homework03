homework03
================
Brendan Zimmer
10/5/2021

\#For my project I decided to look at visibility, season (month), and
then split them by airport leaving from as my third pattern. I created
some visulization to show when the best time and month would be to have
the least amount of delay. What I got out of this was that you would
want to leave from EWR at 0700 in September because the mean visibility
is highest in this month. But the only contradiction is that the
visibility was not the highest in this hour. Still though, I believe my
data still give a compeling enough argument to have September at 0700
from EWR to be the best for the least amount of delay.

\#This shows the lowest amount of delay and actaully shows planes
arriving early. This is seperated by hour so there are multiple hours of
each airport.

``` r
delay_per_airline <- flights %>% 
  group_by(carrier, hour, origin) %>% 
  summarise(mean_arr_delay = mean(arr_delay, na.rm = T),
            mean_dep_delay = mean(dep_delay, na.rm = T)) %>% 
  ungroup() %>% 
  arrange(mean_arr_delay)
```

    ## `summarise()` has grouped output by 'carrier', 'hour'. You can override using the `.groups` argument.

``` r
head(delay_per_airline)
```

    ## # A tibble: 6 x 5
    ##   carrier  hour origin mean_arr_delay mean_dep_delay
    ##   <chr>   <dbl> <chr>           <dbl>          <dbl>
    ## 1 VX         12 JFK             -36.3          2.86 
    ## 2 DL         22 JFK             -26           -3    
    ## 3 9E         21 JFK             -24           -2    
    ## 4 F9          7 LGA             -22           -4    
    ## 5 VX          7 EWR             -19.0          0.714
    ## 6 VX         11 JFK             -18.5          1.62

\#This is the mean arrival delay against hour seperated by airport. This
shows that there is a small difference with EWR having a longer delay in
15-20 hours which you would want to avoid. But with the lowest delay
just by a bit in EWR this would be best time to go at 0700 from there.

``` r
ggplot(delay_per_airline, aes(hour, mean_arr_delay)) +
  geom_bar(stat = "identity") +
  facet_wrap(~origin)
```

    ## Warning: Removed 1 rows containing missing values (position_stack).

![](README_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

\#The head of a table showing the mean arrival delay by month with its
origin and mean visibility. This data table was helpful in showing me
how visibility correlated with mean delay and compare the months and
origin.

``` r
season_delay <- flights %>% 
  group_by(month, origin) %>% 
  summarise(mean_delay = mean(arr_delay, na.rm = T)) %>% 
  left_join(weather3) %>% 
  arrange(mean_delay)
```

    ## `summarise()` has grouped output by 'month'. You can override using the `.groups` argument.

    ## Joining, by = c("month", "origin")

``` r
head(season_delay)
```

    ## # A tibble: 6 x 4
    ## # Groups:   month [3]
    ##   month origin mean_delay mean_vis
    ##   <int> <chr>       <dbl>    <dbl>
    ## 1     9 EWR        -4.73      9.75
    ## 2     9 JFK        -4.46      9.56
    ## 3    10 JFK        -3.59      9.49
    ## 4     9 LGA        -2.83      9.68
    ## 5    11 JFK        -0.873     9.49
    ## 6    10 LGA         0.186     9.58

\#This is the visulization of the table above, but more throughly
seperated. Again this shows the best month to leave is in September (9)
from EWR with JFK right behind it. But JFK also having the highest mean
delay in June (6) suprisingly. The pattern in this graph showed me that
September is the best month to go and there is usually a jump in delay
between 5 and 7.

``` r
ggplot(season_delay, aes(month, mean_delay)) +
  geom_bar(stat = "identity") +
  facet_wrap(~origin)
```

![](README_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

\#This graph displays the mean visibility per month by airport. This
graph did not show much correlation for anything, but it is helpful
seeing which months have the worst (smallest visibility) by airport. It
did show the pattern there was between each month as each airport had
almost the same exact graph.

``` r
ggplot(season_delay, aes(month, mean_vis)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~origin)
```

![](README_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

\#this is the mean visibility by hour to help corollate between the
average delay per hour and aiport. Oddly enough EWR at one of its lowest
visibility averages has the least amount of delay as you can see from my
graph with delay average vs hour. This could mean there is an importance
to that airport and that visibility may not be as strong as a factor for
EWR.

``` r
ggplot(delay_per_hour, aes(hour, mean_vis)) +
  geom_bar(stat = "identity") +
  facet_wrap(~origin)
```

![](README_files/figure-gfm/unnamed-chunk-11-1.png)<!-- --> \#Finally
this visual shows the positive correlation between mean arrival delay
and mean visibility. There is a positive correlation bewtween each
airporst, but EWR has the great one. This again helps prove my point
from my earlier statement that EWR is the best place to flight out from.

``` r
ggplot(delay_per_hour, aes(mean_vis, mean_arr_delay, color = origin)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~origin)
```

    ## `geom_smooth()` using formula 'y ~ x'

    ## Warning: Removed 1 rows containing non-finite values (stat_smooth).

    ## Warning: Removed 1 rows containing missing values (geom_point).

![](README_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

\#At the very end I also wanted to add in the wind speed average to see
if there would be any time of correlation between that and delay. What I
found was that there was a smaller positive correlation between
windspeed and delay. LGA had the biggest positive which would mean that
wind speed has the biggest affect on delay for the airport compared to
the others. As for EWR, I would say that it is not as important as
visibility.

``` r
ggplot(delay_per_hour, aes(mean_wind_speed, mean_arr_delay, color = origin)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~origin)
```

    ## `geom_smooth()` using formula 'y ~ x'

    ## Warning: Removed 1 rows containing non-finite values (stat_smooth).

    ## Warning: Removed 1 rows containing missing values (geom_point).

![](README_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->
