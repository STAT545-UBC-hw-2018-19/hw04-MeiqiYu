hw04-Tidy data and joins
================

# Introduction

The goal of this homework is to solidify data wrangling skills by
working some realistic problems in the grey area between data
aggregation and data reshaping.

For this assignment, we have been tasked with two prompts. The first
will be a data reshaping prompt while the second will be a data joining
prompt.

``` r
suppressPackageStartupMessages(library(tidyverse))
library(knitr)
library(gapminder)
```

# Data Reshaping (Activity \#2)

**Problem**: Make a tibble with one row per year and columns for life
expectancy for two or more countries.

**Solution**: Let’s make a table comparing life expectancies between
Canada and Italy over the years sampled in the `gapminder` dataset.
Specifically, we want each row to represent a year and have a column for
each country that records life expectancy in that year.

``` r
table1 <- gapminder %>% 
  filter(country == "Canada" | country == "Italy") %>% 
  select(year,country,lifeExp) %>% 
  spread(key=country, value=lifeExp) 
kable(table1, caption = "LifeExp in Canada vs Italy",
      table.placement = "tbp", 
      caption.placement = "top", # table and title position
      col.names = c(" Year ", " LifeExp_Canada ", " LifeExp_Italy ")) # add title, edit columns names
```

| Year | LifeExp\_Canada | LifeExp\_Italy |
| :--: | :-------------: | :------------: |
| 1952 |     68.750      |     65.940     |
| 1957 |     69.960      |     67.810     |
| 1962 |     71.300      |     69.240     |
| 1967 |     72.130      |     71.060     |
| 1972 |     72.880      |     72.190     |
| 1977 |     74.210      |     73.480     |
| 1982 |     75.760      |     74.980     |
| 1987 |     76.860      |     76.420     |
| 1992 |     77.950      |     77.440     |
| 1997 |     78.610      |     78.820     |
| 2002 |     79.770      |     80.240     |
| 2007 |     80.653      |     80.546     |

LifeExp in Canada vs Italy

From the table above, we get the lifeExp for Canada and France. Now,
let’s make a plot of life expectancy for Canada against France which
will be visually appealing.

``` r
table1 %>% 
  ggplot(aes(year,Canada/Italy))+
  geom_point(size=2)+
  geom_line(color="red",size=1)+
  geom_text(aes(label = year),hjust = 0.5, vjust = -0.5)+
  # adjust the position of marks
  labs(title="Life Expectancy in Canada vs Italy",x = "Year", y = "LifeExp_Canada / LifeExp_Italy")+
  # add title, lable x and y axis
  theme(plot.title = element_text(hjust = 0.5)) #center the title
```

![](hw004-MeiqiYu_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

From the graph above, it’s clear that Canada has a higher life
expectancy over Italy except in 1997 and 2002. And the difference
between the life expectancies of these two countries is going to be
smaller and smaller.

# Data Joining (Activity \#1)

**Problem**: Create a second data frame, complementary to Gapminder.
Join this with (part of) Gapminder using a dplyr join function and make
some observations about the process and result. Explore the different
types of joins.

**Solution**: Let’s use `LifeCycleSavings` which is a built-in data
frame in r as the second data frame. `LifeCycleSavings` is a dataset
which contains 50 observations on 5 variables.

| Variable |            Description            |
| -------- | :-------------------------------: |
| sr       |    aggregate personal savings     |
| pop15    |     % of population under 15      |
| pop75    |      % of population over 75      |
| dpi      | real per-capita disposable income |
| ddpi     |        growth rate of dpi         |

Let’s take a look at this dataset and select two varibles from it.The
new dataset we get is called `mytable`.

``` r
mytable <- LifeCycleSavings %>% 
  select(sr,dpi) %>% 
  rownames_to_column("country") %>% 
# convert the row names into first column
  mutate_if(is.factor, as.character)

kable(mytable)
```

| country        |    sr |     dpi |
| :------------- | ----: | ------: |
| Australia      | 11.43 | 2329.68 |
| Austria        | 12.07 | 1507.99 |
| Belgium        | 13.17 | 2108.47 |
| Bolivia        |  5.75 |  189.13 |
| Brazil         | 12.88 |  728.47 |
| Canada         |  8.79 | 2982.88 |
| Chile          |  0.60 |  662.86 |
| China          | 11.90 |  289.52 |
| Colombia       |  4.98 |  276.65 |
| Costa Rica     | 10.78 |  471.24 |
| Denmark        | 16.85 | 2496.53 |
| Ecuador        |  3.59 |  287.77 |
| Finland        | 11.24 | 1681.25 |
| France         | 12.64 | 2213.82 |
| Germany        | 12.55 | 2457.12 |
| Greece         | 10.67 |  870.85 |
| Guatamala      |  3.01 |  289.71 |
| Honduras       |  7.70 |  232.44 |
| Iceland        |  1.27 | 1900.10 |
| India          |  9.00 |   88.94 |
| Ireland        | 11.34 | 1139.95 |
| Italy          | 14.28 | 1390.00 |
| Japan          | 21.10 | 1257.28 |
| Korea          |  3.98 |  207.68 |
| Luxembourg     | 10.35 | 2449.39 |
| Malta          | 15.48 |  601.05 |
| Norway         | 10.25 | 2231.03 |
| Netherlands    | 14.65 | 1740.70 |
| New Zealand    | 10.67 | 1487.52 |
| Nicaragua      |  7.30 |  325.54 |
| Panama         |  4.44 |  568.56 |
| Paraguay       |  2.02 |  220.56 |
| Peru           | 12.70 |  400.06 |
| Philippines    | 12.78 |  152.01 |
| Portugal       | 12.49 |  579.51 |
| South Africa   | 11.14 |  651.11 |
| South Rhodesia | 13.30 |  250.96 |
| Spain          | 11.77 |  768.79 |
| Sweden         |  6.86 | 3299.49 |
| Switzerland    | 14.13 | 2630.96 |
| Turkey         |  5.13 |  389.66 |
| Tunisia        |  2.81 |  249.87 |
| United Kingdom |  7.81 | 1813.93 |
| United States  |  7.56 | 4001.89 |
| Venezuela      |  9.22 |  813.39 |
| Zambia         | 18.56 |  138.33 |
| Jamaica        |  7.72 |  380.47 |
| Uruguay        |  9.24 |  766.54 |
| Libya          |  8.89 |  123.58 |
| Malaysia       |  4.71 |  242.69 |

## mutating join

### left\_join and right\_join

Let’s use left\_join and right\_join seperately to join part of
gapminder(4 columns, 142 rows) and mytable(3 columns, 50 rows) together.

``` r
gapminder %>% 
  filter(year == 1967) %>% 
  select(country,continent,year,gdpPercap) %>%  #add a restriction to get a smaller data set %>% 
  left_join(mytable,by=c("country")) %>% 
  head() %>% # the table is too long so I just present some rows here
  kable()
```

    ## Warning: Column `country` joining factor and character vector, coercing
    ## into character vector

| country     | continent | year |  gdpPercap |    sr |     dpi |
| :---------- | :-------- | ---: | ---------: | ----: | ------: |
| Afghanistan | Asia      | 1967 |   836.1971 |    NA |      NA |
| Albania     | Europe    | 1967 |  2760.1969 |    NA |      NA |
| Algeria     | Africa    | 1967 |  3246.9918 |    NA |      NA |
| Angola      | Africa    | 1967 |  5522.7764 |    NA |      NA |
| Argentina   | Americas  | 1967 |  8052.9530 |    NA |      NA |
| Australia   | Oceania   | 1967 | 14526.1246 | 11.43 | 2329.68 |

``` r
gapminder %>% 
  filter(year == 1967) %>% 
  select(country,continent,year,gdpPercap) %>% #add a restriction to get a smaller data set
  right_join(mytable,by=c("country")) %>% 
  head() %>% # the table is too long so I just present some rows here
  kable()
```

    ## Warning: Column `country` joining factor and character vector, coercing
    ## into character vector

| country   | continent | year | gdpPercap |    sr |     dpi |
| :-------- | :-------- | ---: | --------: | ----: | ------: |
| Australia | Oceania   | 1967 | 14526.125 | 11.43 | 2329.68 |
| Austria   | Europe    | 1967 | 12834.602 | 12.07 | 1507.99 |
| Belgium   | Europe    | 1967 | 13149.041 | 13.17 | 2108.47 |
| Bolivia   | Americas  | 1967 |  2586.886 |  5.75 |  189.13 |
| Brazil    | Americas  | 1967 |  3429.864 | 12.88 |  728.47 |
| Canada    | Americas  | 1967 | 16076.588 |  8.79 | 2982.88 |

  - `left_join(x,y)` return all rows from x. Join matching rows from `y`
    to `x`. Rows in x with no match in y will show NA in the new
    columns. As we only have 50 rows in `mytable` and while 142 rows in
    the selected part of gapminder, NA is displayed to represent some
    unavailable data. The final table contains 142 rows.

  - `right_join(x,y)` return all rows from y. Join matching rows from
    `x` to `y`. Rows in y with no match in x will show NA in the new
    columns. Similarly, we get some NA due to lack of information in the
    selected part of gapminder. The final table contains 50 rows.

### inner\_join

If we only want those completed rows, we can drop those with NA by
inner\_join.

``` r
gapminder %>% 
  filter(year == 1967) %>% 
  select(country,continent,year,gdpPercap) %>% #add a restriction to get a smaller data set
  inner_join(mytable,by=c("country")) %>% 
  head() %>% # the table is too long so I just present some rows here
  kable()
```

    ## Warning: Column `country` joining factor and character vector, coercing
    ## into character vector

| country   | continent | year | gdpPercap |    sr |     dpi |
| :-------- | :-------- | ---: | --------: | ----: | ------: |
| Australia | Oceania   | 1967 | 14526.125 | 11.43 | 2329.68 |
| Austria   | Europe    | 1967 | 12834.602 | 12.07 | 1507.99 |
| Belgium   | Europe    | 1967 | 13149.041 | 13.17 | 2108.47 |
| Bolivia   | Americas  | 1967 |  2586.886 |  5.75 |  189.13 |
| Brazil    | Americas  | 1967 |  3429.864 | 12.88 |  728.47 |
| Canada    | Americas  | 1967 | 16076.588 |  8.79 | 2982.88 |

  - `inner_join(x,y)` retain only rows in both sets. The final table
    contains 45 rows.

### full\_join

If we want all the rows from both data set, we can use full\_join.

``` r
gapminder %>% 
  filter(year == 1967) %>% 
  select(country,continent,year,gdpPercap) %>% #add a restriction to get a smaller data set
  full_join(mytable,by=c("country")) %>% 
  head() %>% # the table is too long so I just present some rows here
  kable()
```

    ## Warning: Column `country` joining factor and character vector, coercing
    ## into character vector

| country     | continent | year |  gdpPercap |    sr |     dpi |
| :---------- | :-------- | ---: | ---------: | ----: | ------: |
| Afghanistan | Asia      | 1967 |   836.1971 |    NA |      NA |
| Albania     | Europe    | 1967 |  2760.1969 |    NA |      NA |
| Algeria     | Africa    | 1967 |  3246.9918 |    NA |      NA |
| Angola      | Africa    | 1967 |  5522.7764 |    NA |      NA |
| Argentina   | Americas  | 1967 |  8052.9530 |    NA |      NA |
| Australia   | Oceania   | 1967 | 14526.1246 | 11.43 | 2329.68 |

  - `full_join(x,y)` return all rows and columns from both x and y. For
    the unmatching values,it returns NA for the missing one. The final
    table contains 147 rows.

## filtering join

### semi\_join

If we only want the data from the gapminder which matches data from
mytable, semi\_join is a good choice which not show any information from
mytable.

``` r
gapminder %>% 
  filter(year == 1967) %>% 
  select(country,continent,year,gdpPercap) %>% #add a restriction to get a smaller data set
  semi_join(mytable,by=c("country")) %>% 
  head() %>% 
  kable()
```

    ## Warning: Column `country` joining factor and character vector, coercing
    ## into character vector

| country   | continent | year | gdpPercap |
| :-------- | :-------- | ---: | --------: |
| Australia | Oceania   | 1967 | 14526.125 |
| Austria   | Europe    | 1967 | 12834.602 |
| Belgium   | Europe    | 1967 | 13149.041 |
| Bolivia   | Americas  | 1967 |  2586.886 |
| Brazil    | Americas  | 1967 |  3429.864 |
| Canada    | Americas  | 1967 | 16076.588 |

  - `semi_join(x,y)`return only columns from x which have matching
    values in y. A semi join differs from an inner join because an inner
    join returns the columns from both tables, while an semi join only
    return columns from x. The final table contains 45 rows and 4
    columns while the table resulted from inner join contains 45 rows
    and 6 columns.

### anti\_join

In some cases, we want to see all the data from the part of gapminder
which do not have a match in mytable, anti join is a good choice.

``` r
gapminder %>% 
  filter(year == 1967) %>% 
  select(country,continent,year,gdpPercap) %>% #add a restriction to get a smaller data set
  anti_join(mytable,by=c("country")) %>% 
  head() %>% 
  kable()
```

    ## Warning: Column `country` joining factor and character vector, coercing
    ## into character vector

| country           | continent    |    year |                                        gdpPercap |
| :---------------- | :----------- | ------: | -----------------------------------------------: |
| Afghanistan       | Asia         |    1967 |                                         836.1971 |
| Albania           | Europe       |    1967 |                                        2760.1969 |
| Algeria           | Africa       |    1967 |                                        3246.9918 |
| Angola            | Africa       |    1967 |                                        5522.7764 |
| Argentina         | Americas     |    1967 |                                        8052.9530 |
| Bahrain           | Asia         |    1967 |                                       14804.6727 |
| \* \`anti\_join(x | ,y)\` return | all row | s from x which do not have matching values in y. |
| The final tabl    | e contains 9 | 7 rows. |                                                  |
