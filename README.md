# tuichartr

> Htmlwidget to create interactive visualisations with JavaScript library [tui-chart](https://github.com/nhn/tui.chart)

[![Travis build status](https://travis-ci.org/dreamRs/tuichartr.svg?branch=master)](https://travis-ci.org/dreamRs/tuichartr)
[![Project Status: WIP – Initial development is in progress, but there has not yet been a stable, usable release suitable for the public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)



## Installation

You can install from GitHub with:

``` r
remotes::install_github("dreamRs/tuichartr")
```

## Charts

You can create various type of chart : bar, column, line, area, boxplot, scatter, heatmap, treemap, radar, pie.

``` r
library(gapminder)
library(tuichartr)

# Datas
n_countries <- gapminder %>% 
  filter(year == 2007) %>% 
  count(continent, sort = TRUE)

# Chart
tuichart("bar") %>% 
  add_data(n_countries, aes(x = continent, y = n)) %>% 
  tui_chart(title = "Countries by continent in 2007") %>% 
  tui_xAxis(title = "Number of countries") %>% 
  tui_legend(visible = FALSE) %>% 
  tui_series(showLabel = TRUE)
```

![](man/figures/tuichart-example.png)


## Maps

You can also make maps :

```r
# Retrieve Italy polygons
uk <- ne_states("united kingdom", returnclass = "sf")
# add a random numeric variable
uk$random <- sample(1:100, nrow(uk), TRUE)

# draw map
tuimap() %>% 
  add_map_data(uk, aes(code = adm1_code, label = name, value = random)) %>% 
  tui_chart(title = "UK map")
```

![](man/figures/tuimap-example.png)


