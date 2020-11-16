# bikesharing-factors-evaluation
Seminar paper analyzing factors driving bikesharing demand in Hamburg. 

I created this analysis in a Computational Social Science Seminar. It uses a dataset of bikesharing trips with  start and stop station
in Hamburg from "Call A Bike", a station based bikesharing system from Deutsche Bahn AG. The dataset [1] is
available [here](https://data.deutschebahn.com/dataset/data-call-a-bike).

The README gives a short overview over the project, the detailed description can be found in "reporting/2020_06_seminar_paper_css_classen".


## Requirements
The project was conducted in R using the following packages, requiring an R Version > 3.4: 

```R
data.table
tidyverse
lubridate
sp
sf
ggplot2
leaflet
```

## Analysis
The project uses a negative binomial regression with zero-inflation to account for the large number of zero-trips in the bikesharing
network (> 90 % of all station combinations had zero trips). Model estimation can be found in `04_analysis.R`.

The target variable is number of trips between station-pairs per day.

The input features are:
- Number of Points of Interest (POI) of different categories within a radius of 400 m of origin (O) and destination (D) station, categories are leisure, transport, eudcaiton, health
- Distance to closest POI for O and D
- Number of other stations within a radius of 400 m of O and D, shortest distance to closest station
- Distance to city center for O and D
- Percentage of landuse type around 400 m of O and D
- Percentage cycle way on shortest path
- Trip length in minutes (based on estimated shortest path)

The results showed decrease in bikesharing trips with increased trip length and smaller population, as well as a positive impact of percentage of cycle ways on demand.

