# Using Job Search Queries to Estimate Traffic Levels

## Project Objective
The purpose of this project was to determine if the number of miles driven could be determined by the number of online job searches.


## Methods
- Data Visualization
- Inferential Statistics

## Technologies
- R

## Project Description
The data used in this study include 164 observations of monthly time series data for the entire
United States and they range from January 2004 to August 2017. Included are the monthly
number of miles driven as the dependent variable. For these data, seasonally adjusted and not
seasonally adjusted data were evaluated. These data, as well as the population data come from
Federal Reserve Economic Data (FRED) which is maintained by the Federal Reserve Bank of St.
1Louis. Retail fuel prices come from the U.S. Energy Information Agency (EIA) and are
expressed in US Dollars per gallon including taxes. The prices of conventional gasoline,
reformed gasoline, and number 2 diesel were evaluated as separate variables. Weather data come
from the National Centers for Environmental Information of the National Oceanic and
Atmospheric Administration. These data measure the number of weather anomalies that have
occurred for a given month. An anomaly is defined as any day with weather that is in the top or
bottom 10 percent of the historical distribution. With respect to the data used for this study, the
top 10 percent includes “very warm” and/or “very wet” days while the bottom 10 percent
includes days that are classified as “very cold” and/or “very dry”. Jobs data are from Google
Trends and are the aggregates of what Google refers to as “interest over time”. Numbers reflect a
level of interest relative to the greatest and lowest levels of interest over a given timeframe.
Interest is expressed when individuals enter search terms at the search bar. For this study, queries
used in the aggregate figure include, but are not limited to the following: “jobs”,
“unemployment”, and “job listings”.

Linear regressions are used to determine the number of miles driven. And, the results of the preferred model follow:

The variables are significant at the 99.0 percent level with the exception of the variable for very
warm days which is significant at the 90.0 percent level. Its inclusion resulted in the adjusted R^2
increasing to 0.8410 from Model (3) (0.8382). The warm days variable explains some variation
in the number of miles driven due to weather and its inclusion is in line with theory. The number
of miles driven is expected to increase slightly as employees must go to work on these days and
driving due to recreation such as travelling to an ocean or to a lake is likely to increase. This is
reflected in both sign and magnitude of the coefficient (2.133). The sign and the greater
magnitude for very cold days (-6.453) is likely due to an inability to drive due to snow and/or a
lack of desire to leave the house. The coefficient for job searches is -0.0436. The model
estimates that, on average, the number of miles driven each month decreases by 0.44 billion, all
other things remaining equal. And, because the jobs variable is significant at the 99.0 percent
level, the null hypothesis that β=0 is rejected. The number of online jobs searches, therefore is
significant enough to include in this model for the number of miles driven. Chart 1 shows the
actual versus fitted values for the model.


## Getting Started
1. Clone this repo.
2. Raw data are kept in /assets
