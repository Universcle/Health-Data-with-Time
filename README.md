# Health-Data-with-Time(user interactive)
Provide users with functions to handle time data<br />
The __R functions__ are based on __Apple health data__<br />
Which is downloaded from __Simple Health Export CSV__ app<br />

## Time core code(point)
__So far focus on sleeptime data__<br />
use _sleeptime()_ to remove duplicates<br />
use _overall(df)_ to see the trend by time<br />
use _specific(df, num)_ to see each specific year<br />

## Time core code(period)
__Process data that: (startDate, endDate, unit, value)__<br />
use _df <- preprocess()_ to generate a processed dataframe<br />
use _basicinfo_y(df)_ to show basic info of each year<br />
use _basicinfo_m(df, num)_ to show basic info of each month<br />
use _sub <- newsubset(df)_ to subset specific time period data<br />

## Provide grapical examples using ggplot2
__Example 1-3 are:__<br />
_Stripchart, Boxplot, Violin plot_<br>
__Example 4-6 are:__<br />
_Histogram, Scatter plot, Density curve_<br>

## Further data internal relationship
_Hypothesis test like z test, f test, t test_<br />
_Linear Regression & Correlation_<br />

## Provide powerpoint for better understanding
just simply download csv and put into health_data file<br />
then you can enjoy preprocess data without coding<br />
