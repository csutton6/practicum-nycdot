# Data wrangling / Exploratory analysis


## Trip Data

### Part 1 Data Cleaning

There are three types of data that needed to be clean. 

The first type is that the tracks are outside of New York City, and they are not connect to the NYC. Like Figure 1, which is March 5th, there are tracks in Montreal.<br>
<img src="https://github.com/kateesutt/practicum-nycdot/blob/main/images/type1_March5th.png" width="80%" height="40%">

Figure 1. Tracks in March 5th, 2018.

The other type is that the tracks are outside of New York City, and they are connected to the NYC. Like Figure 2, the tracks fly outside of USA.<br>
<img src="https://github.com/kateesutt/practicum-nycdot/blob/main/images/type2_March25th.png" width="80%" height="40%">

Figure 2. Tracks in March 25th, 2018.

The first type of data could be cleaned by using clip. We clip the trip line strings within the NYC boundary. For the second type, we at first buffered the NYC boundary, then do the clip, and we also find out the points which intersect with the buffered boundary. To illustrate, we need to buffer the NYC boundary because some of tracks are close to shorelines. If the shape file of boundary is not buffered, these tracks will not be included. Then we select out the linestrings which intersect with the buffered boundary. 

#### The third type is more complicated. <br>
The trips is inside NYC, however, when using mapview to visualize the basemap, we find that these tracks are not aligned with existing roads. 

<img src="https://github.com/kateesutt/practicum-nycdot/blob/main/images/Type3_0705_1.png" width="80%" height="40%">

Figure 3. July 5th, 2018.

When we zoom in, we found a lot of similar issues. This is a snapshot of July, 5th.<br>

<img src="https://github.com/kateesutt/practicum-nycdot/blob/main/images/Type3_0705_2.png" width="80%" height="40%"><br>

Even when we use more fine-grained boundary, like shorlines of NYC to clip the data, it is still hard for R to tell whether a specific track is valid.<br>

<img src="https://github.com/kateesutt/practicum-nycdot/blob/main/images/Type3_0705_3.png" width="80%" height="40%"><br>
#### QUESTION: how to clean wrong tracks fall within the NYC boundary?
<br>
### Part 2 Spatial and Temporal Visualizations
#### Ridership Trend in 2018

<img src="https://github.com/kateesutt/practicum-nycdot/blob/main/images/hourly_T.png" ><br>

The ridership by 24-hours, 2018

As you can see from the figure above, the trend within a day, 24 hours, remains similar for all 10 months. The morning peak appreas around 12:00 pm and the evening peak appears around 9:00 pm. Ridership decrease in midnight and early morning, the lowest ridership appears in 7:30 am.

<img src="https://github.com/kateesutt/practicum-nycdot/blob/main/images/weekly_T.png"><br>

The ridership by day of the week, 2018

Surprisingly, the peak of the ridership does not appears during weekends.The ridership trend by day of the week varies between different months. 

<img src="https://github.com/kateesutt/practicum-nycdot/blob/main/images/monthly_T2.png"><br>

The ridership by month, 2018

No obvious trends/patterns

#### Ridership Trend in July, 2018

The weather and temperature in July is most suitable for cycling. Therefore we focus on ridership in July and see the biking trend within the month.

<img src="https://github.com/kateesutt/practicum-nycdot/blob/main/images/trendInJuly.png"><br>

There is clearly a daily periodicity
