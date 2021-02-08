# Data wrangling / Exploratory analysis


## Trip Data

### Data Cleaning

There are three types of data that needed to be clean. 

The first type is that the tracks are outside of New York City, and they are not connect to the NYC. Like Figure 1, which is March 5th, there are tracks in Montreal.
<img src="https://github.com/kateesutt/practicum-nycdot/blob/main/images/type1_March5th.png" width="80%" height="40%">

Figure 1. Tracks in March 5th, 2018.

The other type is that the tracks are outside of New York City, and they are connected to the NYC. Like Figure 2, the tracks fly outside of USA.
<img src="https://github.com/kateesutt/practicum-nycdot/blob/main/images/type2_March25th.png" width="80%" height="40%">

Figure 2. Tracks in March 25th, 2018.

The first type of data could be cleaned by using clip. We clip the trip line strings within the NYC boundary. For the second type, we at first buffered the NYC boundary, then do the clip, and we also find out the points which intersect with the buffered boundary. To illustrate, we need to buffer the NYC boundary because some of tracks are close to shorelines. If the shape file of boundary is not buffered, these tracks will not be included. Then we select out the linestrings which intersect with the buffered boundary. 

#### The third type is more complicated. 
The trips is inside NYC, however, when using mapview to visualize the basemap, we find that these tracks are not aligned with existing roads. 
<img src="https://github.com/kateesutt/practicum-nycdot/blob/main/images/Type3_0705_1.png" width="80%" height="40%">
Figure 3. July 5th, 2018.
When we zoom in, we found a lot of similar issues. This is a snapshot of July, 5th.
<img src="https://github.com/kateesutt/practicum-nycdot/blob/main/images/Type3_0705_2.png" width="80%" height="40%">
<img src="https://github.com/kateesutt/practicum-nycdot/blob/main/images/Type3_0705_3.png" width="80%" height="40%">
### Spatial and Temporal Visualizations

