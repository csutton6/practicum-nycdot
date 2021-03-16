## Modeling

<br>

Q:
1)Modeling strategy <br>
2)What is your plan for generalizability

### Treatment and Control Groups

**Lower Manhattan** <br>
*Motivation*: 7th Avenue was the longest addition of new protected bikelane in 2018. 
3rd Avenue was chosen as the control because it was the only avenue without a bike lane in this area in 2018.
|Group Type|Street|Street Direction|Bike Lane Type|Date Added|Pre Period|Post Period|
|----------|------|----------------|--------------|----------|----------|-----------|
|Treatment|7th Avenue|Southbound|Protected|June 31 2018|May & June 2018|July & August 2018|
|Control|3rd Avenue|Northbound|No Bike Lane|

![lower manhattan map](images/downtown_chosenlanes_1.png)

**East Harlem** <br>
*Motivation*: Four streets in East Harlem all got new bike lanes in 2018. This area is lower density than Chelsea and may generalize better to the rest of NYC. Control streets are other side streets in the neighborhood. 
|Group Type|Street|Street Direction|Bike Lane Type|Date Added|Pre Period|Post Period|
|----------|------|----------------|--------------|----------|----------|-----------|
|Treatment|E 128th St|Eastbound|Unprotected|Sep 31 2018|Aug & Sep 2018|Oct & Nov 2018|
|Treatment|E 126th St|Westbound|Unrpotected|Sep 31 2018|Aug & Sep 2018|Oct & Nov 2018|
|Treatment|E 110th St|Eastbound|Unrpotected|Sep 31 2018|Aug & Sep 2018|Oct & Nov 2018|
|Treatment|E 111th St|Westbound|Unrpotected|Sep 31 2018|Aug & Sep 2018|Oct & Nov 2018|
|Control|E 109th St|Westbound|No Bike Lane|
|Control|E 112th St|Eastbound|No Bike Lane|
|Control|E 127th St|Westbound|No Bike Lane|
|Control|E 124th St|Eastbound|No Bike Lane|

![east harlem map](images/eharlem_chosenlanes_1.png)

### Difference in Difference

<br>

#### East Harlem


|         | Pre   |  Post   |   Difference  |
|---------|------:|--------:|--------------:|
|Treatment|  325  |    405  |            	80|
|Control  |  309  |    307  |            -2 |
|Difference|	-16 |     -98 |            -82|


<img src="https://github.com/kateesutt/practicum-nycdot/blob/main/images/eh_diff.jpeg">

#### Lower Manhattan

<img src="https://github.com/kateesutt/practicum-nycdot/blob/main/images/diff%20in%20diff.png">

|         | Pre   |  Post   |   Difference  |
|---------|------:|--------:|--------------:|
|Treatment|  4729  |    7046  |            	2317|
|Control  |  1809  |    2679  |              870|
|Difference|	2920 |     4367 |             1447|



<br>
<br>

### Difference in Difference over year


#### East Harlem

Using Sep18 and Sep19 as comparison:


|         | Pre   |  Post   |   Difference  |
|---------|------:|--------:|--------------:|
|Treatment|  178  |    332  |            154|
|Control  |  152  |    407  |            255|
|Difference|	-26 |      75 |            101|

<br>
<img src="https://github.com/kateesutt/practicum-nycdot/blob/main/images/eh_Diff1819.jpeg">



### Coefficients between the number of trips and whether there is a bikelanes

<br>

Steps:

<br>
1)Aggregate monthly trip data into one
<br>
2)Slice the aggregated data into borough: Manhattan, Bronx, Brooklyn, Queens, Staten Island
<br> Inside Staten Island, there is no bike trips from March to July
<br>
3)We create a new feature called bikeland. Also, **we assume that if the bike track intersects with the bikelanes, the bike trip passes through/uses/is benefited from the bikelanes**. Therefore, if the bike track intersect with bikelanes, the value of column bikelane will be "yes", and vise versa.
<br>


```r
March2July_M <- March2July_M %>%
  mutate(bikelane = ifelse(id %in% M_inbike$id, "yes", "no"))
```

<br>

#### March to July

<br>

**Manhattan**

<br>

```r
reg_M <- lm(count ~ bikelane, data=March2July_M)
```

<br>


<table style="text-align:center"><caption><strong>Regression results</strong></caption>
<tr><td colspan="2" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left"></td><td><em>Dependent variable:</em></td></tr>
<tr><td></td><td colspan="1" style="border-bottom: 1px solid black"></td></tr>
<tr><td style="text-align:left"></td><td>count</td></tr>
<tr><td colspan="2" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left">bikelaneyes</td><td>-0.000 (0.000)</td></tr>
<tr><td style="text-align:left">Constant</td><td>1.000<sup>***</sup> (0.000)</td></tr>
<tr><td colspan="2" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left">Observations</td><td>176,232</td></tr>
<tr><td style="text-align:left">R<sup>2</sup></td><td>0.500</td></tr>
<tr><td style="text-align:left">Adjusted R<sup>2</sup></td><td>0.500</td></tr>
<tr><td style="text-align:left">Residual Std. Error</td><td>0.000 (df = 176230)</td></tr>
<tr><td style="text-align:left">F Statistic</td><td>176,230.000<sup>***</sup> (df = 1; 176230)</td></tr>
<tr><td colspan="2" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left"><em>Note:</em></td><td style="text-align:right"><sup>*</sup>p<0.1; <sup>**</sup>p<0.05; <sup>***</sup>p<0.01</td></tr>
</table>



<br>
<br>
Questions:
1) For now, like the new lanes added in 2018c, we assume the lane is added at Sep 2018, and then use the Aug, Sep, Oct, and Nov to do previous and post comparison. We are wondering is the assumption acceptable? Or?
2) Would our controls be ok?
3) Question about normalizing data
4) BoxR-->cannot read shp file

