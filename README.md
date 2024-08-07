# R_Midterm_Project

Ahmed Mokhtar

Below is the basic flow of my project:

1. Start by performing very simple structural fixes to the datasets.
  This is done so that the EDA can be useful, and yield comprehensive results.
  
2. Perform an EDA on all 3 datasets
 This EDA will be done in order to provide myself and the reader a brief introduction
 to the data at hand. Results from this may influence the cleaning that will be performed 
 in the next step. 

3. Start Cleaning
 This step involves going through each dataset meticulously and making sure everything
 is in the right format. Throughout the project, I ended up coming back to this step and
 doing further cleaning as needed.

4. Do the Rush Hour analysis
 In order to this, I utilized a series of steps that can be viewed in the Rscript. This 
 step is essential since it will allows us to figure out what stations are most frequently 
 visited during rush hours.

5. Weekday Station Frequencies analysis
  In this step, I will start by determining the most commonly visited stations
  as starting and end points during rush hours on weekdays. This analysis was fairly 
  complex. I did end up splitting it into morning and evening rush hours, since I 
  thought it would be more insightful. Once this was done, I also research the
  grid and gridExtra functions in order to export the results nicely.

6. Weekend Station Frequency Analysis
 This was a fairly simple step. I first filtered the data based on weekend days, and followed
 a similar process to the one in the weekdays. The data was exported in a similar way. 

7. Average Utilization
 In this step, the average utilization rate was calculated. I did opt to go for the 
 overall rate, instead of per bike. Reasoning can be found in the report. The grid 
 package was used again to export data.

8. Weather Analysis
 Next we have to do an analysis to test the correlation between some weather factors
 and bike use indicators. In order to this, a lot of data joining had to be done between 
 the three datasets. Following this, the matrix was created with duration and number of trips
 as the two indicators for bike use. The grid package was used once again to show a table of the 
 correlations, in addition to a visual correlation matrix plot. 
