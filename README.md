# COVID-tracking
COVID-19 Outbreak Detection Tool

[Main App](https://analytics-modeling.shinyapps.io/outbreakdetection/)

The COVID-19 Outbreak Detection Tool detects recent COVID-19 outbreaks in U.S. counties. The tool leverages machine learning to predict how fast an outbreak could spread at the county level by estimating the doubling time of COVID-19 cases. It accounts for reported COVID-19 cases and deaths, face mask mandates, social distancing policies, the CDC’s Social Vulnerability Index, changes in tests performed and rate of positive tests. The tool offers an interactive map and a data explorer allowing users to filter and rearrange counties based on predicted trends, which get updated at least once per week.

![alt text](https://github.com/Runespear/COVID-tracking/blob/master/OutbreakNY_1016.png?raw=true)

Data files are included in the ```data``` folder. Source code is located at the ```src``` folder. The backend workflow can be found at ```WorkFlow.txt```.

To reference, please cite [the paper](https://arxiv.org/abs/2011.01219).

