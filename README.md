# COVID-tracking
NYAS COVID-Tracking

[Launchpad Link](https://joinlaunchpad.com/#/projects/1682/an-algorithm-to-detect-community-outbreak-of-covid-19)

Read the Executive Summary and Submission Slide to understand what's going on

Example code demonstrated in the ipynb file

Data files are also included in the ```data``` folder

Workflow:

```Preprocess-us-counties.R ``` to pull the latest data from NYTimes repo and preprocess data  
```Specific_Cases.R``` to backtest grf and lm  
```Generate_Predictions_grf.R``` to generate whether there will be an outbreak in the next 7 days  