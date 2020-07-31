# COVID-tracking
NYAS COVID-Tracking

[Launchpad Link](https://joinlaunchpad.com/#/projects/1682/an-algorithm-to-detect-community-outbreak-of-covid-19)

Read the Executive Summary and Submission Slide to understand what's going on

Example code demonstrated in the ipynb file

Data files are also included in the ```data``` folder

Backend Workflow:

```Preprocess-us-counties.R ``` to pull the latest data from NYTimes repo + augment with various features and preprocess data. The processed data exceeeds GitHub file size limits so you have to construct it locally first.   
```Specific_Cases.R``` to backtest the log linear model, simple grf, and the augmented grf