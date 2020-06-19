from __future__ import print_function

import math
import numpy as np
import csv as csv
import pandas as pd
from pprint import pprint
import datetime


def load_nyt(filename="data/us-counties_2020-06-16"):
    """
    Loads in NYTimes data

    Returns dataframe and start datetime

    Adds in column showing how many days from start
    """
    

    cases_data = pd.read_csv(filename)
    cases_data.dropna(subset=["fips"], inplace=True)
    
    cases_data["fips"] = cases_data["fips"].astype("int")
    fips_list = cases_data["fips"].unique()

    cases_data["datetime"] = pd.to_datetime(cases_data["date"])
    cases_data["logcases"] = np.log(cases_data["cases"])

    for fips in fips_list:
        cases_data.loc[cases_data["fips"]==fips,"difflogcases"] = cases_data.loc[cases_data["fips"]==fips,"logcases"].diff()

    start = (cases_data.nsmallest(1, "datetime")["datetime"].values)[0]
    end = (cases_data.nlargest(1, "datetime")["datetime"].values)[0]

    cases_data["days_from_start"] = (cases_data["datetime"] - start).dt.days

    return cases_data, start, end


def load_adj(adjfile="data/countyadj.csv", masterfile="data/county_fips_master.csv"):
    """
    Loads adjacency matrix as dataframe

    Replaces columns and rows with fips 
    """
    county_adj = pd.read_csv(adjfile)
    county_adj = county_adj.rename(columns={'Unnamed: 0':'long_name'})
    county_fips = pd.read_csv(masterfile)

    county_fips.loc[county_fips['long_name']=="Autauga County AL"]

    # Becareful of Dona Ana County NM
    county_fips_dict = {}
    fips_county_dict = {}
    for long_name in county_adj['long_name']:
        # print(long_name)
        temp_df = county_fips.loc[county_fips['long_name']==long_name]["fips"]
        # print("{0} {1}".format(long_name,temp_df.iloc[0]))
        county_fips_dict[long_name] = temp_df.iloc[0]
        fips_county_dict[temp_df.iloc[0]] = long_name

    county_adj = county_adj.rename(county_fips_dict, axis="columns")
    county_adj["fips"]=[county_fips_dict[long_name] for long_name in county_adj["long_name"]]

    return county_adj


def sliding_window_backwards(dataframe, last_date, window_length):
    """
    Slices DataFrame backwards
    """

    assert window_length >= 1

    last_days_from_start = dataframe.loc[dataframe["datetime"] == last_date, ["days_from_start"]].values.flatten()[0]
    first_days_from_start = last_days_from_start - window_length + 1

    new_df = dataframe.loc[(dataframe["days_from_start"] <= last_days_from_start) & (dataframe["days_from_start"] >= first_days_from_start)]

    new_df["window_days"] = new_df["days_from_start"] - new_df.nsmallest(1,"days_from_start")["days_from_start"].values.flatten()[0]

    return new_df


def normalize_window(window):
    """
    Assumes Infection Numbers per county is of the form

    I_t = I_start * exp( rate * t )
    Converts I_t -> I_t / I_start
    """
    fips_list = window["fips"].unique()
    fips_list.sort()

    window["normalized_cases"] = window["cases"].astype("float")

    for fips in fips_list:
        county_window = window.loc[window["fips"] == fips]
        A = county_window.nsmallest(1, "cases")["cases"].values[0]
        # pprint(A)
        window.loc[window["fips"] == fips, "normalized_cases"] = window.loc[window["fips"] == fips, "cases"]/A

    return window