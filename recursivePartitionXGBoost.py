from __future__ import print_function

import numpy as np
import xgboost as xgb


class rXGB:
    """
    Object to solve conditional least squares
    """

    def __init__(self, feature_df_dict=None, fips_list=None, max_depth=1):
        """
        fipslist: List of fips of counties
        dataframe: dataframe of cases of each county
        """
        self.feature_df_dict = feature_df_dict
        self.fips_list = fips_list
        self.max_depth = max_depth

    def MAPE(self, y_true, y_pred): 
        """
        Calculates Mean Absolute Percentage Error (MAPE) given y_true and y_pred
        """
        y_true, y_pred = np.array(y_true), np.array(y_pred)
        return np.mean(np.abs((y_true - y_pred) / y_true)) * 100

    def XGBHelper(self, fips_indices=None, df=None):
        """
        Runs XGBoost on the given list of fips

        Assumes DataFrame passed in is already from one of the folds
        """

        if fips_indices is None or df is None:
            # print("haha")
            return 0.0

        df = df.loc[df["fips"].isin(fips_indices)]

        HorizonList = df["Horizon"].unique()
        HorizonList.sort()

        train_data = df.loc[df["Horizon"].isin(HorizonList[:-1])]
        X_train = train_data.drop([0, "fips", "Horizon", "fold"], axis=1)
        y_train = train_data[[0]]

        test_data = df.loc[df["Horizon"] == HorizonList[-1]]
        X_test = test_data.drop([0, "fips", "Horizon", "fold"], axis=1)
        y_test = test_data[[0]].values
        y_test = y_test.flatten()
        for i in range(len(y_test)):
            if y_test[i] - 0.0 < 0.0001:
                y_test[i] = y_test[i] + 0.01

        reg = xgb.XGBRegressor(n_estimators=1000)
        reg.fit(X_train, y_train, eval_set=[(X_train, y_train), (X_test, y_test)], early_stopping_rounds=50, verbose=False)

        y_predict = reg.predict(X_test)

        mape_error = self.MAPE(y_test, y_predict)

        #pprint(y_predict)
        #pprint(y_test)
        return mape_error
