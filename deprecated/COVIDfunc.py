import numpy as np
import math
from scipy import optimize
from itertools import chain, combinations


# Create object to solve dynamic program of conditional least squares
class dynamicCLS:
    """
    Object to solve conditional least squares
    """

    def __init__(self, fipslist, cases_dataframe):
        """
        fipslist: List of fips of counties
        dataframe: dataframe of cases of each county
        """

        self.fipslist = fipslist
        self.cases_dataframe = cases_dataframe

        # Checks if solution is already available
        self.LookupTable = {"exp": {}, "log": {}}
        # Stores optimal value of total SE of this set
        self.OptTable = {"exp": {}, "log": {}}
        # Stores the optimal assignment given a set
        self.AssignmentTable = {"exp": {}, "log": {}}
        # Stores the optimal parameters obtained from curve fitting
        self.ParaTable = {"exp": {}, "log": {}}

        self.pset = self.powerset(self.fipslist)
        # Populate Lookup tables
        for model in ["exp", "log"]:
            for combo in self.pset:
                self.LookupTable[model][repr(list(combo))] = {}
                self.OptTable[model][repr(list(combo))] = {}
                self.AssignmentTable[model][repr(list(combo))] = {}
                self.ParaTable[model][repr(list(combo))] = {}

                for i in range(len(self.fipslist)):
                    self.LookupTable[model][repr(list(combo))][i+1] = False
                    self.OptTable[model][repr(list(combo))][i+1] = math.inf
                    self.AssignmentTable[model][repr(list(combo))][i+1] = []
                    self.ParaTable[model][repr(list(combo))][i+1] = []

    def powerset(self, iterable):
        """
        powerset([1,2,3]) --> [(), (1,), (2,), (3,), (1,2), (1,3), (2,3), (1,2,3)]
        """
        s = list(iterable)
        return list(chain.from_iterable(combinations(s, r) for r in range(len(s)+1)))

    def problem(self, comboSets, nLines, model):
        """
        recursive DP for solving the problem
        """
        # Already solved before
        if self.LookupTable[model][repr(comboSets)][nLines]:
            return self.OptTable[model][repr(comboSets)][nLines]

        # Empty set assigned
        if len(comboSets) == 0:
            self.LookupTable[model][repr(comboSets)][nLines] = True
            self.OptTable[model][repr(comboSets)][nLines] = 0
            self.AssignmentTable[model][repr(comboSets)][nLines] = []
            self.ParaTable[model][repr(comboSets)][nLines] = [
                [] for line in range(nLines)]

            return self.OptTable[model][repr(comboSets)][nLines]

        # Base Case 1 line
        if nLines == 1:
            data_dict = fitData(self.cases_dataframe, comboSets)

            self.LookupTable[model][repr(comboSets)][nLines] = True
            self.AssignmentTable[model][repr(comboSets)][nLines] = [comboSets]

            if model == "exp":
                self.OptTable[model][repr(
                    comboSets)][nLines] = data_dict["SEExp"]
                self.ParaTable[model][repr(comboSets)][nLines] = [
                    data_dict["poptexp"]]
            else:
                self.OptTable[model][repr(
                    comboSets)][nLines] = data_dict["SELog"]
                self.ParaTable[model][repr(comboSets)][nLines] = [
                    data_dict["poptlog"]]

            return self.OptTable[model][repr(comboSets)][nLines]

        # Base Case 1 line for each county
        elif nLines == len(comboSets):

            for fips in comboSets:
                self.OptTable[model][repr(comboSets)][nLines] = 0

                #print("Current fips is {0}".format(fips))

                self.AssignmentTable[model][repr(
                    comboSets)][nLines] += [[fips]]

                data_dict = fitData(self.cases_dataframe, [fips])

                self.problem([fips], 1, model)

                # pprint(data_dict["poptexp"])
                # pprint(data_dict["SEExp"])
                # pprint(self.ParaTable[model][repr([fips])][1])
                # pprint(self.ParaTable[model][repr(comboSets)][nLines])

                self.OptTable[model][repr(
                    comboSets)][nLines] += self.OptTable[model][repr([fips])][1]
                self.ParaTable[model][repr(
                    comboSets)][nLines] += self.ParaTable[model][repr([fips])][1]

            self.LookupTable[model][repr(comboSets)][nLines] = True

            return self.OptTable[model][repr(comboSets)][nLines]

        # Recursive case
        else:
            accOpt = math.inf

            pset = self.powerset(comboSets)

            for combo in pset:
                dupe = comboSets.copy()
                for entry in combo:
                    dupe.remove(entry)
                tempOpt = self.problem(
                    list(combo), 1, model) + self.problem(dupe, nLines-1, model)
                if tempOpt < accOpt:

                    accOpt = tempOpt

                    self.OptTable[model][repr(comboSets)][nLines] = tempOpt
                    self.AssignmentTable[model][repr(comboSets)][nLines] = self.AssignmentTable[model][repr(
                        list(combo))][1] + self.AssignmentTable[model][repr(dupe)][nLines - 1]
                    self.ParaTable[model][repr(comboSets)][nLines] = self.ParaTable[model][repr(
                        list(combo))][1] + self.ParaTable[model][repr(dupe)][nLines - 1]

            self.LookupTable[model][repr(comboSets)][nLines] = True

            return self.OptTable[model][repr(comboSets)][nLines]

    def fillup(self):
        """
        Fills up all nLines and models (for use in cross validation later)
        """
        for model in ["exp", "log"]:
            self.problem(self.fipslist, len(self.fipslist), model)
            self.problem(self.fipslist, len(self.fipslist) - 1, model)


def logLinearModel(t, start, rate):
    """
    Independent Variable = t
    Parameters:
        start = t_0,c
        rate = r_c
    returns r_c(t - t_0,c)
    """
    # assert t >= start

    return rate*(t - start)


def exponentModel(t, start, rate):
    """
    Independent Variable = t
    Parameters:
        start = t_0,c
        rate = r_c
    returns exp(r_c(t - t_0,c))
    """
    return np.exp(logLinearModel(t, start, rate))


def fitData(cases_df, fipslist):
    """
    Takes in list of fips and slices the dataframe given

    Then fits to logLinearModel and exponentModel and reports Squared Errors
    """
    cases_sliced = cases_df[cases_df["fips"].isin(fipslist)]

    xdata = cases_sliced["days_from_start"]
    logydata = cases_sliced["logcases"]
    ydata = cases_sliced["cases"]

    poptlog, pcovlog = optimize.curve_fit(logLinearModel, xdata, logydata)
    poptexp, pcovexp = optimize.curve_fit(exponentModel, xdata, ydata)

    data_dict = {}
    data_dict["cases_sliced"] = cases_sliced
    data_dict["xdata"] = xdata
    data_dict["logydata"] = logydata
    data_dict["ydata"] = ydata
    data_dict["poptlog"] = poptlog
    data_dict["pcovlog"] = pcovlog
    data_dict["poptexp"] = poptexp
    data_dict["pcovexp"] = pcovexp

    AbsErrorLog = logLinearModel(xdata, *poptlog) - logydata
    SELog = np.sum(np.square(AbsErrorLog))
    ExpSELog = np.sum(np.square(exponentModel(xdata, *poptlog)-ydata))

    # print(type(poptlog))

    data_dict["SELog"] = SELog
    data_dict["ExpSELog"] = ExpSELog

    AbsErrorExp = exponentModel(xdata, *poptexp) - ydata
    SEExp = np.sum(np.square(AbsErrorExp))
    LogSEExp = np.sum(np.square(logLinearModel(xdata, *poptexp) - logydata))

    data_dict["SEExp"] = SEExp
    data_dict["LogSEExp"] = LogSEExp

    return data_dict
