import pandas as pd
from sklearn.model_selection import train_test_split, cross_val_score
from sklearn.ensemble import RandomForestClassifier
from sklearn.model_selection import cross_validate
from sklearn.metrics import accuracy_score, confusion_matrix
import glob
import numpy as np
import matplotlib.pyplot as plt

def concate_df(list_of_df):
    to_model = pd.concat(list_of_df, ignore_index=True)
    to_model = to_model.drop("Site_Key", 1)
    to_model = to_model.drop_duplicates()
    print("Shape: " + str(to_model.shape))
    return to_model


def nan_checker(dataframe):
    num_nan = dataframe.isnull().sum()
    total = np.sum(num_nan)
    return total


def training_splitter(dataframe, data_split):
    train_x, test_x, train_y, test_y = train_test_split(dataframe.loc[:, dataframe.columns != "fraud_status"],
                                                        dataframe['fraud_status'], train_size=data_split)
    return train_x, test_x, train_y, test_y


def random_forest_train(features, label, no_trees):
    clf = RandomForestClassifier()
    clf.fit(features, label)
    return clf


def random_forest_predict(model, features):
    results = model.predict(features)
    return results


def cross_validation(model, features, label, k, score):
    cv_score = cross_validate(model, features, label, cv = k, scoring=score)
    return cv_score


def plotter(scores, array1, array2, tree_list, dir, file):
    plt.plot(tree_list, scores)
    plt.plot(tree_list, array1 + array2, 'b--')
    plt.plot(tree_list, array1 - array2, 'b--')
    plt.ylabel('CV score')
    plt.xlabel('# of trees')
    plt.savefig(dir + 'accuracy_plots/dataset'+str(file).strip(file_dir)+".png")

file_dir = "/home/tpin3694/Documents/university/MSc/fundamental/fraud/data/stratified/"
fraud = pd.read_csv(file_dir+"fraud_data.csv", usecols = range(51))
files = glob.glob(file_dir+"dataset*.csv")


for file in files:
    print("Working on dataset" + str(file).strip(file_dir), sep = "")
    data = pd.read_csv(file, encoding = "ISO-8859-1")
    to_model = concate_df([data, fraud])
    nan_count = nan_checker(to_model)
    if nan_count > 0:
        print("Number of NaN: " + str(nan_count))
    else:
        pass
    train_x, test_x, train_y, test_y = training_splitter(to_model, 0.7)
    print("Dataset" + str(file) + " split into train/test", sep = "")
    number_of_tree = np.linspace(1, 501, 26)-1
    mean_scores, sds = [], []
    for tree in number_of_tree:
        if tree == 0:
            tree = 1
        else:
            tree = tree.astype(int)
        print("Dataset" + str(file) + " tree number: ", str(tree), sep="")
        #rf_model = random_forest_train(train_x, test_x, tree)
        #prediction_results = random_forest_predict(rf_model, test_x)
        #preds = test_y[prediction_results]
        recogniser = RandomForestClassifier(tree)
        score = cross_val_score(recogniser, train_x.astype(int), train_y.astype(int))
        mean_scores.append(np.mean(score))
        sds.append(np.std(score))
    mean_array = np.array(mean_scores)
    sds_array = np.array(sds)
    plotter(mean_scores, mean_array, sds_array, number_of_tree, file_dir, file)
    #print("Cross Validation for Dataset", str(file), " beginning...", sep = "")
    #cv_score = cross_validation(rf_model, test_x, test_y, 10, "recall")


    # Split Up Dataset
    # thg_features = thg_data.loc[:, thg_data.columns != "fraud_status"].astype(int)
    # thg_target = thg_data['fraud_status'].astype(int)
    #
    # # Build the Random Forest Classifier
    # recogniser = RandomForestClassifier(25)
    #
    # # Fit Data
    # recogniser.fit(thg_features, thg_target)
    #
