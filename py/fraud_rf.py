import pandas as pd
import logging
from sklearn.model_selection import train_test_split, cross_val_score
from sklearn.ensemble import RandomForestClassifier
from sklearn.model_selection import cross_validate
from sklearn.decomposition import PCA
import glob
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
from imblearn.combine import SMOTETomek
from collections import Counter


def data_reader(file):
    data_read = pd.read_csv(file, encoding="ISO-8859-1")
    data = data_read.drop(["Ordered_Product_Key", "Campaign_Key"], 1)
    return data


def file_checker(dataframe):
    """Checks for NaNs and if any fraud actually exists in the file."""
    num_nan = np.sum(dataframe.isnull().sum())
    num_fraud = dataframe['fraud_status'].sum()
    return num_nan, num_fraud


def get_smote(feature, label):
    """Uses SMOTE and Tomek Links to under and over sample to combat the class imbalance."""
    print("Raw Data: " + str(sorted(Counter(label).items())))
    smt = SMOTETomek(random_state=42)
    feature_resampled, label_resampled = smt.fit_sample(feature, label)
    print("Resampled: " + str(sorted(Counter(label_resampled).items())))
    return feature_resampled, label_resampled


def smote_visalisation(file, feature, y, resampled_feature, y_resampled):

    pca = PCA(n_components=2)
    X_vis = pca.fit_transform(feature)
    X_res_vis = pca.fit_transform(resampled_feature)
    # Two subplots, unpack the axes array immediately
    f, (ax1, ax2) = plt.subplots(1, 2)
    c0 = ax1.scatter(X_vis[y == 0, 0], X_vis[y == 0, 1], label="Class #0",
                     alpha=0.5)
    c1 = ax1.scatter(X_vis[y == 1, 0], X_vis[y == 1, 1], label="Class #1",
                     alpha=0.5)
    ax1.set_title('Original set')
    ax2.scatter(X_res_vis[y_resampled == 0, 0], X_res_vis[y_resampled == 0, 1],
                label="Class #0", alpha=0.5)
    ax2.scatter(X_res_vis[y_resampled == 1, 0], X_res_vis[y_resampled == 1, 1],
                label="Class #1", alpha=0.5)
    ax2.set_title('SMOTE + Tomek')
    # make nice plotting
    for ax in (ax1, ax2):
        ax.spines['top'].set_visible(False)
        ax.spines['right'].set_visible(False)
        ax.get_xaxis().tick_bottom()
        ax.get_yaxis().tick_left()
        ax.spines['left'].set_position(('outward', 10))
        ax.spines['bottom'].set_position(('outward', 10))
        ax.set_xlim([-100, 100])
        ax.set_ylim([-100, 100])
    plt.figlegend((c0, c1), ('Class #0', 'Class #1'), loc='lower center',
                  ncol=2, labelspacing=0.)
    plt.tight_layout(pad=3)
    plt.savefig("/home/tpin3694/Documents/university/MSc/fundamental/fraud/plots/smoted/dataset" +
                str(file).strip(file_dir)+"smoted.png")
    f.savefig("/home/tpin3694/Documents/university/MSc/fundamental/fraud/plots/smoted/dataset" +
                str(file).strip(file_dir)+"_fig_smoted.png")
    plt.show()



def training_splitter(dataframe):
    """Splits data out into training and testing labels and features"""
    data_x, data_y = dataframe.loc[:, dataframe.columns != "fraud_status"], dataframe['fraud_status']
    return data_x, data_y


def random_forest_train(features, label, no_trees):
    """Train the forest on a user-specified number of trees"""
    clf = RandomForestClassifier(no_trees)
    clf.fit(features, label)
    return clf


def random_forest_predict(model, features):
    """Make model predictions"""
    results = model.predict(features)
    return results


def cross_validation(model, features, label, k, score_metric):
    cv_score = cross_validate(model, features, label, cv=k, scoring=score_metric)
    return cv_score


def plotter(scores, array1, array2, tree_list, directory, file_write):
    plt.plot(tree_list, scores)
    plt.plot(tree_list, array1 + array2, 'b--')
    plt.plot(tree_list, array1 - array2, 'b--')
    plt.ylabel('CV score')
    plt.xlabel('# of trees')
    plt.savefig(directory + 'accuracy_plots/dataset' + str(file_write).strip(file_dir) + ".png")


def model_scorer(tree_list, train, test, scoring):
    """Train the classifier and report on scoring metrics through cross-validation"""
    for tree in tree_list:
        if tree == 0:
            tree = 1
        else:
            tree = tree.astype(int)
        classifier = RandomForestClassifier(tree)
        score = cross_validate(classifier, train.astype(int), test.astype(int), scoring=scoring)
        return score


def test_model(no_trees, features, labels):
    random_forest = RandomForestClassifier(no_trees)
    model_metric = cross_val_score(random_forest, features, labels)
    return model_metric


# Setup Error Logging
logging.basicConfig(filename="/home/tpin3694/Documents/university/MSc/fundamental/fraud/forest_errors.log",
                    level=logging.DEBUG, format='%(asctime)s %(message)s')

# Setup File Structure
file_dir = "/home/tpin3694/Documents/university/MSc/fundamental/fraud/data/stratified/"
files = glob.glob(file_dir + "dataset*.csv")

for file in files:
    print("Working on dataset" + str(file).strip(file_dir), sep="")
    to_model = data_reader(file)
    file_checks = file_checker(to_model)
    if file_checks[1] > 0:
        logging.warning(str(file_checks[0]) + " NaNs in Dataset" + str(file).strip(file_dir))
        logging.warning(str(file_checks[1]) + " fraudulent transactions present in Dataset" + str(file).strip(file_dir))
        feature, label = training_splitter(to_model)
        train_x, train_y = get_smote(feature, label)
        smote_visalisation(file, feature, label, train_x, train_y)
        scoring = ['precision_macro', 'recall_macro']
        model_scores = model_scorer(np.linspace(1, 6, 2) - 1, train_x, train_y, scoring)
        model_recall = model_scores['test_recall_macro']
        model_precision = model_scores['test_precision_macro']
    else:
        logging.warning("No Fraud present in Dataset" + str(file).strip(file_dir))


        prediction_results = random_forest_predict(rf_model, test_x)
    #     # preds = test_y[prediction_results]
    #     recogniser = RandomForestClassifier(tree)
    #     score = cross_val_score(recogniser, train_x.astype(int), train_y.astype(int))
    #     mean_scores.append(np.mean(score))
    #     sds.append(np.std(score))
    # mean_array = np.array(mean_scores)
    # sds_array = np.array(sds)
    #plotter(mean_scores, mean_array, sds_array, number_of_tree, file_dir, file)
    # print("Cross Validation for Dataset", str(file), " beginning...", sep = "")
    # cv_score = cross_validation(rf_model, test_x, test_y, 10, "recall")

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
