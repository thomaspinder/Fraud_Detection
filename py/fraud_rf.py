import pandas as pd
from sklearn.ensemble import RandomForestClassifier
from sklearn.model_selection import cross_validate

thg_data = pd.read_csv("/home/tpin3694/Documents/university/MSc/fundamental/hot_pot_encoded.csv",
                       parse_dates = True, encoding = "ISO-8859-1")

# Split Up Dataset
thg_features = thg_data.loc[:, thg_data.columns != "fraud_status"].astype(int)
thg_target = thg_data['fraud_status'].astype(int)

# Build the Random Forest Classifier
recogniser = RandomForestClassifier(25)

# Fit Data
recogniser.fit(thg_features, thg_target)

