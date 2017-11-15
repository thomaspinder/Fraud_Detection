#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Mon Nov 13 23:47:18 2017

@author: tpin3694
"""
from glob import glob
import os
import pandas as pd
import seaborn as sns
from sklearn import preprocessing
import numpy as np


def pd_std(df_col):
    vec_col = df_col.as_matrix()
    std_vec = preprocessing.scale(vec_col)
    return std_vec


dir = "/home/tpin3694/Documents/university/MSc/fundamental/THG-Jarvis/"

for file in glob(os.path.join(dir, "*.*")):
    print(file.replace(dir, ""))


customer_trans = pd.read_csv(dir+"MAIN_transaction_data.csv", parse_dates=True,
                             encoding = "ISO-8859-1", usecols = list(range(23)))

print(customer_trans.shape)
print(customer_trans.info())

trans_mat = customer_trans.as_matrix()
charge_by_order = pd_std(customer_trans['Product_Charge_Price']/customer_trans["Ordered_Qty"])
sns.distplot(charge_by_order)

import numpy as np
top_perc = list(np.percentile(customer_trans['Product_Charge_Price'], q = list(range(95, 101))))
print(*top_perc, sep = "\n")

import math
thresh = math.floor(len(customer_trans)*0.01)
high_pricing = customer_trans.nlargest(thresh, "Product_Charge_Price")
low_pricing = customer_trans.nsmallest(len(customer_trans)-thresh, "Product_Charge_Price")

low_price_std = pd_std(low_pricing)
sns.distplot(low_pricing)


##sun
