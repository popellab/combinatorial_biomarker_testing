##!/usr/bin/env python
# feature selection with recursive feature elimination
from IPython.display import display
from numpy import inf
from sklearn.preprocessing import StandardScaler
from sklearn.metrics import classification_report
from sklearn.model_selection import train_test_split
from sklearn import svm
from sklearn import metrics
from sklearn.feature_selection import RFECV
from sklearn.ensemble import RandomForestClassifier

import random
import pandas as pd
import numpy as np
import sys

#n = len(sys.argv) # number of command line arguments
#rnumber = random.randint(1, 500) # generate random number
rnumber = 407 #or set the seed
# index of unused variables (index starting from 1)
unused_vars = [1, 2, 5, 6, 7, 8, 9, 10, 21, 24, 32, 33, 34, 47, 48, 62, 83, 86, 93, 94, 95, 96, 97, 100, 102, 103, 115]

SD_status = sys.argv[1]  # SD as responder ("R") or non-responder ("NR")
filter_compartment = sys.argv[2] # Compartment: " Blood", " Tumor", " Lymph node", "all"
ML_method = sys.argv[4] # "SVM" for support vector machine & "RF" for random forest

# input file names
biomarker_file_name = sys.argv[3]+"_biomarkers_trainset.csv"
annotatons_file_name = "biomarker_names_units_v1.txt"
# output file name
outputfile = "output_"+ML_method+"_"+sys.argv[3]+"_sd"+sys.argv[1]+"_"+sys.argv[2]+".csv"

# display the filenames
print(biomarker_file_name)
print(outputfile)

# read data files
data_raw = pd.read_csv(biomarker_file_name)
annotations_raw = pd.read_csv(annotatons_file_name)

# clean the data
idx_remove = np.add(unused_vars,-1) # subtract by 1 as indices start from 0
data_raw = data_raw.drop(columns=data_raw.columns[idx_remove]) # drop unused columns
annotations_raw = annotations_raw.drop(labels=idx_remove) # drop corresponding rows in the annotation file

#data_raw = data_raw.dropna()
#data_raw = data_raw.replace([np.nan, -np.inf], 0)
data_raw = data_raw.fillna(0)

data_raw = data_raw.replace('CR/PR','R')
data_raw = data_raw.replace('PD','NR')
if SD_status == "R": data_raw = data_raw.replace('SD','R') # SD as R or NR
elif SD_status == "NR": data_raw = data_raw.replace('SD','NR') # SD as R or NR
else: sys.exit("Error: Unknown SD status")

annotations_raw=annotations_raw.reset_index() # reset index
targets = data_raw["Response"]

# filter data by compartment
if filter_compartment != "all":
    index_selected = annotations_raw[annotations_raw[' Source'].str.contains(filter_compartment, regex=False)].index
    annotations = annotations_raw[annotations_raw[' Source'].str.contains(filter_compartment, regex=False)]
    data = data_raw.iloc[:,index_selected].copy()
else:
    annotations = annotations_raw
    data = data_raw.drop(columns=['Response'])
    
annotations=annotations.reset_index()

# scale data
data[data == -inf] = 0
data[data == inf] = 0
features_unscaled = data.copy()
scaler = StandardScaler()
scaler.fit(features_unscaled.values)
features = scaler.transform(features_unscaled.values)

if ML_method == "SVM": # SVM - linear kernel
    clf = svm.SVC(kernel = 'linear')
    rfecv = RFECV(estimator = clf,scoring = 'f1_macro')
elif ML_method == "RF": # Random forest classifier
    rfecv = RFECV(estimator = RandomForestClassifier(random_state=rnumber),scoring = 'f1_macro')
else: sys.exit("Error: Unknown ML method specified")

rfecv.fit(features,targets)
selecInfo = rfecv.support_
selecIndex = np.where(selecInfo==1)

# display results
print('Optimal number of features: {}'.format(rfecv.n_features_))
print('Selected features:')
np.savetxt(sys.stdout, selecIndex, fmt="%i", newline='\n')
print('CV results:')
print(rfecv.cv_results_)
print('Random state: {}'. format(rnumber))

# write selected feature names & indices in the output file
col1_data =np.array(selecIndex).ravel()
col2_data = annotations.loc[col1_data, :]['Name']
dataset = pd.DataFrame({'index': col1_data, 'name': col2_data}, columns=['index', 'name'])
#np.savetxt(outputfile ,dataset, fmt="%s")

dataset.to_csv(outputfile, index = False)


