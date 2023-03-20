#reads two csv files and merges them based on a common key column#

# import the pandas library

import pandas as pd

# Read the files into two dataframes.
#df0 = pd.read_csv('Animalstroop_cleaned.csv')
df1 = pd.read_csv('Age.csv')
df2 = pd.read_csv('KG_edited.csv')

# Merge the two dataframes, using _ID column as key
merged = df1.merge(df2, on='P_ID', how="outer").fillna("")
merged.to_csv("covariates_of_interest.csv", index=False)