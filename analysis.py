# %%
import pandas as pd


df = pd.read_json("acc_measures.json").T
# Loop over each value in the dataframe
for col in df.columns:
    df[col] = df[col].apply(lambda x: x["mape"] if x["mape"] != "NaN" else None)


df.style.highlight_min().to_excel("acc_measures.xlsx")