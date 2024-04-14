# %%
import pandas as pd

df = pd.read_json("accuracy_measures.json").T

# %%


def output_acc_metric(df, column):
    # Loop over each value in the dataframe
    df = df.copy()
    for col in df.columns:
        df[col] = df[col].apply(
            lambda x: x[column] if column in x and x[column] != "NaN" else None
        )

    df.style.highlight_min().to_excel(f"{column}.xlsx")


output_acc_metric(df, "pct_bias")
output_acc_metric(df, "coverage_80")
output_acc_metric(df, "coverage_95")
output_acc_metric(df, "mape")
