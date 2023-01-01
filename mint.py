from turtle import width
import pandas as pd
import streamlit as st
import matplotlib.pyplot as plt
import seaborn as sns
import altair as alt
from tqdm import tqdm

annual_return = 1.08
num_years = 40

df = pd.DataFrame({'year': range(1, num_years+1)})

# list from 0 to 5000 step size of 50
ls = list(range(0, 5000, 50))
@st.cache
def get_data():
    # create a dataframe with column month and balance
    df = pd.DataFrame({'year': range(1, num_years+1)})
    
    for monthly_contribution in list(range(0, 11000, 50)):
        for initial_investment in list(range(0, 110000, 5000)):
            # create a list of monthly balances
            balance = [initial_investment]
            for _ in range(1,len(df)):
                balance.append(balance[-1] * annual_return + monthly_contribution*12)
            df[f'MC{monthly_contribution}_II{initial_investment}'] = balance

    return df

df = get_data()

# add a slider to change the monthly_contribution
slider_your_age = st.sidebar.slider('Your Age', 18, 65, step=1)
slider_desired_FI_income = st.sidebar.slider('Desired Passive Income / Year ($)', 20000, 200000)
slider_initial_investment = st.sidebar.slider('Initial Investment ($)', 0, 100000, step=5000)
slider_monthly_contribution = st.sidebar.slider('Monthly Contribution ($)', 0, 10000, step=50)
entire_plot = st.sidebar.checkbox('Show entire plot')

# filter the dataframe to only show the rows with the selected monthly_contribution
df_filt = df[['year',f'MC{slider_monthly_contribution}_II{slider_initial_investment}']].rename(columns={f'MC{slider_monthly_contribution}_II{slider_initial_investment}': 'balance'})
df_filt['Age'] = df_filt['year'] + slider_your_age
df_filt['type'] = 'Balance'

temp_df = df_filt[['Age','year','type']]
temp_df['balance'] = slider_desired_FI_income / 0.04
temp_df['type'] = 'Comfortable Retirement (4%)'

temp_df1 = df_filt[['Age','year','type']]
temp_df1['balance'] = slider_desired_FI_income / 0.08
temp_df1['type'] = 'Desired Income Achieved (8%)'

for i in range(len(df_filt)):
    if df_filt['balance'].iloc[i] > (slider_desired_FI_income / 0.08):
        income_achieved_age = df_filt['Age'].iloc[i]
        break
else:
    income_achieved_age = 'Never'

for i in range(len(df_filt)):
    if df_filt['balance'].iloc[i] > (slider_desired_FI_income / 0.04):
        comfortable_retirement_age = df_filt['Age'].iloc[i]
        break
else: 
    comfortable_retirement_age = 'Never'

# stack df_filt and temp_df vertically
df_filt = pd.concat([df_filt, temp_df,temp_df1], axis=0)

if not entire_plot:
    df_filt = df_filt[df_filt['balance'] < (slider_desired_FI_income / 0.04)*2]

title = f'Desired Income Achieved at age {income_achieved_age}, Financial Independence achieved at age {comfortable_retirement_age}'
# y axis range 0 to 5000000
fig = alt.Chart(df_filt,width=1000,height=600,title=title).mark_line().encode(
    x='Age',
    y=alt.Y('balance', scale=alt.Scale(domain=[0, max((slider_desired_FI_income / 0.04)*1.2,df_filt['balance'].max())]), axis=alt.Axis(title='Balance ($)')),
    color='type'
    )

# add horizontal line at slider_desired_FI_income


st.altair_chart(alt.layer(fig))