import pandas as pd
import datetime
import math
#Notes: May want to switch id to list and str.contains() to something vectorized so we can pull all desired values at once.

#Bureau of Labor Statistics Pre-processor
def BLS_product_preprocessor(filename,id,var_name):
    df = pd.read_csv(filename, sep='\t')
    df = df.rename(columns=lambda x: x.strip())
    df = df[df.period!='MO13'].drop(['footnote_codes','period'],axis=1)
    df = df[df.series_id.str.contains(id)]
    df.value = pd.to_numeric(df.value)

    df = df.groupby(['series_id','year']).mean().reset_index()

    df[var_name] = df[df.series_id.str.contains(id)].value
    df = df.drop(['series_id','value'],axis=1)

    return(df)

def BLS_wages_preprocessor(filename,id,var_name):
    """

    :param filename: filename to draw from
    :param id: Industry code.  8 digit number, example: '05000000' - Total Private
    :param var_name: name of variable
    :return:
    """
    #CE: Baseline
    #S,U: Seasonally adjusted, not seasonally adjusted
    #id (8 digits)
    #07,08: Average Weekly hours, average hourly earnings

    id_hours,id_earnings = id+'07',id+'08'
    df = pd.read_csv(filename, sep='\t')
    df = df.rename(columns=lambda x: x.strip())
    df = df[df.period!='MO13'].drop(['footnote_codes','period'],axis=1)
    df = df[df.series_id.str.contains(id_hours) | df.series_id.str.contains(id_earnings)]
    df.value = pd.to_numeric(df.value)
    df = df.groupby(['series_id','year']).mean().reset_index()

    df = pd.merge(df[df.series_id.str.contains(id_hours)].drop(['series_id'],axis=1),
             df[df.series_id.str.contains(id_earnings)].drop(['series_id'],axis=1),
             how='inner',on='year')

    df = df.rename(columns={'value_x':var_name+'_hours','value_y':var_name})
    #df[var_name+'_weekly']=df[var_name+'_hours'] * df[var_name+'_hourly']
    #df[var_name+'_annual']=df[var_name+'_weekly'] * 52

    return(df)

def auronum_preprocessor(filename,id,var_name):
    #Get File
    df = pd.read_excel(filename)
    #Isolate columns of interest
    df = df.iloc[:,df.columns.get_loc(id):df.columns.get_loc(id)+2]
    #Standardize names
    df.columns = ['year',var_name]
    #Get year averages
    df.year = df.year.dt.year
    #Unit Conversions
    df[var_name] = df[var_name]*16
    df = df.groupby(['year']).mean().reset_index()
    return(df)

def bitcoin_data_parser(filename,id,var_name):
    #Get File
    df = pd.read_csv(filename)
    #Isolate columns of interest
    df = df[['Timestamp',id]]
    #Standardize names
    df.columns = ['year',var_name]
    #Clean
    df[var_name] = pd.to_numeric(df[var_name],errors='coerce')
    df = df.dropna()
    #Convert to Annual
    df.year = pd.to_datetime(df.year).dt.year
    #Unit Conversions
    #Average by Year
    df = df.groupby(['year']).mean().reset_index()
    return(df)