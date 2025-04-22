from functions import *

df_data_dict = pd.read_csv('C:\\Users\\Thoma\\OneDrive\\Desktop\\portoflio_project_1\\data\\data_dictionary.csv').fillna('')
data_dir = 'C:\\Users\\Thoma\\OneDrive\\Desktop\\portoflio_project_1\\data\\'
df_data = pd.DataFrame({'year': [*range(1900,2023,1)], 'dollar':1})

for row in df_data_dict[df_data_dict.Identifier!=''].itertuples():
    if (row.filename != '') & (row.preprocessor_function != ''):
        filename = data_dir + row.filename
        id = row.Identifier
        var_name = row.var_name
        preprocessor_function= row.preprocessor_function
        df_data = pd.merge(df_data, locals()[preprocessor_function](filename,id,row.var_name), how='outer', on='year')
df_data.to_csv(data_dir+'main_data_file.csv')