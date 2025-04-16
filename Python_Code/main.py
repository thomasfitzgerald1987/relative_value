from functions import *

df_data_dict = pd.read_csv('C:\\Users\\Thoma\\OneDrive\\Desktop\\portoflio_project_1\\data\\data_dictionary.csv').fillna('')
data_dir = 'C:\\Users\\Thoma\\OneDrive\\Desktop\\portoflio_project_1\\data\\'

df_data = pd.DataFrame({'year': [*range(1900,2023,1)], 'dollar':1})

for row in df_data_dict[df_data_dict.USBLS_Identifier!=''].itertuples():
    if row.filename != '':
        filename = data_dir + row.filename
        print(filename)
        id = row.USBLS_Identifier
        match row.input_type:
            case 'USBLS_product':
                df_data = pd.merge(df_data, BLS_product_preprocessor(filename, id, row.name), how='outer', on='year')
            case 'USBLS_wages':
                df_data = pd.merge(df_data, BLS_wages_preprocessor(filename, id, row.name), how='outer', on='year')
            case 'Auronum.com':
                df_data = pd.merge(df_data,auronum_preprocessor(filename,id,row.name), how='outer', on='year')

df_data.to_csv(data_dir+'main_data_file.csv')