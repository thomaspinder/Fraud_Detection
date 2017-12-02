import pandas as pd

chargeback = pd.read_csv("/home/tpin3694/Documents/university/MSc/fundamental/THG-Jarvis/MAIN_transaction_data.csv",
                          parse_dates = True, encoding = "ISO-8859-1")
fraud_trans = pd.read_csv("/home/tpin3694/Documents/university/MSc/fundamental/THG-Jarvis/MAIN_chargeback_data.csv",
                          parse_dates = True, encoding = "ISO-8859-1")
print(chargeback)