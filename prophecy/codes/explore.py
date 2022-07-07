import pandas as pd
import os
import sweetviz as sv
import re

os.chdir('E:\projects\prophecy')

log_df = pd.read_excel( 'Experiment log file.xlsx' )
log_df['Test: Post Nebulization Resistance(kΩ)'] = pd.to_numeric(log_df['Test: Post Nebulization Resistance(kΩ)'], errors='coerce')

#my_report = sv.analyze(log_df)
#my_report.show_html()

files = os.listdir('E:\projects\prophecy\data')

df = []

for file in files:
  df0 = pd.read_csv( 'E:\projects\prophecy\data' + '\\' + file )
  df0['Exp. No.'] = int( re.findall(r'\d+', file)[0] )
  df1 = pd.merge( df0, log_df[[ 'Exp. No.', 'Virus Particle' ]], on = 'Exp. No.', how = 'left' )
  df.append( df1 )

df = pd.concat( df )
df.drop('Sequence', axis = 1 , inplace = True)

my_report = sv.analyze(df)
my_report.show_html()