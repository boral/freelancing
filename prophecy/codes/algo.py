import pandas as pd
import numpy as np
from datar.all import *
import sweetviz as sv
import plotly.express as px
import plotly.io as pio
from scipy.stats import kurtosis
pio.renderers.default='browser'
import os
from sklearn.tree import DecisionTreeClassifier, export_graphviz

from sklearn import tree

def visualize_tree(tree, feature_names):
    """Create tree png using graphviz.

    Args
    ----
    tree -- scikit-learn DecsisionTree.
    feature_names -- list of feature names.
    """
    with open("dt.dot", 'w') as f:
        export_graphviz(tree, out_file=f,
                        feature_names=feature_names)

    command = ["dot", "-Tpng", "dt.dot", "-o", "dt.png"]
    try:
        subprocess.check_call(command)
    except:
        exit("Could not run dot, ie graphviz, to "
             "produce visualization")

os.chdir( 'E:\projects\prophecy' )

df = pd.read_csv("E:\projects\prophecy\exp_df.csv")

log_df = pd.read_excel( 'E:\projects\prophecy\Experiment log file.xlsx' )

df['pre_init_diff'] = round( df['Test: Pre Nebulization Resistance (kΩ)'] - df['Initial Resistance (kΩ)'], 2 )

df['post_init_diff'] = round( df['Test: Post Nebulization Resistance(kΩ)'] - df['Initial Resistance (kΩ)'], 2 )

df['post_pre_diff'] = round( df['Test: Post Nebulization Resistance(kΩ)'] - df['Test: Pre Nebulization Resistance (kΩ)'], 2 )

df['exp_virus'] = df['Exp. No.'].astype(pd.StringDtype()) + '_' + df['Virus Particle']

df['exp_virus_state'] = df['exp_virus'] + '_' + df['state1'].astype(str)

stats_df = df >> filter( f.state1 != 2 ) >> select( f.exp_virus_state, f.frequency11 ) >> \
    group_by( f.exp_virus_state ) >> \
    summarise( median = median( f.frequency11 ), quan_2 = quantile( f.frequency11, 0.02 ), \
              quan_98 = quantile( f.frequency11, 0.98 ), \
    skewness = ( quantile( f.frequency11, 0.75 ) + quantile( f.frequency11, 0.25 ) - median( f.frequency11 ) )/( quantile( f.frequency11, 0.75 ) - quantile( f.frequency11, 0.25 ) ) ) >> \
    mutate( dispersion = f.quan_98 - f.quan_2 )
    
stats_df[['exp_no', 'virus', 'state']] = stats_df['exp_virus_state'].str.split('_', 2, expand=True)


stats_df_2 = pd.DataFrame( stats_df >> select( ~f.exp_no, ~f.exp_virus_state ) )

dt = DecisionTreeClassifier()
dt.fit(stats_df_2[['median', 'quan_2', 'quan_98', 'skewness', 'dispersion', 'state']], stats_df_2.virus)

visualize_tree(dt, ['median', 'quan_2', 'quan_98', 'skewness', 'dispersion', 'state'])

my_report = sv.analyze( stats_df_2 )
my_report.show_html()

unique_exp_virus = unique( df['exp_virus'] )

for i in range( len( unique_exp_virus ) ):
    exp_var = unique_exp_virus[i]
    df0 = df >> filter( f.exp_virus == exp_var )
    df0.reset_index( inplace = True )
    title_val = exp_var + ',pre_init_diff:' + str( df0['pre_init_diff'][0] ) + ',post_init_diff:' + str( df0['post_init_diff'][0] ) + ',post_pre_diff:' + str( df0['post_pre_diff'][0] )
    fig = px.scatter(df0, y="frequency11", color="state1", title= title_val )
    fig.write_image( 'plots/' + exp_var + '.png' )
