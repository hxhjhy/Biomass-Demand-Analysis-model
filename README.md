# Biomass-Demand-Analysis-model
data and code for the paper A machine learning estimation reveals/uncover massive biomass consumption uncounted in China

code.py is the main script of code for the paper
data contains underlying critical data necessary to reproduce analysis and research process
footprint_calc.py is a function to actually compute energy footprints via power laws
gini.py computes the gini coefficient
several csv files (one xlsx file) that are directly loaded into the mainscript (so they are necessary to run the script also) and that are sometimes not our primary data but adjusted data by other openly accessible sources. The credit for this data thus is completely due to their authors which are cited in the paper. But the data is uploaded here so that our work is entirely reproducible. Listed in the following
cumulativepincome_alv ||| necessary for Figure 1 population vector of adjusted Alvaredo data ||| originating from World Inequality Lab/ World Inequality Report 2018
incomeranked_alv ||| to 1. corresponding income vector
laknercumpop ||| necessary for Figure 1 ||| original from Lakner and Milanovic https://openknowledge.worldbank.org/handle/10986/16935
laknerincomeranked ||| to 3. corresponding income vector
inequality_metrics_fig4 ||| data to reproduce fig.4
expenditure_real_world.xlsx || data to compare expenditure distributions real world vs. model
