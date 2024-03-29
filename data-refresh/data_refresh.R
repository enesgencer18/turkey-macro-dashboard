print("Loading new data...")
print("Raw data is started to be updated!")
source('./raw_data/raw_data.R')
print("Raw data is updated!")
source('./processed_data/data_processing.R')
print("Data is up to date")
print("Predictive analysis is started")
source('./analysis/ml-flow.R')
print("Predictive analysis is done!")
print("Rendering dashboard")
rmarkdown::render_site()
print("Dashboard is rendered successfully")
