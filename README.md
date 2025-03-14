# 5243project2_team6
# BlueStats: A data analysis app using R shiny

## Project Overview
This is an class project for Columbia University project for GRSTAT5243. 
This app lets you easily upload, explore, and analyze datasets. 
Perform data cleaning, handle outliers, engineer features, and create interactive visualizations—all in one place.

### Collaborators
- Ziyue Gao (ziyuegao0302)  
- Keito Taketomi (kt2849)  
- Anqi Wu (Owner of this repo, WAWQAQMAKABAKA)  
- Yixin Xiao (yx2888)  

### Repository Structure
#### Data Files
- **AB_NYC_2019.csv**: A big complex dataset to test on, which is from New York City Airbnb Open Data. Original dataset from [[source](https://www.kaggle.com/datasets/dgomonov/new-york-city-airbnb-open-data)]  

#### App file
- **app.R**
- **data analysis suite.R**
- **app_314.R**
- **please refer ### for final**

#### Report file
- **5243_Project 2_Team6.pdf**
- **5243_Project 2_Team6 - Google Docs.pdf**
- **please refer ### for final**

### Project Objectives
1. Data cleaning and preprocessing:
   - Handle missing values
   - Address data inconsistencies
   - Feature normalization

2. Exploratory Analysis:
   - Correlation analysis
   - Feature distributions
   - Pattern identification

3. Modeling:
   - Linear regression
   - Performance evaluation

### Installation & Setup
```bash
# Clone repository
git clone https://github.com/WAWQAQMAKABAKA/5243project2_team6.git

# Install R dependencies
Rscript -e "install.packages(c('shiny', 'shinythemes', 'DT', 'tidyverse', 'colourpicker', 'data.table', 'readxl', 'jsonlite', 'corrplot', 'palmerpenguins', 'mgcv'))"

# Run the app
cd 5243project2_team6
Rscript -e "shiny::runApp('app_314.R', port=3838)" #any other ports if it is already in use

```

### License
This project is for educational purposes and is licensed under the MIT License.<br>
