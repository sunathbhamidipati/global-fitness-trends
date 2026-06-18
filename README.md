# Global Fitness Trends: A Spatial Analytics Dashboard

[![Live Application](https://img.shields.io/badge/Live_Dashboard-shinyapps.io-blue?style=for-the-badge)](https://sunathb.shinyapps.io/SunathB_global_fitness/)

An R Shiny spatial dashboard tracking digital fitness intent across 80 countries. This project integrates global health metrics, urbanisation data, and behavioral search trends to visualize how physical infrastructure and baseline health dictate worldwide exercise habits.

## 🚀 The Problem & Impact
Digital health platforms are experiencing unprecedented growth, but motivation alone does not dictate fitness behavior. This dashboard helps public health planners and urban strategists explore the complex relationships between digital fitness interest and actual clinical health outcomes. By transitioning users from a macro-level spatial overview down to micro-level, country-specific metrics, it exposes the structural and infrastructural barriers shaping global wellness.

## 🔍 Key Discoveries
* **The Urbanisation Paradox:** Counterintuitively, higher national search volumes for facility-based workouts ("Gym") correlate positively with higher national obesity and inactivity rates. It highlights the sedentary reality of highly developed nations where fitness is a scheduled indoor activity rather than an integrated daily routine.
* **The Urban Equipment Divide:** Foundational cardiovascular intent (running and cycling) is completely decoupled from a nation's health baseline. Instead, hyper-urbanisation acts as a filter: mega-cities heavily favor zero-equipment running over cycling due to severe physical infrastructure and safety barriers.

## 🏗️ Technical Architecture
This project is built on a decoupled architecture, utilizing Python for heavy data engineering and R Shiny for the reactive frontend.

* **Frontend / Visualization:** R, Shiny, Leaflet (Geospatial), Plotly, ggplot2
* **Data Engineering:** Python, Pandas, Pycountry
* **Data Sources:** World Health Organization (WHO), World Bank, Google Trends, Natural Earth

### Repository Structure
```text
global-fitness-trends/
├── app/                  # The main R Shiny application and interactive UI/Server logic
├── data_pipeline/        # Python scripts for data extraction, wrangling, and quality checks
├── processed/            # Final harmonized datasets (e.g., final_global_fitness_data.csv, clean_global_map.rds)
├── raw/                  # Original unprocessed CSVs from WHO, World Bank, and Google Trends
└── README.md             # Project documentation
