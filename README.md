


# Project Title

Do Experimental Schools Drive Housing Prices?

## Project Description

This project examines whether housing prices near public experimental schools in Taipei have increased faster than prices near regular public schools after the schools converted to experimental status. We use 170,551 real estate transactions from 2015 to 2023 combined with geocoded locations of 9 experimental and 103 regular schools to compare price trends within natural catchment zones in the same districts. The difference-in-differences analysis on 5 schools with sufficient data shows that 3 out of 5 (Binjiang +9.9%, Fanghe +4.1%, Xihu +2.2%) had experimental areas appreciating faster than regular areas after conversion.

## Getting Started

Prerequisites

R 4.4.1 or later
RStudio (recommended)

Required R Packages
install.packages(c("tidyverse", "lubridate", "geosphere", "tidygeocoder"))

Download real estate transaction CSVs from plvr.land.moi.gov.tw (非本期下載 tab, 進階下載, select 臺北市 + 不動產買賣, quarters 104Q1 through 112Q4)
Download school directories from data.gov.tw/dataset/6087 (elementary) and data.gov.tw/dataset/6089 (junior high)
Place all files in the data/ folder

## File Structure

├── README.md
├── data/
│   ├── 04-1.csv ... 12-4.csv      # 36 quarterly real estate transaction files
│   ├── e1_new.csv                   # Elementary school directory
│   ├── high.csv                     # Junior high school directory
│   ├── road_coordinates.csv         # Geocoded road coordinates (generated)
│   └── school_coordinates.csv       # Geocoded school coordinates (generated)
├── scripts/
│   ├── analysis.R                   # Script 1: Data loading, cleaning, district-level analysis
│   └── proximity_analysis_v2.R      # Script 2: Proximity-based analysis with geocoding
├── plots/
│   ├── plot7_proximity_comparison.png
│   ├── plot10_matched_proximity.png
│   ├── plot11_per_school_premium.png
│   ├── plot12_per_school_before_after.png
│   └── plot13_per_school_yearly.png
├── output/
│   ├── Final_Paper.docx             # Final report
│   └── Research_Poster.pdf          # A1 research poster

## Analysis

Methods

Data Cleaning: Combined 36 quarterly CSV files (212,905 raw transactions) into a single dataset. Parsed ROC dates, converted types, filtered invalid entries. Final cleaned dataset: 170,551 transactions.
Geocoding: Experimental school coordinates obtained manually from Google Maps. Regular school addresses geocoded using Google Maps Geocoding API. Property locations approximated via road-level geocoding using OSM Nominatim (1,509 of 1,649 roads, 92% success rate).
Catchment Zones: For each property, calculated Haversine distance to nearest experimental and nearest regular school within the same district. Properties tagged based on which school is closer, creating natural catchment zones.
Comparison: Overall proximity comparison, per-school price premium, matched apartment comparison (2-4 room apartments only), and difference-in-differences using each school's conversion date.


## Results

At the overall level, properties near experimental and regular schools show similar price trends within the same district, with the gap narrowing over time.
All 5 analyzed schools show negative absolute price premiums (experimental areas are cheaper), ranging from -2% (Xihu) to -33% (Quanyuan).
However, the difference-in-differences analysis shows 3 out of 5 schools with experimental areas appreciating faster after conversion:

Binjiang: +9.9 percentage points
Fanghe: +4.1 percentage points
Xihu: +2.2 percentage points


Two schools show negative DiD: Bojia (-9.2%) and Quanyuan (-30.2%), likely due to peripheral/mountain locations with sparse transaction data.
The effect appears location-dependent: urban, accessible schools show positive effects while remote schools do not.
Four schools (Xishan, Hutian, Minzu, Zhinan) were excluded due to insufficient transactions within the 1km radius.

## Contributors

Radek Matloch (113266016): Data collection, data cleaning, geocoding, proximity analysis, visualization, report writing
Bernard Kwizera (114266019): Project member

## Acknowledgments

Professor Chung-pei Pien (ICI, NCCU) for project guidance, topic suggestion, and feedback throughout the semester

## References

Ministry of the Interior, Taiwan. Real Price Registration System. plvr.land.moi.gov.tw
K12 Education Administration, Ministry of Education. k12ea.gov.tw
Ministry of Education, Taiwan. School Directory Data. data.gov.tw/dataset/6087, data.gov.tw/dataset/6089
Wickham, H. et al. (2019). Welcome to the tidyverse. Journal of Open Source Software, 4(43), 1686.
Hijmans, R. J. (2022). geosphere: Spherical Trigonometry. R package.
