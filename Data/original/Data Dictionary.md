# Original Datasets

[Link to archived datasets](https://1drv.ms/f/s!AidNWjxCQLvGlGqTcG3Mksmakbde?e=wEYwLk)

[Link to unified dataset](https://1drv.ms/u/s!AtU8phbft0vdgt89NMAAnpJpP_AGsw?e=wVv0kD)

## Ember Electricity

This table excludes some columns that may not be of primary interest.

| Column Name(s) | Description | Data Type | Missing Values |
| -------------- | ----------- | --------- | -------------- |
| Area | Area/region name. Corresponds to country name when *Area type* is "Country" | Text | No |
| Country code | ISO 3166-1 alpha-3 country code | Text | Yes, when *Area type* is "Region", but all countries have a code.  |
| Year | 4 digit year, 2000-2022 | Numeric, discrete | No |
| Area type | Type of area: "Country" or "Region" | Text | No
| Continent | Continent in which country is located | Text | Yes, when *Area type* is "Region", but all countries have a continent. |
| EU, OECD, G20, G7, ASEAN | Is country in specified alliance/organization | Binary, indicator | Yes, when *Area type* is "Region", but all countries have either 0 or 1. |
| Category | Type of measure for this observation's *Value* | Text | No |
| Subcategory | Subcategory for this observation's type of measure. E.g., "Aggregate fuel" for a measure of multiple fuel types, vs "Fuel" for a measure of a specific fuel type. | Text | No |
| Variable | What specifically is measured for this observation. E.g, fuel or aggregate fuel type, demand, total generation. | Text | Yes |
| Unit | Unit of measure for associated *Value*. E.g., GW, MWh, %. | Text | No
| Value | Value of measure specified by *Category*, *Subcategory*, and *Variable* | Numeric, continuous | Yes |

## GDP

| Column Name | Description | Data Type | Missing Values |
| ----------- | ----------- | --------- | -------------- |
| Entity | Country, region, or descriptive aggregate category name. E.g., "Algeria", "North America (WB)", or "Middle-income countries". | Text | No |
| Code | ISO 3166-1 alpha-3 country code | Text | Yes, when *Entity* is a region or aggregate", but all individual countries have a code.  |
| Year | 4 digit year, 2000-2022 | Numeric, discrete | No |
| GDP per capita, PPP (constant 2017 international $) | *Entity*'s GDP at purchaing power parity (PPP) converted to 2017 international dollars | Numeric, continuous | No |

## HDI

### HDR21-22_Composite_indices_complete_time_series.csv

This is HDI data in time series format, where each year corresponds to a column. The table below excludes some columns that are not of primary interest.

| Column Name(s) | Description | Data Type | Missing Values |
| -------------- | ----------- | --------- | -------------- |
| iso3 | ISO 3166-1 alpha-3 country code, or code for a region described by the *country* column value | Text | No |
| country | Country, region, or descriptive aggregate category name. E.g., "China", "Arab States", or "High human development". | No |
| hdicode | HDI classification from "low" to "very high". | Text | Yes, some countries and all non-country rows. | 
| region | Abbreviation for specific region to which a country may belong | Text | Yes |
| hdi_rank_2021 | Position of country in 2021 HDI rank | Numeric, ordinal | Yes |
| hdi_1990, hdi_1991, hdi_1992, etc. | HDI of given *country* from 1990 to 2021 | Numeric, continuous | Yes |
