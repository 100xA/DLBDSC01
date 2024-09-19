# World Bank Data Analysis Project

This R script analyzes socioeconomic indicators from the World Bank database for 2000-2019.

## Features

- Fetches data from World Bank API
- Cleans and filters data
- Interpolates missing values
- Visualizes data before and after interpolation
- Calculates correlations between indicators
- Creates a scatter plot

## Requirements

- R
- Packages:
  - dplyr
  - tidyr
  - wbstats
  - ggplot2

Install packages:

```r
install.packages(c("dplyr", "tidyr", "wbstats", "ggplot2"))
```

## Usage

1. Ensure all required packages are installed.
2. Load the R script file into your R environment.
3. Run the script.

## Output

- PNG files with data visualizations
- Correlation matrix of selected indicators
- Scatter plot showing relationship between education and GDP per capita

## Customization

Modify the `indicators` vector at the beginning of the script to analyze different World Bank indicators:

```r
indicators <- c(
    "NY.GDP.PCAP.CD", # GDP per capita
    "SP.DYN.TFRT.IN", # Fertility rate
    "SP.DYN.LE00.IN", # Life expectancy
    "SE.SEC.CUAT.PO.ZS", # Post-secondary education
    "SL.TLF.ACTI.ZS", # Labor force participation
    "SH.XPD.CHEX.PC.CD" # Health expenditure per capita
)
```

## Note

Data quality may vary. The script filters out countries with insufficient data and uses linear interpolation, which may lead to estimation inaccuracies in some cases.

## Contributing

Feel free to fork this project and submit pull requests with improvements or use it as a base for your own analyses.
