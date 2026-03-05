# Leiden Ranking Data Analysis

This project involves creating an application in R using Shiny, a system for developing web applications with R. We use it to graphically present analyses published by the Leiden Ranking on universities worldwide.

The CWTS Leiden Ranking is an annual global university ranking based exclusively on bibliometric indicators. It provides important information on the scientific performance of more than 1000 major universities worldwide through various indicators.

We use the latest results published by the Leiden Ranking in 2020 for our analyses, but through our application, you can update quickly with each new data release from them, without needing to modify the code, just by processing the CSV into R data format (Rds).

## Technologies

- **R**: Programming language for data analysis.
- **Shiny**: Framework for creating interactive web applications in R.
- **Main Libraries**:
  - `tidyverse`: For data manipulation and visualization (includes dplyr, ggplot2, etc.).
  - `leaflet`: For interactive maps.
  - `plotly`: For interactive charts.
  - `data.table`: For efficient data manipulation.
  - `stringr`: For string manipulation.
  - `htmlTable`: For HTML tables.
  - `ggthemes`: For chart themes.
  - `shinythemes`: For Shiny interface themes.
  - Others: `tidyr`, `httr`, `lubridate`, `knitr`.

## How to Use?

1. **Prerequisites**:
   - Install R (version 4.0 or higher) from [cran.r-project.org](https://cran.r-project.org/).
   - Install RStudio (optional, but recommended) from [rstudio.com](https://www.rstudio.com/products/rstudio/download/).
   - Install the Shiny package: Run `install.packages("shiny")` in the R console.

2. **Installing Dependencies**:
   - Open the project in RStudio (file `LeidenRankingR.Rproj`).
   - Run the package installation script or manually install the libraries listed above using `install.packages()`.

3. **Running the Application**:
   - Navigate to the `shiny/` folder.
   - Open `ui.R` or `server.R` in RStudio.
   - Click "Run App" in the top right corner, or run `shiny::runApp()` in the console.

4. **Updating Data**:
   - Download new data from the Leiden Ranking in CSV format.
   - Convert to Rds format using scripts like `Code.R` or `Leiden.R`.
   - Replace the files in the `dados/` or `shiny/` folders.


## Contribution

Contributions are welcome! To contribute:

1. Fork the repository.
2. Create a branch for your feature (`git checkout -b feature/new-feature`).
3. Commit your changes (`git commit -am 'Add new feature'`).
4. Push to the branch (`git push origin feature/new-feature`).
5. Open a Pull Request.

To report bugs or suggest improvements, open an issue in the repository.

## License
[MIT](https://choosealicense.com/licenses/mit/)