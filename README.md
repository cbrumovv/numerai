# perf+numerai

A R-based performance analytics system for Numerai tournament staking strategies and portfolio analysis.

## Overview

The perf+numerai project automates financial performance calculations that were previously managed in Excel spreadsheets, specifically replacing the manual "Cash_Yield_Ledger.xls" workflow with robust, reproducible R analytics. The system integrates Numerai staking data with traditional asset portfolio management to provide comprehensive performance metrics.

## Features

### Core Analytics
- **Automated Cash Yield Ledger**: Replaces manual Excel calculations with R automation
- **Numerai Staking Analysis**: Performance tracking across multiple staked models
- **Multi-Asset Portfolio Management**: Supports cryptocurrencies, commodities, and ETFs
- **Daily Performance Metrics**: Automated calculation of returns, payouts, and risk metrics
- **Currency-Adjusted Returns**: NMR price-adjusted performance calculations
- **Business Day Adjustments**: Proper handling of weekends and trading calendar

### Data Processing
- **Time Series Analysis**: Daily aggregation and cumulative performance tracking
- **Asset Price Integration**: Automated price data retrieval and processing
- **Investment Blotter Management**: CSV-based transaction tracking
- **Cash Balance Calculations**: Automated cash flow and balance management
- **Risk Metrics**: At-risk calculations and leverage analysis

## Project Structure

```
perf+numerai/
├── cash_yield_ledger.R          # Main analysis script
├── data/                        # Input data directory
│   ├── investments.csv          # Investment transactions
│   ├── fixedincome.csv         # Fixed income data
│   └── cashbalance.csv         # Cash balance history
├── numerai/
│   └── sandbox/
│       ├── staked_models_perfseries.R  # Numerai model definitions
│       └── staked/              # Individual model CSV files
│           ├── v1cdf.csv
│           ├── v1bt.csv
│           └── ...
└── README.md
```

## Dependencies

### Required R Packages
```r
library(glue)           # String interpolation
library(tidyquant)      # Financial data analysis
library(tidyverse)      # Data manipulation and visualization
library(timeperiodsR)   # Time period utilities
library(vctrs)          # Vector tools
```

### External Dependencies
- Calendar settings from `~/Dropbox/Sennosen/apps/src/calendar/calendar_settings.R`
- Performance utilities from `~/Dropbox/Sennosen/apps/src/numerai/performance_utils.R`

## Installation

1. **Clone the repository**:
   ```bash
   git clone <repository-url>
   cd perf+numerai
   ```

2. **Install R dependencies**:
   ```r
   install.packages(c("glue", "tidyquant", "tidyverse", "timeperiodsR", "vctrs"))
   ```

3. **Set up data directories**:
   - Ensure `./data/` directory exists with required CSV files
   - Verify access to external dependency files

## Usage

### Basic Execution

```r
# Run the complete cash yield ledger analysis
source("cash_yield_ledger.R")
```

### Key Configuration

The system uses a configuration list that can be customized:

```r
config <- list(
  persistdir = "~/Desktop/TRADING/",
  datadir = "./data/",
  filenames = list(
    investments = "investments.csv",
    fixedincome = "fixedincome.csv",
    cashbalance = "cashbalance.csv"
  ),
  date_range = list(
    start = as.Date("2020-12-07"),
    end = as.Date("2025-07-25")
  )
)
```

### Asset Configuration

Modify the `asset_config` tibble to add/remove assets or change priorities:

```r
asset_config <- tibble(
  symbol = c("GLD", "ETH-USD", "BTC-USD", "SOL-USD", "MATIC-USD", "NMR-USD", "SPXZ"),
  category = c("commodity", "crypto", "crypto", "crypto", "crypto", "crypto", "etf"),
  active = c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE),
  priority = c(1, 1, 1, 2, 2, 1, 3)
)
```

## Data Requirements

### Input Files

1. **investments.csv**: Investment transaction history
2. **fixedincome.csv**: Fixed income investment data  
3. **cashbalance.csv**: Cash balance history
4. **Numerai Model Files**: Individual CSV files for each staked model in `numerai/sandbox/staked/`

### Expected Format

All CSV files should include a `date` column in YYYY-MM-DD format. Additional columns vary by file type but typically include transaction amounts, balances, and identifiers.

## Output

The system generates several key outputs:

### Data Objects
- `stakedmodel_ts`: Time series data for individual models with daily metrics
- `stakedmodel_by_date`: Daily aggregated data by model with date adjustments
- `cumstakedmodel_by_date`: Cumulative performance metrics in NMR and USD
- `allmodels`: Portfolio-level aggregated performance across all models

### Persisted Files
- `investmentBlotter.rds`: Processed investment data
- `cashBalance.rds`: Processed cash balance data

### Key Metrics
- **Daily Payout**: Daily change in payout amounts
- **Daily Returns**: Daily change in return percentages  
- **Currency-Adjusted Returns**: Returns adjusted for NMR price movements
- **Cumulative Balance**: Cumulative balance including payouts and wallet estimates
- **USD Conversion**: Cumulative balance converted to USD using NMR prices

## Performance

- **Typical Runtime**: ~5 minutes for complete analysis
- **Numerai Data Processing**: ~115.6 seconds for staked model data
- **Memory Requirements**: Varies based on historical data range and number of models

## Architecture

The system follows modern R best practices with:

- **Tidyverse Approach**: Consistent use of dplyr, tidyr, and related packages
- **Functional Programming**: Modular functions with clear inputs/outputs
- **Error Handling**: Robust error handling with informative messages
- **Documentation**: Comprehensive roxygen2 documentation
- **Reproducibility**: Version-controlled configuration and dependencies

## Contributing

### Code Style
- Follow tidyverse style guide
- Use roxygen2 documentation for all functions
- Include examples in function documentation
- Add appropriate error handling and logging

### Testing
- Verify functionality with sample data
- Test edge cases (missing data, empty files, etc.)
- Validate output against known benchmarks

## Troubleshooting

### Common Issues

1. **Missing Dependencies**: Ensure all required packages are installed
2. **File Path Issues**: Verify working directory and file paths
3. **Data Format Issues**: Check CSV file formats and date columns
4. **Memory Issues**: Consider reducing date range for large datasets

### Debug Mode
Set `options(warn = 2)` to convert warnings to errors for debugging.

## License
MIT License

Copyright (c) 2025 Craig Brumfield

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.

## Author

Craig Brumfield  
Created: April 11, 2022  
Last Updated: July 29, 2025

## Changelog

### 2025-07-29
- Replaced nested loops with tidyverse approach for improved performance
- Added comprehensive roxygen2 documentation
- Enhanced error handling and logging
- Improved business day calendar handling

### 2022-04-11
- Initial creation
- Basic cash yield ledger functionality
- Integration with Numerai staking data

---

For questions or support, please open an issue in the GitHub repository.
