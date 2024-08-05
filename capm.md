# CAPM Analysis

## Introduction

In this assignment, you will explore the foundational concepts of the Capital Asset Pricing Model (CAPM) using historical data for AMD and the S&P 500 index. This exercise is designed to provide a hands-on approach to understanding how these models are used in financial analysis to assess investment risks and returns.

## Background

The CAPM provides a framework to understand the relationship between systematic risk and expected return, especially for stocks. This model is critical for determining the theoretically appropriate required rate of return of an asset, assisting in decisions about adding assets to a diversified portfolio.

## Objectives

1.  **Load and Prepare Data:** Import and prepare historical price data for AMD and the S&P 500 to ensure it is ready for detailed analysis.
2.  **CAPM Implementation:** Focus will be placed on applying the CAPM to examine the relationship between AMD's stock performance and the overall market as represented by the S&P 500.
3.  **Beta Estimation and Analysis:** Calculate the beta of AMD, which measures its volatility relative to the market, providing insights into its systematic risk.
4.  **Results Interpretation:** Analyze the outcomes of the CAPM application, discussing the implications of AMD's beta in terms of investment risk and potential returns.

## Instructions

### Step 1: Data Loading

-   We are using the `quantmod` package to directly load financial data from Yahoo Finance without the need to manually download and read from a CSV file.
-   `quantmod` stands for "Quantitative Financial Modelling Framework". It was developed to aid the quantitative trader in the development, testing, and deployment of statistically based trading models.
-   Make sure to install the `quantmod` package by running `install.packages("quantmod")` in the R console before proceeding.

```{r load-data}
# Set start and end dates
start_date <- as.Date("2019-05-20")
end_date <- as.Date("2024-05-20")

# Load data for AMD, S&P 500, and the 1-month T-Bill (DTB4WK)
amd_data <- getSymbols("AMD", src = "yahoo", from = start_date, to = end_date, auto.assign = FALSE)
gspc_data <- getSymbols("^GSPC", src = "yahoo", from = start_date, to = end_date, auto.assign = FALSE)
rf_data <- getSymbols("DTB4WK", src = "FRED", from = start_date, to = end_date, auto.assign = FALSE)

# Convert Adjusted Closing Prices and DTB4WK to data frames
amd_df <- data.frame(Date = index(amd_data), AMD = as.numeric(Cl(amd_data)))
gspc_df <- data.frame(Date = index(gspc_data), GSPC = as.numeric(Cl(gspc_data)))
rf_df <- data.frame(Date = index(rf_data), RF = as.numeric(rf_data[,1]))  # Accessing the first column of rf_data

# Merge the AMD, GSPC, and RF data frames on the Date column
df <- merge(amd_df, gspc_df, by = "Date")
df <- merge(df, rf_df, by = "Date")
```

#### Data Processing

```{r data}
colSums(is.na(df))
# Fill N/A RF data
df <- df %>%
  fill(RF, .direction = "down") 
```

### Step 2: CAPM Analysis

The Capital Asset Pricing Model (CAPM) is a financial model that describes the relationship between systematic risk and expected return for assets, particularly stocks. It is widely used to determine a theoretically appropriate required rate of return of an asset, to make decisions about adding assets to a well-diversified portfolio.

#### The CAPM Formula

The formula for CAPM is given by:

$$ E(R_i) = R_f + \beta_i (E(R_m) - R_f) $$

Where:

-   $E(R_i)$ is the expected return on the capital asset,
-   $R_f$ is the risk-free rate,
-   $\beta_i$ is the beta of the security, which represents the systematic risk of the security,
-   $E(R_m)$ is the expected return of the market.

#### CAPM Model Daily Estimation

-   **Calculate Returns**: First, we calculate the daily returns for AMD and the S&P 500 from their adjusted closing prices. This should be done by dividing the difference in prices between two consecutive days by the price at the beginning of the period. $$
    \text{Daily Return} = \frac{\text{Today's Price} - \text{Previous Trading Day's Price}}{\text{Previous Trading Day's Price}}
    $$

```{r return}

# Calculate Daily Returns for AMD

#Set all values in the new column AMD_return as NA first
df$AMD_return <- NA

#For loop to calculate the daily returns 
for (i in 2:nrow(df)) {
  if (!is.na(df$AMD[i]) && !is.na(df$AMD[i - 1]) && df$AMD[i - 1] != 0) {
    df$AMD_return[i] <- (df$AMD[i] - df$AMD[i - 1]) / df$AMD[i - 1]
  }
}

# Calculate Daily Returns for S&P 500
df$GSPC_return <- NA

for (i in 2:nrow(df)) {
  if (!is.na(df$GSPC[i]) && !is.na(df$GSPC[i - 1]) && df$GSPC[i - 1] != 0) {
    df$GSPC_return[i] <- (df$GSPC[i] - df$GSPC[i - 1]) / df$GSPC[i - 1]
  }
}
```

-   **Calculate Risk-Free Rate**: Calculate the daily risk-free rate by conversion of annual risk-free Rate. This conversion accounts for the compounding effect over the days of the year and is calculated using the formula: $$
    \text{Daily Risk-Free Rate} = \left(1 + \frac{\text{Annual Rate}}{100}\right)^{\frac{1}{360}} - 1
    $$

```{r riskfree}

# Calculate daily risk-free rate
df$RF_daily <- (1 + df$RF/100)^(1/360) - 1
```

-   **Calculate Excess Returns**: Compute the excess returns for AMD and the S&P 500 by subtracting the daily risk-free rate from their respective returns.

```{r excess return}
df$AMD_excess_return <- NA
df$GSPC_excess_return <- NA

for (i in 2:nrow(df)) {
  # Calculate excess returns for AMD
  df$AMD_excess_return[i] <- df$AMD_return[i] - df$RF_daily[i]
  
  # Calculate excess returns for S&P 500
  df$GSPC_excess_return[i] <- df$GSPC_return[i] - df$RF_daily[i]
}
```

-   **Perform Regression Analysis**: Using linear regression, we estimate the beta ($\beta$) of AMD relative to the S&P 500. Here, the dependent variable is the excess return of AMD, and the independent variable is the excess return of the S&P 500. Beta measures the sensitivity of the stock's returns to fluctuations in the market.

```{r lm}
# Perform linear regression
model1 <- lm(AMD_excess_return ~ GSPC_excess_return, data = df)

# Extract key statistics
beta <- coef(model1)[2]
alpha <- coef(model1)[1]
r_squared <- summary(model1)$r.squared

print(beta)
```

#### Interpretation

What is your $\beta$? Is AMD more volatile or less volatile than the market?

**Answer:**

Based on our CAPM analysis, AMD's beta ($\beta$) of 1.569999 indicates that the stock is more volatile than the overall market. This means that, on average, for every 1% movement in the S&P 500 index, AMD's stock price tends to move by a larger percentage. This higher volatility suggests that AMD carries greater systematic risk compared to the market. For investors, this implies that AMD may offer the potential for higher returns in bullish markets but also greater losses in bearish conditions. Understanding this risk profile is crucial for portfolio construction and risk management strategies. Investors considering AMD should be prepared for potentially larger price swings compared to the broader market and should factor this higher volatility into their investment decisions and risk tolerance assessments.

#### Plotting the CAPM Line

Plot the scatter plot of AMD vs. S&P 500 excess returns and add the CAPM regression line.

```{r plot}
# Plot the regression line
ggplot(subset(df, !is.na(GSPC_excess_return)), aes(x = GSPC_excess_return, y = AMD_excess_return)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "red", formula = y ~ x) +
  labs(title = "CAPM Regression: AMD vs S&P 500",
       x = "S&P 500 Excess Return",
       y = "AMD Excess Return") +
  theme_minimal() +
  annotate("text", x = min(df$GSPC_excess_return, na.rm = TRUE), 
           y = max(df$AMD_excess_return, na.rm = TRUE),
           label = paste("Beta =", round(beta, 6), "\n",
                         "Alpha =", round(alpha, 6), "\n",
                         "R-squared =", round(r_squared, 6)),
           hjust = 0, vjust = 1) +
  coord_cartesian(clip = 'off')
```

### Step 3: Predictions Interval

Suppose the current risk-free rate is 5.0%, and the annual expected return for the S&P 500 is 13.3%. Determine a 90% prediction interval for AMD's annual expected return.

*Hint: Calculate the daily standard error of the forecast (*$s_f$), and assume that the annual standard error for prediction is $s_f \times \sqrt{252}$. Use the simple return average method to convert daily stock returns to annual returns if needed.

**Answer:**

```{r pi}
# Given information
rf_annual <- 0.05  # 5% risk-free rate
rm_annual <- 0.133  # 13.3% expected market return
trading_days <- 252
k <- 1

n <- length(model1$fitted.values)
X_bar <- mean(df$GSPC_excess_return, na.rm = TRUE) * trading_days

rm_daily = 0.133 / trading_days
rf_daily = (1+rf_annual)^(1/360) - 1
X_f <- rm_annual-rf_annual

# Extract beta and calculate expected return for AMD
beta <- coef(model1)[2]
expected_return_amd <- rf_annual + beta * (rm_annual - rf_annual)

# Calculate Sum of Square Errors (SSE)
SSE <- sum(residuals(model1)^2, na.rm = TRUE)

# Calculate daily standard error of the estimate (from the data)
se_daily <- sqrt(SSE/(n-2))

# Convert to annual standard error of the estimate
se_annual <- se_daily * sqrt(252)
sf_annual <- se_annual*sqrt(1+1/n+(X_f - X_bar)^2/SSE)
                            
# Calculate 90% prediction interval
t_score <- qt(0.95, df=n-2)  # for 90% interval
lower_bound <- expected_return_amd - t_score * sf_annual
upper_bound <- expected_return_amd + t_score * sf_annual
cat("Confidence Interval: [", lower_bound, " ", upper_bound, "]")
```
