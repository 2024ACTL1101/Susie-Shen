```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
## Algorithmic Trading Strategy

## Introduction

In this assignment, you will develop an algorithmic trading strategy by incorporating financial metrics to evaluate its profitability. This exercise simulates a real-world scenario where you, as part of a financial technology team, need to present an improved version of a trading algorithm that not only executes trades but also calculates and reports on the financial performance of those trades.

## Background

Following a successful presentation to the Board of Directors, you have been tasked by the Trading Strategies Team to modify your trading algorithm. This modification should include tracking the costs and proceeds of trades to facilitate a deeper evaluation of the algorithm’s profitability, including calculating the Return on Investment (ROI).
  
After meeting with the Trading Strategies Team, you were asked to include costs, proceeds, and return on investments metrics to assess the profitability of your trading algorithm.

## Objectives

1.  **Load and Prepare Data:** Open and run the starter code to create a DataFrame with stock closing data.

2.  **Implement Trading Algorithm:** Create a simple trading algorithm based on daily price changes.

3.  **Customize Trading Period:** Choose your entry and exit dates.

4.  **Report Financial Performance:** Analyze and report the total profit or loss (P/L) and the ROI of the trading strategy.

5.  **Implement a Trading Strategy:** Implement a trading strategy and analyze the total updated P/L and ROI.

6.  **Discussion:** Summarise your finding.

## Instructions

### Step 1: Data Loading

Start by running the provided code cells in the "Data Loading" section to generate a DataFrame containing AMD stock closing data. This will serve as the basis for your trading decisions. First, create a data frame named `amd_df` with the given closing prices and corresponding dates.

```{r load-data}

# Load data from CSV file
amd_df <- read.csv("AMD.csv")

# Convert the date column to Date type and Adjusted Close as numeric
amd_df$date <- as.Date(amd_df$Date)
amd_df$close <- as.numeric(amd_df$Adj.Close)

amd_df <- amd_df[, c("date", "close")]
```

##Plotting the Data Plot the closing prices over time to visualize the price movement.

```{r plot}
plot(amd_df$date, amd_df$close,'l')
```

## Step 2: Trading Algorithm

Implement the trading algorithm as per the instructions. You should initialize necessary variables, and loop through the dataframe to execute trades based on the set conditions.

-   Initialize Columns: Start by ensuring dataframe has columns 'trade_type', 'costs_proceeds' and 'accumulated_shares'.
-   Change the algorithm by modifying the loop to include the cost and proceeds metrics for buys of 100 shares. Make sure that the algorithm checks the following conditions and executes the strategy for each one:
    -   If the previous price = 0, set 'trade_type' to 'buy', and set the 'costs_proceeds' column to the current share price multiplied by a `share_size` value of 100. Make sure to take the negative value of the expression so that the cost reflects money leaving an account. Finally, make sure to add the bought shares to an `accumulated_shares` variable.
    -   Otherwise, if the price of the current day is less than that of the previous day, set the 'trade_type' to 'buy'. Set the 'costs_proceeds' to the current share price multiplied by a `share_size` value of 100.
    -   You will not modify the algorithm for instances where the current day’s price is greater than the previous day’s price or when it is equal to the previous day’s price.
    -   If this is the last day of trading, set the 'trade_type' to 'sell'. In this case, also set the 'costs_proceeds' column to the total number in the `accumulated_shares` variable multiplied by the price of the last day.

```{r trading}
# Initialize columns for trade type, cost/proceeds, and accumulated shares in amd_df
amd_df$trade_type <- NA
amd_df$costs_proceeds <- NA  # Corrected column name
amd_df$accumulated_shares <- 0  # Initialize if needed for tracking

# Initialize variables for trading logic
previous_price <- 0
share_size <- 100
accumulated_shares <- 0

for (i in 1:nrow(amd_df)) {
 current_price <- amd_df$close[i]
  
  # If the previous price is 0
  if (previous_price == 0) {
    amd_df$trade_type[i] <- 'buy'
    amd_df$costs_proceeds[i] <- -(current_price * share_size)
    accumulated_shares <- accumulated_shares + share_size
    
    # If the current price is less than the previous day's price
  } else if (current_price < previous_price) {
    amd_df$trade_type[i] <- 'buy'
    amd_df$costs_proceeds[i] <- -(current_price * share_size)
    accumulated_shares <- accumulated_shares + share_size
  }
  
  # If it is the last day of trading
  if (i == nrow(amd_df)) {
    amd_df$trade_type[i] <- 'sell'
    amd_df$costs_proceeds[i] <- accumulated_shares * current_price
  }
  
  # Update the accumulated shares in the data frame
  amd_df$accumulated_shares[i] <- accumulated_shares
  
  if(i == nrow(amd_df)) {
    amd_df$accumulated_shares[i] <- 0
  }
  # Update the previous price for the next iteration
  previous_price <- current_price
}
```

## Step 3: Customize Trading Period

-Define a trading period you wanted in the past five years

```{r period}

# Define dates
start_date <- as.Date('2024-01-01')
end_date <- as.Date('2024-05-17')

# Filter the data to include only the trading period
amd_df <- amd_df[amd_df$date >= start_date & amd_df$date <= end_date, ]

# Initialize columns for trade type, cost/proceeds, and accumulated shares in amd_df
amd_df$trade_type <- NA
amd_df$costs_proceeds <- NA  # Corrected column name
amd_df$accumulated_shares <- 0  # Initialize if needed for tracking

# Initialize variables for trading logic
previous_price <- 0
share_size <- 100
accumulated_shares <- 0

for (i in 1:nrow(amd_df)) {
 current_price <- amd_df$close[i]
  
  # If the previous price is 0
  if (previous_price == 0) {
    amd_df$trade_type[i] <- 'buy'
    amd_df$costs_proceeds[i] <- -(current_price * share_size)
    accumulated_shares <- accumulated_shares + share_size
    
    # If the current price is less than the previous day's price
  } else if (current_price < previous_price) {
    amd_df$trade_type[i] <- 'buy'
    amd_df$costs_proceeds[i] <- -(current_price * share_size)
    accumulated_shares <- accumulated_shares + share_size
  }
  
  # If it is the last day of trading
  if (i == nrow(amd_df)) {
    amd_df$trade_type[i] <- 'sell'
    amd_df$costs_proceeds[i] <- accumulated_shares * current_price
  }
  
  # Update the accumulated shares in the data frame
  amd_df$accumulated_shares[i] <- accumulated_shares
  
   if(i == nrow(amd_df)) {
    amd_df$accumulated_shares[i] <- 0
   }
  
  # Update the previous price for the next iteration
  previous_price <- current_price
}

# A plot is provided to visualise the flunctions in stock prices for the new trading period 
plot(amd_df$date, amd_df$close,'l')
```

## Step 4: Run Your Algorithm and Analyze Results

After running your algorithm, check if the trades were executed as expected. Calculate the total profit or loss and ROI from the trades.

-   Total Profit/Loss Calculation: Calculate the total profit or loss from your trades. This should be the sum of all entries in the 'costs_proceeds' column of your dataframe. This column records the financial impact of each trade, reflecting money spent on buys as negative values and money gained from sells as positive values.
-   Invested Capital: Calculate the total capital invested. This is equal to the sum of the 'costs_proceeds' values for all 'buy' transactions. Since these entries are negative (representing money spent), you should take the negative sum of these values to reflect the total amount invested.
-   ROI Formula: $$\text{ROI} = \left( \frac{\text{Total Profit or Loss}}{\text{Total Capital Invested}} \right) \times 100$$

```{r}
#Initialise the variables
total_investment_capital <- 0
# for loop to calculate the sum of investment capital by summing all the trades
#that has trade_type as 'buy',  removing NA values 
for (i in 1:nrow(amd_df)) {
  if (!is.na(amd_df$trade_type[i]) && amd_df$trade_type[i] == 'buy') {
    total_investment_capital <- total_investment_capital + amd_df$costs_proceeds[i]
  }
}

#the original total_investment_capital is a negative number, to reflect the 
#total amount invested, the amount is the negative of the original result
total_investment_capital <- - total_investment_capital
# Sum the cost_proceeds column, removing NA values to find the total profit/loss
Profit_Loss  <- sum(amd_df$costs_proceeds, na.rm = TRUE)
ROI = (Profit_Loss/total_investment_capital)*100

#The resulting ROI:
ROI 
#The resulting Profit/loss:
Profit_Loss
```

## Step 5: Profit-Taking Strategy or Stop-Loss Mechanisum (Choose 1)

-   Option 1: Implement a profit-taking strategy that you sell half of your holdings if the price has increased by a certain percentage (e.g., 20%) from the average purchase price.
-   Option 2: Implement a stop-loss mechanism in the trading strategy that you sell half of your holdings if the stock falls by a certain percentage (e.g., 20%) from the average purchase price. You don't need to buy 100 stocks on the days that the stop-loss mechanism is triggered.

```{r option}

# Initialize variables for trading logic
previous_price <- 0
share_size <- 100
accumulated_shares <- 0
total_purchase_price <- 0
number_of_purchase <- 0

# Initialize columns
amd_df$accumulated_shares <- 0
amd_df$average_purchase_price <- NA

for (i in 1:nrow(amd_df)) {
  current_price <- amd_df$close[i]

  
  # Calculate the average purchase price and update accumulated shares in the dataframe
  if (i > 1) {
    amd_df$average_purchase_price[i] <- amd_df$average_purchase_price[i-1] # Use previous day's value if not updated
    amd_df$accumulated_shares[i] <- amd_df$accumulated_shares[i-1]  # Use previous day's value if not updated
  }
  

  # If the previous price is 0
  if (previous_price == 0) {
    amd_df$trade_type[i] <- 'buy'
    amd_df$costs_proceeds[i] <- -(current_price * share_size)
    accumulated_shares <- accumulated_shares + share_size
    total_purchase_price <- total_purchase_price + current_price
    number_of_purchase <- number_of_purchase + 1

  
  # If it is the last day of trading
  } else if (i == nrow(amd_df)) {
    amd_df$trade_type[i] <- 'sell'
    accumulated_shares <- 0
    # Implement profit-taking strategy
  } else if (!is.na(amd_df$average_purchase_price[i]) && current_price > amd_df$average_purchase_price[i] * 1.2) {
    amd_df$trade_type[i] <- 'sell_half'
    amd_df$costs_proceeds[i] <- (current_price * accumulated_shares) / 2  # Sell half the holdings
    accumulated_shares <- accumulated_shares / 2  # Update accumulated shares

  # If the current price is less than the previous day's price
  } else if (current_price < previous_price) {
    amd_df$trade_type[i] <- 'buy'
    amd_df$costs_proceeds[i] <- -(current_price * share_size)
    accumulated_shares <- accumulated_shares + share_size
    total_purchase_price <- total_purchase_price + current_price
    number_of_purchase <- number_of_purchase + 1
  } 

  # Calculate the average purchase price and update accumulated shares in the 
  #data frame for the end of this iteration
  if (number_of_purchase > 0) {
    average_purchase_price <- total_purchase_price / number_of_purchase
    amd_df$average_purchase_price[i] <- average_purchase_price
  } else {
    amd_df$average_purchase_price[i] <- NA
  }
  
  
  amd_df$accumulated_shares[i] <- accumulated_shares

  
  # Update the previous price for the next iteration
  previous_price <- current_price
}

total_investment_capital <- 0
# for loop to calculate the sum of investment capital by summing all the trades 
#that has trade_type as 'buy',  removing NA values 
for (i in 1:nrow(amd_df)) {
  if (!is.na(amd_df$trade_type[i]) && amd_df$trade_type[i] == 'buy') {
    total_investment_capital <- total_investment_capital + amd_df$costs_proceeds[i]
  }
}

#the original total_investment_capital is a negative number, to reflect the 
#total amount invested, the amount is the negative of the original result
total_investment_capital <- - total_investment_capital
# Sum the cost_proceeds column, removing NA values to find the total profit/loss
Profit_Loss  <- sum(amd_df$costs_proceeds, na.rm = TRUE)
ROI = (Profit_Loss/total_investment_capital)*100

#The resulting ROI:
ROI 
#The resulting Profit/loss:
Profit_Loss
```

## Step 6: Summarize Your Findings

-   Did your P/L and ROI improve over your chosen period?
-   Relate your results to a relevant market event and explain why these outcomes may have occurred.

```{r}
#Initialise the variables
total_investment_capital_after_profit_taking_strategy <- 0
# for loop to calculate the sum of investment capital by summing all the trades that has trade_type as 'buy',  removing NA values 
for (i in 1:nrow(amd_df)) {
  if (!is.na(amd_df$trade_type[i]) && amd_df$trade_type[i] == 'buy') {
    total_investment_capital_after_profit_taking_strategy <- total_investment_capital_after_profit_taking_strategy + amd_df$costs_proceeds[i]
  }
}

# Plot the line plot
plot(amd_df$date, amd_df$close, type='l')

# Identify the dates for marking
mark_dates <- as.Date(c("2024-03-06", "2024-03-07"))

# Filter points corresponding to mark_dates to visually demmonstrate some key dates that is discussed below in this section
points(amd_df$date[amd_df$date %in% mark_dates], 
       amd_df$close[amd_df$date %in% mark_dates],
       col = "red", pch = 19)

#the original total_investment_capiial is a negative number, to reflect the total amount invested
total_investment_capital_after_profit_taking_strategy <- - total_investment_capital_after_profit_taking_strategy
# Sum the column, removing NA values to find the total profit/loss
Profit_Loss_after_profit_taking_strategy  <- sum(amd_df$costs_proceeds, na.rm = TRUE)
ROI_after_profit_taking_strategy <- (Profit_Loss_after_profit_taking_strategy/total_investment_capital_after_profit_taking_strategy)*100 

```

Discussion: After the implementation of the profit-taking strategy, the ROI has improved by 10.30598% from -2.169976% to 8.136007%, and P/L has improved by $76753.40 from -$17510.99 to $59242.41. The superiority of the profit-taking strategy in this scenario is due to how the share price in between the period has reached a price much higher than the price at the end of the period (the only time we sold in the first basic algorithm). By selling at these high prices, the algorithm is taking advantage of the occurrences of high prices which may not occur again later in the period as these sudden increases in prices is closely related to some real-time market events or financial news that has occurred that has convinced more shareholders to purchase shares of AMD, creating a high demand. For example, on Tuesday March 5 2024,  Jean Hu, AMD executive vice president, chief financial officer and treasurer, has presented at the Morgan Stanley Technology, Media and Telecom Conference, demonstrating how AMD has driven innovation in high-performance computing, graphics and visualization technologies and the company's success at updating their  AI opportunities for TAM to $400 billion in 2027, indicating its potential for further growth. Soon after,  the share prices has reached a very high peak on 2024-03-06 and 2024-03-07.

My algorithm took advantage of this growth, and sold on both days. This allows me to receive a greater profit as I am selling the shares at a greater price, which contributed to a better ROI and P/L at the end of my period. 


