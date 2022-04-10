library(tidyverse)
df <- read.csv("investments_VC_V2.csv")

market <- as.data.frame(table(df$market))
market_list <- market[market$Freq >= 100 & market$Var1 != "", ]$Var1
new_df <- df[df$market %in% market_list, ]
colnames(df)
new_df <- new_df[, c(
  "name", "homepage_url", "market", "funding_total_usd", "status", "country_code", "state_code", "city",
  "funding_rounds", "founded_at", "first_funding_at", "last_funding_at", "seed", "venture",
  "equity_crowdfunding", "undisclosed", "convertible_note", "debt_financing",
  "angel", "grant", "private_equity", "post_ipo_equity", "post_ipo_debt", "secondary_market",
  "product_crowdfunding", "round_A", "round_B", "round_C", "round_D", "round_E",
  "round_F", "round_G", "round_H"
)]

# remove rows wth wired date format like "0001-05-14" "0001-11-14" "0007-05-13" "0019-11-20" "0020-06-14" "0029-09-14"
new_df <- new_df[!new_df$first_funding_at %in% sort(new_df$first_funding_at)[1:6], ] # table(new_df$first_funding_at)   # check error values
new_df <- new_df[!new_df$last_funding_at %in% sort(new_df$last_funding_at)[1:3], ] # table(new_df$last_funding_at)

# change to date format
new_df$first_funding_at <- as.Date(new_df$first_funding_at)
new_df$last_funding_at <- as.Date(new_df$last_funding_at)
new_df$founded_at <- as.Date(new_df$founded_at)

# an interesting facts: many organizations which were founded before 1960 were university like Harvard
# View(new_df[new_df$founded_at %in% sort(new_df$founded_at)[1:133],]) #see the list

# compute the date difference (days) between the companies founded_at/first_funding_at/last_funding_at
# Assuming "2015-01-01" is the dataset published date because no date is later than this date
new_df$founded_span_days <- as.Date("2015-01-01") - new_df$founded_at
new_df$first_funding_span_days <- new_df$first_funding_at - new_df$founded_at
new_df$last_funding_span_days <- new_df$last_funding_at - new_df$founded_at
new_df$funding_span_days <- new_df$last_funding_at - new_df$first_funding_at

# extract year from dates
new_df$founded_year <- format(new_df$founded_at, format = "%Y")
new_df$first_funding_year <- format(new_df$first_funding_at, format = "%Y")
new_df$last_funding_year <- format(new_df$last_funding_at, format = "%Y")

# if these date differences are negative, removing them
new_df <- new_df |> filter(founded_span_days >= 0 & first_funding_span_days >= 0 & last_funding_span_days >= 0 & funding_span_days >= 0)
new_df$funding_total_usd <- as.numeric(extract_numeric(new_df$funding_total_usd))

# relocate variables related to date for better observation
new_df <- new_df |> relocate(founded_year, founded_span_days, .after = founded_at)
new_df <- new_df |> relocate(first_funding_year, first_funding_span_days, .after = first_funding_at)
new_df <- new_df |> relocate(last_funding_year, last_funding_span_days, funding_span_days, .after = last_funding_at)

# change data type
new_df$market <- as.factor(new_df$market)
new_df$status <- as.factor(new_df$status)
new_df$country_code <- as.factor(new_df$country_code)
new_df$state_code <- as.factor(new_df$state_code)
new_df$funding_rounds <- as.ordered(new_df$funding_rounds) # not sure about this, funding_rounds in ordered format would be better than numeric because it is too small, compared with total fundings
# new_df$city<-as.factor(new_df$city) we should clean city name first, Need to do future!

str(new_df)
View(new_df)
write.csv(new_df, "investment_cleaned.csv", row.names = FALSE)
write_feather(new_df, "investment_cleaned.feather")


