# PS6

library(rvest)
library(tidyverse)
library(ggplot2)

## Bring in Current NBA Draft Odds

url <- "https://www.tankathon.com/pick_odds"

## Select Desired Table

css_selector <- "#pick-distribution"

## Recreate Desired Table in R

webpage <- read_html(url)

Current_2024_NBA_Draft_Lottery_Odds <- webpage %>% html_node(css_selector) %>% html_table()

# Store the Team column as characters
Team <- Current_2024_NBA_Draft_Lottery_Odds[[1]]

# Convert the rest of the table to numeric
numeric_table <- as.data.frame(lapply(Current_2024_NBA_Draft_Lottery_Odds[-1], as.numeric))

# Rename the columns to Describe the Draft Pick each team has the odds of winning.
new_names <- c("1st Pick", "2nd Pick", "3rd Pick", paste0(4:14, "th Pick"), "Avg")
colnames(numeric_table) <- new_names

# Combine the Team column with the numeric table
Current_2024_NBA_Draft_Lottery_Odds <- cbind(Team, numeric_table)

# Replace empty strings with NA
Current_2024_NBA_Draft_Lottery_Odds[Current_2024_NBA_Draft_Lottery_Odds == ''] <- NA

# Replace NA with 0
Current_2024_NBA_Draft_Lottery_Odds[is.na(Current_2024_NBA_Draft_Lottery_Odds)] <- 0

# Enjoy
view(Current_2024_NBA_Draft_Lottery_Odds)

# I will first create a distribution of the odds of each team to receive the 1st pick in the draft.

# Create a new column to store the original row numbers
Current_2024_NBA_Draft_Lottery_Odds$Team_Order <- seq(nrow(Current_2024_NBA_Draft_Lottery_Odds))

# Create a histogram of the "1st Pick" column with teams on the x-axis
ggplot(Current_2024_NBA_Draft_Lottery_Odds, aes(x = reorder(Team, Team_Order), y = `1st Pick`)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(title = "1st Pick Odds by Team",
       x = "Team",
       y = "1st Pick Odds (In %)") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),  # Rotate x-axis labels
        axis.ticks.y = element_blank()) +  # Remove y-axis ticks
  scale_y_continuous(breaks = seq(0, max(Current_2024_NBA_Draft_Lottery_Odds$`1st Pick`), by = 1))  # Set y-axis breaks by 1

# Next, I will create a distribution of the odds for where the San Antonio Spurs' pick will fall

# Extract the row corresponding to the San Antonio Spurs ("SA") and exclude the first and last columns
sa_row <- Current_2024_NBA_Draft_Lottery_Odds[grep("SA", Current_2024_NBA_Draft_Lottery_Odds$Team), -c(1, ncol(Current_2024_NBA_Draft_Lottery_Odds))]

# Remove columns with 0% probability
sa_row_filtered <- sa_row[, colSums(sa_row) > 0]

# Plot histogram
barplot(as.matrix(sa_row_filtered), main = "Probability Distribution for San Antonio Spurs (SA) Draft Picks", 
        xlab = "Draft Pick", ylab = "Probability (%)",
        ylim = c(-2.5, 55), names.arg = colnames(sa_row_filtered))

# Add footnote
text(x = 0.5, y = -1.5, 
     labels = "Some picks excluded, as the Spurs' probability of getting those picks is 0%.", 
     pos = 4, cex = 0.8)

# Finally, The Toronto Raptors have a Top-6 Protected Pick. If the pick lands outside of the Top 6, they lose the pick to the Spurs. 
# I create a pie chart displaying the probability of "keeping" or "sending" the pick.

# Extracting Toronto Raptors' row (accounting for extra spaces)
raptors_row <- Current_2024_NBA_Draft_Lottery_Odds[grep("^\\s*7\\s+TOR\\s*$", Current_2024_NBA_Draft_Lottery_Odds$Team), ]

# Transposing the table and converting it into a data frame, preserving row names
raptors_transposed <- as.data.frame(t(raptors_row), stringsAsFactors = FALSE)

# Remove "Team" and "Avg" rows
raptors_transposed <- raptors_transposed[!rownames(raptors_transposed) %in% c("Team", "Avg"), , drop = FALSE]

# Add the "Keep or Send" column
raptors_transposed$Keep_or_Send <- ifelse(row.names(raptors_transposed) %in% c("1st Pick", "2nd Pick", "3rd Pick", "4th Pick", "5th Pick", "6th Pick"), "Keep", "Send")

print(raptors_transposed)

# Sum the probabilities for "Keep" and "Send"
keep_prob <- sum(as.numeric(raptors_transposed[raptors_transposed$Keep_or_Send == "Keep", 1]), na.rm = TRUE)
send_prob <- sum(as.numeric(raptors_transposed[raptors_transposed$Keep_or_Send == "Send", 1]), na.rm = TRUE)

# Create a vector of probabilities
probabilities <- c(Keep = keep_prob, Send = send_prob)

# Create a pie chart with legend and numerical values
pie(probabilities, labels = c("Keep", "Send"), col = c("green", "red"),
    main = "Toronto Raptors Keep or Send Draft Pick?",
    sub = "The Toronto Raptors send their draft pick to the San Antonio Spurs if their draft pick falls 7th or later. Otherwise, they get to keep it.")

# Add legend
legend("topright", legend = names(probabilities), fill = c("green", "red"))

# Add numerical values to the chart
text(0.4, 0.4, paste(keep_prob,"%"), col = "black", cex = 0.8)
text(-0.4, -0.4, paste(send_prob,"%"), col = "black", cex = 0.8)