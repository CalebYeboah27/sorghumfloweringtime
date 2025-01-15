# Create first data frame
df1 <- data.frame(player = LETTERS[1:5], team = c('Mavs', 'Lakers', 'Rockets', 'Heat', 'Celtics'))

# Create second data frame
df2 <- data.frame(player = LETTERS[1:5], points = c(14, 15, 16, 17, 18))

# Merge the two data frames
result <- merge(df1, df2, by = "player")

############################################
############ OR USE INNER_JOIN #############
############################################

# inner_join in contained in the dplyr library
library(dplyr)
# Merge the two data frames using inner_join
result <- inner_join(df1, df2, by = "player")

print(result)

library(dplyr)


##########################################################################
### Suppose you have two data frames, df1 and df2, 
### and you want to merge them based on a common column, 
### but only include specific columns from each data frame in the result.
##########################################################################


# Create first data frame
df1 <- data.frame(ID = 1:3, Name = c("John", "Jane", "Doe"), Age = c(23, 25, 30))

# Create second data frame
df2 <- data.frame(ID = 1:3, Score = c(85, 90, 95), Grade = c("A", "A", "A"))

# Select specific columns and perform inner join
result <- df1 %>%
  select(ID, Name) %>%
  inner_join(df2 %>% select(ID, Score), by = "ID")

print(result)
