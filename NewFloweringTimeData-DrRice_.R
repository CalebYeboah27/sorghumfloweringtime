# Project Directory
setwd("/Users/calebyeboah/Documents/Caleb/GraduateResearchAssistance/Year1/SorghumFloweringTime/Reconnaissance/")

# import data
library(readxl)
BotanicalTypeFile <- read_xlsx("SorghumAccessionNames.xlsx", sheet = "BotanicalTypesNew")

# Get file name
meta_data_dr <- read.csv("Sorghum_Metadata_2144g_BR_New.csv")


# convert list to dataframe
df_meta_data_dr <- data.frame(meta_data_dr)


# subset the Botanical Type column from the dataframe
BotanicalType <- df_meta_data_dr$BotanicalType


# Subsetting metadata from file
BotanicalType_MetaData <- BotanicalTypeFile[, 1:3]
FloweringTime_MetaData <- BotanicalTypeFile[2:1888, 7:8]


# convert to a dataframe
BotanicalType_MetaData <- data.frame(BotanicalType_MetaData)
FloweringTime_MetaData <- data.frame(FloweringTime_MetaData)



# rename columns in dataset
colnames(BotanicalType_MetaData)[1] <- "PINumber"
colnames(BotanicalType_MetaData)[2] <- "BotanicalType"
colnames(BotanicalType_MetaData)[3] <- "LIB"

colnames(FloweringTime_MetaData)[1] <- "PINumber"
colnames(FloweringTime_MetaData)[2] <- "DaysToFlower"

# ######################################################################################
### Find corresponding BotanicalTypes for PI Numbers within the Flowering time dataframe
### from the  dataframe 
### Use inner_join from dplyr. This is similar to index and match in Excel.
library(dplyr)


# Select the relevant columns from BotanicalType dataframe
BotanicalType_selected <- BotanicalType_MetaData %>% select(PINumber, BotanicalType, LIB)


# Select the relevant columns from FloweringTime dataframe
FloweringTime_selected <- FloweringTime_MetaData %>% select(PINumber, DaysToFlower)


# Use match to find corresponding BotanicalType for the PINumbers within the FT metadata
FloweringTime_selected$BotanicalType <- BotanicalType_selected$BotanicalType[
                                            match(FloweringTime_selected$PINumber, BotanicalType_selected$PINumber)
                                        ]


# Use match to find corresponding LIB Data for the PINumbers within the FT metadata
FloweringTime_selected$LIB <- BotanicalType_selected$LIB[
                                            match(FloweringTime_selected$PINumber, BotanicalType_selected$PINumber)
                                        ]



# Combine the results into a new dataframe
BotanicalType_FloweringTime <- data.frame(BotanicalType = FloweringTime_selected$BotanicalType, 
                                          LIB = FloweringTime_selected$LIB,
                                          PInumber = FloweringTime_selected$PINumber,
                                          DaysToFlower = FloweringTime_selected$DaysToFlower)


# ######################################################################################

# converting 'NA' of type chr to type logical
BotanicalType_FloweringTime$BotanicalType <- ifelse(BotanicalType_FloweringTime$BotanicalType == "NA", NA, BotanicalType_FloweringTime$BotanicalType)



# convert datatype into factor
BotanicalType_FloweringTime$BotanicalType <- factor(BotanicalType_FloweringTime$BotanicalType)



# calculate the mean and standard deviation for the flowering based on the 
# botanical type (subpopulations) being factors
summary_stats <- BotanicalType_FloweringTime_Clean %>% 
                group_by(BotanicalType) %>%
                summarise(
                  Freq. = n(),
                  Mean = mean(DaysToFlower),
                  SD = sd(DaysToFlower),
                  Variance = var(DaysToFlower)
                )


# Filter out Botanical Types with Freq. less than 10 
summary_stats_filtered <- summary_stats %>% filter(Freq. >= 10)


print(summary_stats)
sum(summary_stats$Freq.[1:9])
#################################################################################################
########################### Filtering out NA's from Botanical Types #############################
#################################################################################################

# Filter out rows where any column has NA
BotanicalType_FloweringTime_Clean <- BotanicalType_FloweringTime %>% 
                                            filter(complete.cases(.))






#################################################################################################
############################# Summary Stats - Mean, SD, Variance ################################
#################################################################################################

# calculate the mean and standard deviation for the flowering based on the 
# botanical type (subpopulations) being factors
summary_stats <- BotanicalType_FloweringTime_Clean %>% 
                      group_by(BotanicalType) %>%
                      summarise(
                        Freq. = n(),
                        Mean = mean(DaysToFlower),
                        SD = sd(DaysToFlower),
                        Variance = var(DaysToFlower)
                )

print(summary_stats, n = Inf)


#################################################################################################
####################### Filtering out Botanical Types with Freq. < 10 ###########################
#################################################################################################

# Filter the original data frame using the filtered botanical types 
BotanicalType_FloweringTime_Filtered <- BotanicalType_FloweringTime_Clean %>% 
                                            filter(BotanicalType %in% summary_stats_filtered$BotanicalType)




#################################################################################################
####################### testing for normality within subpopulations #############################
#################################################################################################

# 1. Use Q-Q Plot to Visualize for each subgroup

BotanicalType_FloweringTime_Filtered %>% group_by(BotanicalType) %>% 
                                do({
                                    qqnorm(.$DaysToFlower, main = paste("Q-Q Plot for Botanical Type:", unique(.$BotanicalType)))
                                    qqline(.$DaysToFlower, col = 'red')
                                })



# 2. Apply shapiro-Wilk test for each subgroup
normality_tests <- BotanicalType_FloweringTime_Filtered %>%
                      group_by(BotanicalType) %>%
                      summarise(p_value = shapiro.test(DaysToFlower)$p.value)
print(normality_tests)

# *Interpretation: p < 0.05 indicates non-normality for that Botanical Type



# 3: Comparing distributions between subgroups
# Levene's Test for homogeneity of variances (if comparing variances)
# Install and load car package

library(car)
leveneTest(DaysToFlower ~ BotanicalType, data = BotanicalType_FloweringTime)



#################################################################################################
#################### test for and compare variance between subpopulations #######################
#################################################################################################
# libraries
library(car)
library(dplyr)
library(ggplot2)
library(multcompView) 
library(stringr)

BotanicalType_FloweringTime_Filtered$BotanicalType<-str_replace(BotanicalType_FloweringTime_Filtered$BotanicalType,
                                                                "-","")
# Perform ANOVA 
mod <- lm(DaysToFlower ~ BotanicalType,data = BotanicalType_FloweringTime_Filtered)
anova_model <- anova(mod)

t_mod <- aov(DaysToFlower ~ BotanicalType,data = BotanicalType_FloweringTime_Filtered)


# Apply Tukey's HSD test 
tukey_results <- TukeyHSD(t_mod) 

# View the results 
print(tukey_results)



# Extract p-value
p_value_bot_grp <- anova_model$`Pr(>F)`[1]



# Extract the Tukey HSD results into a data frame 
tukey_df <- as.data.frame(tukey_results$BotanicalType) 


# Ensure p-values are correctly formatted with simplified names 
simplified_names <- sub("\\..*", "", rownames(tukey_df)) 


# Keep only the first part of each comparison 
p_values <- setNames(tukey_df$`p adj`, simplified_names)


# Use multcompView to generate letter groupings 
tukey_letters <- multcompLetters(p_values, Letters = c("a", "b", "c", "d"), threshold = 0.05) 


# Create a data frame to store the letters 
letters_df <- data.frame( 
                  BotanicalType = names(tukey_letters$Letters), 
                  Letters = tukey_letters$Letters 
              )

Letters <- letters_df$Letters


# Merge the letters with your original data 
BotanicalType_FloweringTime_Filtered <- 
                                    BotanicalType_FloweringTime_Filtered %>% 
                                    left_join(letters_df, by = "BotanicalType")



BotanicalType_FloweringTime_Filtered

# 1. using boxplots visualize phenotype variance
p1 <- BotanicalType_FloweringTime_Filtered %>%
      # order boxes based on data not variable names 
      mutate(BotanicalType = reorder(BotanicalType, DaysToFlower, FUN = mean)) %>%
      ggplot(aes(x = BotanicalType, y = DaysToFlower, fill = Letters)) +
              # default - colored fill
              geom_boxplot(show.legend = FALSE) +
              # no color fill until further decided
              geom_boxplot(fill = "white", color = "black") +
              geom_text(aes(label = Letters, y = max(DaysToFlower) + 2), position = position_dodge(width = 0.75), vjust = 0) +
              annotate("text", 
                       x = length(unique(BotanicalType_FloweringTime_Filtered$BotanicalType)) - 0.5, 
                       y = max(BotanicalType_FloweringTime_Filtered$DaysToFlower) + 10,
                       label = paste("ANOVA p-value:", format(p_value_bot_grp, digits = 3)), 
                       hjust = 2, 
                       vjust = 0.5, 
                       size = 5) +
              theme_minimal() +
              geom_text(data = BotanicalType, aes(x = BotanicalType, y = min(BotanicalType_FloweringTime_Filtered$DaysToFlower) - 5,, label = paste("n=", Freq.)), 
                      position = position_dodge(width = 0.75), vjust = 0.1, color = "#777", size = 3.5) + theme_minimal() +
              theme(axis.text.x = element_text(angle = 45, hjust = 1))  # rotates the labels of the x-axis to diagonal

p1

# optionally show data points on boxplots
# p1 + geom_point(show.legend = FALSE) 

ggsave("output/SummaryStats_BotanicalType_without_data.jpeg", p1, dpi = 300)    



# 3. Calculate and compare variance directly
variance_summary <- BotanicalType_FloweringTime_Filtered %>%
                          group_by(BotanicalType) %>%
                              summarise(variance = var(DaysToFlower))
    

#################################################################################################
######################## Export dataset to Excel #######################
#################################################################################################

# Load necessary libraries 
library(tibble) # To work with tibbles
library(openxlsx) # To export to Excel


Summary_stats_bot_grp <- tibble::tibble(
                              BotanicalType = summary_stats$BotanicalType,
                              Freq = floor(summary_stats$Freq. * 100) / 100,
                              Mean = summary_stats$Mean,
                              SD = summary_stats$SD,
                              Variance = summary_stats$Variance
                            )



