# Project Directory
setwd("/Users/calebyeboah/Documents/Caleb/GraduateResearchAssistance/Year1/SorghumFloweringTime/Gene_list_reseq_snpeff")

# contains read.vcfR required to Genotype data in vcf format
library(vcfR)
library(readxl)
library(dplyr)


vcf_Sobic.001G086500 <- read.vcfR(
                            "Sobic.001G086500_INDEL_snpeff.vcf", 
                            verbose = FALSE
                        )

sorghum_accessions_file <- read_xlsx(
                            "../Reconnaissance/SorghumAccessionNames.xlsx", 
                            sheet = "BotanicalTypesNew"
                        )



vcf_Sobic.001G086500_data <- cbind(as.data.frame(
                                      getFIX(vcf_Sobic.001G086500), 
                                      INFO2df(vcf_Sobic.001G086500)))


# Recode the @gt file to numbers and use that with the phenotype for your model
# after which you can extract the additive variance from the model


# Extract genotype data for the target gene
gt_matrix <- extract.gt(vcf_Sobic.001G086500, element = "GT")
View(gt_matrix)


# Function to recode genotypes
recode_genotypes <- function(gt) {
  if (gt %in% c("0/0", "0|0")) return(0)
  else if (gt %in% c("0/1", "1/0", "0|1", "1|0")) return(1)
  else if (gt %in% c("1/1", "1|1")) return(2)
  else return(NA)  # Handle missing or unknown data
}


# Apply the function to recode genotypes
numeric_gt <- apply(gt_matrix, 2, function(column) {
                  sapply(column, recode_genotypes)
                })


# Convert to data frame if needed
numeric_df <- as.data.frame(numeric_gt)
View(numeric_df)


# View the result
head(numeric_df)
numeric_df_t <- as.data.frame(t(numeric_df))
View(numeric_df_t)


# From the NewFloweringDataTime-DrRice.R script
# assign BotanicalType_FloweringTime_Filtered
# to pheno_w_rep

pheno_w_rep <- BotanicalType_FloweringTime_Filtered


pheno_wo_rep <- sorghum_accessions_file[1:911, 11:14]


colnames(pheno_wo_rep)[1] <- "LIB"
colnames(pheno_wo_rep)[2] <- "BotanicalType"
colnames(pheno_wo_rep)[3] <- "PINumber"
colnames(pheno_wo_rep)[4] <- "DaysToFlower"


# converting 'NA' of type chr to type logical
pheno_wo_rep$BotanicalType <- ifelse(pheno_wo_rep$BotanicalType == "NA", NA, pheno_wo_rep$BotanicalType)
pheno_wo_rep$PINumber <- ifelse(pheno_wo_rep$PINumber == "NA", NA, pheno_wo_rep$PINumber)
pheno_wo_rep$LIB <- ifelse(pheno_wo_rep$LIB == "NA", NA, pheno_wo_rep$LIB)


# Convert pheno to a dataframe
pheno_wo_rep_df <- data.frame(
                       LIB = pheno_wo_rep$LIB,
                       BotanicalType = pheno_wo_rep$BotanicalType,
                       PINumber = pheno_wo_rep$PINumber,
                       DaysToFlower = pheno_wo_rep$DaysToFlower
                  )


# remove rows that has NAs
filtered_pheno_wo_rep_df <- pheno_wo_rep_df[complete.cases(pheno_wo_rep_df), ]


# merge phenotype and genotype data
master_dat_wo_rep <- merge(filtered_pheno_wo_rep_df, numeric_df_t, by.x="LIB", by.y="row.names")

full_data_w_rep <- merge(pheno_w_rep, numeric_df_t, by.x = "LIB", by.y = "row.names")

# remove subpopulations of not interest
# master_dat <- master_dat %>% filter(BotanicalType != "Shattercane")

#library
library(lme4)

#First get BLUPs from full data frame (with replicates)


model1 <- lmer(DaysToFlower ~ (1|PInumber), data = full_data_w_rep)
Blup <- coefficients(model1)$PInumber["(Intercept)"][[1]] + ranef(model1)$PInumber #make sure ranef(model) is just PIs
Blup_df <- data.frame(
                  Intercept = coefficients(model1)$PInumber["(Intercept)"][[1]],
                  BLUP_EffectSize = ranef(model1)$PInumber,
                  CombinedEffect = coefficients(model1)$PInumber["(Intercept)"][[1]] + ranef(model1)$PInumber
                )




colnames(Blup_df)[2] <- "BLUP_EffectSize"
colnames(Blup_df)[3] <- "CombinedEffect"


coef(model1)$PInumber["(Intercept)"][[1]]
coefficients(model1)$PInumber["(Intercept)"][[1]]


str(model1)


# Model with random effect for genotype (PINumber) and fixed effect for environment (if available)
#run this as a loop, save the P_value and additive effect for each variant
  #if monophormic (no variation) leave as NA
  #plot box plot with slope (make nice version) - include dots for each observation


# Initialize the result data frame
Additive_df <- data.frame(Loci = character(),
                      p_value = numeric(),
                      Additive_Effect = numeric(),
                      stringsAsFactors = FALSE)


for (col in names(master_dat_wo_rep)[5:ncol(master_dat_wo_rep)]) { 
    # Check if the column is unique - this indicates monomorphic loci
    if (length(unique(master_dat_wo_rep[[col]])) == 1) { 
        new_row <- data.frame(
                          Loci = col, 
                          p_value = NA,
                          Additive_Effect = NA)
        # print(new_row)
   } 
    else {
          model2 <- lm(DaysToFlower ~ master_dat_wo_rep[[col]] , data = master_dat_wo_rep)
          anova_model <- anova(model2)
          p_value <- anova_model$`Pr(>F)`
          p_value_formatted <- format.pval(p_value, digits = 3)[1]
          Additive_Effect <- coef(model2)[[2]]
          new_row <- data.frame(Loci = col, p_value = p_value_formatted, Additive_Effect = Additive_Effect)
          
          # print(new_row)
  }

  Additive_df <- rbind(Additive_df, new_row)
}


boxplot(master_dat_wo_rep$DaysToFlower ~ 
        master_dat_wo_rep$INDEL_Chr01_6767983)
abline(model2, col="#ffb800", lwd=2, xlim=c(0,2))


Allele <- as.factor(master_dat_wo_rep$INDEL_Chr01_6767983)
DaysToFlower <- master_dat_wo_rep$DaysToFlower


library(ggplot2)
library(ggpubr)
library(ggtext)


model2 <- lm(DaysToFlower ~ INDEL_Chr01_6767983 , data = master_dat_wo_rep)
anova_model <- anova(model2)
p_value <- anova_model$`Pr(>F)`
p_value_formatted <- format.pval(p_value, digits = 3)[1]
Additive_Effect <- coef(model2)[[2]]

intercept2 <- model2$coefficients["(Intercept)"][[1]]
slope2 <- coef(model2)[[2]]


p1 <- ggplot(master_dat_wo_rep, aes(x = Allele, y = DaysToFlower, fill = Allele)) +
          geom_boxplot() +
          theme_minimal() +
          theme(axis.text.x = element_text(angle = 0, hjust = 1)) + # angle -> rotates the labels of the x-axis to diagonal
          geom_abline(intercept = intercept2, slope = slope2, color = "#dc0073", linetype = "solid") + 
          scale_fill_manual(values = c("#caf0f8", "#48cae4", "#0096c7")) # custom fill colors


p1 + annotate( 
              "text", 
              x = 1.5, # Adjust x position 
              y = max(master_dat_wo_rep$DaysToFlower) + 0.5, # Adjust y position to place above the highest boxplot 
              label = glue::glue("p-value: {p_value_formatted}"),
              size = 5, 
              hjust = 0.5, 
              color = "black", 
              parse = FALSE
            )




ggsave("../Reconnaissance/output/AdditiveEffect.jpeg", p1, width = 10, height = 8, dpi = 300)    





# ggplot(master_dat_wo_rep, aes(x = Allele, y = DaysToFlower, fill = Allele)) +
#           geom_boxplot() +
#           theme_minimal() +
#           theme(axis.text.x = element_text(angle = 0, hjust = 1)) + # rotates the labels of the x-axis to diagonal
#           scale_fill_manual(values = c("#caf0f8", "#48cae4", "#0096c7")) +  # custom fill colors
#           stat_compare_means(method = "anova", label.y = 9, method.args = list(var.equal = FALSE)) + # Use Welch's t-test
#           # geom_signif(comparisons = list(c("ctrl", "trt1"), c("trt1", "trt2"), c("ctrl", "trt2")), test = "t.test", map_signif_level = TRUE, y_position = c(6.5, 7.5, 6)) + # Significance bars with adjusted y positions
#           stat_compare_means(comparisons = list(c("0", "1"), c("1", "2"), c("0", "2")),
#                              method = "t.test", method.args = list(var.equal = FALSE),
#                              label.y = c(6.9, 7.6, 8.3))
#         

intercept2 <- model2$coefficients["(Intercept)"][[1]]
slope2 <- coef(model2)[[2]]

ggplot(master_dat_wo_rep, aes(x = Allele, y = DaysToFlower, fill = Allele)) +
      geom_boxplot() +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 0, hjust = 1)) +  # rotate the labels of the x-axis to diagonal
      scale_fill_manual(values = c("#caf0f8", "#48cae4", "#0096c7")) + # custom fill colors
      geom_abline(intercept = intercept2, slope = slope2, color = "#dc0073", linetype = "solid") +  # add a line with intercept 10 and slope 2
      ylim(0, max(master_dat_wo_rep$DaysToFlower) + 10)  # adjust y-axis limits

ggsave("../Reconnaissance/output/AdditiveEffect.jpeg", p1, dpi = 300)    


######## Export data


# Load necessary libraries 
library(tibble) # To work with tibbles
library(openxlsx) # To export to Excel


Additive_table <- tibble::tibble(
                      Loci = Additive_df$Loci,
                      P_value = Additive_df$p_value,
                      Additive_Effect = Additive_df$Additive_Effect 
                  )


# write the tibble to an excel file
write.xlsx(Additive_table,  file = "../Reconnaissance/output/Additive_Effect.xlsx")













# Aggregate DaysToFlower by PINumber (mean)
# master_dat_aggregated <- master_dat %>%
#                             group_by(PINumber) %>%
#                             summarise(DaysToFlower = mean(DaysToFlower, na.rm = TRUE))
# 






