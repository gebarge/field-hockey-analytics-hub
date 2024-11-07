# Load necessary libraries
library(readr)
library(readxl)     
library(dplyr)      
library(summarytools)  

# Load the data
data <- read_excel("FieldHockeyCornerData.xlsx", sheet = "Sheet1")

# View the first few rows
head(data)

data$L1 <- factor(data$L1)
data$Success <- factor(data$Success)
data$MacyInsertion <- factor(data$MacyInsertion)
data$BarbInsertion <- factor(data$BarbInsertion)
data$KiraInsertion <- factor(data$KiraInsertion)
data$InsertOff <- factor(data$InsertOff)
data$'1Hit' <- factor(data$'1Hit')
data$'2Sweep' <- factor(data$'2Sweep')
data$'1L1' <- factor(data$'1L1')
data$'1Pass2' <- factor(data$'1Pass2')
data$'1RevTip' <- factor(data$'1RevTip')
data$'1L1RevTip' <- factor(data$'1L1RevTip')
data$'1Drag' <- factor(data$'1Drag')
data$'1InsertWide' <- factor(data$'1InsertWide')
data$'2Hit' <- factor(data$'2Hit')
data$'2L1' <- factor(data$'2L1')
data$'2R2' <- factor(data$'2R2')

# Perform logistic regression
# Success is the dependent variable
logit_model <- glm(Success ~ ReceptiontoReleaseTime,
                   data = data, family = binomial)

# Checking a few calculations
exp(0.003721)
exp(-16.9194)
4.487433e-08/1+4.487433e-08
50 - 8.974866e-08
1-0.02354128

# View the summary of the logistic regression model
summary(logit_model)

table(data$MacyInsertion, data$Success)
