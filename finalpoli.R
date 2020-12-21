#Xueru Xie A15451232

#Read the Excel dataset, name it to the name file
getwd()
setwd('C:/Users/16413/Desktop')
library('readxl')
file <- read_excel('final.xlsx')

#Overview of the dataset
summary(file)

#Select important columns
shrinked <- file[c('FIPS State Code',
                   'FIPS County Code',
                   'measure_9_value', 
                   'measure_11_value', 
                   'measure_49_value', 
                   'measure_45_value', 
                   'measure_14_value',
                   'measure_63_value')]


#Rename the columns
names(shrinked)[names(shrinked) == 'measure_9_value'] <- 'Adult smoking'
names(shrinked)[names(shrinked) == 'measure_11_value'] <- 'Adult obesity'
names(shrinked)[names(shrinked) == 'measure_49_value'] <- 'Excessive drinking'

names(shrinked)[names(shrinked) == 'measure_45_value'] <- 'Sexually transmitted infections'
names(shrinked)[names(shrinked) == 'measure_14_value'] <- 'Teen births'
names(shrinked)[names(shrinked) == 'measure_63_value'] <- 'Median Household Income'

#Histogram of Adult smoking
summary(shrinked)
hist(shrinked$`Adult smoking`, 
     main = 'Distribution of Adult Smoking',
     xlab = 'Percentage')

#Boxplot of smoke, obesity, and drinking
health <- c('Adult smoking', 'Adult obesity', 'Excessive drinking')
healthy <- shrinked[health]
healthy
boxplot(healthy)

#Histogram of STI and teen births
unhealth <- c('Sexually transmitted infections', 'Teen births')
unhealthy <- shrinked[unhealth]
hist(unhealthy$`Sexually transmitted infections`,
     main = 'Distribution of STI',
     xlab = 'Counts per 100,000 Population')
hist(unhealthy$`Teen births`,
     main = 'Distribution of Teen births',
     xlab = 'Counts per 100,000 Population')

#Scatter plots of IV and DV
plot(shrinked$`Median Household Income`, 
     shrinked$`Adult smoking`,
     main = 'Relationship between Income & Smoking',
     xlab = 'Household Income',
     ylab = 'Adult Smoking Percentage')

plot(shrinked$`Median Household Income`, 
     shrinked$`Adult obesity`,
     main = 'Relationship between Income & Obesity',
     xlab = 'Household Income',
     ylab = 'Adult Obesity Percentage')

plot(shrinked$`Median Household Income`, 
     shrinked$`Excessive drinking`,
     main = 'Relationship between Income & Drinking',
     xlab = 'Household Income',
     ylab = 'Excessive Drinking Percentage')

plot(shrinked$`Median Household Income`, 
     shrinked$`Sexually transmitted infections`,
     main = 'Relationship between Income & STI',
     xlab = 'Household Income',
     ylab = 'STI Counts')

plot(shrinked$`Median Household Income`, 
     shrinked$`Teen births`,
     main = 'Relationship between Income & Teen births',
     xlab = 'Household Income',
     ylab = 'Teen births Counts')

#Linear Regressions

reg_smoke <- lm(shrinked$`Adult smoking` ~ shrinked$`Median Household Income`)
abline(reg_smoke, col = 'red')

reg_obe <- lm(shrinked$`Adult obesity` ~ shrinked$`Median Household Income`)
abline(reg_obe, col = 'blue')

reg_dri <- lm(shrinked$`Excessive drinking` ~ shrinked$`Median Household Income`)
abline(reg_dri, col = 'red')

reg_sti <- lm(shrinked$`Sexually transmitted infections` ~ shrinked$`Median Household Income`)
abline(reg_sti, col = 'blue')

reg_tee <- lm(shrinked$`Teen births` ~ shrinked$`Median Household Income`)
abline(reg_tee, col = 'red')

library('stargazer')
stargazer(reg_smoke, reg_obe, reg_dri, reg_sti, reg_tee, out = 'p.txt')
summary(reg_tee)

#Log Linear Regressions
log_reg_smoke <- lm(shrinked$`Adult smoking` ~ log(shrinked$`Median Household Income`))
plot(log(shrinked$`Median Household Income`), 
     shrinked$`Adult smoking`,
     main = 'Relationship between Income & Smoking',
     xlab = 'Household Income (log)',
     ylab = 'Adult Smoking Percentage')
abline(log_reg_smoke, col = 'red')
summary(log_reg_smoke)

log_reg_obe <- lm(shrinked$`Adult obesity` ~ log(shrinked$`Median Household Income`))
plot(log(shrinked$`Median Household Income`), 
     shrinked$`Adult obesity`,
     main = 'Relationship between Income & Obesity',
     xlab = 'Household Income (log)',
     ylab = 'Adult Obesity Percentage')
abline(log_reg_obe, col = 'red')
summary(log_reg_obe)

log_reg_dri <- lm(shrinked$`Excessive drinking` ~ log(shrinked$`Median Household Income`))
plot(log(shrinked$`Median Household Income`), 
     shrinked$`Excessive drinking`,
     main = 'Relationship between Income & Drinking',
     xlab = 'Household Income (log)',
     ylab = 'Excessive Drinking Percentage')
abline(log_reg_dri, col = 'red')
summary(log_reg_dri)

log_reg_sti <- lm(shrinked$`Sexually transmitted infections` ~ log(shrinked$`Median Household Income`))
plot(log(shrinked$`Median Household Income`), 
     shrinked$`Sexually transmitted infections`,
     main = 'Relationship between Income & STI',
     xlab = 'Household Income (log)',
     ylab = 'STI Counts')
abline(log_reg_sti, col = 'red')
summary(log_reg_sti)

log_reg_tee <- lm(shrinked$`Teen births` ~ log(shrinked$`Median Household Income`))
plot(log(shrinked$`Median Household Income`), 
     shrinked$`Teen births`,
     main = 'Relationship between Income & Teen births',
     xlab = 'Household Income (log)',
     ylab = 'Teen births Counts')
abline(log_reg_tee, col = 'red')
summary(log_reg_tee)

stargazer(log_reg_smoke, log_reg_obe, log_reg_dri, log_reg_sti, log_reg_tee, out = 'logp.png')
