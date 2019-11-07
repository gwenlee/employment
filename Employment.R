wholeHour <- read.csv("wholeHour.csv")

###Variable Selection

#This is a flitered data - only values with NZD currency have been added
#Number of People in 'Measure' has been excluded

str(wholeHour)

#Incomesource only has one level - "Wage and salary income (main job)" - not necessary to keep this
#for same reason, 'ValueUnit', 'ValueLabel' were deleted as they only have one value

levels(wholeHour$Occupation)
levels(wholeHour$Agegroup)
levels(wholeHour$Ethnicgroup)
levels(wholeHour$NullReason)



###Missing Value Imputation

#Noticed that 'NullReason' data with "suppressed" means they have a Null value

#Deletion would not be wise as losing 9,000 data is quite impactful

#Identify null values first
NaRow<- which(is.na(as.factor(wholeHour$Value)))
length(NaRow)
NAs <- wholeHour[NaRow,]

#We had a suspiscion that "suppressed" means they have NAs
SuppressedRow <- which(grepl("suppressed", wholeHour$NullReason))
length(SuppressedRow)
#Both lengths are the same as 9,060. i.e. Found an evidence that 'NullReason' is linked to a Null value
#We can delete 'NullReason as well'


NotNaRow<- which(!is.na(as.factor(wholeHour$Value)))
hist(wholeHour[NotNaRow,]$Value)
Mean <- mean(wholeHour[NotNaRow,]$Value)

#Replacing NAs with mean
wholeHour[is.na(wholeHour)] <- Mean


###Year without analysis

summary(wholeHour)

model1 <- lm(Value ~ Occupation + Sex + Agegroup + Ethnicgroup, data = wholeHour)
plot(model1)
summary(model1)
anova(model1)

#Data seems to be skewed, may be we need a log
hist(wholeHour$Value)

model2 <- lm(log(Value)~  Occupation + Sex + Agegroup + Ethnicgroup, data = wholeHour)

plot(model2)
#Potential 
anova(model2)
summary(model2)



model3 <- lm(log(Value)~  Occupation + Sex + Agegroup + Ethnicgroup, data = wholeHour[-14572,])
plot(model3)



#No Detection for multicollinearity 
##?what is multicollinearity
Xmat <- model.matrix(model2)[, -1]
round(diag(solve(cor(Xmat))), 2)

library(R330)
 
#Criteria selection? (package is not working)
allpossregs(model3)

#Year with analysis - Time series

