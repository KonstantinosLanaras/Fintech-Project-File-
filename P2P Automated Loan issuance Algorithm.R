# Auxiliary packages
install.packages("rgl")
install.packages("resample")
if(!require(moments)) install.packages("moments")
if(!require(readxl)) install.packages("readxl")
library(moments)
library(ggplot2)
library(dplyr)
if(!require(car)) install.packages("car")
library(car)

setwd("C:/Users/mlong/Documents/Hackaton")

data= read_excel("C:/Users/mlong/Documents/Hackaton/lending_club_loan_two 2.xlsx")

clean_data= na.omit(data)

clean_data$term= as.factor(clean_data$term)
clean_data$term= relevel(clean_data$term, ref = "60 months")

clean_data = clean_data %>% 
  mutate(emp_length= case_when(
    emp_length %in% c("< 1 year", "1 year", "2 years","3 years") ~ "0-3 years",
    emp_length %in% c("4 years", "5 years", "6 years", "7 years", "8 years","9 years")~ "4-9 years",
    emp_length=="10+ years" ~ "10+ years",
    TRUE ~ NA_character_
  ))
clean_data$emp_length=as.factor(clean_data$emp_length)
clean_data$emp_length <- relevel(clean_data$emp_length, ref = "0-3 years")

clean_data$home_ownership= as.factor(clean_data$home_ownership)
clean_data <- clean_data %>% 
  filter(!home_ownership %in% c("ANY", "NONE", "OTHER"))
clean_data$home_ownership = relevel(clean_data$home_ownership, ref = "RENT")

clean_data$verification_status= as.factor(clean_data$verification_status)
clean_data$verification_status = relevel(clean_data$verification_status, ref = "Verified")

clean_data$loan_status= as.factor(clean_data$loan_status)
clean_data$loan_status= relevel(clean_data$loan_status, ref="Charged Off")

clean_data= clean_data %>%
  mutate(purpose = case_when(
    purpose %in% c("car", "educational", "house",
                   "major_purchase","medical","moving",
                   "renewable_energy","small_business",
                   "vacation","wedding") ~ "other",
    TRUE ~ as.character(purpose)  # keep the other existing levels like debt_consolidation, credit_card, home_improvement, other
  ))
clean_data$purpose= as.factor(clean_data$purpose)
clean_data$purpose = relevel(clean_data$purpose, ref = "other")


clean_data$installment=NULL
clean_data$revol_bal=NULL
clean_data$sub_grade=NULL
clean_data$earliest_cr_line=NULL

clean_data$int_rate= as.numeric(clean_data$int_rate)
clean_data$loan_amnt= as.numeric(clean_data$loan_amnt)
clean_data$annual_inc= as.numeric(clean_data$annual_inc)
clean_data$dti= as.numeric(clean_data$dti)
clean_data$open_acc= as.numeric(clean_data$open_acc)
clean_data$pub_rec= as.numeric(clean_data$pub_rec)
clean_data$total_acc= as.numeric(clean_data$total_acc)
clean_data$mort_acc= as.numeric(clean_data$mort_acc)
clean_data$revol_util= as.numeric(clean_data$revol_util)

summary(clean_data)
head(clean_data)
dim(clean_data)
sapply(clean_data, class)

clean_data %>%
  count(loan_status)

logistic_regression = glm(
  loan_status~.,
  data=clean_data,
  family = binomial()
)

summary(logistic_regression)

# Multicollinearity 
vif(logistic_regression)

# McFaddens pseudo R^2
install.packages("pscl")
library(pscl)

pR2_vals= pR2(logistic_regression)
pR2_vals
pR2_vals["McFadden"]

# ACCURACY TEST
install.packages("pROC") 
library("pROC")

dataset= clean_data

set.seed(123)

n = nrow(dataset)
train_index = sample(seq_len(n), size = 0.8 * n)

train <- dataset[train_index, ]
test  <- dataset[-train_index, ]

logit_model= glm(
  loan_status~.,
  data=train,
  family = binomial()
) #Fit logistic regression on TRAINING set

predictTest <- predict(
  logit_model,
  newdata = test,
  type = "response"
) #Predict probabilities on TEST set


table(test$loan_status,
      predictTest>=0.3) #Confusion matrix using threshold

conf_mat= table(test$loan_status,predictTest>=0.3)

accuracy <- (conf_mat[1,1] + conf_mat[2,2]) / sum(conf_mat)

roc_obj= roc(test$loan_status,predictTest) #Build ROC curve (TEST SET ONLY)

plot(
  roc_obj,
  col = "blue",
  lwd = 3,
  main = "ROC Curve (Test Set)"
  )
abline(a = 0, b = 1, col = "grey", lty = 2)

auc_value <- auc(roc_obj)

coords(roc_obj, "best", ret = c("threshold", "sensitivity", "specificity"))

# Descriptive Part
library(ggplot2)
library(dplyr)
install.packages("patchwork")

# Box-plot loan status vs loan amnt

ggplot(clean_data, aes(x=loan_status, y=loan_amnt))+
  geom_boxplot(fill="blue")+
  theme_minimal()

#% of default

clean_data %>% 
  summarise(repaid_pct= mean(loan_status== "Fully Paid")*100,
            default_pct=mean(loan_status=="Charged Off")*100
)


# check for linearity log-odds

Lineariry_df= data.frame(
  Loan_status_num= ifelse(clean_data$loan_status == "Fully Paid", 0, 1),
  interest_rate= as.numeric(clean_data$int_rate),
  income= as.numeric(clean_data$annual_inc),
  DTI= as.numeric(clean_data$dti),
  openAcounts= as.numeric(clean_data$open_acc),
  bankrupcy= as.numeric(clean_data$pub_rec),
  revUtil= as.numeric(clean_data$revol_util+0.1),
  totalACC=as.numeric(clean_data$total_acc),
  MortACC= as.numeric(clean_data$mort_acc+1)
)

View(Lineariry_df)

boxTidwell(Loan_status_num ~ subgrade, data = Lineariry_df)
boxTidwell(Loan_status_num ~ Employment, data = Lineariry_df)
boxTidwell(Loan_status_num ~ income, data = Lineariry_df)


# check for log-odd linearity and the ones that not comply the linearity transform into polinomial
#create interaction terms 



