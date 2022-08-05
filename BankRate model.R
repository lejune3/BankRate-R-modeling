library(readxl)
library(dplyr)
library(ggplot2)

#import the data
df <- read_excel("Pre-Super_Day_candidate_dataset__28candidate_29.xlsx")
head(df)

#information about the variables
glm1 <- glm(Approved ~ Loan_Amount + FICO_score + Reason + 
              Employment_Status + # Employment_Sector + 
              Monthly_Gross_Income + Monthly_Housing_Payment + Lender,
            data = df, family="binomial"
)
#this will help figure out which variables are significant
summary(glm1)

df %>%
  group_by(Fico_Score_group) %>%
  summarise(Approval = mean(Approved))

df %>%
  group_by(Employment_Status) %>%
  summarise(Approval = mean(Approved))

df$prct_housing = df$Monthly_Housing_Payment / df$Monthly_Gross_Income
df$loan_ratio = df$Loan_Amount / df$Monthly_Gross_Income

df %>%
  group_by(Approved) %>%
  summarise(mean_credit = mean(FICO_score),
            mean_income = mean(Monthly_Gross_Income),
            mean_housing = mean(Monthly_Housing_Payment),
            mean_prct_housing = mean(prct_housing),
            mean_loan_ratio = mean(loan_ratio))

ggplot(df, aes(x=prct_housing, fill=factor(df$Approved))) +
  geom_histogram(aes(y = ..density..), alpha=0.5, position="identity")

ggplot(df, aes(x=loan_ratio, fill=factor(df$Approved))) +
  geom_histogram(aes(y = ..density..), alpha=0.5, position="identity")

#Lenders approval rate
df %>%
  group_by(Lender) %>%
  summarise(Approval_Rate = mean(Approved))

#Lenders averages in each variable
df %>%
  group_by(Lender) %>%
  filter(Approved == 1) %>%
  summarise(mean_credit = mean(FICO_score),
            mean_income = mean(Monthly_Gross_Income),
            mean_housing = mean(Monthly_Housing_Payment),
            mean_prct_housing = mean(prct_housing),
            mean_loan_ratio = mean(loan_ratio),
            mean_loan_amount = mean(Loan_Amount))


#Simulation to figure out best fits for lenders
current_revunue <- sum(df$Approved[df$Lender=="A"]*250, 
                       df$Approved[df$Lender=="B"]*350,
                       df$Approved[df$Lender=="C"]*150)

current_revunue

lenderB <- df %>% filter(Lender == "B")
lenderB %>%
  group_by(Approved) %>%
  summarise(
    num_applicants = n(),
    mean_credit = mean(FICO_score),
    mean_income = mean(Monthly_Gross_Income),
    mean_housing = mean(Monthly_Housing_Payment),
    mean_prct_housing = mean(prct_housing),
    mean_loan_ratio = mean(loan_ratio))

ggplot(lenderB, aes(x=FICO_score, fill=factor(Approved))) +
  geom_histogram(aes(y = ..density..), alpha=0.5, position="identity")

ggplot(lenderB, aes(x=prct_housing, fill=factor(Approved))) +
  geom_histogram(aes(y = ..density..), alpha=0.5, position="identity")

ggplot(lenderB, aes(x=Monthly_Gross_Income, fill=factor(Approved))) +
  geom_histogram(aes(y = ..density..), alpha=0.5, position="identity")

ggplot(lenderB, aes(x=loan_ratio, fill=factor(Approved))) +
  geom_histogram(aes(y = ..density..), alpha=0.5, position="identity")

#Setting up rules to exclude low fico scores, monthly gross incomes, and loan ratios
lenderB_rules <- lenderB %>%
  filter(FICO_score > 650,
         Monthly_Gross_Income > 6000,
         loan_ratio < 12)
lenderB_rules %>%
  summarise(num_applicants = n(),
            num_approved = sum(Approved),
            approval_rate = mean(Approved))

sum(lenderB$Approved)

all_lenderB_applicants <- df %>%
  filter(FICO_score > 650,
         Monthly_Gross_Income > 6000,
         loan_ratio < 12)
nrow(all_lenderB_applicants) * 0.2035526

not_all_lenderB_applicants <- df %>%
  filter(! (`User ID` %in% all_lenderB_applicants$`User ID`))
nrow(not_all_lenderB_applicants)

new_rev <- sum(not_all_lenderB_applicants$Approved[not_all_lenderB_applicants$Lender=="A"]*250, 
               not_all_lenderB_applicants$Approved[not_all_lenderB_applicants$Lender=="B"]*350,
               nrow(all_lenderB_applicants) * 0.2035526 * 350,
               not_all_lenderB_applicants$Approved[not_all_lenderB_applicants$Lender=="C"]*150)

current_revunue

new_rev

new_rev - current_revunue