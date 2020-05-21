library(readxl)
library(tidyverse)

raw_data <- read_xlsx("clean data.xlsx")


which(!complete.cases(raw_data)) #show no missing data



# shingle variable explore

raw_data %>% group_by(Attrition) %>% summarise(time = n()) %>% 
  ggplot(aes(x = Attrition, y = time, label = time)) + 
  geom_col(aes(fill = Attrition), show.legend = FALSE) + geom_label() + theme_bw() +
  ggtitle("bar of Attribute") + 
  theme(plot.title = element_text(hjust = 0.5))

raw_data %>% group_by(WorkLifeBalance) %>% summarise(time = n()) %>% 
  ggplot(aes(x = WorkLifeBalance, y = time, label = time)) + 
  geom_col(aes(fill = WorkLifeBalance), show.legend = FALSE) + geom_label() + 
  ggtitle("bar of WorkLifeBalance") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5))


raw_data %>% group_by(YearsAtCompany) %>% summarise(time = n()) %>% 
  ggplot(aes(x = YearsAtCompany, y = time, label = time)) + 
  geom_col(aes(fill = YearsAtCompany), show.legend = FALSE) + geom_label() + 
  ggtitle("bar of YearsAtCompany") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5))

raw_data %>% group_by(YearsInCurrentRole) %>% summarise(time = n()) %>% 
  ggplot(aes(x = YearsInCurrentRole, y = time, label = time)) + 
  geom_col(aes(fill = YearsInCurrentRole), show.legend = FALSE) + geom_label() + 
  ggtitle("bar of YearsInCurrentRole") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5))


raw_data %>% group_by(YearsSinceLastPromotion) %>% summarise(time = n()) %>% 
  ggplot(aes(x = YearsSinceLastPromotion, y = time, label = time)) + 
  geom_col(aes(fill = YearsSinceLastPromotion), show.legend = FALSE) + geom_label() + 
  ggtitle("bar of YearsSinceLastPromotion") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5))

raw_data %>% group_by(YearsWithCurrManager) %>% summarise(time = n()) %>% 
  ggplot(aes(x = YearsWithCurrManager, y = time, label = time)) + 
  geom_col(aes(fill = YearsWithCurrManager), show.legend = FALSE) + geom_label() + 
  ggtitle("bar of YearsWithCurrManager") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5))

# mult- variable

library(corrplot)

corrplot(cor(raw_data[, -1])) # show correlation between numeric variable


ggplot(data = raw_data, 
       aes(x = WorkLifeBalance, color = Attrition, fill = Attrition)) + 
  geom_density(alpha=0.5) + ggtitle("difference of worklifebalance between Attrition") +
  theme_bw() + theme(plot.title = element_text(hjust = 0.5))

ggplot(data = raw_data, 
       aes(x = YearsAtCompany, color = Attrition, fill = Attrition)) + 
  geom_density(alpha=0.5) + ggtitle("difference of YearsAtCompany between Attrition") +
  theme_bw() + theme(plot.title = element_text(hjust = 0.5))


ggplot(data = raw_data, 
       aes(x = YearsAtCompany, y = YearsWithCurrManager, color = Attrition)) + geom_point()


raw_data %>% pivot_longer(cols = -Attrition, names_to = "type", values_to = "value") %>% 
  ggplot(aes(x = value, color = Attrition)) + 
  geom_density(show.legend = FALSE) + facet_wrap(~ type, scales = 'free')


# normalize data 
#categorical variable
raw_data$Attrition <- factor(raw_data$Attrition, levels = c("Yes", "No"))

# numeric variable

normalize_data <- raw_data[]
normalize_data[, 2:6] <- apply(raw_data %>% select(-Attrition),2, FUN = scale)


#for more detail
# in fact ,we can transform numeric variable to categorical variable
# for example

normalize_data2 <- raw_data[]

normalize_data2 %>% select(YearsAtCompany) %>% summary()

normalize_data2 %>% mutate(cate_YearsAtCompany = case_when(
  YearsAtCompany <= 10 ~ 1,
  (YearsAtCompany <= 20) & (YearsAtCompany > 10) ~ 2,
  (YearsAtCompany <= 30) & (YearsAtCompany > 20) ~ 3,
  (YearsAtCompany <= 40) & (YearsAtCompany > 30) ~ 4,
)) %>% mutate(cate_YearsAtCompany = as.factor(cate_YearsAtCompany))


write.csv(normalize_data, file = "type_1.csv", row.names = FALSE)
write.csv(normalize_data2, file = 'type_2.csv', row.names = FALSE)
