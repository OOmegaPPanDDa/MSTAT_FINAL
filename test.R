library(readr)
library(dplyr)
library(randomForest)

############################################################
# load data
pre_year = 2013
year = 2014

pre_year_path = paste0('./merge_data/merge_data_', pre_year, '.csv')
year_path = paste0('./merge_data/merge_data_', year, '.csv')

pre_year_data = read_csv(pre_year_path)
year_data = read_csv(year_path)


year_data <- year_data %>%
  select(Player_Full_Name,Team,Salary)
names(year_data)[ncol(year_data)] = 'Salary_to_predict' 


# merge year and pre_year data
data2014 <- merge(x = pre_year_data, y = year_data, by = c('Player_Full_Name', 'Team')) %>%
  arrange(desc(Salary_to_predict))

year_record = rep(2014, nrow(data2014))
data2014 = cbind(year_record, data2014)
############################################################
# load data
pre_year = 2014
year = 2015

pre_year_path = paste0('./merge_data/merge_data_', pre_year, '.csv')
year_path = paste0('./merge_data/merge_data_', year, '.csv')

pre_year_data = read_csv(pre_year_path)
year_data = read_csv(year_path)


year_data <- year_data %>%
  select(Player_Full_Name,Team,Salary)
names(year_data)[ncol(year_data)] = 'Salary_to_predict' 


# merge year and pre_year data
data2015 <- merge(x = pre_year_data, y = year_data, by = c('Player_Full_Name', 'Team')) %>%
  arrange(desc(Salary_to_predict))

year_record = rep(2015, nrow(data2015))
data2015 = cbind(year_record, data2015)
############################################################
# load data
pre_year = 2015
year = 2016

pre_year_path = paste0('./merge_data/merge_data_', pre_year, '.csv')
year_path = paste0('./merge_data/merge_data_', year, '.csv')

pre_year_data = read_csv(pre_year_path)
year_data = read_csv(year_path)


year_data <- year_data %>%
  select(Player_Full_Name,Team,Salary)
names(year_data)[ncol(year_data)] = 'Salary_to_predict' 


# merge year and pre_year data
data2016 <- merge(x = pre_year_data, y = year_data, by = c('Player_Full_Name', 'Team')) %>%
  arrange(desc(Salary_to_predict))


year_record = rep(2016, nrow(data2016))
data2016 = cbind(year_record, data2016)

############################################################













data <- rbind(rbind(data2014, data2015), data2016)


# replace NA by -1
data[is.na(data)] <- -1

# position dummy coding 
pos_dummy = as.data.frame(model.matrix(~ PosTwo - 1, data = data))
names(pos_dummy) = sapply(names(pos_dummy), function(x) paste0('pos_',substr(x,7,8)))
data = cbind(cbind(data[,c(1,2,3,4,5,6,7)], pos_dummy), data[,seq(from = 8, to = ncol(data))])


cat('Data Count', nrow(data), '\n')

# fetch numeric data
featue_data = data[,seq(from = 8, to = ncol(data))]
# View(featue_data)

# regression model
print('Regression')
f <- paste('Salary_to_predict ~', paste(colnames(featue_data)[1:27], collapse='+'))
linear_regr <- lm(f, featue_data)



# error measurement
evaluate = data.frame(y_true = data$Salary_to_predict, y_pred = predict(linear_regr))
evaluate = cbind(data[,c(1,2,3,4,5,6,7)], evaluate)
evaluate <- evaluate %>%
  mutate(relative_error = abs(y_true - y_pred)/y_true) %>%
  arrange(desc(relative_error))


mean_relative_error = mean(evaluate$relative_error)
cat('mean_relative_error', mean_relative_error, '\n')

rmse = mean((evaluate$y_true - evaluate$y_pred) ** 2) ** 0.5
cat('rmse', rmse, '\n')




# random forest
print('Random Forest')
rf = randomForest(Salary_to_predict~.,data=featue_data)
importance(rf)


# error measurement
evaluate = data.frame(pre_Salary = data$Salary, y_true = data$Salary_to_predict, y_pred = predict(rf))
evaluate = cbind(data[,c(1,2,3,4,5,6,7)], evaluate)
evaluate <- evaluate %>%
  mutate(relative_error = abs(y_true - y_pred)/y_true) %>%
  arrange(desc(relative_error))


mean_relative_error = mean(evaluate$relative_error)
cat('mean_relative_error', mean_relative_error, '\n')

rmse = mean((evaluate$y_true - evaluate$y_pred) ** 2) ** 0.5
cat('rmse', rmse, '\n')