library(readr)
library(dplyr)

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
data <- merge(x = pre_year_data, y = year_data, by = c('Player_Full_Name', 'Team')) %>%
  arrange(desc(Salary_to_predict))


# replace NA by -1
data[is.na(data)] <- -1

# position dummy coding 
pos_dummy = as.data.frame(model.matrix(~ PosTwo - 1, data = data))
names(pos_dummy) = sapply(names(pos_dummy), function(x) paste0('pos_',substr(x,7,8)))
data = cbind(cbind(data[,c(1,2,3,4,5,6)], pos_dummy), data[,seq(from = 7, to = ncol(data))])


cat('Data Count', nrow(data), '\n')

# fetch numeric data
featue_data = data[,seq(from = 7, to = ncol(data))]
View(featue_data)

# regression model
f <- paste('Salary_to_predict ~', paste(colnames(featue_data)[1:27], collapse='+'))
model <- lm(f, featue_data)



# error measurement
evaluate = data.frame(y_true = data$Salary_to_predict, y_pred = predict(model))
evaluate = cbind(data[,c(1,2,3,4,5,6)], evaluate)
evaluate <- evaluate %>%
  mutate(relative_error = abs(y_true - y_pred)/y_true) %>%
  arrange(desc(relative_error))


mean_relative_error = mean(evaluate$relative_error)
cat('mean_relative_error', mean_relative_error, '\n')

rmse = mean((evaluate$y_true - evaluate$y_pred) ** 2) ** 0.5
cat('rmse', rmse, '\n')
