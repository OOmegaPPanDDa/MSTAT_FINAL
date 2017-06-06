library(readr)
library(dplyr)
library(randomForest)

rank_stage = 20

############################################################
# load data
pre_year = 2011
year = 2012

pre_year_path = paste0('./merge_data/merge_data_', pre_year, '.csv')
year_path = paste0('./merge_data/merge_data_', year, '.csv')

pre_year_data = read_csv(pre_year_path)
year_data = read_csv(year_path)


year_data <- year_data %>%
  select(Player_Full_Name,Team,Salary)
names(year_data)[ncol(year_data)] = 'Salary_to_predict' 


# merge year and pre_year data
data2012 <- merge(x = pre_year_data, y = year_data, by = c('Player_Full_Name', 'Team')) %>%
  arrange(desc(Salary_to_predict))

year_record = rep(2012, nrow(data2012))
data2012 = cbind(year_record, data2012)

data2012 <- data2012 %>% 
  mutate(Salary_rank = floor((rank(data2012$Salary, ties.method = 'last')/nrow(data2012))*rank_stage- 0.00001) + 1) %>% 
  mutate(Salary_to_predict_rank = floor((rank(data2012$Salary_to_predict, ties.method = 'last')/nrow(data2012))*rank_stage- 0.00001) + 1)


############################################################
# load data
pre_year = 2012
year = 2013

pre_year_path = paste0('./merge_data/merge_data_', pre_year, '.csv')
year_path = paste0('./merge_data/merge_data_', year, '.csv')

pre_year_data = read_csv(pre_year_path)
year_data = read_csv(year_path)


year_data <- year_data %>%
  select(Player_Full_Name,Team,Salary)
names(year_data)[ncol(year_data)] = 'Salary_to_predict' 


# merge year and pre_year data
data2013 <- merge(x = pre_year_data, y = year_data, by = c('Player_Full_Name', 'Team')) %>%
  arrange(desc(Salary_to_predict))

year_record = rep(2013, nrow(data2013))
data2013 = cbind(year_record, data2013)

data2013 <- data2013 %>% 
  mutate(Salary_rank = floor((rank(data2013$Salary, ties.method = 'last')/nrow(data2013))*rank_stage- 0.00001) + 1) %>% 
  mutate(Salary_to_predict_rank = floor((rank(data2013$Salary_to_predict, ties.method = 'last')/nrow(data2013))*rank_stage- 0.00001) + 1)

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

data2014 <- data2014 %>% 
  mutate(Salary_rank = floor((rank(data2014$Salary, ties.method = 'last')/nrow(data2012))*rank_stage- 0.00001) + 1) %>% 
  mutate(Salary_to_predict_rank = floor((rank(data2014$Salary_to_predict, ties.method = 'last')/nrow(data2014))*rank_stage- 0.00001) + 1)
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

data2015 <- data2015 %>% 
  mutate(Salary_rank = floor((rank(data2015$Salary, ties.method = 'last')/nrow(data2015))*rank_stage- 0.00001) + 1) %>% 
  mutate(Salary_to_predict_rank = floor((rank(data2015$Salary_to_predict, ties.method = 'last')/nrow(data2015))*rank_stage- 0.00001) + 1)
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

data2016 <- data2016 %>% 
  mutate(Salary_rank = floor((rank(data2016$Salary, ties.method = 'last')/nrow(data2016))*rank_stage- 0.00001) + 1) %>% 
  mutate(Salary_to_predict_rank = floor((rank(data2016$Salary_to_predict, ties.method = 'last')/nrow(data2016))*rank_stage- 0.00001) + 1)

############################################################


bdata = do.call("rbind", list(data2012, data2013, data2014, data2015, data2016))

bdata <- bdata %>%
  mutate(Trend = Salary_to_predict_rank - Salary_rank)

bdata$Trend[bdata$Trend > 0] = 'up'
bdata$Trend[bdata$Trend == 0] = 'hold'
bdata$Trend[bdata$Trend < 0] = 'down'

# replace NA by -1
bdata[is.na(bdata)] <- -1

# position dummy coding 
pos_dummy = as.data.frame(model.matrix(~ PosTwo - 1, data = bdata))
names(pos_dummy) = sapply(names(pos_dummy), function(x) paste0('pos_',substr(x,7,8)))
bdata = cbind(cbind(bdata[,c(1,2,3,4,5,6,7)], pos_dummy), bdata[,seq(from = 8, to = ncol(bdata))])

# Data select
bdata = subset(bdata, pos_P == 0)

cat('Data Count', nrow(bdata), '\n')

# "year_record"            "Player_Full_Name"       "Team"                   "Player"                
# "PosOne_Detailed"        "PosOne"                 "PosTwo"                 "pos_1B"                
# "pos_2B"                 "pos_3B"                 "pos_C"                  "pos_CF"                
# "pos_DH"                 "pos_LF"                 "pos_P"                  "pos_RF"                
# "pos_SS"                 "G"                      "AB"                     "R"                     
# "H"                      "TwoB"                   "ThreeB"                 "HR"                    
# "RBI"                    "BB"                     "SO"                     "SB"                    
# "CS"                     "AVG"                    "OBP"                    "SLG"                   
# "OPS"                    "Salary"                 "Salary_to_predict"      "Salary_rank"           
# "Salary_to_predict_rank" "Trend"

data_x = bdata[,c("G", "AB", "R", "H" , "TwoB", "ThreeB", "HR", 
             "RBI", "BB", "SO", "SB", "CS", "AVG", "OBP", "SLG",                 
             "OPS", "Salary_rank")]

data_y = bdata[,c("Trend")]
data_y = as.factor(data_y)

smp_size <- floor(0.9 * nrow(data_x))

set.seed(46)
train_ind <- sample(seq_len(nrow(data_x)), size = smp_size)

train_x = data_x[train_ind, ]
valid_x = data_x[-train_ind, ]
train_y = data_y[train_ind]
valid_y = data_y[-train_ind]



brf = randomForest(x = train_x, 
                   y = train_y,
                   xtest = valid_x,
                   ytest = valid_y
                   )

print(brf)









