library(readr)
library(dplyr)
library(randomForest)
library(ggplot2)

rank_stage = 500
year_considered = c(2012,2013,2014,2015,2016)

year_data_list = list()
year_data_list_index = 1


for (the_year in year_considered){
  
  # load data
  pre_year = the_year - 1
  year = the_year
  
  pre_year_path = paste0('./merge_data/merge_data_', pre_year, '.csv')
  year_path = paste0('./merge_data/merge_data_', year, '.csv')
  
  pre_year_data = read_csv(pre_year_path)
  year_data = read_csv(year_path)
  
  
  year_data <- year_data %>%
    select(Player_Full_Name,Team,Salary)
  names(year_data)[ncol(year_data)] = 'Salary_to_predict' 
  
  
  # merge year and pre_year data
  # merge by Player_Full_Name
  the_data <- merge(x = pre_year_data, y = year_data, by = c('Player_Full_Name')) %>%
    arrange(desc(Salary_to_predict))
  
  
  the_data <- the_data[,-which(names(the_data) %in% c('Team.y'))]
  
  
  # remove duplicated
  name_duplicated_data = the_data$Player_Full_Name[duplicated(the_data$Player_Full_Name)]
  # print(name_duplicated_data)
  the_data <- the_data %>%
    filter(!(Player_Full_Name %in% name_duplicated_data))
  
  
  year_record = rep(the_year, nrow(the_data))
  the_data = cbind(year_record, the_data)
  
  the_data <- the_data %>% 
    mutate(Salary_rank = floor((rank(the_data$Salary, ties.method = 'last')/nrow(the_data))*rank_stage- 0.00001) + 1) %>% 
    mutate(Salary_to_predict_rank = floor((rank(the_data$Salary_to_predict, ties.method = 'last')/nrow(the_data))*rank_stage- 0.00001) + 1)
  
  
  
  # scale data
  the_data[8:23] = scale(the_data[8:23])
  
  year_data_list[[year_data_list_index]] = the_data
  year_data_list_index = year_data_list_index + 1
  
  
}


bdata = do.call("rbind", year_data_list)

bdata <- bdata %>%
  mutate(Trend = Salary_to_predict_rank - Salary_rank)

bdata$Trend[bdata$Trend > 0] = 'up'
bdata$Trend[bdata$Trend <= 0] = 'hold&down'

# bdata$Trend[bdata$Trend == 0] = 'hold'
# bdata$Trend[bdata$Trend < 0] = 'down'

# replace NA by -1
bdata[is.na(bdata)] <- -1

# position dummy coding 
pos_dummy = as.data.frame(model.matrix(~ PosTwo - 1, data = bdata))
names(pos_dummy) = sapply(names(pos_dummy), function(x) paste0('pos_',substr(x,7,8)))
bdata = cbind(cbind(bdata[,c(1,2,3,4,5,6,7)], pos_dummy), bdata[,seq(from = 8, to = ncol(bdata))])

# Data select
bdata = subset(bdata, pos_P == 0)

cat('Data Count', nrow(bdata), '\n')






bdata_rank_group <- bdata %>%
  group_by(Salary_to_predict_rank) %>%
  summarise(mean_Salary_to_predict = mean(Salary_to_predict))

bdata_rank_group$Salary_to_predict_rank <- as.factor(bdata_rank_group$Salary_to_predict_rank)


View(bdata_rank_group)

rank_bar <- ggplot(bdata_rank_group, aes(x=Salary_to_predict_rank, y=mean_Salary_to_predict)) +
  geom_bar(stat="identity", fill = 'deepskyblue') +
  # geom_text(aes(label=mean_Salary_to_predict), vjust=-0.2) +
  ggtitle("Salary Rank") + 
  theme(text = element_text(family= 'Arial Unicode MS'))

print(rank_bar)

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



tree_nums = c(200,400,500,600,800,1000,1200,1500)
ms = seq(from = 2, to = ncol(data_x), by = 2)

best_tree_num = 0
best_m = 0
f1_all = c()

for (tree_num in tree_nums){
  for (m in ms){
    brf = randomForest(x = train_x,
                       y = train_y,
                       ntree = tree_num,
                       mtry = m,
                       xtest = valid_x,
                       ytest = valid_y)
    print(brf)

    conf = brf$test$confusion

    # View(conf)
    tp = conf[2,2]
    fp = conf[1,2]
    tn = conf[1,1]
    fn = conf[2,1]

    precision = tp / ( tp + fp )
    recall = tp / ( tp + fn )

    f1score = 2*precision*recall/(precision+recall)
    print(f1score)

    f1_all = c(f1_all, f1score)

    if(which.max(f1_all) == length(f1_all)){
      best_tree_num = tree_num
      best_m = m
    }

  }
}


brf = randomForest(x = train_x,
                   y = train_y,
                   ntree = best_tree_num,
                   mtry = best_m,
                   xtest = valid_x,
                   ytest = valid_y)
print(brf)

conf = brf$test$confusion

# View(conf)
tp = conf[2,2]
fp = conf[1,2]
tn = conf[1,1]
fn = conf[2,1]

precision = tp / ( tp + fp )
recall = tp / ( tp + fn )

f1score = 2*precision*recall/(precision+recall)
cat('f1_score: ', f1score, '\n')









