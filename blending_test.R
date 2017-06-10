library(readr)
library(dplyr)
library(randomForest)
library(e1071)
library(ggplot2)
options(scipen=999)


set.seed(464646)

rank_stage = 20
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
  names(the_data)[3] = 'Team'
  
  
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
  the_data[seq(from=8,to=ncol(the_data)-4)] = scale(the_data[seq(from=8,to=ncol(the_data)-4)])
  
  year_data_list[[year_data_list_index]] = the_data
  year_data_list_index = year_data_list_index + 1
  
  
}


bdata = do.call("rbind", year_data_list)

bdata <- bdata %>%
  mutate(Trend = Salary_to_predict_rank - Salary_rank)

bdata$Trend[bdata$Trend > 0] = 'improved'
bdata$Trend[bdata$Trend <= 0] = 'unimproved'

# bdata$Trend[bdata$Trend == 0] = 'hold'
# bdata$Trend[bdata$Trend < 0] = 'down'

# replace NA by -1
bdata[is.na(bdata)] <- -1

# position dummy coding 
pos_dummy = as.data.frame(model.matrix(~ Pos2 - 1, data = bdata))
names(pos_dummy) = sapply(names(pos_dummy), function(x) paste0('pos_',substr(x,5,6)))
bdata = cbind(cbind(bdata[,c(1,2,3,4,5,6,7)], pos_dummy), bdata[,seq(from = 8, to = ncol(bdata))])

# Data select
bdata = subset(bdata, pos_P == 0)
bdata = subset(bdata, pos_C == 0)

cat('Data Count', nrow(bdata), '\n')






bdata_rank_group <- bdata %>%
  group_by(Salary_to_predict_rank) %>%
  summarise(mean_Salary_to_predict = mean(Salary_to_predict))

bdata_rank_group$Salary_to_predict_rank <- as.factor(bdata_rank_group$Salary_to_predict_rank)


# View(bdata_rank_group)

rank_bar <- ggplot(bdata_rank_group, aes(x=Salary_to_predict_rank, y=mean_Salary_to_predict)) +
  geom_bar(stat="identity", fill = 'deepskyblue') +
  # geom_text(aes(label=mean_Salary_to_predict), vjust=-0.2) +
  ggtitle("Salary Rank") + 
  theme(text = element_text(family= 'Arial Unicode MS'))

print(rank_bar)

data_x = bdata[,seq(from=18,to=ncol(bdata)-4)]


data_y = bdata[,c("Trend")]
data_y = as.factor(data_y)







smp_size <- floor(0.1 * nrow(data_x))
test_ind <- sample(seq_len(nrow(data_x)), size = smp_size)

test_x = data_x[test_ind, ]
not_test_x = data_x[-test_ind, ]
test_y = data_y[test_ind]
not_test_y = data_y[-test_ind]



smp_size = nrow(not_test_x)
fold = sample(smp_size)

split_step = length(fold)/10


brf_train_accs = c()
brf_valid_accs = c()
brf_test_accs = c()

bsvm_train_accs = c()
bsvm_valid_accs = c()
bsvm_test_accs = c()

test_preds = rep(0, length(test_y))

for (i in 1:10){
  valid_ind = fold[seq(from = floor(split_step*(i-1)+1), to = floor(split_step*(i)))]
  
  valid_x = not_test_x[valid_ind,]
  train_x = not_test_x[-valid_ind,]
  valid_y = not_test_y[valid_ind]
  train_y = not_test_y[-valid_ind]
  
  
  
  ###########################
  #### feature selection ####
  ########## START ##########
  ###########################
  
  chi_threshold = 50
  selected_cols = c()
  
  for(i in 1:ncol(train_x)){
    chi = chisq.test(train_y,train_x[,i], correct = FALSE)
    # print(chi$statistic)
    if(chi$statistic >= chi_threshold ){
      selected_cols = c(selected_cols, i)
    }
  }
  
  # no selection

  the_train_x = train_x
  the_valid_x = valid_x
  the_test_x = test_x
  
  
  # # do selection
  # the_train_x = train_x[selected_cols]
  # the_valid_x = valid_x[selected_cols]
  # the_test_x = test_x[selected_cols]
  
  the_train_y = train_y
  the_valid_y = valid_y
  the_test_y = test_y
  
  
  
  ###########################
  #### feature selection ####
  ########### END ###########
  ###########################
  
  
  
  ###########################
  ###### random forest ######
  ########## START ##########
  ###########################
  
  
  best_m = 20
  best_tree_num = 500
  
  brf = randomForest(x = the_train_x,
                     y = the_train_y,
                     ntree = best_tree_num,
                     mtry = best_m,
                     xtest = the_valid_x,
                     ytest = the_valid_y,
                     keep.forest=TRUE
  )
  print(brf)
  cat('\n\n\n')
  
  brf_train_conf = brf$confusion
  
  # View(brf_train_conf)
  tp = brf_train_conf[2,2]
  fp = brf_train_conf[1,2]
  tn = brf_train_conf[1,1]
  fn = brf_train_conf[2,1]
  
  brf_train_acc = (tp+tn)/(tp+fp+tn+fn) 
  
  
  
  brf_valid_conf = brf$test$confusion
  
  # View(brf_valid_conf)
  tp = brf_valid_conf[2,2]
  fp = brf_valid_conf[1,2]
  tn = brf_valid_conf[1,1]
  fn = brf_valid_conf[2,1]
  
  brf_valid_acc = (tp+tn)/(tp+fp+tn+fn) 
  
  
  
  brf_test_pred = predict(brf, the_test_x)
  brf_test_conf = table(the_test_y, brf_test_pred)
  print(brf_test_conf)
  tp = brf_test_conf[2,2]
  fp = brf_test_conf[1,2]
  tn = brf_test_conf[1,1]
  fn = brf_test_conf[2,1]
  
  brf_test_acc = (tp+tn)/(tp+fp+tn+fn) 
  
  
  cat('rf train acc', brf_train_acc, '\n\n')
  cat('rf valid acc', brf_valid_acc, '\n\n')
  cat('rf test acc', brf_test_acc, '\n\n')
  
  
  # blending
  test_pred = as.vector(brf_test_pred)
  test_pred[test_pred=='improved'] = 1
  test_pred[test_pred=='unimproved'] = 0
  test_pred = as.numeric(test_pred)
  # print(test_pred)
  test_preds = test_preds + test_pred
  
  
  
  ###########################
  ###### random forest ######
  ########### END ###########
  ###########################
  
  cat('\n\n\n')
  
  ###########################
  ########### svm ###########
  ########## START ##########
  ###########################
  
  
  
  bsvm <- svm(the_train_x, the_train_y)
  
  train_pred <- predict(bsvm, the_train_x)
  bsvm_train_conf = table(the_train_y, train_pred)
  print(bsvm_train_conf)
  cat('\n\n\n')
  
  tp = bsvm_train_conf[2,2]
  fp = bsvm_train_conf[1,2]
  tn = bsvm_train_conf[1,1]
  fn = bsvm_train_conf[2,1]
  
  bsvm_train_acc = (tp+tn)/(tp+fp+tn+fn) 
  
  

  valid_pred <- predict(bsvm, the_valid_x)
  bsvm_valid_conf = table(the_valid_y, valid_pred)
  print(bsvm_valid_conf)
  cat('\n\n\n')
  
  
  tp = bsvm_valid_conf[2,2]
  fp = bsvm_valid_conf[1,2]
  tn = bsvm_valid_conf[1,1]
  fn = bsvm_valid_conf[2,1]
  
  bsvm_valid_acc = (tp+tn)/(tp+fp+tn+fn) 
  
  
  bsvm_test_pred = predict(bsvm, the_test_x)
  bsvm_test_conf = table(the_test_y, bsvm_test_pred)
  print(bsvm_test_conf)
  tp = bsvm_test_conf[2,2]
  fp = bsvm_test_conf[1,2]
  tn = bsvm_test_conf[1,1]
  fn = bsvm_test_conf[2,1]
  
  bsvm_test_acc = (tp+tn)/(tp+fp+tn+fn) 
  
  
  cat('svm train acc', bsvm_train_acc, '\n\n')
  cat('svm valid acc', bsvm_valid_acc, '\n\n')
  cat('svm test acc', bsvm_test_acc, '\n\n')
  
  
  # blending
  test_pred = as.vector(bsvm_test_pred)
  test_pred[test_pred=='improved'] = 1
  test_pred[test_pred=='unimproved'] = 0
  test_pred = as.numeric(test_pred)
  # print(test_pred)
  test_preds = test_preds + test_pred
  
  
  ###########################
  ########### svm ###########
  ########### END ###########
  ###########################
  
  
  brf_train_accs = c(brf_train_accs, brf_train_acc)
  brf_valid_accs = c(brf_valid_accs, brf_valid_acc)
  brf_test_accs = c(brf_test_accs, brf_test_acc)
  
  
  bsvm_train_accs = c(bsvm_train_accs, bsvm_train_acc)
  bsvm_valid_accs = c(bsvm_valid_accs, bsvm_valid_acc)
  bsvm_test_accs = c(bsvm_test_accs, bsvm_test_acc)
  
  print(test_preds)
  
  
}

cat('RESULT', '\n\n')
cat('mean rf train acc', mean(brf_train_accs), '\n\n')
cat('mean rf valid acc', mean(brf_valid_accs), '\n\n')
cat('mean rf test acc', mean(brf_test_accs), '\n\n')
cat('\n')
cat('mean svm train acc', mean(bsvm_train_accs), '\n\n')
cat('mean svm valid acc', mean(bsvm_valid_accs), '\n\n')
cat('mean svm test acc', mean(bsvm_test_accs), '\n\n')


cat('BLENDING MODEL RESULT', '\n\n')

thresh = 0.4
blend_test_pred = test_preds/max(test_preds)
blend_test_pred[blend_test_pred >= thresh] = 1
blend_test_pred[blend_test_pred < thresh] = 0
blend_test_pred[blend_test_pred==1] = 'improved'
blend_test_pred[blend_test_pred==0] = 'unimproved'

blend_test_conf = table(test_y, blend_test_pred)
print(blend_test_conf)
tp = blend_test_conf[2,2]
fp = blend_test_conf[1,2]
tn = blend_test_conf[1,1]
fn = blend_test_conf[2,1]

blend_test_acc = (tp+tn)/(tp+fp+tn+fn) 

cat('blend test acc', blend_test_acc, '\n\n')







