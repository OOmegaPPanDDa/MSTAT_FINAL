library(readr)
library(dplyr)
library(randomForest)
library(e1071)
library(ggplot2)
library(xgboost)
options(scipen=999)


set.seed(4646)


# position: P for Pitcher, C for Catcher, F for Fielder
pos_assigned = 'F'

rank_stage = 20
test_size = 0.1
if(pos_assigned == 'C'){
  test_size = 0.2
}
blend_thresh = 0



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
View(bdata)
print(names(bdata))

# replace NA by -1
bdata[is.na(bdata)] <- -1

bdata <- bdata %>%
  mutate(Trend = Salary_to_predict_rank - Salary_rank)

bdata$Trend[bdata$Trend > 0] = 'improved'
bdata$Trend[bdata$Trend <= 0] = 'unimproved'


# position dummy coding 
pos_dummy = as.data.frame(model.matrix(~ Pos2 - 1, data = bdata))
names(pos_dummy) = sapply(names(pos_dummy), function(x) paste0('pos_',substr(x,5,6)))
bdata = cbind(cbind(bdata[,c(1,2,3,4,5,6,7)], pos_dummy), bdata[,seq(from = 8, to = ncol(bdata))])

cat('Data Count', nrow(bdata), '\n')




###########################
########### Draw ##########
########## START ##########
###########################

bdata_rank_group <- bdata %>%
  group_by(Salary_to_predict_rank) %>%
  summarise(mean_Salary_to_predict = mean(Salary_to_predict))

bdata_rank_group$Salary_to_predict_rank <- as.factor(bdata_rank_group$Salary_to_predict_rank)

# View(bdata_rank_group)

rank_bar <- ggplot(bdata_rank_group, aes(x=Salary_to_predict_rank, y=mean_Salary_to_predict)) +
  geom_bar(stat="identity", fill = 'deepskyblue') +
  # geom_text(aes(label=mean_Salary_to_predict), vjust=-0.2) +
  ggtitle("Salary Rank") + 
  theme(text = element_text(family= 'Arial Unicode MS'),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()
  )
print(rank_bar)


POSITION = c()

for (i in 1:nrow(bdata)){
  if(bdata$pos_P[i] == 1){
    the_pos = 'Pitcher'
  }else if(bdata$pos_C[i] == 1){
    the_pos = 'Catcher'
  }else{
    the_pos = 'Fielder'
  }
  POSITION = c(POSITION, the_pos)
}

bdata$POSITION = POSITION



type_boxplot <- ggplot(bdata, aes(POSITION, Salary_to_predict, color = POSITION)) +
  geom_point() +
  geom_boxplot()

print(type_boxplot)

bdata <- bdata[,-which(names(bdata) %in% c('POSITION'))]

###########################
########### Draw ##########
########### END ###########
###########################




# Data select

# Pitcher
# better with random seed 4646
# better with test size 0.1

if(pos_assigned == 'P'){
  bdata = subset(bdata, pos_P == 1)
}


# Catcher
# better with random seed 4646
# better with test size 0.2

if(pos_assigned == 'C'){
  bdata = subset(bdata, pos_C == 1)
}




# Fielder
# better with random seed 4646
# better with test size 0.1

if(pos_assigned == 'F'){
  bdata = subset(bdata, pos_P == 0)
  bdata = subset(bdata, pos_C == 0)
}


cat('Selected Data Count', nrow(bdata), '\n')




# # Regression
# regr_data = bdata[,seq(from=18,to=ncol(bdata)-3)]
# 
# chi_threshold = 10000
# selected_cols = c()
# for(i in 1:(ncol(regr_data)-1)){
# 
#   if(length(unique(regr_data[,i]))>1){
#     chi = chisq.test(regr_data$Salary_to_predict,regr_data[,i], correct = FALSE)
#     print(names(regr_data)[i])
#     print(chi$statistic)
#     if(chi$statistic >= chi_threshold ){
#       selected_cols = c(selected_cols, i)
#     }
# 
#   }
# }
# 
# 
# selected_cols = c(selected_cols, ncol(regr_data))
# print(selected_cols)
# regr_data = regr_data[selected_cols]
# 
# regr <- lm(Salary_to_predict ~ ., regr_data)
# regr_pred = predict(regr)
# regr_true = bdata$Salary_to_predict
# regr_rmse = (mean((regr_pred - regr_true)**2))**0.5
# cat('regr_rmse', regr_rmse, '\n')
# regr_relative_mean_error = mean(abs((regr_pred - regr_true)/regr_true))
# cat('regr_relative_mean_error', regr_relative_mean_error, '\n')
# 






data_x = bdata[,seq(from=18,to=ncol(bdata)-4)]


data_y = bdata[,c("Trend")]
data_y = as.factor(data_y)








smp_size <- floor(test_size * nrow(data_x))
test_ind <- sample(seq_len(nrow(data_x)), size = smp_size)

test_x = data_x[test_ind, ]
not_test_x = data_x[-test_ind, ]
test_y = data_y[test_ind]
not_test_y = data_y[-test_ind]



smp_size = nrow(not_test_x)
fold = sample(smp_size)

split_step = length(fold)/10


blog_train_accs = c()
blog_valid_accs = c()
blog_test_accs = c()

brf_train_accs = c()
brf_valid_accs = c()
brf_test_accs = c()

bsvm_train_accs = c()
bsvm_valid_accs = c()
bsvm_test_accs = c()

bxgb_train_accs = c()
bxgb_valid_accs = c()
bxgb_test_accs = c()

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
    
    if(length(unique(train_y)) == 2 && length(unique(train_x[,i]))!=1){
      chi = chisq.test(train_y,train_x[,i], correct = FALSE)
      # print(chi$statistic)
      if(chi$statistic >= chi_threshold ){
        selected_cols = c(selected_cols, i)
      }
      
    }
  }
  
  
  
  # # no selection
  # 
  # the_train_x = train_x
  # the_valid_x = valid_x
  # the_test_x = test_x
  
  
  # do selection
  the_train_x = train_x[selected_cols]
  the_valid_x = valid_x[selected_cols]
  the_test_x = test_x[selected_cols]
  
  the_train_y = train_y
  the_valid_y = valid_y
  the_test_y = test_y
  
  cat('Feature Selection Ratio', ncol(the_train_x)/ncol(train_x), ncol(the_train_x), ncol(train_x), '\n\n')
  
  
  ###########################
  #### feature selection ####
  ########### END ###########
  ###########################
  
  
  ###########################
  ### logistic regression ###
  ########## START ##########
  ###########################
  
  log_train_y = ifelse(the_train_y == 'improved',1,0)
  log_valid_y = ifelse(the_valid_y == 'improved',1,0)
  log_test_y = ifelse(the_test_y == 'improved',1,0)
  
  log_train = the_train_x
  log_train$trend = log_train_y
  
  blog = lm(trend~., data = log_train)
  blog_train_acc = sum(round(predict(blog)) == log_train_y)/length(log_train_y)
  blog_valid_acc = sum(round(predict(blog, the_valid_x)) == log_valid_y)/length(log_valid_y)
  blog_test_acc = sum(round(predict(blog, the_test_x)) == log_test_y)/length(log_test_y)
  
  
  cat('logistic train acc', blog_train_acc, '\n\n')
  cat('logistic valid acc', blog_valid_acc, '\n\n')
  cat('logistic test acc', blog_test_acc, '\n\n')
  
  blog_test_pred = round(predict(blog, the_test_x))
  
  # blending
  
  if(blog_valid_acc >= blend_thresh){
    test_pred = as.vector(blog_test_pred)
    test_pred = as.numeric(test_pred)
    # print(test_pred)
    test_preds = test_preds + blog_valid_acc*test_pred
    }
  
  ###########################
  ### logistic regression ###
  ########## START ##########
  ########### END ###########
  ###########################
  
  cat('\n\n\n')
  
  ###########################
  ###### random forest ######
  ########## START ##########
  ###########################
  
  
  # tuning best_m best_tree_num
  
  # m_min = 2
  # m_max = ncol(the_train_x)
  # 
  # mgrids = unique(round(seq(m_min, m_max, length = 5)))
  # 
  # tree_min = 400
  # tree_max = 1000
  # 
  # treeNgrids = unique(round(seq(tree_min, tree_max, length = 3)))
  # 
  # best_m = 0
  # best_tree_num = 0
  # best_acc = -1
  # for (m1 in mgrids){
  #   for (treeN1 in treeNgrids){
  # 
  # 
  #     rf = randomForest(x = the_train_x,
  #                       y = the_train_y,
  #                       mtry = m1,
  #                       ntree = treeN1,
  #                       xtest = the_valid_x,
  #                       ytest = the_valid_y)
  # 
  # 
  #     rf_valid_conf = rf$test$confusion
  # 
  #     # View(brf_valid_conf)
  #     tp = rf_valid_conf[2,2]
  #     fp = rf_valid_conf[1,2]
  #     tn = rf_valid_conf[1,1]
  #     fn = rf_valid_conf[2,1]
  # 
  #     rf_valid_acc = (tp+tn)/(tp+fp+tn+fn)
  # 
  #     print(rf_valid_acc)
  # 
  #     if(best_acc < rf_valid_acc){
  #       cat('Randomforest Tuning..', m1, treeN1, '\n')
  #       best_acc = rf_valid_acc
  #       best_m = m1
  #       best_tree_num = treeN1
  #     }
  # 
  #   }
  # 
  # }
  # 
  # cat('best mtry', best_m, '\n')
  # cat('best ntree', best_tree_num, '\n')
  
  # no tuning, just assign
  best_m = 20
  best_tree_num = 500
  
  
  brf = randomForest(x = the_train_x,
                     y = the_train_y,
                     mtry = best_m,
                     ntree = best_tree_num,
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
  if(brf_valid_acc >= blend_thresh){
    
    test_pred = as.vector(brf_test_pred)
    test_pred[test_pred=='improved'] = 1
    test_pred[test_pred=='unimproved'] = 0
    test_pred = as.numeric(test_pred)
    # print(test_pred)
    test_preds = test_preds + brf_valid_acc*test_pred
  }
  
  
  
  ###########################
  ###### random forest ######
  ########### END ###########
  ###########################
  
  cat('\n\n\n')
  
  ###########################
  ########### svm ###########
  ########## START ##########
  ###########################
  
  
  kernel_opt = c('linear', 'radial', 'sigmoid')
  best_acc = -1
  
  for (k in kernel_opt){
    bsvm <- svm(the_train_x, the_train_y, kernel = k)
    
    valid_pred <- predict(bsvm, the_valid_x)
    bsvm_valid_conf = table(the_valid_y, valid_pred)
    # print(bsvm_valid_conf)
    # cat('\n\n\n')
    
    
    tp = bsvm_valid_conf[2,2]
    fp = bsvm_valid_conf[1,2]
    tn = bsvm_valid_conf[1,1]
    fn = bsvm_valid_conf[2,1]
    
    bsvm_valid_acc = (tp+tn)/(tp+fp+tn+fn)
    
    if(best_acc < bsvm_valid_acc){
      cat('Libsvm Tuning..', k, '\n')
      best_acc = bsvm_valid_acc
      best_k = k
    }
    
  }
  
  
  # no tuning, just assign
  # best_k = 'radial'
  
  bsvm <- svm(the_train_x, the_train_y, kernel = best_k)
  
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
  if(bsvm_valid_acc >= blend_thresh){
    test_pred = as.vector(bsvm_test_pred)
    test_pred[test_pred=='improved'] = 1
    test_pred[test_pred=='unimproved'] = 0
    test_pred = as.numeric(test_pred)
    # print(test_pred)
    test_preds = test_preds + bsvm_valid_acc*test_pred
  }
  
  
  ###########################
  ########### svm ###########
  ########### END ###########
  ###########################
  
  
  ###########################
  ########### XGB ###########
  ########## START ##########
  ###########################
  
  
  xgb_train_y = as.vector(the_train_y)
  xgb_train_y[xgb_train_y=='improved'] = 1
  xgb_train_y[xgb_train_y=='unimproved'] = 0
  xgb_train_y = as.numeric(xgb_train_y)
  
  
  xgb_valid_y = as.vector(the_valid_y)
  xgb_valid_y[xgb_valid_y=='improved'] = 1
  xgb_valid_y[xgb_valid_y=='unimproved'] = 0
  xgb_valid_y = as.numeric(xgb_valid_y)
  
  xgb_test_y = as.vector(the_test_y)
  xgb_test_y[xgb_test_y=='improved'] = 1
  xgb_test_y[xgb_test_y=='unimproved'] = 0
  xgb_test_y = as.numeric(xgb_test_y)
  
  
  dtrain <- xgb.DMatrix(as.matrix(the_train_x), label = xgb_train_y)
  dvalid <- xgb.DMatrix(as.matrix(the_valid_x), label = xgb_valid_y)
  
  # tuning depth, nround
  
  d_min = 2
  d_max = 10
  dgrids = unique(round(seq(d_min, d_max, length= 5)))
  
  r_min = 2
  r_max = 10
  rgrids = unique(round(seq(r_min, r_max, length= 5)))
  
  best_acc = -1
  best_d = 0
  best_r = 0
  
  for (d1 in dgrids){
    for (r1 in rgrids){
      param <- list(max_depth = d1, eta = 1, silent = 1)
      watchlist <- list(eval = dvalid, train = dtrain)
      bxgb = xgb.train(param, dtrain, watchlist = watchlist, nrounds = r1)
      bxgb_valid_pred = ifelse((predict(bxgb, as.matrix(the_valid_x)))>=0.5,1,0)
      bxgb_valid_acc = sum(xgb_valid_y == bxgb_valid_pred)/length(xgb_valid_y)
      
      print(bxgb_valid_acc)

      if(best_acc < bxgb_valid_acc){
        cat('Xgb Tuning..', d1, r1, '\n')
        best_acc = bxgb_valid_acc
        best_d = d1
        best_r = r1
      }
    }
  }
  
  
  # no tuning, just assign
  # best_d = 2
  # best_r = 2
  
  param <- list(max_depth = best_d, eta = 1, silent = 1)
  watchlist <- list(eval = dvalid, train = dtrain)
  bxgb = xgb.train(param, dtrain, watchlist = watchlist, nrounds = best_r)
  
  bxgb_train_pred = ifelse((predict(bxgb, as.matrix(the_train_x)))>=0.5,1,0)
  bxgb_valid_pred = ifelse((predict(bxgb, as.matrix(the_valid_x)))>=0.5,1,0)
  bxgb_test_pred = ifelse((predict(bxgb, as.matrix(the_test_x)))>=0.5,1,0)
  
  bxgb_train_acc = sum(xgb_train_y == bxgb_train_pred)/length(xgb_train_y)
  bxgb_valid_acc = sum(xgb_valid_y == bxgb_valid_pred)/length(xgb_valid_y)
  bxgb_test_acc = sum(xgb_test_y == bxgb_test_pred)/length(xgb_test_y)
  
  cat('xgb train acc', bxgb_train_acc, '\n\n')
  cat('xgb valid acc', bxgb_valid_acc, '\n\n')
  cat('xgb test acc', bxgb_test_acc, '\n\n')
  
  
  bxgb_test_pred = ifelse((predict(bxgb, as.matrix(the_test_x))) >= 0.5, 1, 0)
  
  # blending
  if(bxgb_valid_acc >= blend_thresh){
    test_pred = as.vector(bxgb_test_pred)
    test_preds = test_preds + bxgb_valid_acc*test_pred
  }
  
  
  
  ###########################
  ########### XGB ###########
  ########### END ###########
  ###########################
  
  
  
  blog_train_accs = c(blog_train_accs, blog_train_acc)
  blog_valid_accs = c(blog_valid_accs, blog_valid_acc)
  blog_test_accs = c(blog_test_accs, blog_test_acc)
  
  
  brf_train_accs = c(brf_train_accs, brf_train_acc)
  brf_valid_accs = c(brf_valid_accs, brf_valid_acc)
  brf_test_accs = c(brf_test_accs, brf_test_acc)
  
  
  bsvm_train_accs = c(bsvm_train_accs, bsvm_train_acc)
  bsvm_valid_accs = c(bsvm_valid_accs, bsvm_valid_acc)
  bsvm_test_accs = c(bsvm_test_accs, bsvm_test_acc)
  
  bxgb_train_accs = c(bxgb_train_accs, bxgb_train_acc)
  bxgb_valid_accs = c(bxgb_valid_accs, bxgb_valid_acc)
  bxgb_test_accs = c(bxgb_test_accs, bxgb_test_acc)
  
  print(test_preds)
  
  
}

cat('RESULT', '\n\n')
cat('mean logr train acc', mean(blog_train_accs), '\n\n')
cat('mean logr valid acc', mean(blog_valid_accs), '\n\n')
cat('mean logr test acc', mean(blog_test_accs), '\n\n')
cat('\n')
cat('mean rf train acc', mean(brf_train_accs), '\n\n')
cat('mean rf valid acc', mean(brf_valid_accs), '\n\n')
cat('mean rf test acc', mean(brf_test_accs), '\n\n')
cat('\n')
cat('mean svm train acc', mean(bsvm_train_accs), '\n\n')
cat('mean svm valid acc', mean(bsvm_valid_accs), '\n\n')
cat('mean svm test acc', mean(bsvm_test_accs), '\n\n')
cat('\n')
cat('mean xgb train acc', mean(bxgb_train_accs), '\n\n')
cat('mean xgb valid acc', mean(bxgb_valid_accs), '\n\n')
cat('mean xgb test acc', mean(bxgb_test_accs), '\n\n')



cat('BLENDING MODEL RESULT', '\n\n')

# all_w = sum(c(blog_valid_accs,brf_valid_accs,bsvm_valid_accs,bxgb_valid_accs))

thresh = 0.5
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







