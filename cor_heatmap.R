library(readr)
library(dplyr)
library(ggplot2)
merge_data_2011 <- read_csv("./merge_data/merge_data_2011.csv")
merge_data_2012 <- read_csv("./merge_data/merge_data_2012.csv")
merge_data_2013 <- read_csv("./merge_data/merge_data_2013.csv")
merge_data_2014 <- read_csv("./merge_data/merge_data_2014.csv")
merge_data_2015 <- read_csv("./merge_data/merge_data_2015.csv")
merge_data_2016 <- read_csv("./merge_data/merge_data_2016.csv")
data_2011 = merge_data_2011
data_2011[is.na(data_2011)] = 0
data_2012 = merge_data_2012
data_2012[is.na(data_2012)] = 0
data_2013 = merge_data_2013
data_2013[is.na(data_2013)] = 0
data_2014 = merge_data_2014
data_2014[is.na(data_2014)] = 0
data_2015 = merge_data_2015
data_2015[is.na(data_2015)] = 0
data_2016 = merge_data_2016
data_2016[is.na(data_2016)] = 0
names(data_2016)[ncol(data_2016)] = 'Salary_to_predict'
data_2016 <- data_2016 %>%
  select(Player_Full_Name,Team,Salary_to_predict)
complete_2015 <- merge(x = data_2015, y = data_2016, by = c('Player_Full_Name')) %>%
  arrange(desc(Salary_to_predict))
names(data_2015)[ncol(data_2015)] = 'Salary_to_predict'
data_2015 <- data_2015 %>%
  select(Player_Full_Name,Team,Salary_to_predict)
complete_2014 <- merge(x = data_2014, y = data_2015, by = c('Player_Full_Name')) %>%
  arrange(desc(Salary_to_predict))
names(data_2014)[ncol(data_2014)] = 'Salary_to_predict'
data_2014 <- data_2014 %>%
  select(Player_Full_Name,Team,Salary_to_predict)
complete_2013 <- merge(x = data_2013, y = data_2014, by = c('Player_Full_Name')) %>%
  arrange(desc(Salary_to_predict))
names(data_2013)[ncol(data_2013)] = 'Salary_to_predict'
data_2013 <- data_2013 %>%
  select(Player_Full_Name,Team,Salary_to_predict)
complete_2012 <- merge(x = data_2012, y = data_2013, by = c('Player_Full_Name')) %>%
  arrange(desc(Salary_to_predict))
names(data_2012)[ncol(data_2012)] = 'Salary_to_predict'
data_2012 <- data_2012 %>%
  select(Player_Full_Name,Team,Salary_to_predict)
complete_2011 <- merge(x = data_2011, y = data_2012, by = c('Player_Full_Name')) %>%
  arrange(desc(Salary_to_predict))
complete_p_2011 = complete_2011[which(complete_2011$Pos2 == 'P'),]
complete_p_2012 = complete_2012[which(complete_2012$Pos2 == 'P'),]
complete_p_2013 = complete_2013[which(complete_2013$Pos2 == 'P'),]
complete_p_2014 = complete_2014[which(complete_2014$Pos2 == 'P'),]
complete_p_2015 = complete_2015[which(complete_2015$Pos2 == 'P'),]
complete_c_2011 = complete_2011[which(complete_2011$Pos2 == 'C'),]
complete_c_2012 = complete_2012[which(complete_2012$Pos2 == 'C'),]
complete_c_2013 = complete_2013[which(complete_2013$Pos2 == 'C'),]
complete_c_2014 = complete_2014[which(complete_2014$Pos2 == 'C'),]
complete_c_2015 = complete_2015[which(complete_2015$Pos2 == 'C'),]
complete_f_2011 = complete_2011[which(complete_2011$Pos2 != 'P'),]
complete_f_2012 = complete_2012[which(complete_2012$Pos2 != 'P'),]
complete_f_2013 = complete_2013[which(complete_2013$Pos2 != 'P'),]
complete_f_2014 = complete_2014[which(complete_2014$Pos2 != 'P'),]
complete_f_2015 = complete_2015[which(complete_2015$Pos2 != 'P'),]
complete_cor_p_2011 = cor(complete_p_2011[,c(35:42, 48:92, 94)])
complete_cor_p_2012 = cor(complete_p_2012[,c(35:42, 48:92, 94)])
complete_cor_p_2013 = cor(complete_p_2013[,c(35:42, 48:92, 94)])
complete_cor_p_2014 = cor(complete_p_2014[,c(35:42, 48:92, 94)])
complete_cor_p_2015 = cor(complete_p_2015[,c(35:42, 48:92, 94)])
complete_cor_c_2011 = cor(complete_c_2011[,c(7:49, 92, 94)])
complete_cor_c_2012 = cor(complete_c_2012[,c(7:49, 92, 94)])
complete_cor_c_2013 = cor(complete_c_2013[,c(7:49, 92, 94)])
complete_cor_c_2014 = cor(complete_c_2014[,c(7:49, 92, 94)])
complete_cor_c_2015 = cor(complete_c_2015[,c(7:49, 92, 94)])
complete_cor_f_2011 = cor(complete_f_2011[,c(7:42, 48:49, 92, 94)])
complete_cor_f_2012 = cor(complete_f_2012[,c(7:42, 48:49, 92, 94)])
complete_cor_f_2013 = cor(complete_f_2013[,c(7:42, 48:49, 92, 94)])
complete_cor_f_2014 = cor(complete_f_2014[,c(7:42, 48:49, 92, 94)])
complete_cor_f_2015 = cor(complete_f_2015[,c(7:42, 48:49, 92, 94)])
complete_cor_p = c()
complete_cor_p = rbind(complete_cor_p, complete_cor_p_2011[nrow(complete_cor_p_2011),])
complete_cor_p = rbind(complete_cor_p, complete_cor_p_2012[nrow(complete_cor_p_2012),])
complete_cor_p = rbind(complete_cor_p, complete_cor_p_2013[nrow(complete_cor_p_2013),])
complete_cor_p = rbind(complete_cor_p, complete_cor_p_2014[nrow(complete_cor_p_2014),])
complete_cor_p = rbind(complete_cor_p, complete_cor_p_2015[nrow(complete_cor_p_2015),])
complete_cor_p = complete_cor_p[,1:ncol(complete_cor_p)-1]
complete_cor_c = c()
complete_cor_c = rbind(complete_cor_c, complete_cor_c_2011[nrow(complete_cor_c_2011),])
complete_cor_c = rbind(complete_cor_c, complete_cor_c_2012[nrow(complete_cor_c_2012),])
complete_cor_c = rbind(complete_cor_c, complete_cor_c_2013[nrow(complete_cor_c_2013),])
complete_cor_c = rbind(complete_cor_c, complete_cor_c_2014[nrow(complete_cor_c_2014),])
complete_cor_c = rbind(complete_cor_c, complete_cor_c_2015[nrow(complete_cor_c_2015),])
complete_cor_c = complete_cor_c[,1:ncol(complete_cor_c)-1]
complete_cor_f = c()
complete_cor_f = rbind(complete_cor_f, complete_cor_f_2011[nrow(complete_cor_f_2011),])
complete_cor_f = rbind(complete_cor_f, complete_cor_f_2012[nrow(complete_cor_f_2012),])
complete_cor_f = rbind(complete_cor_f, complete_cor_f_2013[nrow(complete_cor_f_2013),])
complete_cor_f = rbind(complete_cor_f, complete_cor_f_2014[nrow(complete_cor_f_2014),])
complete_cor_f = rbind(complete_cor_f, complete_cor_f_2015[nrow(complete_cor_f_2015),])
complete_cor_f = complete_cor_f[,1:ncol(complete_cor_f)-1]
complete_cor_p = abs(complete_cor_p)
complete_cor_c = abs(complete_cor_c)
complete_cor_f = abs(complete_cor_f)
rownames(complete_cor_p) = c(2011, 2012, 2013, 2014, 2015)
rownames(complete_cor_c) = c(2011, 2012, 2013, 2014, 2015)
rownames(complete_cor_f) = c(2011, 2012, 2013, 2014, 2015)
result_p <- expand.grid(years = c(rownames(complete_cor_p)), factors = c(colnames(complete_cor_p)))
result_c <- expand.grid(years = c(rownames(complete_cor_c)), factors = c(colnames(complete_cor_c)))
result_f <- expand.grid(years = c(rownames(complete_cor_f)), factors = c(colnames(complete_cor_f)))
set.seed(41)
result_p$performance <- cut(as.vector(complete_cor_p), breaks = 40)
result_c$performance <- cut(as.vector(complete_cor_c), breaks = 40)
result_f$performance <- cut(as.vector(complete_cor_f), breaks = 40)
colors = colorRampPalette(c("white", "deepskyblue"))(40)
##### pitcher heatmap start #####
ggplot(data = result_p, aes(x = years, y = factors)) +
  geom_tile(aes(fill = performance)) +
  scale_fill_manual(breaks=result_p$performance, values=colors, guide=FALSE) +
  ggtitle("Pitcher") + 
  theme(plot.title = element_text(hjust = 0.5))
###### pitcher heatmap end ######
##### catcher heatmap start #####
ggplot(data = result_c, aes(x = years, y = factors)) +
  geom_tile(aes(fill = performance)) +
  scale_fill_manual(breaks=result_c$performance, values=colors, guide=FALSE) +
  ggtitle("Catcher") + 
  theme(plot.title = element_text(hjust = 0.5))
###### catcher heatmap end ######
##### fielder heatmap start #####
ggplot(data = result_f, aes(x = years, y = factors)) +
  geom_tile(aes(fill = performance)) +
  scale_fill_manual(breaks=result_f$performance, values=colors, guide=FALSE) +
  ggtitle("Fielder") + 
  theme(plot.title = element_text(hjust = 0.5))
###### fielder heatmap end ######
complete_cor_p = rbind(complete_cor_p, c(rep(0, ncol(complete_cor_p))))
for (i in 1:ncol(complete_cor_p)) {
  complete_cor_p[6,i] = mean(complete_cor_p[1:5,i])
}
complete_cor_c = rbind(complete_cor_c, c(rep(0, ncol(complete_cor_c))))
for (i in 1:ncol(complete_cor_c)) {
  complete_cor_c[6,i] = mean(complete_cor_c[1:5,i])
}
complete_cor_f = rbind(complete_cor_f, c(rep(0, ncol(complete_cor_f))))
for (i in 1:ncol(complete_cor_f)) {
  complete_cor_f[6,i] = mean(complete_cor_f[1:5,i])
}
complete_cor_p = complete_cor_p[, order(complete_cor_p[6,], complete_cor_p[1,], complete_cor_p[2,], complete_cor_p[3,], complete_cor_p[4,], complete_cor_p[5,])]
complete_cor_c = complete_cor_c[, order(complete_cor_c[6,], complete_cor_c[1,], complete_cor_c[2,], complete_cor_c[3,], complete_cor_c[4,], complete_cor_c[5,])]
complete_cor_f = complete_cor_f[, order(complete_cor_f[6,], complete_cor_f[1,], complete_cor_f[2,], complete_cor_f[3,], complete_cor_f[4,], complete_cor_f[5,])]
result_p <- expand.grid(years = c(rownames(complete_cor_p)[1:5]), factors = c(colnames(complete_cor_p)))
result_c <- expand.grid(years = c(rownames(complete_cor_c)[1:5]), factors = c(colnames(complete_cor_c)))
result_f <- expand.grid(years = c(rownames(complete_cor_f)[1:5]), factors = c(colnames(complete_cor_f)))
set.seed(41)
result_p$performance <- cut(as.vector(complete_cor_p[1:5,]), breaks = 40)
result_c$performance <- cut(as.vector(complete_cor_c[1:5,]), breaks = 40)
result_f$performance <- cut(as.vector(complete_cor_f[1:5,]), breaks = 40)
colors = colorRampPalette(c("white", "deepskyblue"))(40)
##### pitcher sorted heatmap start #####
ggplot(data = result_p, aes(x = years, y = factors)) +
  geom_tile(aes(fill = result_p$performance)) +
  scale_fill_manual(breaks=result_p$performance, values=colors, guide=FALSE) +
  ggtitle("Pitcher") + 
  theme(plot.title = element_text(hjust = 0.5))
###### pitcher sorted heatmap end ######
##### catcher sorted heatmap start #####
ggplot(data = result_c, aes(x = years, y = factors)) +
  geom_tile(aes(fill = result_c$performance)) +
  scale_fill_manual(breaks=result_c$performance, values=colors, guide=FALSE) +
  ggtitle("Catcher") + 
  theme(plot.title = element_text(hjust = 0.5))
###### catcher sorted heatmap end ######
##### fielder sorted heatmap start #####
ggplot(data = result_f, aes(x = years, y = factors)) +
  geom_tile(aes(fill = result_f$performance)) +
  scale_fill_manual(breaks=result_f$performance, values=colors, guide=FALSE) +
  ggtitle("Fielder") + 
  theme(plot.title = element_text(hjust = 0.5))
###### fielder sorted heatmap end ######