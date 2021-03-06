library(readr)
library(dplyr)


# load data
year = 2015
salary_path = paste0('./salary_data/salary_', year, '_valid.csv')
hitting_path = paste0('./hitting_data/data_', year, '_hitting_valid.csv')
fielding_path = paste0('./fielding_data/data_', year, '_fielding_valid.csv')
pitching_path = paste0('./pitching_data/data_', year, '_pitching_valid.csv')

salary_data = read_csv(salary_path)
hitting_data = read_csv(hitting_path)
fielding_data = read_csv(fielding_path)
pitching_data = read_csv(pitching_path)


pos_transfer <- function(x){
  if(grepl('PITCHERS',x) || grepl('STARTING PITCHER',x) || grepl('RELIEF PITCHER',x) || grepl('CLOSER',x)){
    return('P')
  }
  
  if(grepl('CATCHER', x)){
    return('C')
  }
  
  if(grepl('1ST BASE', x)){
    return('1B')
  }
  
  if(grepl('2ND BASE', x)){
    return('2B')
  }
  
  if(grepl('3RD BASE', x)){
    return('3B')
  }
  
  if(grepl('3RD BASE', x)){
    return('3B')
  }
  
  if(grepl('LEFT FIELD', x)){
    return('LF')
  }
  
  if(grepl('RIGHT FIELD', x)){
    return('RF')
  }
  
  if(grepl('CENTER FIELD', x)){
    return('CF')
  }
  
  
  if(grepl('SHORTSTOP', x)){
    return('SS')
  }
  
  if(grepl('DESIGNATED HITTER', x)){
    return('DH')
  }
  
  if(grepl('OUTFIELDERS', x)){
    return(NA)
  }
  
  cat('out_of_pos_transfer: ', x, '\n')
  return(NA)
  
  
}






# handle salary_data
position = salary_data[seq(from = 2, to = nrow(salary_data), by = 2),][2]
names(position) = c('POSITION')
salary_data = salary_data[seq(from = 1, to = nrow(salary_data), by = 2),]
salary_data = cbind(salary_data, position)


salary_data$SALARY = gsub('\xa0','',(salary_data$SALARY))
salary_data$SALARY = gsub("\\$",'',(salary_data$SALARY))
salary_data$SALARY = gsub(',','',(salary_data$SALARY))
salary_data$SALARY = as.numeric(salary_data$SALARY)

salary_data$Player = sapply(salary_data$PLAYER, function(x) paste0(tail(strsplit(x,' ')[[1]],1),', ', substr(x,1,1)))

salary_data$Player = sapply(salary_data$Player, function(x) paste0(toupper(substr(x, 1, 1)), substr(x, 2, nchar(x))))

salary_data$Pos = sapply(salary_data$POSITION, function(x) pos_transfer(x))

names(salary_data) = c('Team', 'Player_Full_Name', 'Salary', 'Pos1_Detailed', 'Player', 'Pos1')
salary_data = salary_data[,c('Player', 'Player_Full_Name', 'Team', 'Pos1', 'Pos1_Detailed', 'Salary')]


salary_data <- salary_data %>%
  arrange(Player) %>%
  arrange(desc(Salary))

# handle mlb data

mlb_key_col = c('Player','Team')

if(year==2011){
  hitting_data <- hitting_data[,-which(names(hitting_data) %in% c('RK'))]
}

fielding_data <- fielding_data[,-which(names(fielding_data) %in% c('RK'))]
pitching_data <- pitching_data[,-which(names(pitching_data) %in% c('RK'))]

names(hitting_data) = paste('h_', names(hitting_data), sep='')
names(fielding_data) = paste('f_', names(fielding_data), sep='')
names(pitching_data) = paste('p_', names(pitching_data), sep='')

names(hitting_data)[1:2] = mlb_key_col
names(fielding_data)[1:2] = mlb_key_col
names(pitching_data)[1:2] = mlb_key_col

mlb_data <- merge(x = hitting_data, y = fielding_data, all=TRUE, by = mlb_key_col)
mlb_data <- merge(x = mlb_data, y = pitching_data, all=TRUE, by = mlb_key_col)
names(mlb_data) = gsub('¡¿','',names(mlb_data))

mlb_pos = c()
for (i in 1:nrow(mlb_data)){
  if(!is.na(mlb_data[,c('h_Pos')][i])){
    mlb_pos[i] = mlb_data[,c('h_Pos')][i]
  }else{
    mlb_pos[i] = mlb_data[,c('f_Pos')][i]
  }
}


mlb_data <- mlb_data[,-which(names(mlb_data) %in% c('h_Pos','f_Pos'))]
mlb_data$Pos = mlb_pos
mlb_data = do.call("cbind", list(mlb_data[,c('Player','Team')],mlb_data[,c('Pos')],mlb_data[,-which(names(mlb_data) %in%c('Player','Team','Pos'))]))
names(mlb_data)[3] = 'Pos2'


mlb_data$Player = sapply(mlb_data$Player, function(x) substr(x, 2, nchar(x)))

mlb_data <- mlb_data %>%
  arrange(Player)


 

# merge data

data <- merge(x = mlb_data, y = salary_data, by = c('Player', 'Team'))

numeric_data_name = names(data)[seq(from=4, to=ncol(data)-4)]




data = data[,c("Player", "Player_Full_Name", "Team", "Pos1_Detailed", "Pos1", "Pos2", numeric_data_name, "Salary")]

data[data=='.---'] = NA
data[data=='-.--'] = NA
data[data=='-'] = NA
data[data=='*.**'] = NA




data <- data %>%
  arrange(Player) %>%
  arrange(desc(Salary))




salary_data_loss = setdiff(salary_data$Player_Full_Name, data$Player_Full_Name)
# print(salary_data_loss)
mlb_data_loss = setdiff(mlb_data$Player, data$Player)
# print(mlb_data_loss)





# handle duplicated
# same name, team
duplicated_data = data[duplicated(data[,c(1,3)]),c(1,3)]
duplicated_data = duplicated_data[!duplicated(duplicated_data),]

duplicated_data <- data %>%
  filter(Player %in% duplicated_data$Player & Team %in% duplicated_data$Team) %>%
  arrange(Player)

data <- data %>%
  filter(!(Player %in% duplicated_data$Player & Team %in% duplicated_data$Team))



# same name
name_duplicated_data = data[duplicated(data[,c(1,2)]),c(1,2)]
name_duplicated_data = name_duplicated_data[!duplicated(name_duplicated_data),]

name_duplicated_data <- data %>%
  filter(Player_Full_Name %in% name_duplicated_data$Player_Full_Name) %>%
  arrange(Player)

# # data <- data %>%
# #   filter(!(Player_Full_Name %in% name_duplicated_data$Player_Full_Name))






cat('salary_data_loss', length(salary_data_loss), '\n')
cat('mlb_data_loss', length(mlb_data_loss), '\n\n')
cat('data_collect', nrow(data), '\n')



# View(salary_data)
# View(mlb_data)
# View(duplicated_data)
# View(name_duplicated_data)
# View(data)



write.csv(data, file = paste0('./merge_data/merge_data_',year,'.csv'), row.names = FALSE)







