library(readr)
library(dplyr)


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
  
  cat('out_of_pos_transfer: ', x, '\n')
  return(NA)
  
  
}




# load data
year = 2012
salary_path = paste0('./salary_data/salary_', year, '.csv')
data_path = paste0('./mlb_data/data_', year, '_valid.csv')

salary_data = read_csv(salary_path)
mlb_data = read_csv(data_path)


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

# handle mlb_data

mlb_data$Player = sapply(mlb_data$Player, function(x) substr(x, 2, nchar(x)))

mlb_data <- mlb_data %>%
  arrange(Player)

names(mlb_data)[3] = 'Pos2'
names(mlb_data)[16] = 'AVG'



# merge data

data <- merge(x = mlb_data, y = salary_data, by = c('Player', 'Team'))

data_name = names(data)[4:19]




data = data[,c("Player", "Player_Full_Name", "Team", "Pos1_Detailed", "Pos1", "Pos2", data_name, "Salary")]

data[data=='.---'] = NA

data <- data %>%
  arrange(Player) %>%
  arrange(desc(Salary))


salary_data_loss = setdiff(salary_data$Player_Full_Name, data$Player_Full_Name)
# print(salary_data_loss)
mlb_data_loss = setdiff(mlb_data$Player, data$Player)
# print(mlb_data_loss)


# same name, team
duplicated_data = data[duplicated(data[,c(1,3)]),]


if(year == 2012){

}

if(year == 2013){

}

if(year == 2014){

}

if(year == 2015){
  data = data[c(-685),]
}

if(year == 2016){

}

if(year == 2017){

}


cat('salary_data_loss', length(salary_data_loss), '\n')
cat('mlb_data_loss', length(mlb_data_loss), '\n\n')
cat('data_collect', nrow(data), '\n')


View(data)
View(salary_data)
View(mlb_data)
View(duplicated_data)

write.csv(data, file = paste0('./merge_data/merge_data_',year,'.csv'))







