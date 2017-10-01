library (readxl)

speed_extract <- function(df){
  df = subset(df, xCordinate <= 960)
  
  time_next = df$timeStampUnix
  time_next = time_next[-1]
  time_next = c(time_next,0)
  
  
  df$timeInterval =  time_next - df$timeStampUnix
  df = df[-nrow(df),]
  
  cut_index = which(df$timeInterval > 0.06)
  
  cluster_reading = list()
  start_index = 1
  for (i in 1:length(cut_index)){
    cluster_reading[[i]] = df[(start_index:cut_index[i]),]
    start_index = cut_index[i] + 1
  }
  
  average_distance_each_group = c()
  
  for (i in 1:length(cluster_reading)){
    if (nrow(cluster_reading[[i]]) < 2) next
    cluster_reading[[i]]$distance = NA
    for (j in 2:nrow(cluster_reading[[i]])){
      cluster_reading[[i]]$distance[j]=sqrt((cluster_reading[[i]]$xCordinate[j]- cluster_reading[[i]]$xCordinate[j-1])^2 +
                                              (cluster_reading[[i]]$yCordinate[j]- cluster_reading[[i]]$yCordinate[j-1])^2)
    }
    distances = cluster_reading[[i]]$distance[2:nrow(cluster_reading[[i]])]
    average_distance = mean(distances)
    average_distance_each_group = c(average_distance_each_group, average_distance)
  }
  
  average_distance_each_group = average_distance_each_group[which(average_distance_each_group < 60)]
  
  #plot(average_distance_each_group, ylim = c(0,60))
  #print(mean(average_distance_each_group))
  return(mean(average_distance_each_group))
}

extract_feature <- function(path){
  print(paste("Extracting", path, "..."))
  
  # Read all CSV file
  df = read.csv(path, header = FALSE)
  colnames(df) = c("timeStamp", "xCordinate", "yCordinate")
  
  # Clean the CSV file
  remove_list = c()
  for (i in 1:nrow(df)){
    if ((df$xCordinate[i] == 0 && df$yCordinate[i] == 0) ||
        df$xCordinate[i] < 0 || df$yCordinate[i] < 0) {
      remove_list = c(remove_list, i)
    }
  }
  
  df.cleaned = df[- remove_list, ]
  
  # Get the time stamp
  df.cleaned$timeStampUnix = as.numeric(as.POSIXct(df.cleaned$timeStamp))
  time.begin = df.cleaned$timeStampUnix[1]
  time.end = df.cleaned$timeStampUnix[nrow(df.cleaned)]
  
  Total_Time = (time.end-time.begin)/60 #In minutes
  print("Total time was extracted.")
  
  # Split the passage and the questions 
  df.passage = subset(df.cleaned, xCordinate <= 1920/2)
  df.question = subset(df.cleaned, xCordinate > 1920/2)
  
  slices = c(nrow(df.passage), nrow(df.question))
  pct = round(slices/sum(slices)*100)
  
  Percent_Passage = pct[1]
  Percent_Question = pct[2]
  print("Percent passage and percent question were extracted.")
  
  # Count how many time you switch
  Total_Switch_Count = 0
  for (i in 1:(nrow(df.cleaned)-1)) {
    if (df.cleaned$xCordinate[i] <= 960 && df.cleaned$xCordinate[i+1] >960 ) Total_Switch_Count = Total_Switch_Count + 1
    if (df.cleaned$xCordinate[i] > 960 && df.cleaned$xCordinate[i+1] <= 960 ) Total_Switch_Count = Total_Switch_Count + 1
  }
  print("Total switch count was extracted.")
  
  #Find how much time is spent on beginning for reading passage
  #Calculate time distrubution for each interval seconds
  t1 = time.begin
  passage.all.count = c()
  question.all.count = c()
  time.mark <- c()
  interval = 10
  
  while (t1 < time.end) {
    t2 = t1 +interval
    df.temp = subset(df.cleaned, df.cleaned$timeStampUnix >= t1 & df.cleaned$timeStampUnix < t2 )
    if (nrow(df.temp) > 0) {
      # Count how many times you read the passage and how many times you read the questions
      df.passage <- subset(df.temp, xCordinate <= 1920/2)
      df.question <- subset(df.temp, xCordinate > 1920/2)
      
      passage.all.count = c(passage.all.count, nrow(df.passage))
      question.all.count = c(question.all.count, nrow(df.question))
    } else {
      passage.all.count = c(passage.all.count,0)
      question.all.count = c(question.all.count,0)
    }
    time.mark = c(time.mark, interval * length(passage.all.count))
    t1 = t2
  }
  
  percent.passage = (passage.all.count / (passage.all.count + question.all.count)) * 100 
  percent.question = (question.all.count / (passage.all.count + question.all.count)) * 100
  
  i = 2
  while(percent.passage[i] >= 80){
    i = i+1
  }
  
  Time_Passage_Beginning = (i-1)*10
  print("Time passage beginning has been extracted")
  
  # Find the amount of interval (10s) that the student spend solely on reading Passage in the whole time 
  Amount_Interval_Only_Passage <- sum(percent.passage > 80)
  if (is.na(Amount_Interval_Only_Passage)) Amount_Interval_Only_Passage = 0
  print("Amount of interval (10s) that the student spend solely on reading Passage has been extracted")
  
  # Find the amount of interval (10s) that the student spend solely on reading Question in the whole time 
  Amount_Interval_Only_Question <- sum(percent.question > 80)
  if (is.na(Amount_Interval_Only_Question)) Amount_Interval_Only_Question = 0
  print("Amount of interval (10s) that the student spend solely on reading Question has been extracted")
  
  # Find the percent of reading passage in first 4 mins and 
  # Find the percent of reading question in first 4 mins
  point_passage_first_4_mins = sum(passage.all.count[1:24])
  point_question_first_4_mins = sum(question.all.count[1:24])
  
  First_4mins_Percent_Passage = point_passage_first_4_mins / (point_passage_first_4_mins + point_question_first_4_mins)
  First_4mins_Percent_Question = point_question_first_4_mins / (point_passage_first_4_mins + point_question_first_4_mins)
  
  print("First 4 mins, percent of reading Passage and Question has been extracted")
  
  # Find the percent of reading passage in last 4 mins and 
  # Find the percent of reading question in last 4 mins
  n = length(passage.all.count)
  point_passage_last_4_mins = sum(passage.all.count[(n-24):n])
  point_question_last_4_mins = sum(question.all.count[(n-24):n])
  
  Last_4mins_Percent_Passage = point_passage_last_4_mins / (point_passage_last_4_mins + point_question_last_4_mins)
  Last_4mins_Percent_Question = point_question_last_4_mins / (point_passage_last_4_mins + point_question_last_4_mins)
  
  print("Last 4 mins, percent of reading Passage and Question has been extracted")
  
  # Find the speed of reading passage. The faster the speed, the distance between 2 points should be further.
  # The function return the average distance between 2 points
  
  Reading_Passage_Speed <- speed_extract(df.cleaned)
  print("Reading passage speed has been extracted")
  
  result <- c(Total_Time, 
              Percent_Passage, 
              Percent_Question, 
              Total_Switch_Count, 
              Time_Passage_Beginning,
              Amount_Interval_Only_Passage,
              Amount_Interval_Only_Question,
              First_4mins_Percent_Passage,
              First_4mins_Percent_Question,
              Last_4mins_Percent_Passage,
              Last_4mins_Percent_Question, 
              Reading_Passage_Speed)
  
  names(result) <- c("Total_Time", 
                     "Percent_Passage", 
                     "Percent_Question", 
                     "Total_Switch_Count", 
                     "Time_Passage_Beginning",
                     "Amount_Interval_Only_Passage",
                     "Amount_Interval_Only_Question",
                     "First_4mins_Percent_Passage",
                     "First_4mins_Percent_Question",
                     "Last_4mins_Percent_Passage",
                     "Last_4mins_Percent_Question",
                     "Reading_Passage_Speed"
  )
  return(result)
}

# Set the working directory as the script file
script.dir <- dirname(sys.frame(1)$ofile)
setwd(script.dir)

# Read the excel file
df = read_excel("DATA COLLECTION STATS.xlsx", sheet = 1)
df <- as.data.frame(df)

# Clean bad data out 
df = subset(df, df$Bad_Data == FALSE)

# Prepare the path for files
base.folder<- paste(script.dir,"/../EyeGazeData/",sep="")
df$path = ""

for (i in 1:nrow(df)){
  df$path[i] = paste(base.folder, df$Date_of_Experiment[i], "_", 
                     df$Time_of_Experiment[i], "_", 
                     df$First_Name[i], "_", 
                     df$Last_Name[i], "_",
                     "GazeData.csv", sep="")
}

# Prepare the feature columns
df$Total_Time = NA
df$Percent_Passage = NA
df$Percent_Question = NA
df$Total_Switch_Count = NA
df$Time_Passage_Beginning = NA
df$Amount_Interval_Only_Passage = NA
df$Amount_Interval_Only_Question = NA
df$First_4mins_Percent_Passage = NA
df$First_4mins_Percent_Question = NA
df$Last_4mins_Percent_Passage = NA
df$Last_4mins_Percent_Question = NA
df$Reading_Passage_Speed = NA
nfeatures = 12

for (i in 1:nrow(df)){
  result <- extract_feature(df$path[i])
  df$Total_Time[i] = result["Total_Time"]
  df$Percent_Passage[i] = result["Percent_Passage"]
  df$Percent_Question[i] = result["Percent_Question"]
  df$Total_Switch_Count[i] = result["Total_Switch_Count"]
  df$Time_Passage_Beginning[i] = result["Time_Passage_Beginning"]
  df$Amount_Interval_Only_Passage[i] = result["Amount_Interval_Only_Passage"]
  df$Amount_Interval_Only_Question[i] = result["Amount_Interval_Only_Question"]
  df$First_4mins_Percent_Passage[i] = result["First_4mins_Percent_Passage"]
  df$First_4mins_Percent_Question[i] = result["First_4mins_Percent_Question"]
  df$Last_4mins_Percent_Passage[i] = result["Last_4mins_Percent_Passage"]
  df$Last_4mins_Percent_Question[i] = result["Last_4mins_Percent_Question"]
  df$Reading_Passage_Speed[i] = result["Reading_Passage_Speed"]
}

feature_name = names(result)

#split the data set into two different groups
# Upper group correct asnwer = > 9, then else
df$high.score = df$Correct_Answer >= 9
df.upper = subset(df, df$high.score)
df.lower = subset(df, !df$high.score)


for (i in 1:nfeatures){
  feature = df[feature_name[i]][,1]
  correlation = cor(df$Correct_Answer, feature)
  plot(feature, df$Correct_Answer,
       main = paste("Correlation of # of correct answer vs",feature_name[i], ":\n" , correlation),
       xlab = feature_name[i],
       ylab = "Correct answers",
       pch = 19, cex = 2,
       #col = rgb(0,0,1,0.5))
       col = rgb(0,1,df$high.score,0.5))
  
  r <- readline(prompt = "Please type Enter to continue...")
  if (r == "q") {
    stop("You have quit!")
  }
}

#Stat test to verify the features between upper and lower are different
plot(density(df.lower$Total_Time), ylim=c(0,0.2))
lines(density(df.upper$Total_Time), col="red")

plot(density(df.lower$Percent_Question), ylim=c(0,0.06))
lines(density(df.upper$Percent_Question), col="red")

plot(density(df.lower$Total_Switch_Count), ylim=c(0,0.015))
lines(density(df.upper$Total_Switch_Count), col="red")

plot(density(df.lower$Amount_Interval_Only_Passage), ylim=c(0,0.1))
lines(density(df.upper$Amount_Interval_Only_Passage), col="red")

plot(density(df.lower$First_4mins_Percent_Passage), ylim=c(0,3))
lines(density(df.upper$First_4mins_Percent_Passage), col="red")


plot(density(df.lower$First_4mins_Percent_Question), ylim=c(0,3))
lines(density(df.upper$First_4mins_Percent_Question), col="red")

plot(density(df.lower$Last_4mins_Percent_Passage), ylim=c(0,4))
lines(density(df.upper$Last_4mins_Percent_Passage), col="red")

plot(density(df.lower$Reading_Passage_Speed), ylim=c(0,0.1))
lines(density(df.upper$Reading_Passage_Speed), col="red")

t.test(df$Total_Time ~ df$high.score, data = df)  # Significant
t.test(df$Percent_Passage~ df$high.score, data = df) # Not sig
t.test(df$Percent_Question~ df$high.score, data = df) # Not sig
t.test(df$Time_Passage_Beginning ~ df$high.score, data = df) # Significant
t.test(df$Total_Switch_Count ~ df$high.score, data = df) # Significant
t.test(df$Amount_Interval_Only_Passage ~ df$high.score, data = df) #Not sig
t.test(df$Amount_Interval_Only_Question ~ df$high.score, data = df) #Not sig 
t.test(df$First_4mins_Percent_Passage ~ df$high.score, data = df) #Significant
t.test(df$First_4mins_Percent_Question ~ df$high.score, data = df) #Significant
t.test(df$Last_4mins_Percent_Passage ~ df$high.score, data = df) #Significant
t.test(df$Last_4mins_Percent_Question ~ df$high.score, data = df) #Significant
t.test(df$Reading_Passage_Speed ~ df$high.score, data = df) #Significant

