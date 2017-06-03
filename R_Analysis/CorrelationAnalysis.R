library (readxl)

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
  
  result <- c(Total_Time, 
              Percent_Passage, 
              Percent_Question, 
              Total_Switch_Count, 
              Time_Passage_Beginning)
  
  names(result) <- c("Total_Time", 
                     "Percent_Passage", 
                     "Percent_Question", 
                     "Total_Switch_Count", 
                     "Time_Passage_Beginning")
  return(result)
}

# Set the working directory as the script file
script.dir <- dirname(sys.frame(1)$ofile)
setwd(script.dir)

# Read the excel file
df = read_excel("DATA COLLECTION STATS.xlsx", sheet = 1)

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

for (i in 1:nrow(df)){
  result <- extract_feature(df$path[i])
  df$Total_Time[i] = result["Total_Time"]
  df$Percent_Passage[i] = result["Percent_Passage"]
  df$Percent_Question[i] = result["Percent_Question"]
  df$Total_Switch_Count[i] = result["Total_Switch_Count"]
  df$Time_Passage_Beginning[i] = result["Time_Passage_Beginning"]
}

cor(df$Correct_Answer, df$Total_Time)
plot(df$Total_Time, df$Correct_Answer)
