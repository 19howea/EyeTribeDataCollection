# Set the working directory as the script file
script.dir <- dirname(sys.frame(1)$ofile)
setwd(script.dir)

# Prepare the path for files
base.folder<- paste(script.dir,"/../EyeGazeData/",sep="")
file.list <- list.files(base.folder)
path <- paste(base.folder, file.list, sep ="")


for (j in 1:length(path)) {
  # Read all CSV file
  df = read.csv(path[j], header = FALSE)
  colnames(df) = c("timeStamp", "xCordinate", "yCordinate")
  
  remove_list = c()
  for (i in 1:nrow(df)){
    if ((df$xCordinate[i] == 0 && df$yCordinate[i] == 0) ||
        df$xCordinate[i] < 0 || df$yCordinate[i] < 0) {
      remove_list = c(remove_list, i)
    }
  }
  
  df.cleaned = df[- remove_list, ]
  df.cleaned$timeStampUnix = as.numeric(as.POSIXct(df.cleaned$timeStamp))
  time.begin = df.cleaned$timeStampUnix[1]
  time.end = df.cleaned$timeStampUnix[nrow(df.cleaned)]
  
  #Calculate time distrubution for each 30 seconds
  t1 = time.begin
  passage.all.count = c()
  question.all.count = c()
  time.mark <- c()
  
  while (t1 < time.end) {
    t2 = t1 +30
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
    time.mark = c(time.mark, 30 * length(passage.all.count))
    t1 = t2
  }
  
  percent.passage = (passage.all.count / (passage.all.count + question.all.count)) * 100 
  percent.question = (question.all.count / (passage.all.count + question.all.count)) * 100
  
  matrix =(rbind(passage.all.count, question.all.count))
  percent.table <- prop.table(matrix, 2)
  colnames(percent.table) = time.mark
  barplot(percent.table,main = paste("Reading Pattern Over Time\n",file.list[j]))
  
  r <- readline(prompt = "Please type Enter to continue...")
  if (r == "q") {
    stop("You have quit!")
  }
}