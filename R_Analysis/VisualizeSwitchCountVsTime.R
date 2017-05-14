# Set the working directory as the script file
script.dir <- dirname(sys.frame(1)$ofile)
setwd(script.dir)

# Prepare the path for files
base.folder<- paste(script.dir,"/../EyeGazeData/",sep="")
file.list <- list.files(base.folder)
path <- paste(base.folder, file.list, sep ="")

#Read each file in a loop
for (j in 1:length(path)) {
  # Read the CSV file
  df = read.csv(path[j], header = FALSE, stringsAsFactors = FALSE)
  colnames(df) = c("timeStamp", "xCordinate", "yCordinate")
  
  # Clean the data when you blink your eyes or look outside 
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
  print(paste("Total time to complete one passage test", (time.end - time.begin)/60, "minutes"))
  
  t1 = time.begin
  all.count = c()
  time.mark <- c()
  while (t1 < time.end) {
    t2 = t1 +30
    df.temp = subset(df.cleaned, df.cleaned$timeStampUnix >= t1 & df.cleaned$timeStampUnix < t2 )
    if (nrow(df.temp) > 0) {
      # Count how many time you switch
      count = 0
      for (i in 1:(nrow(df.temp)-1)) {
        if (df.temp$xCordinate[i] <= 960 && df.temp$xCordinate[i+1] >960 ) count = count + 1
        if (df.temp$xCordinate[i] > 960 && df.temp$xCordinate[i+1] <= 960 ) count = count + 1
      }
      all.count = c(all.count, count)
    } else (all.count = c(all.count,0))
    time.mark = c(time.mark, 30 * length(all.count))
    t1 = t2
  }
  
  plot(time.mark,all.count, type = "l", xlab = "Time (s)", ylab = "Switch Count per 30s",
       main = paste("Switch Count Vs Time\n",file.list[j]))
  
  readline(prompt = "Please type Enter to continue...")
}