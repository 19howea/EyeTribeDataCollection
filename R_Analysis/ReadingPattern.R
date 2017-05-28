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
  df.cleaned$P_or_Q = ifelse(df.cleaned$xCordinate< 960, 0, 1)
  
  
  plot(df.cleaned$P_or_Q, df.cleaned$timeStampUnix, 
       #ylim = c(time.end, time.end-20),
       ylim = c(time.end, time.begin),
       type = "p",
       ylab = "Time line",
       xlab = "Passage                                                                    Question",
       xaxt='n',
       main = paste("Reading Pattern\n",file.list[j]))
  
  r <- readline(prompt = "Please type Enter to continue...")
  if (r == "q") {
    stop("You have quit!")
  }
}
  
  