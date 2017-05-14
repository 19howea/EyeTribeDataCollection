# Set the working directory as the script file
script.dir <- dirname(sys.frame(1)$ofile)
setwd(script.dir)

# Prepare the path for files
base.folder<- paste(script.dir,"/../EyeGazeData/Data/",sep="")
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
  
  df.passage <- subset(df.cleaned, xCordinate <= 1920/2)
  df.question <- subset(df.cleaned, xCordinate > 1920/2)
  
  slices <- c(nrow(df.passage), nrow(df.question))
  labels <- c("Passage", "Question")
  pct <- round(slices/sum(slices)*100)
  labels <- paste(labels, " ", pct, "%", sep = "")
  
  pie(slices, labels = labels, 
      main = paste("Time spent distribution\n",file.list[j]))
  
  readline(prompt = "Please type Enter to continue...")
}

