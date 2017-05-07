# Set the working directory as the script file
script.dir <- dirname(sys.frame(1)$ofile)
setwd(script.dir)

# Read csv file
df = read.csv("../EyeGazeData/20170429_160122_Andrew_Howe_GazeData.csv", header = FALSE)
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

pie(slices, labels = labels, main = "Time Distribution")