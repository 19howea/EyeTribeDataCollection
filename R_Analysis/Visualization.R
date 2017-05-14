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
  plot(df.cleaned$xCordinate, df.cleaned$yCordinate,
       main = paste("X and Y Cordinates of Gaze\n",file.list[j]),
       xlab = " X Cordinate", ylab = " Y Cordinate", 
       xlim = c(0,1920), ylim = c(990,0), 
       pch=19, col=rgb(0, 0, 1, 0.05),
       xaxt='n', 
       yaxt='n')
  abline(v = 1920 / 2)
  readline(prompt = "Please type Enter to continue...")
}