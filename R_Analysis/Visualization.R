

AndrewHowe = read.csv("/Users/Andrew/SAT Data Project /EyeTribeDataCollection/EyeGazeData/20170429_160122_Andrew_Howe_GazeData.csv", header = FALSE)
colnames(AndrewHowe) = c("timeStamp", "xCordinate", "yCordinate")

AndrewHowe
remove_list = c()
for (i in 1:nrow(AndrewHowe)){
  if ((AndrewHowe$xCordinate[i] == 0 && AndrewHowe$yCordinate[i] == 0) ||
      AndrewHowe$xCordinate[i] < 0 || AndrewHowe$yCordinate[i] < 0) {
    remove_list = c(remove_list, i)
  }
}


AndrewHoweNew = AndrewHowe[- remove_list, ]

plot(AndrewHoweNew$xCordinate, AndrewHoweNew$yCordinate,
     main = "X and Y Cordinates of Gaze",
     xlab = " X Cordinate", ylab = " Y Cordinate", 
     xlim = c(0,1920), ylim = c(990,0), 
     pch=19, col=rgb(0, 0, 1, 0.05))

