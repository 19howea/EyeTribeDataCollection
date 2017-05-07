# Set the working directory as the script file
script.dir <- dirname(sys.frame(1)$ofile)
setwd(script.dir)

# Prepare the path for files
base.folder<- paste(script.dir,"/../EyeGazeData/",sep="")
file.list <- list.files(base.folder)
path <- paste(base.folder, file.list, sep ="")

all.count <- c()

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

  # Count how many time you switch
  count = 0
  for (i in 1:(nrow(df.cleaned)-1)) {
    if (df.cleaned$xCordinate[i] <= 960 && df.cleaned$xCordinate[i+1] >960 ) count = count + 1
    if (df.cleaned$xCordinate[i] > 960 && df.cleaned$xCordinate[i+1] <= 960 ) count = count + 1
  }
  
  all.count<- c(all.count,count)
}

people <- c()
for (i in 1:length(path)){
  person <- strsplit(file.list[i],"_")[[1]][3]
  people <- c(people, person)
}


names(all.count) <- people
barplot(all.count, ylim = c(0,350))