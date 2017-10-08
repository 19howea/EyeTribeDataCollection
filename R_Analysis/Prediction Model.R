library(rpart)

# Set the working directory as the script file
script.dir <- dirname(sys.frame(1)$ofile)
setwd(script.dir)

#Read the features data in 
df = read.csv("Extracted Features.csv", header = TRUE)

#Shorten column 9 (Name of the passage)
names(df)[9] = "Note"

#Build multiple linear regression 
model=lm(df$Correct_Answer ~ df$Total_Time + df$Total_Switch_Count + df$Time_Passage_Beginning + 
           df$Reading_Passage_Speed + df$Last_4mins_Percent_Question)
summary(model)

prediction = predict(model, newdata = df)
truevalue = df$Correct_Answer 

results <- data.frame(prediction, truevalue)
results = results[order(truevalue),]

plot(results$truevalue, pch = 2, col = "blue")
points(results$prediction, pch = 1, col = "red")

#Build descision tree predictive model
tree=rpart(df$high.score ~ df$Total_Time + df$Total_Switch_Count + df$Time_Passage_Beginning + 
             df$Reading_Passage_Speed + df$Last_4mins_Percent_Question, method = "class")

prediction = predict(tree, newdata = df, type = "class")
truevalue = df$high.score
correct = truevalue == prediction
sum(correct) / length(truevalue)

results=data.frame(prediction,truevalue,correct)

#Visualize the descision tree
plot(tree, uniform = TRUE,
     main = "Classification of High Scores", margin = 0.2)
text(tree, use.n = TRUE, all = TRUE, cex = .8)

