# With the data frame you created last week you will:
unique.char <- c('a','b','c','d','e','f','g','h','i','j','k','l','m','n','o')
rep.char <- c('z','z','z','z','z','y','y','y','y','y','w','w','w','w','w')
unique.num <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
rep.num <- c(1,2,2,3,3,3,4,4,4,4,5,5,5,5,5)
dec.num <- c(1,2,3,4.20,5,6.9,7,8.008,9,10,11.11,12,13,14,15)

df <- as.data.frame(cbind(unique.char, rep.char,unique.num,rep.num,dec.num))
df$unique.num <- as.numeric(as.character(df$unique.num))
df$rep.num <- as.numeric(as.character(df$rep.num))
df$dec.num <- as.numeric(as.character(df$dec.num))
add.row <- data.frame("p","z",610,4,4.610)
colnames(add.row) <- colnames(df)
df1 <- rbind(df, add.row)
row.names(df1) <- df1$unique.char
df1 <- df1[,-1]
df1
# Create a barplot for one numeric column, grouped by the character vector with 3 unique values
  # Add error bars with mean and standard deviation to the plot
  # Change the x and y labels and add a title
  # Export the plot as a PDF that is 4 inches wide and 7 inches tall.
df.mean <-aggregate(df1$rep.num ~df1$rep.char, FUN = "mean")
df.mean
colnames(df.mean) <- c("Factor","Mean")
df.mean
barplot(df.mean$Mean)
barplot(df.mean$Mean, names.arg = df.mean$Factor)
df.sd <-aggregate(df1$rep.num ~df1$rep.char, FUN = "sd")
colnames(df.sd) <- c("Factor","StanDev")
df.sd
b.plot <- barplot(df.mean$Mean, names.arg = df.mean$Factor)
b.plot <- barplot(df.mean$Mean, names.arg = df.mean$Factor, ylim = c(0,5))
arrows(b.plot, df.mean$Mean-df.sd$StanDev,
       b.plot, df.mean$Mean+df.sd$StanDev,angle=90,code=3)
b.plot <- barplot(df.mean$Mean,names.arg = df.mean$Factor, xlab = "Frequency", ylab = "Cases of Letters", main = "The End of the Alphabet")
arrows(b.plot, df.mean$Mean-df.sd$StanDev,
       b.plot, df.mean$Mean+df.sd$StanDev,angle=90,code=3)

# Create a scatter plot between two of your numeric columns.
  # Change the point shape and color to something NOT used in the example.
  # Change the x and y labels and add a title
  # Export the plot as a JPEG by using the "Export" button in the plotting pane.
plot(df1$dec.num ~ df1$unique.num)
plot(df1$dec.num ~ df1$rep.num, xlab = "Explanatory", ylab = "Response", main = "Plot by Numbers", 
     cex.axis=0.3, cex.main=0.4, cex.lab=2.34, pch=24, col = "thistle")
colors()
# Upload both plots with the script used to create them to GitHub.
  # Follow the same file naming format as last week for the script.
  # Name plots as Lastname_barplot or Lastname_scatterplot. Save them to your "plots" folder.
