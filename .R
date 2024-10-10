library(readxl)

#load excel file
Tech <- read_xlsx("TechSales Rep Assignment1.xlsx")

#dimensions
dim(Tech)

#check for missing values
which(!complete.cases(Tech))

# remove missing values
Tech.NOmissong <- na.omit(Tech)
summary(Tech.NOmissong$Salary)

# create histogram
hist(Tech.NOmissong$Salary, main = "Distribution of Salary",ylim = c(0,1200) ,xlab = "Dollar amount", col = "red")

#aggregate salary by gender
SalaryGender <- aggregate(Salary ~ Female, data = Tech.NOmissong, FUN = mean)

barplot(height = SalaryGender$Salary, xlab = "Male = 0                Female = 1",ylim = c(0,80000), col = c( "light green","sky blue"),
        names.arg = SalaryGender$Female,main = "Salary By Gender", ylab = "Dollars")

#subset data
MaleSubset <- Tech.NOmissong[Tech.NOmissong$Female==0,]
FemaleSubset <- Tech.NOmissong[Tech.NOmissong$Female==1,]

#visualizations
boxplot(MaleSubset$Salary,col = "yellow")
boxplot(FemaleSubset$Salary, col = "yellow")

#bins
Tech.NOmissong$Salary <- cut(Tech.NOmissong$Salary,breaks = c(-Inf, 25000, 75000, 125000, Inf),
                                  labels = c("<25000", "25000-75000", "75000-125000", ">125000"))

Tech.NOmissong$Age <- cut(Tech.NOmissong$Age,breaks = c(-Inf,25,30,35,Inf),
                          labels = c("<25","25-30","30-35",">35"))

Heatmapsal <- table(Tech.NOmissong$Salary, Tech.NOmissong$Age)

#need to change to matrix
Heatmapsal.matrix <- as.matrix(Heatmapsal)
heatmap(Heatmapsal.matrix,main = "Heatmap of Salary by age" ,Rowv = NA,Colv = NA)
