library(ProjectTemplate)
load.project()


#to do: Starters-finishers graph.
DifferenceDF = data.frame(Step = CourseS3$Step, 
                          Difference3 = (CourseS3$Total-CourseF3$Total),
                          Difference4 = (CourseS4$Total-CourseF4$Total),
                          Difference5 = (CourseS5$Total-CourseF5$Total),
                          Difference6 = (CourseS6$Total-CourseF6$Total),
                          Difference7 = (CourseS7$Total-CourseF7$Total))

D= ggplot (data=DifferenceDF, aes (x=Step, y=Difference))
D3 = D + geom_point(aes(x=Step, y= Difference3, colour = "Course Run3"))
D4 = D3 + geom_point(aes(x=Step, y= Difference4, colour = "Course Run4"))
D5 = D4 + geom_point(aes(x=Step, y= Difference5, colour = "Course Run5"))
D6 = D5 + geom_point(aes(x=Step, y= Difference6, colour = "Course Run6"))
D7 = D6 + geom_point(aes(x=Step, y= Difference7, colour = "Course Run7"))



FinalDiff = D7
FinalDiff

#DIFFERENCE AS PERCENTAGE

DifferencePercentDF = data.frame(Step = CourseS3$Step, 
                          DifferenceP3 = (((CourseS3$Total-CourseF3$Total)/CourseS3$Total)*100),
                          DifferenceP4 = (((CourseS4$Total-CourseF4$Total)/CourseS4$Total)*100),
                          DifferenceP5 = (((CourseS5$Total-CourseF5$Total)/CourseS5$Total)*100),
                          DifferenceP6 = (((CourseS6$Total-CourseF6$Total)/CourseS6$Total)*100),
                          DifferenceP7 = (((CourseS7$Total-CourseF7$Total)/CourseS7$Total)*100))

D= ggplot (data=DifferencePercentDF, aes (x=Step, y=Difference))
DP3 = D + geom_point(aes(x=Step, y= DifferenceP3, colour = "Course Run3"))
DP4 = DP3 + geom_point(aes(x=Step, y= DifferenceP4, colour = "Course Run4"))
DP5 = DP4 + geom_point(aes(x=Step, y= DifferenceP5, colour = "Course Run5"))
DP6 = DP5 + geom_point(aes(x=Step, y= DifferenceP6, colour = "Course Run6"))
DP7 = DP6 + geom_point(aes(x=Step, y= DifferenceP7, colour = "Course Run7"))

DiffPercent = DP7

DiffPercent

DP8 = DP7 + geom_vline(xintercept, color = "red")

