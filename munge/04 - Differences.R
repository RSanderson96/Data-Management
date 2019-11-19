library(ProjectTemplate)
load.project()


#to do: Starters-finishers graph.
DifferenceDF = data.frame(Step = SFDF$Step, 
                          Difference1 = (SFDF$Run_S1 -SFDF$Run_F1),
                          Difference2 = (SFDF$Run_S2 -SFDF$Run_F2),
                          Difference3 = (SFDF$Run_S3 -SFDF$Run_F3),
                          Difference4 = (SFDF$Run_S4 -SFDF$Run_F4),
                          Difference5 = (SFDF$Run_S5 -SFDF$Run_F5),
                          Difference6 = (SFDF$Run_S6 -SFDF$Run_F6),
                          Difference7 = (SFDF$Run_S7 -SFDF$Run_F7))
                          
    

D= ggplot (data=DifferenceDF, aes (x=Step, y=Difference))
D1 = D + geom_point(aes(x=Step, y= Difference1, colour = "Course Run3"))
D2 = D1 + geom_point(aes(x=Step, y= Difference2, colour = "Course Run3"))
D3 = D2 + geom_point(aes(x=Step, y= Difference3, colour = "Course Run3"))
D4 = D3 + geom_point(aes(x=Step, y= Difference4, colour = "Course Run4"))
D5 = D4 + geom_point(aes(x=Step, y= Difference5, colour = "Course Run5"))
D6 = D5 + geom_point(aes(x=Step, y= Difference6, colour = "Course Run6"))
D7 = D6 + geom_point(aes(x=Step, y= Difference7, colour = "Course Run7"))



FinalDiff = D7
FinalDiff

#DIFFERENCE AS PERCENTAGE

DifferencePercentDF = data.frame(Step = SFDF$Step,
                         DifferenceP1 = (((SFDF$Run_S1 -SFDF$Run_F1)/SFDF$Run_S1)*100),
                         DifferenceP2 = (((SFDF$Run_S2 -SFDF$Run_F2)/SFDF$Run_S2)*100),
                         DifferenceP3 = (((SFDF$Run_S3 -SFDF$Run_F3)/SFDF$Run_S3)*100),
                         DifferenceP4 = (((SFDF$Run_S4 -SFDF$Run_F4)/SFDF$Run_S4)*100),
                         DifferenceP5 = (((SFDF$Run_S5 -SFDF$Run_F5)/SFDF$Run_S5)*100),
                         DifferenceP6 = (((SFDF$Run_S6 -SFDF$Run_F6)/SFDF$Run_S6)*100),
                         DifferenceP7 = (((SFDF$Run_S7 -SFDF$Run_F7)/SFDF$Run_S7)*100))
                         
D= ggplot (data=DifferencePercentDF, aes (x=Step, y=Difference))
DP1 = D + geom_point(aes(x=Step, y= DifferenceP1, colour = "Course Run3"))
DP2 = DP1 + geom_point(aes(x=Step, y= DifferenceP2, colour = "Course Run3"))
DP3 = DP2 + geom_point(aes(x=Step, y= DifferenceP3, colour = "Course Run3"))
DP4 = DP3 + geom_point(aes(x=Step, y= DifferenceP4, colour = "Course Run4"))
DP5 = DP4 + geom_point(aes(x=Step, y= DifferenceP5, colour = "Course Run5"))
DP6 = DP5 + geom_point(aes(x=Step, y= DifferenceP6, colour = "Course Run6"))
DP7 = DP6 + geom_point(aes(x=Step, y= DifferenceP7, colour = "Course Run7"))

DiffPercent = DP7

DiffPercent

DP8 = DP7 + geom_vline(xintercept, color = "red")

