library(ProjectTemplate)
load.project()


QuantitiesStarting.function = function(x){ #x = file selected to analyse
  StepActivity = x
  Steps=c((StepActivity$week_number)+((StepActivity$step_number)/100)) #acknowledging data complication
  StepActivity = cbind(StepActivity, Steps) #editing the data with the new column
  S= unique(StepActivity$Steps, incomparables = FALSE) #list the unique steps
  L =length(S) #How many steps are there?
  
#making the vector of quantities for each step
  
  total_values = vector() #making the vector
  for(i in 1:L){ #for loop: L = how many steps will be assessed/length of vector
    Step= StepActivity %>% filter(Steps==S[i]) #filter: collect all the rows for a single step
    Quant = nrow(Step) #how many rows were collected?
    total_values[i]= Quant} #compile a vector of how many people started each step

#making the dataframe to be able to chart: Step against how many participants.
DFActivity = data.frame(Step = S, Total = total_values)
  
   return(DFActivity)} #return: the final vector


#The final data frames
CourseS1 =QuantitiesStarting.function(cyber.security.1.step.activity) #run the function for each file
CourseS2 =QuantitiesStarting.function(cyber.security.2.step.activity)
CourseS3 =QuantitiesStarting.function(cyber.security.3.step.activity)
CourseS4 =QuantitiesStarting.function(cyber.security.4.step.activity)
CourseS5 =QuantitiesStarting.function(cyber.security.5.step.activity)
CourseS6 =QuantitiesStarting.function(cyber.security.6.step.activity)
CourseS7 =QuantitiesStarting.function(cyber.security.7.step.activity)

StartersDF = merge.data.frame(CourseS1, CourseS2, by = "Step", all=TRUE)
StartersDF = StartersDF%>% rename(Run_S1 = Total.x, Run_S2 = Total.y )
StartersDF = merge.data.frame(StartersDF, CourseS3, by = "Step", all=TRUE)
StartersDF = StartersDF%>% rename(Run_S3 = Total)
StartersDF = merge.data.frame(StartersDF, CourseS4, by = "Step", all=TRUE)
StartersDF = StartersDF%>% rename(Run_S4 = Total)
StartersDF = merge.data.frame(StartersDF, CourseS5, by = "Step", all=TRUE)
StartersDF = StartersDF%>% rename(Run_S5 = Total)
StartersDF = merge.data.frame(StartersDF, CourseS6, by = "Step", all=TRUE)
StartersDF = StartersDF%>% rename(Run_S6 = Total)
StartersDF = merge.data.frame(StartersDF, CourseS7, by = "Step", all=TRUE)
StartersDF = StartersDF%>% rename(Run_S7 = Total)

cache("StartersDF")


#making the graph

#Decision: dismiss 1&2 as won't fit the graph - need to clean the data

Plot = ggplot (data = StartersDF,aes (x = Step, y = Total))
CourseS1Graph = Plot +geom_point(aes( x= Step, y = Run_S1, colour = "Course Run 1"))
CourseS2Graph = CourseS1Graph +geom_point(aes( x= Step, y = Run_S2, colour = "Course Run 2"))
CourseS3Graph = CourseS2Graph +geom_point(aes( x= Step, y = Run_S3, colour = "Course Run 3"))
CourseS4Graph = CourseS3Graph +geom_point(aes( x= Step, y = Run_S4, colour = "Course Run 4"))
CourseS5Graph = CourseS4Graph +geom_point(aes( x= Step, y = Run_S5, colour = "Course Run 5"))
CourseS6Graph = CourseS5Graph +geom_point(aes( x= Step, y = Run_S6, colour = "Course Run 6"))
CourseS7Graph = CourseS6Graph +geom_point(aes( x= Step, y = Run_S7, colour = "Course Run 7"))

CourseS7Graph

#Proportion graph

StartProp = ggplot (data = StartersDF,aes (x = Step, y = Total))
CourseSP1Graph = StartProp +geom_point(aes( x= Step, y = Run_S1/(Cohort_Summaries$Enrolments[1]), colour = "Course Run 1"))
CourseSP2Graph = CourseSP1Graph +geom_point(aes( x= Step, y = Run_S2/(Cohort_Summaries$Enrolments[2]), colour = "Course Run 2"))
CourseSP3Graph = CourseSP2Graph +geom_point(aes( x= Step, y = Run_S3/(Cohort_Summaries$Enrolments[3]), colour = "Course Run 3"))
CourseSP4Graph = CourseSP3Graph +geom_point(aes( x= Step, y = Run_S4/(Cohort_Summaries$Enrolments[4]), colour = "Course Run 4"))
CourseSP5Graph = CourseSP4Graph +geom_point(aes( x= Step, y = Run_S5/(Cohort_Summaries$Enrolments[5]), colour = "Course Run 5"))
CourseSP6Graph = CourseSP5Graph +geom_point(aes( x= Step, y = Run_S6/(Cohort_Summaries$Enrolments[6]), colour = "Course Run 6"))
CourseSP7Graph = CourseSP6Graph +geom_point(aes( x= Step, y = Run_S7/(Cohort_Summaries$Enrolments[7]), colour = "Course Run 7"))

CourseSP7Graph


