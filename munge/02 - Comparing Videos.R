StepPosition = Video3$step_position #seperating columns for vectors
V3Views = Video3$total_views
V4Views = Video4$total_views
V5Views = Video5$total_views
V6Views = Video6$total_views
V7Views = Video7$total_views

Average = (V3Views +V4Views +V5Views +V6Views +V7Views)/5 #average over the runs

DFViews = data.frame (Step = StepPosition, Video3 = V3Views, Video4 = V4Views, Video5 = V5Views,Video6 = V6Views,Video7 = V7Views, 
                      Duration = Video3$video_duration, Mean = Average) #making the data frame

cache ("DFViews")

Graph=ggplot (data = DFViews, aes (x = Step, y= V3Views)) #initial graph

g3 = Graph + geom_point(aes(x = Step, y=V3Views, colour = "Course Run 3")) #adding each course run
g4 = g3+ geom_point (aes (x=Step, y=V4Views, colour = "Course Run 4"))
g5 = g4+ geom_point (aes (x=Step, y=V5Views, colour = "Course Run 5")) 
g6 = g5+ geom_point (aes (x=Step, y=V6Views, colour = "Course Run 6"))
g7= g6+ geom_point (aes (x=Step, y=V7Views, colour = "Course Run 7"))

g8 = g7 + geom_line (aes(x=Step, y=Mean, colour = "Average Views")) #adding the average line
g8 #test


Line=ggplot (data = DFViews, aes (x = Step, y= V3Views)) #initial line coordinates

L3 = Line + geom_line(aes(x = Step, y=V3Views, colour = "Course Run 3")) #adding each run
L4 = L3+ geom_line (aes (x=Step, y=V4Views, colour = "Course Run 4"))
L5 = L4+ geom_line (aes (x=Step, y=V5Views, colour = "Course Run 5")) 
L6 = L5+ geom_line (aes (x=Step, y=V6Views, colour = "Course Run 6"))
L7= L6+ geom_line (aes (x=Step, y=V7Views, colour = "Course Run 7"))

L8 = L7 + geom_line (aes(x=Step, y= Mean, colour = "Average Views")) #adding the average line
L8 #test

 #considering views as a proportion of the cohort

#new data frame needed = how many people started each video step?

VideoSteps = cyber.security.7.video.stats$step_position

VideoStarting.function = function(x){ #x = file selected to analyse
  
  StepActivity = x
  S = VideoSteps #specific steps investigated
  L = length(S) #How many steps are there?
  
  #making the vector of quantities for each step
  
  total_values = vector() #making the vector
  for(i in 1:L){ #for loop: L = how many steps will be assessed/length of vector
    Step= StepActivity %>% filter(step==S[i]) #filter: collect all the rows for a single step
    Quant = nrow(Step) #how many rows were collected?
    total_values[i]= Quant} #compile a vector of how many people started each step
  
  #making the dataframe to be able to chart: Step against how many participants.
  DFVidStart = data.frame(Step = S, Total = total_values)
  return(DFVidStart) }

VideoStart3 = VideoStarting.function (cyber.security.3.step.activity)
VideoStart4 = VideoStarting.function (cyber.security.4.step.activity)
VideoStart5 = VideoStarting.function (cyber.security.5.step.activity)
VideoStart6 = VideoStarting.function (cyber.security.6.step.activity)
VideoStart7 = VideoStarting.function (cyber.security.7.step.activity)


PropVideo3 = V3Views/VideoStart3$Total
PropVideo4 =  V4Views/VideoStart4$Total
PropVideo5 = V5Views/VideoStart5$Total
PropVideo6 = V6Views/VideoStart6$Total
PropVideo7 = V7Views/VideoStart7$Total
Average = (PropVideo3+PropVideo4+PropVideo5+PropVideo6+PropVideo7)/5

DFViewsProportion = data.frame(Step = VideoSteps,Video3 = PropVideo3, Video4 = PropVideo4,
                               Video5 = PropVideo5, Video6 = PropVideo6,
                               Video7 = PropVideo7, Average = Average)


cache("DFViewsProportion")

Line=ggplot (data = DFViewsProportion, aes (x = Step, y= Video3)) #initial line coordinates

L3 = Line + geom_point(aes(x = Step, y=Video3, colour = "Course Run 3")) #adding each run
L4 = L3+ geom_point (aes (x=Step, y=Video4, colour = "Course Run 4"))
L5 = L4+ geom_point (aes (x=Step, y=Video5, colour = "Course Run 5")) 
L6 = L5+ geom_point (aes (x=Step, y=Video6, colour = "Course Run 6"))
L7= L6 + geom_point (aes (x=Step, y=Video7, colour = "Course Run 7"))
L8 = L7 + geom_line (aes ( x=Step, y=DFViewsProportion$Average, colour = "Average"))

L8
 #adding question lines?

  StepActivity = cyber.security.7.question.response
  Questions = unique(StepActivity$quiz_question, incomparables = FALSE)
  
  Steps=c((StepActivity$week_number)+((StepActivity$step_number)/100)) #acknowledging data complication
  StepActivity = cbind(StepActivity, Steps) #editing the data with the new column
  S= unique(StepActivity$Steps, incomparables = FALSE) #list the unique steps
  
L9 = L8 + geom_vline(xintercept = (S[1]), (aes (colour = "red")))
L10 = L9 + geom_vline(xintercept = (S[2]), (aes (colour = "red")))
L11 = L10 + geom_vline(xintercept = (S[3]), (aes (color = "red")))
L12 = L11 + geom_vline(xintercept = (S[4]), (aes (color = "red")))
L13 = L12 + geom_vline(xintercept = (S[5]), (aes (color = "red")))
L13
