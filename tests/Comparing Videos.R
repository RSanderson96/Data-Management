Video3= read.csv("cyber-security-3_video-stats.csv")


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

DFViewsProportion = data.frame (Step = StepPosition, Video3 = (V3Views/(Cohort_Summaries$Entries[3])), Video4 = V4Views/ (Cohort_Summaries$Entries[4]), Video5 = V5Views/(Cohort_Summaries$Entries[5]),Video6 = V6Views/(Cohort_Summaries$Entries[6]),Video7 = V7Views/(Cohort_Summaries$Entries[7]), 
                      Duration = Video3$video_duration)

Line=ggplot (data = DFViewsProportion, aes (x = Step, y= Video3)) #initial line coordinates

L3 = Line + geom_line(aes(x = Step, y=Video3, colour = "Course Run 3")) #adding each run
L4 = L3+ geom_line (aes (x=Step, y=Video4, colour = "Course Run 4"))
L5 = L4+ geom_line (aes (x=Step, y=Video5, colour = "Course Run 5")) 
L6 = L5+ geom_line (aes (x=Step, y=Video6, colour = "Course Run 6"))
L7= L6+ geom_line (aes (x=Step, y=Video7, colour = "Course Run 7"))

L7
 
