# Example preprocessing script.
Video3 = cyber.security.3.video.stats
Video4 = cyber.security.4.video.stats
Video5 = cyber.security.5.video.stats
Video6 = cyber.security.6.video.stats
Video7 = cyber.security.7.video.stats
cache("Video3")
cache("Video4")
cache("Video5")
cache("Video6")
cache("Video7")

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


Enrolements1 = cyber.security.1.enrolments
Enrolements2 = cyber.security.2.enrolments
Enrolements3 = cyber.security.3.enrolments
Enrolements4 = cyber.security.4.enrolments
Enrolements5 = cyber.security.5.enrolments
Enrolements6 = cyber.security.6.enrolments
Enrolements7 = cyber.security.7.enrolments
People1 = nrow(Enrolements1)
People2 = nrow(Enrolements2)
People3 = nrow(Enrolements3)
People4 = nrow(Enrolements4)
People5 = nrow(Enrolements5)
People6 = nrow(Enrolements6)
People7 = nrow(Enrolements7)
Entries = c(People1, People2, People3,People4, People5, People6, People7)
Cohort = 1:7

Cohort_Summaries = data.frame(Cohort = Cohort, Entries = Entries)




