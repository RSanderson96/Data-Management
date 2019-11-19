V7Console = Video7$console_device_percentage
V7Desktop = Video7$desktop_device_percentage
V7Mobile= Video7$mobile_device_percentage
V7TV = Video7$tv_device_percentage
V7Tablet= Video7$tablet_device_percentage
V7Unknown = Video7$unknown_device_percentage
StepPosition = Video7$step_position

DFMethod = data.frame(Step = StepPosition, Console = V7Console, Desktop = V7Desktop, Mobile = V7Mobile, Tablet = V7Tablet, TV = V7TV, Unknown = V7Unknown)


Line=ggplot (data = DFMethod, aes (x = Step, y= Console)) #initial line coordinates

L3 = Line + geom_line(aes(x = Step, y= Console, colour = "Console")) #adding each run
L4 = L3+ geom_line (aes (x=Step, y=Mobile, colour = "Mobile"))
L5 = L4+ geom_line (aes (x=Step, y=Tablet, colour = "Tablet")) 
L6 = L5+ geom_line (aes (x=Step, y=TV, colour = "TV"))
L7= L6+ geom_line (aes (x=Step, y=Unknown, colour = "Unknown"))
L8 = L7 +geom_line(aes(x=Step, y=Desktop, colour = "Desktop"))

L8

Line=ggplot (data = DFMethod, aes (x = Step, y= Console)) #initial line coordinates

L3 = Line + geom_point(aes(x = Step, y= Console, colour = "Console")) #adding each run
L4 = L3+ geom_point (aes (x=Step, y=Mobile, colour = "Mobile"))
L5 = L4+ geom_point (aes (x=Step, y=Tablet, colour = "Tablet")) 
L6 = L5+ geom_point (aes (x=Step, y=TV, colour = "TV"))
L7= L6+ geom_point (aes (x=Step, y=Unknown, colour = "Unknown"))
L8 = L7 +geom_point(aes(x=Step, y=Desktop, colour = "Desktop"))


L8 #test