
#shiny::runApp(system.file("ASYRpKa",package="PipeFish"),launch.browser = TRUE)

miniUI::runGadget(system.file("ASYRpKa",package="PipeFish"), viewer = dialogViewer('thing',width=800,height=600))
