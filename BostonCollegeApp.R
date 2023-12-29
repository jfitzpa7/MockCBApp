library(baseballr)
library(tidyverse)
library(shiny)
library(plyr)
library(dplyr)
library(devtools)
library(DT)
library(ggplot2)
library(ggrepel)
library(ggthemes)
library(gridExtra)
library(janitor)
library(plotly)
library(stringr)
library(tidyr)
library(tidyselect)
library(tidyverse)
library(data.table)
library(reactable)
library(lubridate)
library(ggpubr)




DukeBC=fread("DukeBC.csv")

BC_Percentiles=fread("BC_Percentiles.csv")

DukeBC$Date <- mdy(DukeBC$Date)

# Converts Character Date into Proper Format

DukeBC$Date <- mdy(DukeBC$Date)



# Import data / CSV

BC_Percentiles <-DukeBC[,.(
  'Max Velo' = max(RelSpeed, na.rm = TRUE),
  'Max Spin' = max(SpinRate, na.rm = TRUE)),
  by = .(Pitcher, TaggedPitchType)]


view(BC_Percentiles)

BC_Percentiles$`Max Velo` <- round(BC_Percentiles$`Max Velo`, digits = 1)
BC_Percentiles$`Max Spin` <- round(BC_Percentiles$`Max Spin`, digits = 0)


#Creating Ranking and Percentile for Max Velo with a Fastball

BC_Percentiles$MaxVelo_percentile <- BC_Percentiles$`Max Velo`
BC_Percentiles$MaxVelo_percentile <- round(BC_Percentiles$MaxVelo_percentile, digits=2)
BC_Percentiles$MaxVelo_percentile[BC_Percentiles$TaggedPitchType != "Fastball"] <-NA

BC_Percentiles$MaxVelo_ranking[order(BC_Percentiles$MaxVelo_percentile, 
                                     decreasing = TRUE)] <- 1:nrow(BC_Percentiles)

BC_Percentiles$MaxVelo_ranking[BC_Percentiles$TaggedPitchType != "Fastball"] <- NA

BC_Percentiles$MaxVelo_percentile <-  1 - ((BC_Percentiles$MaxVelo_ranking) / max(BC_Percentiles$MaxVelo_ranking, na.rm = TRUE))

BC_Percentiles$MaxVelo_percentile <- round(BC_Percentiles$MaxVelo_percentile, digits = 2)

BC_Percentiles$MaxVelo_percentile <- BC_Percentiles$MaxVelo_percentile*100

view(BC_Percentiles)

# Creating Ranking and Percentile for Max Spin with a Fastball

BC_Percentiles$MaxSpin_percentile <- BC_Percentiles$`Max Spin`

BC_Percentiles$MaxSpin_percentile <- round(BC_Percentiles$MaxSpin_percentile, digits = 2)

BC_Percentiles$MaxSpin_percentile[BC_Percentiles$TaggedPitchType != "Fastball"] <- NA


BC_Percentiles$MaxSpin_ranking[order(BC_Percentiles$MaxSpin_percentile,
                                     decreasing = TRUE)] <- 1:nrow(BC_Percentiles)

BC_Percentiles$MaxSpin_ranking[BC_Percentiles$TaggedPitchType != "Fastball"] <- NA

BC_Percentiles$MaxSpin_percentile <-  1 - ((BC_Percentiles$MaxSpin_ranking) / max(BC_Percentiles$MaxSpin_ranking, na.rm = TRUE))

BC_Percentiles$MaxSpin_percentile <- round(BC_Percentiles$MaxSpin_percentile, digits = 2)

BC_Percentiles$MaxSpin_percentile <- BC_Percentiles$MaxSpin_percentile*100


##### remove rows with NA (non-fastballs)
BC_Percentiles <- BC_Percentiles[!is.na(BC_Percentiles$MaxVelo_percentile),]


View(BC_Percentiles)

#####
### Creating High And Low Color Scale

All_Data_Low = subset (BC_Percentiles, select = c(1))

All_Data_Low <- All_Data_Low[!duplicated(All_Data_Low)]

colnames(All_Data_Low)

###low
All_Data_Low$TaggedPitchType <- "Low"
All_Data_Low$'Max Velo' <- 0
All_Data_Low$'Max Spin'<- 0
All_Data_Low$MaxVelo_percentile <- -5
All_Data_Low$MaxVelo_ranking <- 20
All_Data_Low$MaxSpin_percentile <- -5
All_Data_Low$MaxSpin_ranking <- 20


# High
All_Data_High = subset (BC_Percentiles, select = c(1))

All_Data_High <- All_Data_High[!duplicated(All_Data_High)]

colnames(All_Data_High)

All_Data_High$TaggedPitchType <- "High"
All_Data_High$'Max Velo' <- 100
All_Data_High$'Max Spin'<- 100
All_Data_High$MaxVelo_percentile <- 105
All_Data_High$MaxVelo_ranking <- 1
All_Data_High$MaxSpin_percentile <- 105
All_Data_High$MaxSpin_ranking <- 1

##RBind to combine High and Low

LowHigh <- rbind(All_Data_Low, All_Data_High)


## Rbind to combine lowhigh with percentiles 
BC_Percentiles <- rbind(BC_Percentiles, All_Data_High)

view(LowHigh)

##########

MaxVelo <- BC_Percentiles%>%
  filter(Pitcher == "Finley, Leighton",
         TaggedPitchType %in% c("Fastball", "High", "Low")) %>%
  ggplot(BC_Percentiles, mapping = aes(x= MaxVelo_percentile, y= TaggedPitchType, colour = (MaxVelo_percentile))) +
  geom_line() + geom_point(size = 9)  +
  ggtitle("Max Velo") + xlim(0, 100) + ylim("Fastball") +
  xlab("") + ylab("") + theme(
    plot.title = element_text(color = "black", size = 15, face = "italic"),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x= element_blank(),
    axis.ticks.y  =element_blank(),
    axis.ticks.x  =element_blank(),
    axis.text.y = element_text(size=12, face="italic", colour = "black"))+
  geom_segment(aes(x = 0, xend = 100, y = TaggedPitchType, yend = TaggedPitchType), color = "#9b9b9b", size = 1) +
  geom_point(aes(x = 0, y = TaggedPitchType), color = "#9b9b9b", size = 5) +
  geom_point(aes(x = 50, y = TaggedPitchType), color = "#9b9b9b", size = 5) +
  geom_point(aes(x = 100, y = TaggedPitchType), color = "#9b9b9b", size = 5) +
  geom_point(aes(x = MaxVelo_percentile, y = TaggedPitchType, fill = MaxVelo_percentile), pch = 21, color = "black", size = 10) +
  geom_text(aes(label=MaxVelo_percentile),hjust=.5, vjust=.4, color = "White",
            size = 5)+theme(legend.position = "none")+
  scale_fill_gradient2(midpoint = 50, low = "#cc0000", mid = "#ffffff", high = "#2952a3",
                       na.value = "grey50") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank())

view(BC_Percentiles)
fwrite(BC_Percentiles, "BC_Percentiles.csv")

BC_Percentiles=fread("BC_Percentiles.csv")
# Converts Character Date into Proper Format
### Link to a list of various Shiny Inputs we can use

# https://shiny.rstudio.com/tutorial/written-tutorial/lesson3/
# Start of the UI - Part 1 of App Structure
ui <- navbarPage(
  "Duke University vs Boston College 4/15/23", theme = "flatly",
  
  tabPanel("Pitchers",
           
           sidebarLayout(
             
             sidebarPanel(
               # Center the image in the sidebarPanel
               tags$div(
                 style = "text-align: center;",
                 tags$img(src = "https://upload.wikimedia.org/wikipedia/commons/7/7b/Boston_College_Eagles_wordmark.png", alt = "Boston College Eagles", width = "300px")
               ),
               
               selectInput("Team", label = "Choose Team",
                           choices = levels(as.factor(DukeBC$PitcherTeam))),
               selectInput("Pitcher", label = "Choose Pitcher",
                           choices = levels(as.factor(DukeBC$Pitcher))),
               dateRangeInput("Date", label = "Choose Date Range",
                              start = min(DukeBC$Date),
                              end = max(DukeBC$Date),
                              min = min(DukeBC$Date),
                              max = max(DukeBC$Date),
                              format = "yyyy-mm-dd",
                              separator = "to"),
               checkboxGroupInput("Pitch", label = "Choose Pitch Type(s)", 
                                  choices = levels(as.factor(DukeBC$TaggedPitchType)))
             ),
             
             mainPanel(
               fluidRow(plotOutput("Percentiles")),
               br(),
               br(),
               br(),
               br(),
               br(),
               fluidRow(DTOutput("Percentiles_Data")),
               br(),
               br(),
               br(),
               br(),
               br(),
               fluidRow(plotOutput("Velo_Chart")),
               br(),
               br(),
               br(),
               br(),
               br(),
               fluidRow(plotOutput("Strike_Zone"))
             )
           )
  )
)


# Start of the Server - Part 2 of App Structure

server = function(input, output, session) {
  
  # Input Reactions -- Pitcher Tab
  
  
  # Pitchers Based on Team    
  
  observeEvent(
    input$Team,
    updateSelectInput(session,
                      "Pitcher", "Choose Pitcher",
                      choices = levels(factor(filter(DukeBC,
                                                     PitcherTeam == isolate(input$Team))$Pitcher))))
  
  
  # Date Range Based on When Pitcher Threw
  
  observeEvent(
    input$Pitcher,
    updateDateRangeInput(session,
                         "Date", "Choose Date Range",
                         start = min(DukeBC$Date),
                         end = max(DukeBC$Date)))
  
  
  # Pitch Types Based on Pitcher
  
  observeEvent(
    input$Pitcher,
    updateCheckboxGroupInput(session,
                             "Pitch", "Choose Pitch Type(s)",
                             choices = levels(factor(filter(DukeBC,
                                                            Pitcher == isolate(input$Pitcher))$TaggedPitchType))))
  
  
  
  
  
  
  
  # Start of Outputs (Plots and Data Tables)
  
  
  
  # Percentiles
  
  output$Percentiles <- renderPlot({
    
    # FB Max Velo
    
    MaxVelo <- BC_Percentiles%>%
      filter(Pitcher == input$Pitcher,
             TaggedPitchType %in% c("Fastball", "High", "Low")) %>%
      ggplot(BC_Percentiles, mapping = aes(x= MaxVelo_percentile, y= TaggedPitchType, colour = (MaxVelo_percentile))) +
      geom_line() + geom_point(size = 9)  +
      ggtitle("Max Velo") + xlim(0, 100) + ylim("Fastball") +
      xlab("") + ylab("") + theme(
        plot.title = element_text(color = "black", size = 15, face = "italic"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x= element_blank(),
        axis.ticks.y  =element_blank(),
        axis.ticks.x  =element_blank(),
        axis.text.y = element_text(size=12, face="italic", colour = "black"))+
      geom_segment(aes(x = 0, xend = 100, y = TaggedPitchType, yend = TaggedPitchType), color = "#9b9b9b", size = 1) +
      geom_point(aes(x = 0, y = TaggedPitchType), color = "#9b9b9b", size = 5) +
      geom_point(aes(x = 50, y = TaggedPitchType), color = "#9b9b9b", size = 5) +
      geom_point(aes(x = 100, y = TaggedPitchType), color = "#9b9b9b", size = 5) +
      geom_point(aes(x = MaxVelo_percentile, y = TaggedPitchType, fill = MaxVelo_percentile), pch = 21, color = "black", size = 10) +
      geom_text(aes(label=MaxVelo_percentile),hjust=.5, vjust=.4, color = "White",
                size = 5)+theme(legend.position = "none")+
      scale_fill_gradient2(midpoint = 50, high = "#cc0000", mid = "#ffffff", low = "#2952a3",
                           na.value = "grey50") +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_blank())
    
    
    
    # FB Max Spin
    
    MaxSpin <- BC_Percentiles%>%
      filter(Pitcher == input$Pitcher,
             TaggedPitchType %in% c("Fastball", "High", "Low")) %>%
      ggplot(BC_Percentiles, mapping = aes(x= MaxSpin_percentile, y= TaggedPitchType, colour = (MaxSpin_percentile))) +
      geom_line() + geom_point(size = 9)  +
      ggtitle("Max Spin") + xlim(0, 100) + ylim("Fastball") +
      xlab("") + ylab("") + theme(
        plot.title = element_text(color = "black", size = 15, face = "italic"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x= element_blank(),
        axis.ticks.y  =element_blank(),
        axis.ticks.x  =element_blank(),
        axis.text.y = element_text(size=12, face="italic", colour = "black"))+
      geom_segment(aes(x = 0, xend = 100, y = TaggedPitchType, yend = TaggedPitchType), color = "#9b9b9b", size = 1) +
      geom_point(aes(x = 0, y = TaggedPitchType), color = "#9b9b9b", size = 5) +
      geom_point(aes(x = 50, y = TaggedPitchType), color = "#9b9b9b", size = 5) +
      geom_point(aes(x = 100, y = TaggedPitchType), color = "#9b9b9b", size = 5) +
      geom_point(aes(x = MaxSpin_percentile, y = TaggedPitchType, fill = MaxSpin_percentile), pch = 21, color = "black", size = 10) +
      geom_text(aes(label=MaxSpin_percentile),hjust=.5, vjust=.4, color = "White",
                size = 5)+theme(legend.position = "none")+
      scale_fill_gradient2(midpoint = 50, high = "#cc0000", mid = "#ffffff", low = "#2952a3") +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_blank())
    
    ggarrange(MaxVelo, MaxSpin ,nrow = 1, ncol = 2)
    
    
  })
  
  
  
  
  # Percentiles_Data
  
  output$Percentiles_Data <- renderDT({
    
    TMP <- BC_Percentiles%>%
      filter(TaggedPitchType == "Fastball",
             Pitcher == input$Pitcher)
    
    datatable(TMP, caption = htmltools::tags$caption( style = 'caption-side: top; 
                                                  text-align: center; color:black; font-size:200% ;',
                                                      'Fastball Percentiles'), options = list(dom = 't', columnDefs = list(list(targets = 0, visible = FALSE)))) %>%
      formatStyle(c(1), `border-left` = "solid 1px") %>% formatStyle(c(7), `border-right` = "solid 1px")
    
    
  })
  
  
  # Velo_Chart
  
  output$Velo_Chart <- renderPlot({
    
    DukeBC%>%
      filter(PitcherTeam == input$Team,
             Pitcher == input$Pitcher,
             TaggedPitchType %in% input$Pitch) %>%
      ggplot(DukeBC, mapping = aes(x=PitchNo, y= RelSpeed, colour = TaggedPitchType)) +
      geom_line() + geom_point() +
      scale_color_manual(values = c(ChangeUp = "blue", Fastball = "black",
                                    Slider = "orange", Curveball = "red",
                                    Cutter = "green",Sinker = "grey",
                                    Splitter = "purple")) +
      ggtitle("Velocity / Pitch") +
      xlab("Pitch") + ylab("Velocity") + theme(
        plot.title = element_text(color = "black", size = 15, face = "bold", hjust = 0.5),
        axis.title.x = element_text(color="black", size = 13, face = "bold"),
        axis.title.y = element_text(color="black" , size = 13, face = "bold"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank())+
      geom_hline(yintercept = seq(from=70, to=100, by = 5))
    
  })
  
  
  
  # Strike_Zone
  
  output$Strike_Zone <- renderPlot({
    
    DukeBC%>%
      filter(PitcherTeam == input$Team,
             Pitcher == input$Pitcher,
             TaggedPitchType %in% input$Pitch) %>%
      ggplot(DukeBC, mapping = aes(x=PlateLocSide, y= PlateLocHeight)) +
      geom_point(aes(color = TaggedPitchType),size = 3) +
      scale_color_manual(values = c(ChangeUp = "blue", Fastball = "black",
                                    Slider = "orange", Curveball = "red",
                                    Cutter = "green",Sinker = "grey",
                                    Splitter = "purple"))+
      geom_segment(x = (-11.5/12)+.25, y = (44.08/12), xend = (5.5/12)+.25, yend = (44.08/12)) +
      geom_segment(x = (-11.5/12)+.25, y = (18.29/12), xend = (5.5/12)+.25, yend = (18.29/12)) +
      geom_segment(x = (-11.5/12)+.25, y = (44.08/12), xend = (-11.5/12)+.25, yend = (18.29/12)) +
      geom_segment(x = (5.5/12)+.25, y = (44.08/12), xend = (5.5/12)+.25, yend = (18.29/12)) +
      
      
      geom_segment(x = (-11.5/12)+.25, y = (35.48/12), xend = (-5.835/12)+.25, yend = (35.48/12), size = .3) +
      geom_segment(x = (-5.835/12)+.25, y = (35.48/12), xend = (-0.165/12)+.25, yend = (35.48/12), size = .3) +
      geom_segment(x = (-0.165/12)+.25, y = (35.48/12), xend = (5.5/12)+.25, yend = (35.48/12), size = .3) +
      geom_segment(x = (-11.5/12)+.25, y = (26.88/12), xend = (-5.835/12)+.25, yend = (26.88/12), size = .3) +
      geom_segment(x = (-5.835/12)+.25, y = (26.88/12), xend = (-0.165/12)+.25, yend = (26.88/12), size = .3) +
      geom_segment(x = (-0.165/12)+.25, y = (26.88/12), xend = (5.5/12)+.25, yend = (26.88/12), size = .3) +
      geom_segment(x = (-5.835/12)+.25, y = (44.08/12), xend = (-5.835/12)+.25, yend = (35.48/12), size = .3) +
      geom_segment(x = (-5.835/12)+.25, y = (35.48/12), xend = (-5.835/12)+.25, yend = (26.88/12), size = .3) +
      geom_segment(x = (-5.835/12)+.25, y = (26.88/12), xend = (-5.835/12)+.25, yend = (18.29/12), size = .3) +
      geom_segment(x = (-0.165/12)+.25, y = (44.08/12), xend = (-0.165/12)+.25, yend = (35.48/12), size = .3) +
      geom_segment(x = (-0.165/12)+.25, y = (35.48/12), xend = (-0.165/12)+.25, yend = (26.88/12), size = .3) +
      geom_segment(x = (-0.165/12)+.25, y = (26.88/12), xend = (-0.165/12)+.25, yend = (18.29/12), size = .3) +
      
      
      geom_segment(x = (-.708 -(3/12))+.25, y = (.708/2), xend = (0-(3/12))+.25, yend = (1.417/2)) +
      geom_segment(x = (0-(3/12))+.25, y = (1.417/2), xend = (.708-(3/12))+.25, yend = (.708/2)) +
      geom_segment(x = (.708-(3/12))+.25, y = (.708/2), xend = (.708-(3/12))+.25, yend = (0)) +
      geom_segment(x = (-.708-(3/12))+.25, y = (0), xend = (-.708-(3/12))+.25, yend = (.708/2)) +
      geom_segment(x = (-.708-(3/12))+.25, y = (0), xend = (.708-(3/12))+.25, yend = (0)) +
      theme(plot.title = element_text(hjust = 0.5),
            panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            legend.position = "none") + 
      xlim(-2.5,2.5) + ylim(-.5, 5) + ggtitle("Strike Zone")
    
    
  })
  
}


# ShinyApp - Part 3 of App Structure

shinyApp(ui = ui, server = server)


