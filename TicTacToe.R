library(shiny)
library("shinyWidgets")

ui <- fluidPage(
  tags$head(tags$style(type="text/css", ".shiny-file-input-progress { display: none }")),
  sidebarLayout(
    sidebarPanel(
      tabsetPanel(id = "tabset",
                  tabPanel("Single player",
                           radioButtons("dificulty", "Distribution type:",c("Easy" = "0","Hard" = "1")),
                           selectInput("xoro", "Your character:",
                                       c("X" = "1",
                                         "O" = "2")),
                           
                           textInput("playername", "Your name"),
                           actionButton("gosingle", "Play"),
                            actionButton("save","Save Players")
                  ),
                  tabPanel("Multi player",
                           textInput("player1name", "Player X"),
                           textInput("player2name", "Player O"),
                           actionButton("gomultiple", "Play"),
                           actionButton("save2","Save Players")
                  )
                  )
      
    ),
  mainPanel(
    actionGroupButtons(
      inputIds = c("btn1", "btn2", "btn3"),
      labels = list("-", "-", "-"),
      fullwidth = TRUE,
      size = 'lg'
      #status = "primary"
    ),
    actionGroupButtons(
      inputIds = c("btn4", "btn5", "btn6"),
      labels = list("-", "-", "-"),
      fullwidth = TRUE,
      size = 'lg'
      #status = "primary"
    ),
    actionGroupButtons(
      inputIds = c("btn7", "btn8", "btn9"),
      labels = list("-", "-", "-"),
      fullwidth = TRUE,
      size = 'lg'
      #status = "primary"
    ),
    br(),
    plotOutput("plot")
  )
  )
  
)
#THE GAME ENGINE
triples <- list(c(1,2,3),c(4,5,6),c(7,8,9),c(1,4,7),c(2,5,8),c(3,6,9),c(1,5,9),c(3,5,7))
winner<-TRUE
state<-as.character(1:9)
x <- data.frame("Name"="Dimitrie", "Wins"=1, "Defeats"=0)
playerX<-""
playerO<-""
human_human<-FALSE
XorO<-1
display<-function(state){
  cat(" ",state[1],"|",state[2],"|",state[3],"\n")
  cat(" ---+---+---","\n")
  cat(" ",state[4],"|",state[5],"|",state[6],"\n")
  cat(" ---+---+---","\n")
  cat(" ",state[7],"|",state[8],"|",state[9],"\n")
}


update<-function(state, who, pos){
  if(who==1){state[pos]<-"x"}
  else if(who==2){state[pos]<-"o"}
  return(state)
}


check_winner<-function(state){
  for(i in 1:length(triples)){
    if(sum(triples[[i]] %in% which(state=="x"))>=3)
    {cat("x wins, type play() to play again","\n");
      y<-x
      if(nrow(y[y$Name == playerX,])>0)
      {
        y[y$Name == playerX,"Wins"]<-y[y$Name == playerX,"Wins"]+1
      }
      else
      {
        y<-rbind(y,data.frame("Name"=playerX,"Wins"=1,"Defeats"=0))
      }
      if(nrow(y[y$Name == playerO,])>0)
      {
        y[y$Name == playerO,"Defeats"]<-y[y$Name == playerO,"Defeats"]+1
      }
      else
      {
          y<-rbind(y,data.frame("Name"=playerO,"Wins"=0,"Defeats"=1))
      }
      x<<-y
      
      winner=TRUE;break}
  }
  for(i in 1:length(triples)){
    if(sum(triples[[i]] %in% which(state=="o"))>=3)
    {cat("o wins, type play() to play again","\n");
      y<-x
        if(nrow(y[y$Name == playerX,])>0)
        {
          y[y$Name == playerX,"Defeats"]<-y[y$Name == playerX,"Defeats"]+1
        }
        else
        {
            y<-rbind(y,data.frame("Name"=playerX,"Wins"=0,"Defeats"=1))
        }
        if(nrow(y[y$Name == playerO,])>0)
        {
          y[y$Name == playerO,"Wins"]<-y[y$Name == playerO,"Wins"]+1
        }
        else
        {
            y<-rbind(y,data.frame("Name"=playerO,"Wins"=1,"Defeats"=0))
        }
      x<<-y
      winner=TRUE;break
      }
  }
  if(sum(state==as.character(1:9))==0 && winner==FALSE){cat("game draw,type play() to play again.");winner=TRUE}
  return(winner)
}


check_illegal<-function(state,location){
  
    pos<-location
    if(pos!=1&&pos!=2&&pos!=3&&pos!=4&&pos!=5&&pos!=6&&pos!=7&&pos!=8&&pos!=9){
      return(FALSE)
    }else{
      if(state[pos] != "x" && state[pos] != "o"){
        state<-update(state,pos)
      }else if(state[pos] == "x" || state[pos] == "o"){
        return(FALSE)
      }
    }
  return(TRUE)
}



computer_turn<-function(state,inteligent_computer){
  if(inteligent_computer==1)
  {
    if(sum(state=="x") > sum(state=="o")){#human goes first playing x and computer second playing o
      before<-state
      for(i in 1:length(triples)){
        if(sum(triples[[i]] %in% which(state=="o"))==2 && 
           any(state[triples[[i]][which((triples[[i]] %in% which(state=="o"))==FALSE)]] %in% as.character(1:9)) && any(state[triples[[i]][which((triples[[i]] %in% which(state=="x"))==FALSE)]] %in% as.character(1:9))){#winning
          state[triples[[i]][which((triples[[i]] %in% which(state=="o"))==FALSE)]]<-"o"
        }
        if(identical(state,before)==FALSE)break
      }
      if(identical(state,before)==TRUE){
        for(i in 1:length(triples)){
          if(sum(triples[[i]] %in% which(state=="x"))==2 && 
             any(state[triples[[i]][which((triples[[i]] %in% which(state=="x"))==FALSE)]] %in% as.character(1:9))&& any(state[triples[[i]][which((triples[[i]] %in% which(state=="o"))==FALSE)]] %in% as.character(1:9))){#blocking
            state[triples[[i]][which((triples[[i]] %in% which(state=="x"))==FALSE)]]<-"o"
          }
          if(identical(state,before)==FALSE)break
        }
      }
      if(identical(state,before)==TRUE){state[sample(which(state == as.character(1:9)),1)]<-"o"}
    }else if(sum(state=="o") >= sum(state=="x") || sum(state=="o")==0){#computer goes first playing x and human second playing o
      before<-state
      for(i in 1:length(triples)){
        if(sum(triples[[i]] %in% which(state=="x"))==2 &&
           any(state[triples[[i]][which((triples[[i]] %in% which(state=="x"))==FALSE)]] %in% as.character(1:9))&& any(state[triples[[i]][which((triples[[i]] %in% which(state=="o"))==FALSE)]] %in% as.character(1:9))){#winning
          state[triples[[i]][which((triples[[i]] %in% which(state=="x"))==FALSE)]]<-"x"
        }
        if(identical(state,before)==FALSE)break
      }
      
      
      if(identical(state,before)==TRUE){
        for(i in 1:length(triples)){
          if(sum(triples[[i]] %in% which(state=="o"))==2 && 
             any(state[triples[[i]][which((triples[[i]] %in% which(state=="o"))==FALSE)]] %in% as.character(1:9))&& any(state[triples[[i]][which((triples[[i]] %in% which(state=="x"))==FALSE)]] %in% as.character(1:9))){#blocking
            state[triples[[i]][which((triples[[i]] %in% which(state=="o"))==FALSE)]]<-"x"
          }
          if(identical(state,before)==FALSE)break
        }
      }
      if(identical(state,before)==TRUE){state[sample(which(state == as.character(1:9)),1)]<-"x"}
    }
  }
  else
  {
    if(sum(state=="x") > sum(state=="o"))
    {
      x3 <- sample(1:9, 1)
      while(!check_illegal(state,x3))x3=sample(1:9,1)
      state[x3]<-'o'
    }
    else
    {
      x3 <- sample(1:9, 1)
      while(!check_illegal(state,x3))x3=sample(1:9,1)
      state[x3]<-'x'
    }
  }
  return(state)
} 

button_step<-function(who,inteligent_computer,pos)
{
  if(winner == FALSE){
    if(who==1){
      if(check_illegal(state,pos))
      {
        state<<-update(state, who, pos) 
        display(state) 
        winner<<-check_winner(state)
        if(winner) return(state)
        state<<-computer_turn(state,inteligent_computer)
        display(state)
        winner<<-check_winner(state)
      }
    }
    else if(who==2){
      if(check_illegal(state,pos))
      {
        state<<-update(state, who, pos)
        display(state)
        winner<<-check_winner(state)
        if(winner) return(state)
        state<<-computer_turn(state,inteligent_computer)
        display(state) 
        winner<<-check_winner(state)
        if(winner) return(state)
      }
      
    }
  }
}
Who<-1
button_step_human_vs_human<-function(pos)
{
  if(winner == FALSE){
    if(check_illegal(state,pos))
    {
      if(Who==1){# x's turn
        state<<-update(state, Who, pos) 
        display(state) 
        winner<<-check_winner(state) 
        Who<<-2 # will this 2 lead to "else if" loop?
      }
      else if(Who==2){# o's turn
        state<<-update(state, Who, pos) 
        display(state)
        winner<<-check_winner(state) 
        Who<<-1
      }
    }
  }
}

play <- function(session,hh,xoro,inteligent_computer){
  #computer_or_not<-readline("how many players?(type 1 to play against computer):")
  human_human<<-hh
  winner<<-FALSE
  Who<<-1
  state<<-as.character(1:9)
  if(!human_human && xoro==2)
  {
    state<<-computer_turn(state,inteligent_computer)
    display(state)
  }
  updateButtons(session)
}
updateButtons<-function(session)
{
  for(i in 1:9)
  {
    if(state[i]>=1&&state[i]<=9)
      updateActionButton(session, paste("btn",toString(i),sep=""),
                         label = '-')
    else
      updateActionButton(session, paste("btn",toString(i),sep=""),
                         label = toString(state[i]))
  }
}
#THE GAME ENGINE END
server <- function(input, output,session) {
  
  observeEvent(input$gosingle, {
    play(session,FALSE,input$xoro,input$dificulty)
    if(input$xoro==1)
    {
      playerX<<-input$playername
      playerO<<-"computer"
    }
    else
    {
      
      playerO<<-input$playername
      playerX<<-"computer"
    }
      counts <- t(x[,c("Wins","Defeats")])
      output$plot <- renderPlot({barplot(counts, main="Players statistics", col=c("darkblue","red"),
                                         legend = c("Win","Defeat"), beside=TRUE,names.arg=x$Name)})
      
    })
  observeEvent(input$gomultiple, {
    play(session,TRUE,1,0)
      playerX<<-input$player1name
      playerO<<-input$player2name
      counts <- t(x[,c("Wins","Defeats")])
      output$plot <- renderPlot({barplot(counts, main="Players statistics", col=c("darkblue","red"),
                                         legend = c("Win","Defeat"), beside=TRUE,names.arg=x$Name)})
  })
  observeEvent(input$save,{
    myFile <- "Players.txt"
    write.table(x,myFile, append = FALSE, sep = " ", dec = ".",
                row.names = TRUE, col.names = TRUE)
   
  })
  observeEvent(input$save2,{
    myFile <- "Players.txt"
    write.table(x,myFile, append = FALSE, sep = " ", dec = ".",
                row.names = TRUE, col.names = TRUE)
    
  })
  observeEvent(input$btn1,{
    if(human_human)
    {
      button_step_human_vs_human(1)
    }
    else
    button_step(input$xoro,input$dificulty,1)
    updateButtons(session)
  })
  observeEvent(input$btn2,{
    if(human_human)
    {
      button_step_human_vs_human(2)
    }
    else
    button_step(input$xoro,input$dificulty,2)
    updateButtons(session)
  })
  observeEvent(input$btn3,{
    if(human_human)
    {
     button_step_human_vs_human(3)
    }
    else
    button_step(input$xoro,input$dificulty,3)
    updateButtons(session)
    
  })
  observeEvent(input$btn4,{
    if(human_human)
    {
      button_step_human_vs_human(4)
    }
    else
    button_step(input$xoro,input$dificulty,4)
    updateButtons(session)
    
  })
  observeEvent(input$btn5,{
    if(human_human)
    {
      button_step_human_vs_human(5)
    }
    else
    button_step(input$xoro,input$dificulty,5)
    updateButtons(session)
  })
  observeEvent(input$btn6,{
    if(human_human)
    {
      button_step_human_vs_human(6)
    }
    else
    button_step(input$xoro,input$dificulty,6)
    updateButtons(session)
    
  })
  observeEvent(input$btn7,{
    if(human_human)
    {
      button_step_human_vs_human(7)
    }
    else
    button_step(input$xoro,input$dificulty,7)
    updateButtons(session)
    
  })
  observeEvent(input$btn8,{
    if(human_human)
    {
      button_step_human_vs_human(8)
    }
    else
    button_step(input$xoro,input$dificulty,8)
    updateButtons(session)
    
  })
  observeEvent(input$btn9,{
    if(human_human)
    {
      button_step_human_vs_human(9)
    }
    else
    button_step(input$xoro,input$dificulty,9)
    updateButtons(session)
    
  })
}

shinyApp(ui, server)
