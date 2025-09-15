#SI web app for the paper: "Cryptic female choice can maintain reproductive isolation"
# 1. Getting everything ready ---------------------------------------------

# * 1.a Loading up libraries ----------------------------------------------
library(shiny)
library(ggh4x)
library(shinythemes)
library(ggplot2)
library(patchwork)
library(ggnewscale)
library(profvis)
library(dplyr)
library(tidyr)
library(viridis)
library(pracma)
# * 1.b Loading up and processing data ------------------------------------
#default theme
mytheme <-
  theme_bw() + theme(
    legend.position = "bottom",
    #this puts legend on the bottom
    axis.title = (element_text(face = "bold")),
    #Makes the axis line black and  thicker
    text = element_text(size = 15, face = "bold"),
    axis.text = element_text(color = "black"),
    panel.grid = element_blank(),
    strip.background = element_rect(fill = "lightgrey")
  )#makes all the text larger and bold
theme_set(mytheme)

# * 1.c global functions --------------------------------------------------
#analytical model
prob_mate<-function(sperm,alpha,beta){
  return((1-(1/(1+exp(-alpha*(sperm-beta))))))
}
solution<-function(q,alpha,beta){
  (q/2 + lambertWp(q*exp(alpha*beta - q/2)/2))/alpha
  
}

#function that makes plot
plot_function_an<-function(alpha,beta){
  #make data frame for vertical lines
  #make plot
  p1 <-
    ggplot(data = data.frame(sperm = 0),
           mapping = aes(sperm = sperm)) + theme_classic() + xlab("Ejaculate investment") +
    ylab("Probability of mating success") + xlim(0, beta*2) + theme(text = element_text(size =25)) + labs(color = "Functions") + scale_y_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1.0),limits = c(0, 1)) + stat_function(
      fun = prob_mate,
      args = (list(
        alpha = alpha,
        beta = beta
      )),
      size = 1.5
    )
  
  return(p1)
}
# * 1.c.1 Strength of selection -------------------------------------------
#function that determines probability of fertilization

prob_success<-function(mphen2,mphen1,sperm2,sperm1,a,d){
  probs_m1 <- exp((-(mphen2 - d)^2) / (2*a)) * sperm2 
  probs_m2 <- exp((-(mphen1 - d)^2) / (2*a)) * sperm1
  return(probs_m1 / (probs_m1 + probs_m2))
}

#function that makes plot
plot_function<-function(mphen,sperm2,sperm1,opt){
  #make data frame for vertical lines
  vline <-
    data.frame(
      Lines = c("Cryptic Preference (f)", "Competitor Sperm Trait (t2)"),
      Xint = c(opt, mphen)
    )
  #make plot
  p <-
    ggplot(data = data.frame(mphen2 = 0),
           mapping = aes(mphen2 = mphen2)) + theme_classic() + xlab("Focal Sperm Trait Value (t1)") +
    ylab("Probability of Fertilization") + xlim(30, 99) + theme(text = element_text(size =25)) + labs(color = "Functions") + scale_y_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1.0),limits = c(0, 1))
  p + stat_function(
    fun = prob_success,
    args = (list(
      mphen1 = mphen,
      sperm2 = sperm2,
      sperm1 = sperm1,
      a = 1,
      d = opt
    )),
    mapping = aes(color = "Strong"),
    size = 1.5
  ) + stat_function(
    fun = prob_success,
    args = (list(
      mphen1 = mphen,
      sperm2 = sperm2,
      sperm1 = sperm1,
      a = 12.5,
      d = opt
    )),
    mapping = aes(color = "Moderate"),
    size = 1.5
  ) + stat_function(
    fun = prob_success,
    args = (list(
      mphen1 = mphen,
      sperm2 = sperm2,
      sperm1 = sperm1,
      a = 50,
      d = opt
    )),
    mapping = aes(color = "Weak"),
    size = 1.5
  ) + scale_color_manual(
    name = "Strength of CSP:",
    breaks = c("Weak", "Moderate", "Strong"),
    values = c("#E69f00", "#000000", "#009E73")
  ) + new_scale_color() + geom_vline(
    data = vline,
    aes(
      xintercept = Xint,
      linetype = Lines,
      color = Lines
    ),
    alpha = 0.5,
    size = 1.5
  ) + scale_color_manual(values = c("blue", "red"), name = "") + scale_linetype_manual(values =c("dashed", "dotted"), name = "")
}


prob_surv<-function(ephen,a,d){
  probs <- exp((-(ephen - d)^2) / (2*a))
  #probP<-exp((-(c(rnorm(1000,d,2),ephen) - d)^2) / (2*a))
  return(probs)
}

#function that makes plot
plot_functionS<-function(a,d){
  #make plot
  p <-
    ggplot(data = data.frame(ephen = 0),
           mapping = aes(ephen = ephen)) + theme_classic() + xlab("Ecological trait (e)") +
    ylab("Probability of survival") + xlim(d-10, d+10) + theme(text = element_text(size =25)) + labs(color = "Functions")
  p + stat_function(
    fun = prob_surv,
    args = (list(
      a = a,
      d = d
    )),
    size = 1.5
  )+ geom_vline(
    aes(
      xintercept = d,
    ),
    alpha = 0.5,
    size = 1.5,
    linetype="dashed"
  )
}
# 2. User interface -------------------------------------------------------
ui <- fluidPage(theme = shinytheme("superhero"),
                HTML('<meta name="viewport" content="width=1024">'),
                navbarPage(
                  "",
                  
                  # * 2.a Homepage ---------------------------------------------------------
                  tabPanel(
                    "Homepage",
                    fluidRow(
                      titlePanel(h1(
                        "Supporting information for:", align = "center"
                      ))),fluidRow(h1("'Cryptic female choice can maintain reproductive isolation'", align = "center")),
                    fluidRow(
                      column(12,
                             h2("Abstract:"),
                             br(),
                             p(
                               "Sexual selection has long been considered an important mechanism of speciation. Despite growing empirical evidence that postmating sexual selection—selection on traits that affect fertilization—is common, most speciation theory has focused on premating sexual selection. Cryptic female choice can result in assortative fertilization, e.g., conspecific sperm precedence—a process where females bias fertilization towards conspecific males during sperm competition. Although there is empirical evidence of conspecific sperm precedence in a wide range of taxa, there is little theory on conspecific sperm precedence via cryptic female choice, limiting our understanding of how it contributes to speciation. We use simulation models of secondary contact to ask under what circumstances conspecific sperm precedence can evolve and maintain reproductive isolation. We found that cryptic female choice alone can maintain reproductive isolation under limited but realistic conditions, specifically when the migration rate is low, cryptic preferences are strong, and multiple mating is intermediate. In combination with ecological divergence, cryptic female choice was able to maintain reproductive isolation even at high rates of migration. We also found that conspecific sperm precedence could evolve through reinforcement. Our results demonstrate that cryptic female choice could maintain reproductive isolation and, therefore, can contribute to species divergence and maintenance."
                             ))),
                    fluidRow(
                      column(12, br(),
                             h2("Description:"),
                             br(),
                             p(
                               "This is a supplemental web application for the paper: 'Cryptic female choice can maintain reproductive isolation.' At each tab, one can make supplemental figures using data from this paper. One can get to different pages of the web application by using the tabs above. Description of the figures are given on each page, and short descriptions of each tab are given below. You can save graphs by right clicking on the graph."
                             ))),
                    fluidRow(
                      column(4,
                             br(),
                             h4("Viability Selection (Eq.1)"),
                             br(),
                             p(
                               "Here one can see how the ecological optimum and strength of viability selection influence survival."
                             )),
                      column(4,
                             br(),
                             h4("Premating Postmating Tradeoff (Eqs. 2, S3)"),
                             br(),
                             p(
                               "Here one can see how the alpha and beta parameter influence the tradeoff between postmating and premating sucess."
                             )),
                      column(4,
                             br(),
                             h4("Conspecific Sperm Precedence (Eq.3)"),
                             br(),
                             p(
                               "Here one can see how the strength of selection (v), cryptic female preference (f), male sperm trait (t), and sperm number (s) influence the probability of fertilization in the model."
                             )))),
                  # * 2.b Viability s -------------------------------------------------
                  tabPanel(
                    "Viability Selection (Eq.1)",
                    sidebarPanel(h3("Figure legend:"),"here you can explore how the ecological optimum and w (strength of viability selection) influence the probability of survival (Equation 1).",
                                 fluidRow(
                                   numericInput(
                                     "ds",
                                     h3("Ecological optimum:"),
                                     min = 1,
                                     max = 10000,
                                     value = 40
                                   ),
                                   numericInput(
                                     "ws",
                                     h3("Strength of selection (w; larger values result in weaker selection):"),
                                     min = 1,
                                     max = 1000000,
                                     value = 1
                                   )
                                 )
                    ),
                    mainPanel(plotOutput("fun_plot_surv",height=600))
                  ),                  
                  
                  # * 2.b Analytical model -------------------------------------------------
                  tabPanel(
                    "Premating Postmating Tradeoff(Eqs. 2 and S3)",
                    sidebarPanel(h3("Figure legend:"),"here you can explore how the alpha and beta parameter influence the tradeoff between premating and postmating sexual selection (Equations 2 and S3). Alpha is the inverse (e.g., selected 20 is plotting when alpha = 1/20).. In all simlations without a tradeoff between sperm number and sperm trait alpha =1/20 and beta = 50;for simulations with a tradeoff alpha = 1/1000 beta = 2500 to keep the scaling of overall investment the same.",
                                 fluidRow(
                                   numericInput(
                                     "alpha",
                                     h3("Alpha shape parameter:"),
                                     min = 1,
                                     max = 10000,
                                     value = 20
                                   ),
                                   numericInput(
                                     "beta",
                                     h3("Beta shape parameter:"),
                                     min = 1,
                                     max = 1000000,
                                     value = 50
                                   )
                                 )
                    ),
                    mainPanel(plotOutput("fun_plot_an",height=600))
                  ),
                  
                  
                  
                  # * 2.c Strength of Selection ---------------------------------------------
                  tabPanel(
                    "Conspecific Sperm Precedence (Eq.3)",
                    sidebarPanel(h3("Figure legend:"),"the probability of fertilization for the focal male is highest when his trait value (t) matches the cryptic preference value (f; red dashed line; Eq. 3). This probability increases the farther the competitor sperm trait value (t; blue dashed line) deviates from the optimal trait value relative to the focal male’s trait value regardless of direction. The fertilization advantage of being closer to the optimal trait value increases as strength of selection increases. One can use the sliders to change the focal male sperm number (s), the competitor male sperm number (s), the competitor sperm trait value (t), and the focal female trait value (f). Values can take any positive number. This graphs the case of only two male competitors. Strong cryptic preference is v =1, moderate is v = 12.5, and weak is v = 50.",
                                 fluidRow(
                                   sliderInput(
                                     "fm",
                                     h3("Competitor sperm trait value (t2):"),
                                     min = 30,
                                     max = 99,
                                     value = 40,
                                     step = 0.01
                                   ),
                                   sliderInput(
                                     "om",
                                     h3("Focal cryptic preference value (f; optimal sperm trait value):"),
                                     min = 30,
                                     max = 99,
                                     value = 40,
                                     step = 0.01
                                   ),
                                   sliderInput(
                                     "fs",
                                     h3("Focal male sperm number(s1):"),
                                     min = 1,
                                     max = 700,
                                     value = 50,
                                     step = 10
                                   ),
                                   sliderInput(
                                     "cs",
                                     h3("Competitor male sperm number (s2):"),
                                     min = 1,
                                     max = 700,
                                     value = 50,
                                     step = 10
                                   )
                                 )
                    ),
                    mainPanel(plotOutput("fun_plot",height=600))
                  )
                )
)

# 3. Server side ----------------------------------------------------------
server <- function(input, output,session) {
  # Observes ----------------------------------------------------------------
  
  # * 3.a Analtyical model -------------------------------------------------
  #fun_plot_an
  output$fun_plot_an<-renderPlot({
    plot_function_an(1/input$alpha,input$beta)
  })
  
  # * 3.a Analtyical model -------------------------------------------------
  #fun_plot_an
  output$fun_plot_surv<-renderPlot({
    plot_functionS(input$ws,input$ds)
  })
  
  # * 3.b Selection function ----------------------------------------------
  output$fun_plot<-renderPlot({
    plot_function(input$fm,input$fs,input$cs,input$om)
  })
  
}

# Run app ----
shinyApp(ui, server)
