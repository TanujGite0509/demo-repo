library(shiny)
library(ggplot2)
library(dplyr)
library(readr)

ui <- fluidPage(
  
  titlePanel("📞 Call Center Modeling using Poisson & M/M/1"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      fileInput("file", "Upload CSV Dataset", accept = ".csv"),
      
      uiOutput("service_col"),
      uiOutput("arrival_col"),
      
      numericInput("hours", "Working Hours", value = 8, min = 1),
      
      actionButton("analyze", "Run Analysis", class = "btn-primary")
    ),
    
    mainPanel(
      
      tabsetPanel(
        
        tabPanel("Data Summary",
                 tableOutput("summary_table")
        ),
        
        tabPanel("Poisson Distribution",
                 plotOutput("poisson_plot"),
                 plotOutput("calls_plot")
        ),
        
        tabPanel("Inter-Arrival Time",
                 plotOutput("exp_plot")
        ),
        
        tabPanel("Queue Performance",
                 tableOutput("queue_results")
        ),
        tabPanel("Significance Test",
                 tableOutput("significance_results")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  data <- reactive({
    req(input$file)
    read_csv(input$file$datapath)
  })
  
  output$service_col <- renderUI({
    req(data())
    selectInput("service", "Select Service Column (Service(total) per Hour)",
                choices = names(data()))
  })
  
  output$arrival_col <- renderUI({
    req(data())
    selectInput("arrival", "Select Arrival Column (Calls Answered per Hour)",
                choices = names(data()))
  })
  
  observeEvent(input$analyze, {
    
    df <- data()
    
    lambda <- mean(df[[input$arrival]])
    mu <- mean(df[[input$service]])
    
    # Traffic Intensity
    rho <- lambda/mu
    
    # M/M/1 Metrics
    L <- lambda/(mu-lambda)
    Lq <- lambda^2/(mu*(mu-lambda))
    W <- 1/(mu-lambda)
    Wq <- lambda/(mu*(mu-lambda))
    
    output$summary_table <- renderTable({
      summary(df)
    })
    #Poisson Plot
    output$poisson_plot <- renderPlot({
      x <- 0:round(lambda*2)
      prob <- dpois(x, lambda)
      plot(x, prob, type="h", col="purple", lwd=2,
           main="Poisson Distribution",
           xlab="No. of Calls",
           ylab="Probability")
    })
    #Simulation Plot
    output$calls_plot <- renderPlot({
      calls<- rpois(input$hours, lambda)
      hist(calls,xlab="Hours",ylab="No. of Calls(freq)",main="Call simulation per hour",col="orange")
    })
    #Exponential Plot
    output$exp_plot <- renderPlot({
      inter_arr <- rexp(100, rate=lambda)
      hist(inter_arr*60, probability=TRUE,
           xlab="Time Between Calls(in mins)",ylab="no. of calls",main="Inter-Arrival Time distribution",col="grey")
    })
    #Queue Results
    output$queue_results <- renderTable({
      
      data.frame(
        Metric = c("Arrival Rate (λ)",
                   "Service Rate (μ)",
                   "Traffic Intensity (ρ)",
                   "Avg Calls in System (L)",
                   "Avg Calls in Queue (Lq)",
                   "Avg Waiting Time (hours)",
                   "Avg Waiting Time in Queue (hours)"),
        Value = round(c(lambda, mu, rho, L, Lq, W, Wq), 3)
      )
    })
    
    # -------------------------
    # STATISTICAL VALIDATION
    # -------------------------
    
    arrival_data <- df[[input$arrival]]
    sample_mean <- mean(arrival_data)
    sample_variance <- var(arrival_data)
    
    freq_table <- table(arrival_data)
    observed <- as.numeric(freq_table)
    values <- as.numeric(names(freq_table))
    
    expected <- dpois(values, lambda) * length(arrival_data)
    
    valid <- expected >= 0
    observed <- observed[valid]
    expected <- expected[valid]
    
    chi_square_stat <- sum((observed - expected)^2 / expected)
    df_chi <- length(observed) - 1
    p_value <- pchisq(chi_square_stat, df=df_chi, lower.tail=FALSE)
    
    poisson_fit <- ifelse(p_value > 0.05,
                          "Fail To reject H0:-Data Follows Poisson Distribution.",
                          "Reject H0:-Data doesn't follow Poisson Distribution.")
    
    stability_check <- ifelse(rho < 1,
                              "System Stable (CALL Service Consistent)",
                              "System Overloaded")
    
    output$significance_results <- renderTable({
      data.frame(
        Test = c("Sample Mean",
                 "Sample Variance",
                 "Variance ≈ Mean?",
                 "Chi-Square Statistic",
                 "p-value",
                 "Poisson Fit",
                 "System Stability"),
        Result = c(round(sample_mean,2),
                   round(sample_variance,3),
                   ifelse(abs(sample_mean - sample_variance) < (0.2 * sample_mean), 
                          "Yes (Statistically Close)", 
                          "No (Significant Dispersion)"),
                   round(chi_square_stat,4),
                   round(p_value,4),
                   poisson_fit,
                   stability_check)
      )
    })
    
  })
}

shinyApp(ui = ui, server = server)
    
