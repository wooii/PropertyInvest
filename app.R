# This R script provides the estimated rate of return on an investment property
# based on the given parameters and the expected rate of appreciation of the
# property. It produces a html file with interactive graphs using Shiny.

# Author: Chenfeng Chen

library(shiny)
library(ggplot2)


# Functions -------------------------------------------------------------------

transfer_duty_tax <- function(x, investment = T, 
                              foreign.investor = F, AFAD = 0.07) {
  # Calculate the transfer duty tax for a given property price.
  # x: the sale price of the property, AUD.
  # foreign.investor: T, the investor is a foreign purchaser. F, the investor 
  #   is a domestic purchaser. The foreign purchaser will need to pay more on
  #   the stamp duty.
  # AFAD: Additional Foreign Acquirer Duty. AFAD = 0.07 in QLD.
  # Return: the amount of transfer duty tax payable.
  
  # No home concession of the foreign investors.
  if (foreign.investor) investment <- T 
  if (investment) {    
    # The transfer duty tax for investment property.
    y <- ifelse(x > 5000 & x <= 75000, (x - 5000)*0.015, 
                ifelse(x > 75000 & x <= 540000, 1050 + (x - 75000)*0.035,
                       ifelse(x > 540000 & x <= 1e6, 17325 + (x - 540000)*0.045,
                              ifelse(x > 1e6, 38025 + (x - 1e6)*0.0575, 0))))
    # The transfer duty tax with home concession.
  } else {
    y <- ifelse(x > 0 & x <= 350000, x*0.01,
                ifelse(x > 350000 & x <= 540000, 3500 + (x - 350000)*0.035, 
                       ifelse(x > 540000 & x <= 1e6, 10150 + (x - 540000)*0.045,
                              ifelse(x > 1e6, 30850 + (x - 1e6)*0.0575, 0))))
  }
  if (foreign.investor) y <- y + x*AFAD
  return(y)
}


first_home_concession <- function(x) {
  # Calculate the first home owner concession for a given property price.
  # x: the sale price of the property, AUD.
  # Return: the amount of the first home owner concession deductable.
  if (x >= 550000 | x < 0) {
    y <- 0
  } else {
    # concession range.
    cr <- rbind(c(545000, 549999.99, -875),
                c(540000, 544999.99, -1750),
                c(535000, 539999.99, -2625),
                c(530000, 534999.99, -3500),
                c(525000, 529999.99, -4375),
                c(520000, 524999.99, -5250),
                c(515000, 519999.99, -6125),
                c(510000, 514999.99, -7000),
                c(505000, 509999.99, -7875),
                c(0, 504999.99, -8750))
    y <- cr[x >= cr[, 1] & x <= cr[, 2], 3]
  }
  return(y)
}


initial_invest <- function(price = 600000,
                           deposit.ratio = 0.2,
                           investment = T, 
                           foreign.investor = F,
                           first.home = F, 
                           new.home = T,
                           other.fees = 3000,
                           great.start.grant = -15000) {
  # Calculate the initial amount of investment needed for purchasing the
  # property based on the given conditions.
  # price: The sale price of the property, AUD.
  # deposit.ratio: The ratio of the initial deposit to the total sale price.
  # investment: T, this is for investment; F, this is not.
  # first.home: T, this is the investor's first home; F, this is not.
  # new.home: T, this is a newly built home; F, this is not.
  # other.fees: The other cost such as the registration of transfer fee, lawyer
  #   fees and financing costs, AUD. e.g. other.fees = 3000
  # great.start.grant: The Great Start Grant for a new property that is less
  #   than $750000 and is not for investment. e.g. great.start.grant = -15000
  # Return: the amount of initial investment for purchasing the property.
  
  # Conditions for applying the concessions.
  c1 <- ifelse(first.home & !investment, 1, 0)
  c2 <- ifelse(first.home & !investment & new.home & price < 750000, 1, 0)
  
  # Intial investment.
  y <- price*deposit.ratio + 
    transfer_duty_tax(x = price, investment = investment,
                      foreign.investor = foreign.investor) + 
    other.fees + 
    first_home_concession(x = price)*c1 + 
    great.start.grant*c2
  return(y)
}


mortgage <- function(loan = 500000,
                     loan.rate = 0.05, 
                     loan.term = 25,
                     payment.frequency = 12, 
                     loan.term.paid = 0,
                     interest.only = F) { 
  # Calculate the repayment amount for a moragage.
  # loan: The total amount of the loan, AUD.
  # loan.rate: The interest rate of the loan.
  # loan.term: The length of loan, years.
  # payment.frequency: The payment frequency of the loan, 12 for Monthly, 
  #   52 for weekly.
  # loan.term.paid: The number of years that the loan has been serviced.
  # interest.only: The repayment type of the loan. T: interest only, F: fixed
  #   principal and interest.
  # Return: a dataframe contains variables repayment: the amount of repayment 
  #   per payment cycle, remaining.principal: the remaining unpaid principal.
  
  n <- loan.term*payment.frequency # the number of the total payment cycles.
  r <- loan.rate/payment.frequency # the rate of the loan per payment cycle.
  n.paid <- loan.term.paid*payment.frequency # the number of the paid cycles.
  
  if (interest.only) {
    pay <- loan*r
    remaining.principal <- loan
  } else {
    if (r == 0) {
      pay <- loan/n
      remaining.principal <- loan - pay*n.paid
    } else {
      pay <- loan*r*(1 + r)^n / ((1 + r)^n - 1)
      remaining.principal <- loan*(1 + r)^n.paid - (pay/r)*((1 + r)^n.paid - 1)
    }
  }
  y <- data.frame(repayment = pay, remaining.principal)
  return(y)
}


income_tax <- function(x) {
  # Calculate the income tax for Australian residents.
  # x: annual income, e.g. x = 100000
  # Return: the amount of income tax payable.
  y <- ifelse(x > 18200 & x <= 37000, (x - 18200)*0.19,
              ifelse(x > 37000 & x <= 90000, 3572 + (x - 37000)*0.325,
                     ifelse(x > 90000 & x <= 180000, 20797 + (x - 90000)*0.37,
                            ifelse(x > 180000, 54097 + (x - 180000)*0.45, 0))))
  # The above rates do not include the Mdeicare levy of 2%.
  return(y)
}


income_tax_foreign <- function(x) {
  # Calculate the income tax for foreigner.
  # x: annual income, e.g. x = 100000
  # Return: the amount of income tax payable.
  y <- ifelse(x > 0 & x <= 90000, x*0.325,
              ifelse(x > 90000 & x <= 180000, 29250 + (x - 90000)*0.37,
                     ifelse(x > 180000, 62550 + (x - 180000)*0.45, 0)))
  # Foreign residents are not required to pay the Medicare levy.
  return(y)
}


# ui --------------------------------------------------------------------------

ui <- fluidPage(
  
  # Application title.
  titlePanel("Simulation of the annualized rate of return on an investment
             property"),
  
  # Sidebar.
  sidebarLayout(
    sidebarPanel(
      h3("Sale price and tax"),  
      numericInput("price",
                   "Sale price of the property (AUD).",
                   value = 600000,
                   min = 0,
                   step = 10000),
      checkboxInput("foreign.investor",
                    "Tick if the investor is a foreign investor."),
      checkboxInput("investment",
                    "Tick if the property is for investment."),
      checkboxInput("first.home",
                    "Tick if the property is the first home of the investor."),
      checkboxInput("new.home",
                    "Tick if the property is a newly built home."),
      br(),
      h3("Mortgage"),
      sliderInput("deposit.ratio",
                  "Percentage of the initial deposit to the total sale price of
                  the property (%).",
                  value = 20,
                  min = 5,
                  max = 100,
                  step = 1),
      sliderInput("loan.rate",
                  "Loan interest rate (%).",
                  value = 5,
                  min = 0,
                  max = 12,
                  step = 0.1),
      sliderInput("loan.term",
                  "Length of the home loan, years.",
                  value = 25,
                  min = 1,
                  max = 30,
                  step = 1),
      selectInput("payment.frequency",
                  "Repayment frequency per year, select monthly (12), 
                  fortnightly (26) or weekly (52).",
                  choices = c(12, 26, 52)),
      checkboxInput("interest.only",
                    "Tick if the loan is to pay the interest only, untick if 
                    the loan is to pay the fixed principal and interest."),
      br(),
      h3("Expected future sale price and rental income"),
      sliderInput("expected.rate.of.appreciation",
                  "Expected annual rate of appreciation of the property 
                  price (%).",
                  value = 4,
                  min = 0,
                  max = 12,
                  step = 0.1),
      sliderInput("sale.cost.to.price",
                  "Percentage of the cost of selling the property (such as the 
                  stamp duty tax and the commission fees paid to the real
                  estate agents) to the market price of the property.",
                  value = 5,
                  min = 1,
                  max = 10,
                  step = 0.1),
      sliderInput("rent.to.price",
                  "Percentage of the gross annual rental income to the market
                  price of the property.",
                  value = 4,
                  min = 0,
                  max = 15,
                  step = 0.1),
      sliderInput("hold.cost.to.price",
                  "Percentage of the total cost of holding the property (such
                  as the management fees and council rates and etc) to the  
                  market price of the property.",
                  value = 1,
                  min = 0.3,
                  max = 3,
                  step = 0.1),
      sliderInput("rent.cost.to.rent",
                  "Percentage of the cost that is paid to the real estate agent
                  for leasing the property to the gross annual rental income.",
                  value = 8.5,
                  min = 0,
                  max = 12,
                  step = 0.1)
    ),
    
    
    mainPanel(
      plotOutput("Plots"),
      strong("Figure 1. The simulated annualized rate of return on an investment
             property."),
      br(),
      br(),
      p("The simulation of the annualized rate of return on an investment 
        property (Figure 1) may help an investor to make better decisions in 
        the property market. There are at least 15 manipulable variables as
        listed here that may affect the outcome from such an investment."),
      p("Manipulating the variables in the input panel will generate a new
        simulaiton accordingly. The input values will be passed on to the
        server for making calculations, which are described in the following
        steps."),
      tags$ol(
        tags$li("Calculate the net income by subtracting the annual loan 
                repayment, holding cost and leasing cost from the annual gross
                rental income."), 
        tags$li("Calculate the capital gain by subtracting the sale cost, 
                previous purchasing price and purchasing costs from the 
                expected future sale price."), 
        tags$li("Calculate all the invested capital (including the initial
                investment and the ongoing repayment to the capital) till the
                year marked on the x-axis."),
        tags$li("For each year on the x-axis, sum all net income from year 0 to
                the year marked on the x-axis."), 
        tags$li("For the annualized rate of return from only the accumulated 
                net rental income: divide the value calculated from step 4 by 
                the value calculated from step 3."), 
        tags$li("For the annualized rate of return from accumulated net rental 
                income + capital gain: add the values calculated from step 2 
                and 4, and then divide it by the value calculated from
                step 3.")),
      p("For more details, please refer to the source code of this app below."),
      a(href = "https://github.com/wooii/PropertyInvest", 
        "https://github.com/wooii/PropertyInvest")
    )
  )
)


# server ----------------------------------------------------------------------

server <- function(input, output) {
  
  output$Plots <- renderPlot({
    # Variables.
    price <- input$price
    foreign.investor <- input$foreign.investor
    investment <- input$investment
    first.home <- input$first.home
    new.home <- input$new.home
    deposit.ratio <- input$deposit.ratio/100
    loan.rate <- input$loan.rate/100
    loan.term <- input$loan.term
    payment.frequency <- as.numeric(input$payment.frequency)
    interest.only <- input$interest.only
    era <- input$expected.rate.of.appreciation/100
    scp <- input$sale.cost.to.price/100
    rp <- input$rent.to.price/100
    hcp <- input$hold.cost.to.price/100
    rcp <- input$rent.cost.to.rent/100
    
    # These parameters is used to tune all the function in the server.
    if (F) {
      price = 600000
      deposit.ratio = 0.1
      era = 0.001
      loan.rate = 0.05
      loan.term = 25
      payment.frequency = 12
      interest.only = F
      foreign.investor = F
      investment = T
      first.home = F
      new.home = F
      rp = 0.05
      hcp = 0.01
      rcp = 0.09
      scp = 0.05
    }
    
    # Initial investment.
    invest0 <- initial_invest(price = price, 
                              deposit.ratio = deposit.ratio, 
                              investment = investment,
                              foreign.investor = foreign.investor,
                              first.home = first.home, 
                              new.home = new.home)
    
    years <- seq(from = 0, to = loan.term, by = 1)
    mortgage.repayment <- mortgage(loan = price*(1 - deposit.ratio),
                                   loan.rate = loan.rate,
                                   loan.term = loan.term,
                                   payment.frequency = payment.frequency, 
                                   loan.term.paid = years, 
                                   interest.only = interest.only)
    
    # The expected future property price based on the expected annual rate of
    # price appreciation.
    price.e <- vector()
    for (i in years) {
      price.e[i + 1] <- price*(1 + era)^i
    }
    
    # Calculate the net rental income after deducting all costs. 
    pay.loan <- mortgage.repayment$repayment*payment.frequency
    net.income <- price.e*(rp*(1 - rcp) - hcp) - pay.loan
    net.income[1] <- 0  # Year 0 does not incur any rental income.
    
    # Calculate the capital gain when selling the property.
    price0 <- invest0 + price*(1 - deposit.ratio) # Actual total price.
    capital.gain <- price.e*(1 - scp) - price0
    
    # Calculate the total invested capital.
    invested <- price0 - mortgage.repayment$remaining.principal
    
    n <- length(years)
    rr.a1 <- rep(NA, n) # accumulated rate of return for net.income.
    rr.m1 <- rep(NA, n) # geometric mean of rate of return for net.income.
    rr.a2 <- rep(NA, n) # accumulated net.income + capital.gain
    rr.m2 <- rep(NA, n) # accumulated net.income + capital.gain
    for (i in 1:n) {
      p <- i - 1
      if (i == 1)  p <- 1 # to avoid p to be 0.
      rr.a1[i] <- sum(net.income[1:i])/invested[i]
      rr.m1[i] <- (rr.a1[i] + 1)^(1/p) - 1
      rr.a2[i] <- (sum(net.income[1:i]) + capital.gain[i])/invested[i]
      rr.m2[i] <- (rr.a2[i] + 1)^(1/p) - 1
    }
    
    # Output plot.
    df <- data.frame(years, rr.a1, rr.m1, rr.a2, rr.m2)
    df[is.na(df)] <- -1 # rr.m1 may be NA due to rr.a1 < -1.
    x.breaks <- c(seq(from = 0, to = loan.term, by = 2), loan.term)
    y.breaks <- c(seq(from = round(min(df$rr.m2), digits = 2), 
                      to = round(max(df$rr.m2), digits = 2), 
                      by = 0.02), 0)
    ggplot(df, aes(x = years, y = rr.m1, 
                   colour = "accumulated net rental income")) + geom_point() + 
      geom_point(aes(y = rr.m2, 
                     colour = "accumulated net rental income + capital gain")) +
      ylab("Annualized rate of return") + xlab("Years") +
      scale_x_continuous(breaks = x.breaks) +
      scale_y_continuous(labels = scales::percent, breaks = y.breaks) +
      theme(legend.position = c(0.5, 0.1), legend.direction = "vertical",
            legend.title = element_blank(),
            legend.background = element_blank())
  })
}


# Run the application ---------------------------------------------------------

shinyApp(ui = ui, server = server)

