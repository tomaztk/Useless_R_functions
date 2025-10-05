# install.packages("conjoint")
library(conjoint)

levels <- list(
  Tier = c("Basic", "Medium", "Full", "Enterprise"),
  Support = c("Email", "Chat", "Phone", "24/7 Premium"),
  Storage = c("10GB", "100GB", "1TB", "Unlimited"),
  API = c("None", "Limited", "Full Access"),
  Price = c(10, 20, 40, 80)  # in dollars per month
)

profiles <- expand.grid(levels)


# SImulacijai za  10 uporabnikov podatkov
set.seed(123)
n_profiles <- nrow(profiles)
respondents <- 10
ratings <- replicate(respondents, sample(1:n_profiles, n_profiles))

respondent_data <- lapply(1:respondents, function(i) {
  data.frame(profiles, rating = ratings[, i])
})


ca_models <- lapply(respondent_data, function(df) {
  caModel(df, attrib = names(levels), response = "raking")
})


# Conjoint analysis on each respondent
ca_models <- lapply(respondent_data, function(df) {
  caModel(df, ratings="rating")
})



# Aggregate part-worth utilities
utils_list <- lapply(ca_models, function(model) model$utilities)
mean_utils <- Reduce("+", utils_list) / respondents



print("Average Part-Worth Utilities:")
print(mean_utils)

# Simulate total utility for each profile using mean utilities
score_profile <- function(profile, utilities) {
  sum(sapply(names(profile), function(attr) {
    utilities[[attr]][as.character(profile[[attr]])]
  }))
}

profiles$TotalUtility <- apply(profiles, 1, function(row) {
  score_profile(as.list(row), mean_utils)
})

# Recommend top profiles (simulate best pricing strategy)
top_profiles <- profiles[order(-profiles$TotalUtility), ]
head(top_profiles, 10)





-------
  
# another dataset
  
# Create subscription conjoint dataset directly in R
subscriptions_conjoint <- data.frame(
    Tier = factor(c(
      "Basic", "Medium", "Full", "Enterprise",
      "Basic", "Medium", "Full", "Enterprise",
      "Basic", "Medium", "Full", "Enterprise"
    )),
    Support = factor(c(
      "Email", "Chat", "Phone", "24/7 Premium",
      "Email", "Chat", "Phone", "24/7 Premium",
      "Chat", "Phone", "Phone", "24/7 Premium"
    )),
    Storage = factor(c(
      "10GB", "100GB", "1TB", "Unlimited",
      "100GB", "1TB", "Unlimited", "1TB",
      "10GB", "100GB", "1TB", "Unlimited"
    )),
    API = factor(c(
      "None", "Limited", "Full Access", "Full Access",
      "None", "Limited", "Full Access", "Full Access",
      "Limited", "Limited", "Full Access", "Full Access"
    )),
    Price = factor(c(
      "$10", "$20", "$40", "$80",
      "$10", "$20", "$40", "$60",
      "$15", "$25", "$35", "$70"
    )),
    Ranking = c(15, 10, 5, 1, 18, 9, 4, 2, 16, 8, 3, 1)
  )




head(subscriptions_conjoint,3)



------- 
  
  library(shiny)
library(dplyr)
library(mlogit)
library(ggplot2)

tiers <- data.frame(
  Tier = c("Free", "Basic", "Advanced", "Enterprise"),
  Price = c(0, 9.99, 17.99, 69.99),
  Docs = c(10, 100, 1000, 10000),
  Users = c(1, 3, 10, 50),
  Languages = c(1, 3, 5, 20),
  API = c(100, 1000, 5000, 100000)
)


set.seed(2908)

# we need to define betas together
true_betas <- c(Price = -0.1, Docs = 0.002, Users = 0.3, Languages = 0.5, API = 0.0005)

cdd <- tiers %>%
  mutate(ChoiceSet = 1) %>%
  tidyr::expand_grid(UserID = 1:200) %>%
  rowwise() %>%
  mutate(
    Utility = Price * true_betas["Price"] +
      Docs * true_betas["Docs"] +
      Users * true_betas["Users"] +
      Languages * true_betas["Languages"] +
      API * true_betas["API"] +
      rnorm(1, 0, 1) # ali pa dam 0,0,1 ???
  ) %>%
  group_by(UserID) %>%
  mutate(Chosen = if_else(Utility == max(Utility), 1, 0)) %>%  #shall see if needed.
  ungroup() %>%
  mutate (UserID2 = c(1:800)
          ,ChosenL = as.logical(Chosen)) %>%
  select(-UserID) %>%
  rename(UserID = UserID2)


# vzmmm logit model pa da vidmo :)
mdata <- dfidx::dfidx(cdd, idx="UserID", choice = "ChosenL", shape = "long")


#model <- mlogit(Chosen2 ~ Price + Docs + Users + Languages + API | 0, data = mdata)
model <- mlogit(Chosen ~ Price + Docs + Users + Languages + API, data = mdata)
model <- mlogit(id2 ~ FALSE | Price, data = mdata)

ui <- fluidPage(
  titlePanel("PutSimply Subscription Tier Simulator"),
  sidebarLayout(
    sidebarPanel(
      h4("Adjust Tier Prices (€)", style = "margin-top: 20px;"),
      numericInput("price_basic", "Basic Price:", 9.99),
      numericInput("price_advanced", "Advanced Price:", 17.8),
      numericInput("price_enterprise", "Enterprise Price:", 70)
    ),
    mainPanel(
      plotOutput("preferencePlot"),
      tableOutput("simTable")
    )
  )
)

server <- function(input, output) {
  simulate_preferences <- reactive({
    test_tiers <- tiers
    test_tiers$Price <- c(0, input$price_basic, input$price_advanced, input$price_enterprise)
    
    # Predict utility and compute probabilities
    test_tiers$Utility <- predict(model, newdata = test_tiers)
    exp_u <- exp(test_tiers$Utility)
    test_tiers$Prob <- exp_u / sum(exp_u)
    
    test_tiers
  })
  
  output$preferencePlot <- renderPlot({
    df <- simulate_preferences()
    ggplot(df, aes(x = Tier, y = Prob, fill = Tier)) +
      geom_col() +
      labs(title = "Simulated PutSimply Subscription tiers", y = "Preference Probability", x = "Tier") +
      theme_minimal()
  })
  
  output$simTable <- renderTable({
    simulate_preferences() %>% select(Tier, Price, Docs, Users, Languages, API, Prob)
  })
}


shinyApp(ui = ui, server = server)
