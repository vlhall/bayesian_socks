if (("shiny" %in% rownames(installed.packages())) == FALSE){
  install.packages("shiny")
}

shinyUI(
  fluidPage(
    # write title
    titlePanel(
      "Karl Broman’s Socks: An Exploration of Bayesian Statistics"
    ),
    # define sidebar panel with selection of priors and parametric inputs
    sidebarPanel(
      numericInput("n_sims", "Number of Simulations", value=100, min=100, max=1e6),
      hr(),
      h4("Priors:"),
      selectInput("tprior", "Prior for Population Total:", c("Negative Binomial"="nbinom", "Discrete Normal"="norm")),
      hr(),
      # appears if selected input is "Negative Binomial"
      conditionalPanel(
        condition = "input.tprior == 'nbinom'",
        sliderInput("nbprior_mu", "Mu", value=30, min=11, max=1000),
        sliderInput("nbprior_sd", "Sigma", value=15, min=1, max=100)
      ),
      # appears if selected input is "Normal"
      conditionalPanel(
        condition = "input.tprior == 'norm'",
        sliderInput("nprior_mu", "Mu", value=30, min=11, max=1000),
        sliderInput("nprior_sd", "Sigma", value=15, min=1, max=100)
      ),
      hr(),
      selectInput("pprior", "Prior for Population Proportion:", c("Beta"="beta", "Truncated Normal"="truncnorm")),
      hr(),
      # appears if selected input is "Beta"
      conditionalPanel(
        condition = "input.pprior == 'beta'",
        sliderInput("shape1", "Alpha", value=15, min=.5, max=100),
        sliderInput("shape2", "Beta", value=15, min=.5, max=100)
      ),
      # appears if selected input is "Truncated Normal"
      conditionalPanel(
        condition = "input.pprior == 'truncnorm'",
        sliderInput("tnprior_mu", "Mu", value=.5, min=.05, max=.95),
        sliderInput("tnprior_sd", "Sigma", value=15, min=1, max=100)
      )),
    # define main area for plotted posteriors with optional displays, numeric outputs
    mainPanel(
      h4("Results"),
      # posterior for total
      checkboxInput("totalprior", "Show prior", value = FALSE),
      checkboxInput("totalest", "Show total estimates", value = FALSE),
      checkboxInput("totalval", "Show actual total", value = FALSE),
      plotOutput("posterior_total_plot"),
      textOutput("TotalMean"),
      textOutput("TotalMedian"),
      textOutput("TotalCredibleInterval"),
      hr(),
      # posterior for proportion
      checkboxInput("propprior", "Show prior", value = FALSE),
      checkboxInput("propest", "Show proportion estimates", value = FALSE),
      checkboxInput("propval", "Show actual proportion", value = FALSE),
      plotOutput("posterior_prop_plot"),
      textOutput("ProportionMean"),
      textOutput("ProportionMedian"),
      textOutput("ProportionCredibleInterval"),
      hr()
    )
  )
)