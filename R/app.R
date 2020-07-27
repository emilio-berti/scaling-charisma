library(tidyverse)

data_to_shiny <- read_csv("../charisma.csv")

ui <- fluidPage(
   titlePanel(title = h1("Body size is a good proxy for vertebrate charisma", align = "center"),
              windowTitle = "Body size is a good proxy for vertebrate charisma"),
   br(),
   tags$div(
      tags$p(
         "This applications is a complementary tool to explore the data sources used. 
         After choosing the dataset and taxonomic classes to display, species data 
         points are shown together with a linear regression model with only body size 
         as predictor for visualization purposes only. Moreover, residuals of the full
         model as described in Table B.2 are plotted, as well as a quantile range 
         chosen by the user. Species outside this interquantile range (outliers) are 
         displayed below the figures. It also possible to restrict the range of the 
         residuals to focus on specific intervals. Species names are disaplyed when
         overing over individual points."
      )
   ),
   br(),
   fluidRow(
      column(2, offset = 2,
             checkboxGroupInput("dataset",
                                "Datasets to display",
                                choices = levels(as.factor(data_to_shiny$Dataset)))),
      column(2,
             checkboxGroupInput("class",
                                "Classes to display",
                                choices = levels(as.factor(data_to_shiny$Class)),
                                selected = levels(as.factor(data_to_shiny$Class)))),
      column(4,
             sliderInput("quantiles",
                         "Quantiles lines to display (residuals)",
                         min = 0, 
                         max = 1, 
                         step = 0.005, 
                         value = c(0.025, 0.975), 
                         width = "100%")),
      br(),
      column(4,
             sliderInput("range",
                         "Residuals range",
                         min = floor(min(data_to_shiny$Res)), 
                         max = ceiling(max(data_to_shiny$Res)), 
                         step = 0.01, 
                         value = c(floor(min(data_to_shiny$Res)), 
                                   ceiling(max(data_to_shiny$Res))), 
                         width = "100%"))
   ),
   fluidRow(
      column(5, offset = 1,
             plotlyOutput("plot_raw",
                          width = "100%",
                          height = "200%")),
      column(5, offset = 0,
             plotlyOutput("plot_res", 
                          width = "100%",
                          height = "200%"))
   ),
   br(),
   h3("Outliers: species outside quantile range"),
   br(),
   fluidRow(
      column(12,
             textOutput("outliers"))
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   shiny_datasets <- reactive(input$dataset)
   shiny_classes <- reactive(input$class)
   quants <- reactive({input$quantiles})
   r <- reactive({input$range})
   shiny_d <- reactive({
      filter(data_to_shiny, 
             Dataset %in% shiny_datasets(),
             Class %in% shiny_classes())
   })
   low_quant <- reactive({
      quantile(shiny_d()$Res, quants()[1])
   })
   high_quant <- reactive({
      quantile(shiny_d()$Res, quants()[2])
   })
   output$plot_raw <- renderPlotly({
      raw_plot <- shiny_d() %>% 
         ggplot() +
         geom_smooth(aes(Mass, ScaledLogRaw, col = Dataset), method = "lm", alpha = 0.5) +
         geom_point(aes(Mass, ScaledLogRaw, col = Dataset, group = Species), 
                    size = 1, show.legend = FALSE, alpha = 1) +
         scale_x_log10(labels = scales::comma) +
         theme_bw() +
         scale_color_brewer(type = "qual", palette = "Set2") +
         xlab("Body size (g)") +
         ylab("Charisma")
      ggplotly(raw_plot, tooltip = "Species")
   })
   output$plot_res <- renderPlotly({
      res_plot <- shiny_d() %>% 
         ggplot() +
         geom_point(aes(Mass, Res, col = Dataset, group = Species), 
                    size = 1, show.legend = FALSE, alpha = 1) +
         geom_hline(yintercept = 0, linetype = "dashed", col = "tomato", size = 1) +
         geom_hline(yintercept = low_quant(), col = "steelblue") +
         geom_hline(yintercept = high_quant(), col = "steelblue") +
         scale_x_log10(labels = scales::comma) +
         theme_bw() +
         scale_color_brewer(type = "qual", palette = "Set2") +
         xlab("Body size (g)") +
         ylab("LMs Residuals") +
         scale_y_continuous(limits = c(r()[1], r()[2]))
      ggplotly(res_plot, tooltip = "Species")
   })
   output$outliers <- renderText({
      shiny_d() %>% 
         filter(Res < low_quant() | Res > high_quant()) %>% 
         pull(Species) %>% 
         sort() %>% 
         unique()
   }, sep = ", ")
}

# Run the application 
# shinyApp(ui = ui, server = server)

