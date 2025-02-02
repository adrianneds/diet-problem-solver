#####     Author: Adrianne D. Solis      #####
#####     Diet Problem Solver:  Main     #####

# install packages that are not yet installed
packages = c("shiny", "bslib", "shinyWidgets", "readxl")

for (i in 1:length(packages)) {
  if (!(packages[i] %in% rownames(installed.packages()))) {
    install.packages(packages[i])
  }
}

library(shiny); library(bslib); library(shinyWidgets); library(readxl);

# [0] Run simplex function definitions
source("simplex_solver.R")

vboxGreen = value_box_theme(
  bg = '#629946',
  fg = 'white',
)

vboxOrange = value_box_theme(
  bg = '#df9321',
  fg = 'white',
)

# [2] UI structure
ui <- page_sidebar(

  style = 'background-image: url(foodbg6.png); background-repeat:no-repeat',
  title = div(
    class = "d-flex align-items-center justify-content-between",
    h3("Diet Problem Solver", style="font-family:Trebuchet MS; font-weight:bold; color:white"),# Title
    div(
      actionButton("about_btn", "About the Program",
                   style="color:white; font-family:Trebuchet MS; background-color:#6f325c;
                   padding:10px; border-color: #6f325c; font-size:16px"),
      style="padding-left:35px"
    ),
    div(
      actionButton("dev_btn", "Developer",
                   style="color:white; font-family:Trebuchet MS; background-color:#6f325c;
                   padding:10px; border-color: #6f325c; font-size:16px"),
      style="padding-left:20px"
    ),
    div(
      actionButton("how_btn", "How to Use?",
                   style="color:white; font-family:Trebuchet MS; background-color:#6f325c;
                   padding:10px; border-color: #6f325c; font-size:16px"),
      style="padding-left:20px"
    )
  ),

  layout_columns(

    # arranged in a single row
    value_box(
     title = 'Number of Iterations',
     id = 'iter',
     value = span(textOutput('iterVal'), style = "font-size: 50px; font-weight: bold; font-family: Trebuchet MS"),
     theme = vboxGreen,
     max_height = 130,
     showcase = bsicons::bs_icon('clipboard2-check-fill')
    ),

    value_box(
      title = 'Daily Cost',
      id = 'cost',
      value = span(textOutput('costVal'), style = "font-size: 50px; font-weight: bold; font-family: Trebuchet MS"),
      theme = vboxOrange,
      max_height = 130,
      showcase = bsicons::bs_icon("currency-dollar")
    ),

    value_box(
      title = 'Number of Food Items',
      id = 'foods',
      value = span(textOutput('foodVal'), style = "font-size: 50px; font-weight: bold; font-family: Trebuchet MS"),
      theme = vboxGreen,
      max_height = 130,
      showcase = bsicons::bs_icon("basket-fill")
    ),

  ), # end of layout_columns

  layout_columns(

    card(
      span(textOutput("optCost"), style='color: #9d1a75; font-size: 25px; font-style:italic; font-family:Trebuchet MS'),
      id = "final_values",
      span(textOutput("final_values_txt"),     style="font-style:italic; font-weight:bold; font-family: Trebuchet MS"),
      span(textOutput("infeasible_txt"),     style="font-size:25px; font-style:italic; font-weight:bold; font-family: Trebuchet MS"),
      tableOutput("final_table"),
      full_screen = TRUE,
      height=250
    ),

  ), # end of layout_columns

  layout_columns(
    card(
      span(textOutput("ini_tableau_title"), style="font-style:italic; font-weight:bold; font-family: Trebuchet MS"),
      div(tableOutput("ini_table"),style="font-size:80%"),

      span(textOutput("fin_tableau_title"), style="font-style:italic; font-weight:bold; font-family: Trebuchet MS"),
      div(tableOutput("fin_table"),style="font-size:80%"),

      span(textOutput("basic_sol_title"),  style="font-style:italic; font-weight:bold; font-family: Trebuchet MS"),
      div(tableOutput("basic_table"),style="font-size:80%"),

      span(textOutput("simplex_text"),     style="font-style:italic; font-weight:bold; font-family: Trebuchet MS"),
      div(tableOutput("simplex_outputs"),style="font-size:70%"),
      full_screen = TRUE,
      height=250
    )
  ), # end of layout_columns

    sidebar = sidebar(
      bg = "#994e82",
      fg = "#FFF",
      width = 300,
      title = "Select food items for your daily diet",
      checkboxGroupInput(inputId = "checkbox_group", "",
                         choiceNames = foodNamesList,
                         choiceValues = foodValuesList ),

      # select/unselect all
      actionButton(inputId = "select_all", "Select/Unselect All",style="background-color:#FFF; color:#9d1a75; font-family: Trebuchet MS"),

      # submit button
      actionButton(inputId = "submit", "Find my optimal diet!",style="background-color:#FFF; color:#9d1a75; font-family: Trebuchet MS"),

      # Select test case (note: for testing purposes only)
      actionButton(inputId = "test", "Sample Output; Select first 20 food items", style="background-color:#FFF; color:#9d1a75; font-family: Trebuchet MS")
    ), # end of sidebar

  tags$head(tags$style(HTML(
    '#submit:hover, #test:hover {
        background-color: #629946 !important;
        color: white !important;
        font-weight:bold; font-style:italic;
      }
    #select_all:hover {
        background-color: #df9321 !important;
        color: white !important;
    }
    .navbar-static-top {
        background-color:#994e82 !important;
    }
    h1.bslib-page-title.navbar-brand {
        color: white !important;
        font-weight: bold !important;
    }
    #about_btn:hover, #dev_btn:hover, #how_btn:hover {
        background-color: #bc4e9a !important;
        color: white !important;
    }'
  )))

) # end of page_sidebar

modal_abt <-modalDialog(

  h6("About the Program", style="font-weight:bold"),
  p("The diet problem solver gives users an optimal daily diet that minimizes cost and satisfies nutritional constraints given
    their selected food items. Output includes optimal cost, food items, and number of servings per food item for the optimal daily diet",style="font-size:15px"),

  p("The nutrients considered are:
    Calories, Cholesterol, Total Fat, Sodium, Carbohydrates, Dietary Fiber, Protein, Vitamin A, Vitamin C, Calcium, Iron.", style="font-size:15px"),

  h6("Method",style="font-weight:bold"),
  p("The solver uses a Simplex method algorithm for minimization. The variable to be minimized is the daily total cost of the diet. The constraints are for the minimum
    and maximum nutrients and servings per food item.", style="font-size:15px"),
)

modal_dev <-modalDialog(

  h6("About the Developer", style="font-weight:bold"),

  p("This diet problem solver was created by Adrianne D. Solis from BS Statistics Batch 2022 during A.Y. '24'-25
    as a requirement for CMSC 150 under Sir Jamlech Iram N. Gojo-Cruz.",style="font-size:15px"),
)

modal_how <-modalDialog(

  h6("How to Use?", style="font-weight:bold"),
  p("Select your preferred food items in the sidebar on the left. You may
    click the 'Select/Unselect All' button to reset your choices."),

  p("Once done choosing, click the 'Find my optimal diet!' button to obtain your optimal cost and daily diet. Results
    will be shown on the page."),

  p("For a full-screen view, expand the results card (lower right button)",style="font-size:15px; font-weight:bold"),
)


# [3] Control inputs and outputs
server <- function(input, output, session) {

 # observe({    output$food_table = renderTable(foodServingSizes, striped=TRUE)
  observe({output$ini_tableau_title = renderText("Simplex method process will appear here.")
           output$final_values_txt = renderText("Your results will appear here.")})

  # select/unselect food items
  observe({
    if(input$select_all == 0) return(NULL)
    else if (input$select_all%%2 == 0)      # input$select_all = 1 when first selected all, so multiple of 2 -> need to unselect
    { updateCheckboxGroupInput(session,"checkbox_group","", choiceNames = foodNamesList, choiceValues = foodValuesList) }
    else
    { updateCheckboxGroupInput(session,"checkbox_group","", choiceNames = foodNamesList, choiceValues = foodValuesList, selected=foodValuesList) }
  })

  observeEvent(input$test, {updateCheckboxGroupInput(session,"checkbox_group","", choiceNames = foodNamesList, choiceValues = foodValuesList, selected=1:20)})
  observeEvent(input$about_btn, {showModal(modal_abt) })
  observeEvent(input$dev_btn, {showModal(modal_dev) })
  observeEvent(input$how_btn, {showModal(modal_how) })

  # submit food items
  observeEvent(input$submit,
               if (length(input$checkbox_group)==0) {
                 output$infeasible_txt = renderText("ERROR: You selected 0 food items.")
                 output$final_values_txt = renderText("")
                 output$optCost = renderText("")
                 output$final_table = NULL

                 output$basic_sol_title = renderText("")
                 output$basic_table = NULL

                 # update value boxes
                 output$iterVal = renderText( "" )
                 output$costVal = renderText( "" )
                 output$foodVal = renderText( "" )

               output$ini_tableau_title = renderText("")
               output$ini_table = NULL
               output$fin_tableau_title = renderText("")
               output$fin_table = NULL

               output$simplex_text = renderText("")
               output$simplex_outputs = NULL }
               else {
               submit() })

  submit <- function() {

    foodVector = as.numeric(input$checkbox_group)

    # Solve
    sol = solveDiet(foodVector)
    print(sol)

    if (sol$feasible == 0) {
      output$infeasible_txt = renderText("The problem is infeasible.
                                           It is not possible to meet the
                                           nutritional constraints with the selected food items.")

      output$final_values_txt = renderText("")
      output$optCost = renderText("")
      output$final_table = NULL
      output$fin_table = NULL


      output$basic_sol_title = renderText("")
      output$basic_table = NULL

      # update value boxes
      output$iterVal = renderText( sol$count )
      output$costVal = renderText( "NA" )
      output$foodVal = renderText( "NA" )

    } else {
      solutionTable = sol$finalValues
      output$infeasible_txt = renderText("")

      foods = nrow(solutionTable)

      # final tableau
      finalTableau = sol$finalTableau

      output$fin_tableau_title = renderText("Final Tableau:")
      output$fin_table = renderTable(finalTableau, striped = TRUE)

      # Final Basic solution
      basicSolution = sol$basicSolution
      costRound = round(sol$optimumCost,2)

      output$basic_sol_title = renderText("Final Basic solution:")
      output$basic_table = renderTable(basicSolution, striped = TRUE)

      output$optCost = renderText( paste("The cost of this optimal diet is $", costRound , " per day"))
      output$final_values_txt = renderText("Your optimal food servings and cost:")
      output$final_table = renderTable(solutionTable, striped = TRUE)

      # update value boxes
      output$iterVal = renderText( iterCount )
      output$costVal = renderText( costRound )
      output$foodVal = renderText( foods )
    }

    iterCount = sol$count

    # Initial tableau and iterations
    initialTableau = sol$initialTableau
    iterations = sol$iterations

    output$ini_tableau_title = renderText("Initial Tableau:")
    output$ini_table = renderTable(initialTableau, striped = TRUE)

    output$simplex_text = renderText("\nSimplex method iterations: ")
    output$simplex_outputs = renderTable(iterations)
  }

}

# [4] Run Program
shinyApp(ui = ui, server = server)



