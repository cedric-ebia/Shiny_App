
header <- dashboardHeader(title = "Support Vector Machine - Credit Fraud",
                          titleWidth = 400,
                          tags$li(a(id = "download", class = "fa fa-download",
                                    href = "/www/Technical_Manual.pdf", download = "Technical_Manual.pdf"),
                                  class = "dropdown"),
                          tags$li(a(onclick = "onclick =window.open('https://github.com/loicpalma/Shiny_App')",
                                    href = NULL,
                                    icon("github"),
                                    title = "GitHub",
                                    style = "cursor: pointer;"),
                                  class = "dropdown"),
                          tags$li(a(onclick = "openTab('foa')",
                                    href = NULL,
                                    icon("home"),
                                    title = "Homepage",
                                    style = "cursor: pointer;"),
                                  class = "dropdown",
                                  tags$script(HTML("
                                       var openTab = function(tabName){
                                       $('a', $('.sidebar')).each(function() {
                                       if(this.getAttribute('data-value') == tabName) {
                                       this.click()
                                       };
                                       });
                                       }")))
 
                          
)

