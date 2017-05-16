menuSubItemFileInput <- function(inputId, text, icon = shiny::icon("angle-double-right")) {
  script <- "document.getElementById('%id%').click(); return false;"
  script <- gsub("%id%", inputId, script)
  list(div(fileInput(inputId, ""), style = "display: none;"),
       tags$li(a(href = "#", onclick = script, icon, text)))
}

menuSubItemDownloadLink <- function(inputId, label, icon = shiny::icon("angle-double-right")) {
  tags$li(
    tags$a(id = inputId,
           class = "shiny-download-link",
           href = "",
           target = "_blank",
           download = NA,
           icon,
           label)
  )
}

menuSubItemActionButton <- function(inputId, label, icon = shiny::icon("angle-double-right")) {
  script <- "document.getElementById('%id%').click(); return false;"
  script <- gsub("%id%", inputId, script)
  list(actionButton(inputId, "", style = "display: none;"),
       tags$li(a(href = "#", onclick = script, icon, label)))
}

menuSubItemCopyToClipboard <- function(textAreaId, label, icon = shiny::icon("angle-double-right")) {
  script <- "
var aux = document.createElement(\"input\");
aux.setAttribute('value', document.getElementById('%id%').innerHTML);
document.body.appendChild(aux);
aux.select();
document.execCommand('copy');
document.body.removeChild(aux);
//alert(document.getElementById('%id%').value);
return false;
"
#   script <- "
# clipboard.copy(document.getElementById('%id%').innerHTML);
# return false;
# "

  script <- gsub("%id%", textAreaId, script)
  tags$li(a(href = "#", onclick = script, icon, label))
}

# "function copyToClipboard(elementId) {
# var aux = document.createElement(\"input\");
# aux.setAttribute(\"value\", document.getElementById(elementId).value);
# document.body.appendChild(aux);
# aux.select();
# document.execCommand(\"copy\");
# document.body.removeChild(aux);
# }"