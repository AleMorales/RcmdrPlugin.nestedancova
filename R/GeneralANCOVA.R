

#' Callback function for general ANCOVA menu
#' @export
GeneralANCOVA <- function(){
  initializeDialog(title=gettextRcmdr("General AN(C)OVA"))
  defaults <- list(initial.weight = gettextRcmdr("<no variable selected>"),
                   initial.delete.cases=gettextRcmdr("<use all valid cases>"))
  dialog.values <- getDialog("GeneralANCOVA", defaults)
  .activeModel <- ActiveModel()
  currentModel <- if (!is.null(.activeModel))
    class(get(.activeModel, envir=.GlobalEnv))[1] == "lm"
  else FALSE
  if (currentModel) {
    currentFields <- formulaFields(get(.activeModel, envir=.GlobalEnv))
    if (currentFields$data != ActiveDataSet()) currentModel <- FALSE
  }
  if (isTRUE(getRcmdr("reset.model"))) {
    currentModel <- FALSE
    putRcmdr("reset.model", FALSE)
  }
  UpdateModelNumber()
  modelName <- tclVar(paste("GeneralANCOVA.", getRcmdr("modelNumber"), sep=""))
  modelFrame <- tkframe(top)
  model <- ttkentry(modelFrame, width="20", textvariable=modelName)
  removeVariable <- tclVar(dialog.values$initial.delete.cases)
  removeFrame <- tkframe(top)
  removeEntry <- ttkentry(removeFrame, width="60", textvariable=removeVariable)
  removeScroll <- ttkscrollbar(removeFrame, orient="horizontal",
                               command=function(...) tkxview(removeEntry, ...))
  tkconfigure(removeEntry, xscrollcommand=function(...) tkset(removeScroll, ...))
  onOK <- function(){
    closeDialog()
    modelValue <- trim.blanks(tclvalue(modelName))
    if (!is.valid.name(modelValue)){
      errorCondition(recall=GeneralANCOVA, message=sprintf(gettextRcmdr('"%s" is not a valid name.'), modelValue), model=TRUE)
      return()
    }
    subset <- tclvalue(subsetVariable)
    if (trim.blanks(subset) == gettextRcmdr("<all valid cases>") || trim.blanks(subset) == ""){
      subset <- ""
    }
    weight.var <- getSelection(weightComboBox)
    weights <- if (weight.var == gettextRcmdr("<no variable selected>")) ""
    else weight.var
    check.empty <- gsub(" ", "", tclvalue(lhsVariable))
    if ("" == check.empty) {
      errorCondition(recall=GeneralANCOVA, message=gettextRcmdr("Left-hand side of model empty."), model=TRUE)
      return()
    }
    check.empty <- gsub(" ", "", tclvalue(rhsVariable))
    if ("" == check.empty) {
      errorCondition(recall=GeneralANCOVA, message=gettextRcmdr("Right-hand side of model empty."), model=TRUE)
      return()
    }
    if (is.element(modelValue, listGeneralANCOVAs())) {
      if ("no" == tclvalue(checkReplace(modelValue, type=gettextRcmdr("Model")))){
        UpdateModelNumber(-1)
        if (getRcmdr("onApplyCalled")){
          putRcmdr("onApplyCalled", FALSE)
          return()
        }
        GeneralANCOVA()
        return()
      }
    }
    remove <- trim.blanks(tclvalue(removeVariable))
    remove.cases <- if (remove == gettextRcmdr("<use all valid cases>") || remove == ""){
      ""
    } else {
      getCases(remove, remove=TRUE)
    }
    if (remove.cases != "" && inherits(remove.cases, "cases-error")){
      errorCondition(recall = GeneralANCOVA,
                     message = removeRows,
                     model=TRUE)
      return()
    }
    if (subset != "" && remove.cases != ""){
      errorCondition(recall = GeneralANCOVA,
                     message = gettextRcmdr("You cannot specify both case removal and subset cases"),
                     model=TRUE)
      return()
    }
    formula <- paste(tclvalue(lhsVariable), tclvalue(rhsVariable), sep=" ~ ")
    command <- Command("aov", formula, data=ActiveDataSet(), subset=subset, weights=weights,
                       subset=remove.cases, to=modelValue, qr = TRUE)
    doItAndPrint(command)
    command <- paste0("class(",modelValue,") <- c('ancova',class(",modelValue,"))")
    doItAndPrint(command)
    activeModel(modelValue)
    if (subset != "" || remove.cases != "") putRcmdr("modelWithSubset", TRUE) else putRcmdr("modelWithSubset", FALSE)
    initial.delete.cases <- gettextRcmdr("<use all valid cases>")
    putDialog("GeneralANCOVA", list(initial.weight = weight.var,
                                  initial.delete.cases = initial.delete.cases))
    tkfocus(CommanderWindow())
  }
  OKCancelHelp(helpSubject="GeneralANCOVA", model=TRUE, reset="resetGeneralANCOVA", apply="GeneralANCOVA")
  tkgrid(labelRcmdr(modelFrame, text=gettextRcmdr("Enter name for model:")), model, sticky="w")
  tkgrid(modelFrame, sticky="w")
  modelFormula()
  subsetWeightFrame <- tkframe(top)
  subsetBox(window=subsetWeightFrame, model=TRUE)
  weightComboBox <- variableComboBox(subsetWeightFrame, variableList=Numeric(),
                                     initialSelection=dialog.values$initial.weight,
                                     title=gettextRcmdr("Weights"))
  tkgrid(getFrame(xBox), sticky="w")
  tkgrid(outerOperatorsFrame, sticky="w")
  tkgrid(formulaFrame, sticky="w")
  tkgrid(labelRcmdr(removeFrame, text=" "))
  tkgrid(labelRcmdr(removeFrame, text=gettextRcmdr("Indices or names of row(s) to remove"),
                    foreground=getRcmdr("title.color"), font="RcmdrTitleFont"), sticky="w")
  tkgrid(removeEntry, sticky="w")
  tkgrid(removeScroll, sticky="ew")
  if (getRcmdr("model.case.deletion")) tkgrid(removeFrame, sticky="nw", columnspan=3)
  tkgrid(subsetFrame, tklabel(subsetWeightFrame, text="   "),
         getFrame(weightComboBox), sticky="nw")
  tkgrid(subsetWeightFrame, sticky="w")
  tkgrid(buttonsFrame, sticky="w")
  dialogSuffix(focus=lhsEntry, preventDoubleClick=TRUE)
}

resetGeneralANCOVA <- function(){
  putRcmdr("reset.model", TRUE)
  putDialog("GeneralANCOVA", NULL)
  putDialog("GeneralANCOVA", NULL, resettable=FALSE)
  GeneralANCOVA()
}


listGeneralANCOVAs <- function(envir=.GlobalEnv, ...) {
  objects <- ls(envir=envir, ...)
  if (length(objects) == 0) NULL
  else objects[sapply(objects,
                      function(.x) inherits(get(.x, envir=envir), "ancova"))]
}

#' Callback function for Table of Means menu (activates on general ANCOVA model)
#' @export
TableOfMeans <- function() {
  .activeModel <- ActiveModel()
  command <- Command("model.tables", x = .activeModel, type = "'means'")
  doItAndPrint(command)
  return()
}

#' Callback function for Tble of Effects menu (activates on general ANCOVA model)
#' @export
TableOfEffects <- function() {
  .activeModel <- ActiveModel()
  command <- Command("model.tables", x = .activeModel, type = "'effects'")
  doItAndPrint(command)
  return()
}

#' Callback function for Posthoc tests menu (activates on general ANCOVA model)
#' @export
PosthocTests <- function() {
  # Default values
  defaults <- list(initial.vars=NULL,initial.methods="none",initial.conf=0.95)
  dialog.values <- getDialog("PosthocTests", defaults)
  # Start the dialog
  initializeDialog(title=gettextRcmdr("Posthoc tests"))
  # Retrieve the data and the model
  .activeDataSet <- ActiveDataSet()
  .activeModel   <- ActiveModel()
  # Figure out which factor variables are predictors in the model
  all_factors <- Factors()
  model <- get(.activeModel)
  if(inherits(model, "aovlist")) {
    aov_model <- model[[1]]
  } else {
    aov_model <- model
  }
  used_factors <- all_factors[which(all_factors %in% labels(terms(as.formula(aov_model))))]
  # Create box to select the variables for grouping the posthoc tests
  varsBox <- variableListBox(top, used_factors, selectmode="multiple",
                             title=gettextRcmdr("Variables"),
                             initialSelection=varPosn(dialog.values$initial.x, "factor"))
  # Options for method
  optionsFrame <- tkframe(top)
  list_methods <- c(none = "No adjustment", tukey = "Tukey", sidak = "Dunn-Sidak",
               mvt = "Multivariate t", bonferroni = "Bonferroni")
  radioButtons(optionsFrame, name="methods", buttons=names(list_methods),
               labels=gettextRcmdr(unname(list_methods)),
               initialValue=dialog.values$initial.methods,
               title=gettextRcmdr("P-value adjustment"))
  # Box for confidence level
  # <to be implemented>

  # Execution of analysis
  onOK <- function() {
    # Retrieve the choices from the user
    method <- as.character(tclvalue(methodsVariable))
    vars <- getSelection(varsBox)
    # Save current state for the next analysis
    putDialog("PosthocTests", list(initial.methods=method, initial.vars=vars))
    closeDialog()
    # R commands
    #comps = emmeans(.activeModel, ~vars,...)
    formula <- paste0("~",paste0(vars, collapse = "*"))
    command <- Command("comps <- emmeans::emmeans", object = .activeModel, specs = formula)
    doItAndPrint(command)
    #cld(comps, Letters = letters,...)
    command <- Command("multcomp::cld", object = "comps", level = 1 - 0.95,
                       adjust = Q(method), Letters = "letters")
    doItAndPrint(command)
    tkfocus(CommanderWindow())
  }
  OKCancelHelp(reset="PosthocTests", apply="PosthocTests")
  tkgrid(getFrame(varsBox), sticky="nw")
  tkgrid(labelRcmdr(top, text=""))
  tkgrid(methodsFrame, labelRcmdr(optionsFrame, text="  "), sticky="w")
  tkgrid(optionsFrame, sticky="w")
  tkgrid(buttonsFrame, sticky="w")
  dialogSuffix()
}



