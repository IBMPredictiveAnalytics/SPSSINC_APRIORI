# SPSSINC APRIORI
#/***********************************************************************
# * Licensed Materials - Property of IBM 
# *
# * IBM SPSS Products: Statistics Common
# *
# * (C) Copyright IBM Corp. 1989, 2014
# *
# * US Government Users Restricted Rights - Use, duplication or disclosure
# * restricted by GSA ADP Schedule Contract with IBM Corp. 
# ************************************************************************/

# History
# 22-Aug-2008: Initial version utilising the extension module...
# 15-Sep-2008: Support for user-specified consequent
# 24-Aug-2009: Support for large variable lists
# 31-Aug-2009: Support for soting rules by Confidence, Lift, Support
# 03-Nov-2009: Support for specifying a limit for number of rules displayed
# 16-Apr-2013: Rewrite as pure R
# 05-Jun-2013: fix bad help reference
# 16-Jun-2014: help update



#__author__  =  'Jason Burke/Australia/IBM'
#__version__ =  '2.1.2'


help_text = "
OVERVIEW:
The Apriori command discovers association rules in a dataset. Association rules
are statements of the form:

if  antecedent(s)  then  consequent(s)

REQUIREMENTS:
The command requires the SPSS-R Integration Package,
in addition to the R arules package.

SYNTAX:

SPSSINC APRIORI varlist
 [/CRITERIA [SUPPORT={0.1**}] [CONFIDENCE={0.8**}] [MINLIFT={1.0**}
                     {value}              {value}           {value}
 [/FORMAT [{SUPPORT   } ={A}] [MAXRULES={ 25**}]
           {CONFIDENCE}  {D}            {value}
           {LIFT      }
 [/OPTIONS [ITEMPLOT={YES}]   [SUMMARY={YES}]
                     {NO*}             {NO*}    
 [/HELP]

** Default if value is omitted
"

# override for api to account for extra parameter in V19 and beyond
StartProcedure <- function(procname, omsid) {
    if (substr(spsspkg.GetSPSSVersion(),1, 2) >= 19) {
        spsspkg.StartProcedure(procname, omsid)
    }
    else {
        spsspkg.StartProcedure(omsid)
    }
}

gtxt <- function(...) {
    return(gettext(...,domain="SPSSINC_APRIORI"))
}

gtxtf <- function(...) {
    return(gettextf(...,domain="SPSSINC_APRIORI"))
}

Run <- function(args) {
    #Execute the SPSSINC APRIORI command

    cmdname = args[[1]]
    args = args[[2]]
    oobj = spsspkg.Syntax(list(
        spsspkg.Template("", subc="", var="variables", ktype="existingvarlist", islist=TRUE),
        spsspkg.Template("SUPPORT", subc="CRITERIA", var="minsupport", ktype="float", 
            vallist=list(0.0, 1.0)),
        spsspkg.Template("CONFIDENCE", subc="CRITERIA", var="minconfidence",  ktype="float", 
            vallist=list(0.0, 1.0)),
        spsspkg.Template("MINLIFT", subc="CRITERIA", var="minlift", ktype="float", 
            vallist=list(1.0 )),
        spsspkg.Template("CONFIDENCE", subc="FORMAT", var="confidence", ktype="str", 
            vallist=list("a", "d")),
        spsspkg.Template("LIFT", subc="FORMAT", var="lift", ktype="str", 
            vallist=list("a", "d")),
        spsspkg.Template("SUPPORT", subc="FORMAT", var="support", ktype="str", 
            vallist=list("a", "d")),
        spsspkg.Template("MAXRULES", subc="FORMAT", var="maxrules", ktype="float", 
            vallist=list(1.0)),
        spsspkg.Template("CONSEQUENT", subc="CRITERIA", ktype="literal", var="consequent", islist=TRUE),
        spsspkg.Template("ITEMPLOT", subc="OPTIONS", ktype="bool", var="itemplot"),
        spsspkg.Template("SUMMARY", subc="OPTIONS", ktype="bool", var="showsummary"),
        spsspkg.Template("PROGRAMFILE", subc="SAVE", ktype="literal")
        ))

    # A HELP subcommand overrides all else
    if ("HELP" %in% attr(args,"names")) {
        #writeLines(help_text)
        helper(cmdname)
    }
    else {
        res <- spsspkg.processcmd(oobj, args, "doapriori")
    }
}

doapriori = function(variables, minsupport=0.1, minconfidence=0.8, minlift=1.0, 
    confidence=NULL, lift=NULL, maxrules=25, support=NULL, consequent=NULL, 
    itemplot=FALSE, showsummary=FALSE, programfile=NULL) {
    # consequent not currently implemented
    if (!is.null(consequent)) {
        print(gtxt("Consequent keyword not currently implemented"))
    }
    if (!is.null(confidence)) {
        if (confidence == "d") {
            sortorder = expression(sort(subset_rules, decreasing = TRUE, na.last = NA, by = "confidence" ))
        } else {
            sortorder = expression(sort(subset_rules, decreasing = FALSE, na.last = NA, by = "confidence" ))
        }
    } else if (!is.null(lift)) {
        if (lift == "d") {
            sortorder = expression(sort(subset_rules, decreasing = TRUE, na.last = NA, by = "lift" ))
        } else {
            sortorder = expression(sort(subset_rules, decreasing = FALSE, na.last = NA, by = "lift" ))
        }
    } else if (!is.null(support)) {
        if (support == "d") {
            sortorder = expression(sort(subset_rules, decreasing = TRUE, na.last = NA, by = "support" ))
        } else {
            sortorder = expression(sort(subset_rules, decreasing = FALSE, na.last = NA, by = "support" ))
        }
    } else {
        sortorder = expression(subset_rules)
    }
    setuplocalization()

    tryCatch(library(arules), error=function(e){
        stop(gtxtf("The R %s package is required but could not be loaded.","arules"),call.=FALSE)
        }
    )
    if (!is.null(programfile)) {
        print(gtxt("Warning: The programfile keyword is no longer supported"))
        programfile = NULL
    }

    casedata <- spssdata.GetDataFromSPSS(variables)
    trans_table <- as(data.matrix(casedata), "transactions")
    print(gtxt("-- Summary of Transactions Data --"))
    print(summary(trans_table))
    appearance = list(rhs = c(consequent), default="lhs")
    parameter = list(support = minsupport, confidence = minconfidence, target = "rules")
    control = list(verbose = FALSE)
    apargs = list(data=trans_table, parameter=parameter, control=control)
    if (!is.null(consequent)) {
        apargs["appearance"] = list(appearance)
    }
    spsspkg.StartProcedure("SPSSINC APRIORI", "SPSSINCAPRIORI")
    if (itemplot) {
        itemFrequencyPlot(trans_table, support=minsupport, cex.names=.7, main=gtxt("Item Proportions"))
    }

    rules = tryCatch(do.call(apriori, apargs), 
        error=function(e) {
            spsspkg.EndProcedure()
            stop(e$message, call.=FALSE)
        }
    )
    subset_rules <- subset(rules, subset = lift >= minlift)

    if (showsummary) {
        qq = quality(rules)
        names(qq) = list(gtxt("Support"), gtxt("Confidence"), gtxt("Lift"))
        allrules = t(data.frame(Minimum=sapply(qq,min), Mean=sapply(qq, mean), Median=sapply(qq,median), Maximum=sapply(qq,max)))
        spsspivottable.Display(allrules, title=gtxtf("Quality of Entire Set of %s Rules", nrow(qq)))
        qq = quality(subset_rules)
        names(qq) = list(gtxt("Support"), gtxt("Confidence"), gtxt("Lift"))
        subsetrules = t(data.frame(Minimum=sapply(qq,min), Mean=sapply(qq, mean), Median=sapply(qq,median), Maximum=sapply(qq,max)))
        spsspivottable.Display(subsetrules, title=gtxtf("Quality of Selected Subset of %s Rules Meeting Minimum Lift", nrow(qq)))
    }
    rmaxrules <- maxrules
    if (length(subset_rules)<=rmaxrules){
        rmaxrules <- length(subset_rules)
    }
    data_rules <- do.call(as, list(eval(sortorder)[1:rmaxrules], "data.frame"))

    spsspivottable.Display(data_rules, title=gtxt("Apriori Rules"), 
        caption= ifelse(!is.null(consequent), gtxtf("a. Consequent: %s", consequent), ""))
    spsspkg.EndProcedure()
}

# localization initialization
setuplocalization = function() {
    # enable localization		
    domain <- "SPSSINC_APRIORI"
    fpath = Find(file.exists, file.path(.libPaths(), paste(domain, ".R", sep="")))
    bindtextdomain(domain, file.path(dirname(fpath), domain, "lang"))
}

helper = function(cmdname) {
    # find the html help file and display in the default browser
    # cmdname may have blanks that need to be converted to _ to match the file
    
    fn = gsub(" ", "_", cmdname, fixed=TRUE)
    thefile = Find(file.exists, file.path(.libPaths(), fn, "markdown.html"))
    if (is.null(thefile)) {
        print("Help file not found")
    } else {
        browseURL(paste("file://", thefile, sep=""))
    }
}
if (exists("spsspkg.helper")) {
assign("helper", spsspkg.helper)
}