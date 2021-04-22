#' Generate small area estimates using multilevel regression and post-stratification
#' 
#' @param f Model formula. Must not include constituency identifier
#' @param surv Data frame containing survey data.
#' @param ps Data frame containing post-stratification variables
#' @param aux Data frame containing constituency variables
#' @param const Character vector giving the name of the variable used as the constituency identifier
#' @param type Either "binary" for a binary response or "continuous" for a Gaussian response.
#' @param weight.var Character vector giving the name of the variable used as the post-stratification weight.
#' @param ... Additional arguments passed to rstanarm
#' @return A list containing a summary of constituency figures (`constsmry`), the rstanarm object (`fit`), mean squared error (`mse`) and the type (`type`)
#'
#' @importFrom magrittr "%>%"
#' @import stats
#' 
#' @export
#' 
#' @examples
#' \dontrun{
#' data("bes")
#' data("psw")
#' data("results15")
#' 
#' bes$votedLeave <- as.numeric(bes$euRefVote == "Leave the EU")
#' bes$GSSCode <- bes$onscode
#' bes$married <- bes$marital
#' results15$GSSCode <- results15$ONSConstID
#' results15$Con15[is.na(results15$Con15)] <- 0
#' res <- mrp(votedLeave ~ sex + agegroup + education + 
#' married + housing | Con15 + UKIP15,
#' surv = bes,
#' ps = psw,
#' aux = results15,
#' const = "GSSCode",
#' iter = 800,
#' chains = 2,
#' adapt.delta = 0.95,
#' weight.var = "weight")
#' }
#' 
mrp <- function(f,
                surv,
                ps,
                aux,
                const = "const",
                type = "binary",
                weight.var = "w8",
                ...) {
    ## (1) Check formula and type
    f <- check_formula(f)
    if (!is.element(type, c("binary", "continuous"))) {
        stop("`type` must be either `binary` or `continuous`")
    }
    
    ## (2) Check all inputs are of the right type
    if (!inherits(surv, "data.frame")) {
        stop("object surv must be a data frame")
    }
    if (!inherits(ps, "data.frame")) {
        stop("object ps must be a data frame")
    }
    if (!inherits(aux, "data.frame")) {
        stop("object ps must be a data frame")
    }
    ## (2b) coerce tibbles
    class(surv) <- "data.frame"
    class(ps) <- "data.frame"
    class(aux) <- "data.frame"
    
    ## (3) Check that all data frames have the const variable
    if (!is.element(const, names(ps))) {
        stop("Constituency identifier not in post-stratification data frame")
    }
    if (!is.element(const, names(surv))) {
        stop("Constituency identifier not in survey data frame")
    }
    if (!is.element(const, names(aux))) {
        stop("Constituency identifier not in auxiliary data frame")
    }

    ## (4) Check factor levels of constituency variable are consistent
    ## First, coerce to factor if it's not already
    if (is.factor(ps[, const])) {
        ## Do nothing
    } else {
        ps[, const] <- factor(ps[, const])
    }

    ## Coerce surv frame const var to that factor using level set
    surv <- check_constids(surv, levels(ps[, const]), const)
    aux <- check_constids(aux, levels(ps[, const]), const)

    ## (5) Check the weight variable exists in poststrat. frame
    if (!is.element(weight.var, names(ps))) {
        stop(paste0("The post-stratification frame ",
                    "lacks the weighting variable ",
                    weight.var))
    }

    ## (6) Check auxiliary data
    aux <- check_aux(aux, f,
                     const,
                     levels(ps[, const]))
    
    ## (7) Standardize auxiliary data
    numeric_vars <- names(which(sapply(aux, is.numeric)))
    for (v in numeric_vars) {
      aux[,v] <- (aux[,v] - mean(aux[,v], na.rm = TRUE)) / 
        stats::sd(aux[,v], na.rm = TRUE)
    }

    ## (8) check survey data
    surv <- check_surv(surv, f, const)

    ## (9) check poststrat data
    ps <- check_ps(ps, f, const, weight.var)
    
    ## (10) Check survey and poststrat data have same factor levels
    for (v in setdiff(names(ps), weight.var)) {
        ## Coerce PS to factor
        if (is.factor(ps[, v])) {
            ## do nothing
        } else {
            ps[, v] <- factor(ps[, v])
        }
        ## Check against surv data
        tmp <- factor(as.character(surv[, v]),
                      levels = levels(ps[, v]))
        if (any(is.na(tmp) & !is.na(surv[, v]))) {
            errmsg <- paste0("Variable ",
                             v,
                             " had levels not found in ",
                             "the post-stratification data")
            stop(errmsg)
        } else {
            surv[, v] <- tmp
        }
    }

    ## (12) Start merging
    old <- nrow(surv)
    surv <- merge(surv, aux,
                  by = const,
                  all.x = TRUE,
                  all.y = FALSE)
    stopifnot(nrow(surv) == old)

    old <- nrow(ps)
    ps <- merge(ps, aux,
                by = const,
                all.x = TRUE,
                all.y = FALSE)
    stopifnot(nrow(ps) == old)

    stanf <- create_stanf(f, const)
    options(mc.cores = parallel::detectCores() - 1)
    if (type == "binary") {
        fit <- rstanarm::stan_glmer(stanf,
                                    family = stats::binomial(link = "logit"),
                                    data = surv,
                                    prior_intercept = rstanarm::student_t(5, 0, 10, autoscale = FALSE),
                                    prior = rstanarm::student_t(5, 0, 2.5, autoscale = FALSE),
                                    QR = TRUE,
                                    ...)
        
    } else {
        fit <- rstanarm::stan_glmer(stanf,
                                    family = stats::gaussian(),
                                    data = surv,
                                    prior_intercept = rstanarm::student_t(5, 0, 10, autoscale = TRUE),
                                    prior = rstanarm::student_t(5, 0, 2.5, autoscale = TRUE),
                                    QR = TRUE,
                                    ...)
    }

    ## Get the MSE
    mse <- rstanarm::predictive_error(fit, draws = 100) ^ 2
    ## Get the mean by draw
    mse <- rowMeans(mse)
    
    ## Remember to limit the number of draws
    ## If we don't do this, memory usage is huge
    iters <- fit$stanfit@stan_args[[1]]$iter
    draws <- rstanarm::posterior_linpred(fit,
                                         draws = ifelse(iters > 500,
                                                        500,
                                                        iters),
                                         transform = TRUE,
                                         newdata = ps)

    ## For all of ind variables
    ind_terms <- attr(terms(f, rhs = 1), "term.labels")
    ind_terms <- setdiff(ind_terms, const)
    ## iterate over
    holder <- list()
    for (v in ind_terms) {
        holder[[v]] <- intervals_of_interest(ps = ps,
                                             group_facs = v,
                                             cell_counts = weight.var,
                                             ps_reps = draws,
                                             probs = c(0.05, 0.25, 0.5, 0.75, 0.95)
        )
    }
    
    ## 
    ## Now do a special one for GSS Code
    const_intervals <-
        intervals_of_interest(
            ps = ps,
            group_facs = const,
            cell_counts = weight.var,
            ps_reps = draws,
            probs = c(0.05, 0.25, 0.5, 0.75, 0.95)
        )

    retval <- list(constsmry = const_intervals,
                   grpsmry = holder,
                   fit = fit,
                   type = type,
                   mse = mse,
                formula = f)
    attr(retval, "class") <- "mrp"
    retval
}

#' Print the results of an MRP fit
#' 
#' @param obj MRP fit generated by `mrp`
#' @param ... Additional arguments passed on
#' @return Screen output
#' 
#' @export
#' 
print.mrp <- function(obj, ...) {
    cat(paste0("MRP estimation of variable: ",
               as.character(obj$formula)[2],
               "\n\n"))
    print.f <- function(f) { 
        cat(paste(deparse(f,
                          width.cutoff = getOption("width")),
                  collapse = "\n")) 
    }
    cat("Model formula: \n")
    cat(print.f(obj$formula))
    cat("\n\nRespondents: ")
    cat(length(obj$fit$fitted.values))
    cat(". ")
    if (obj$type == "binary") {
        cat("Brier score (0=perfect; 1 = worst): ")
        cat(round(mean(obj$mse), 2))
    } else {
        cat("Mean squared error: ")
        cat(round(mean(obj$mse), 2))
    }

    cat("\n\n")
    ## Top five constituencies
    arr <- obj$constsmry$intervals %>%
        dplyr::arrange(dplyr::desc(mean))

    ## Get proper names
    if (survationmrp:::is_onscode(arr$group)) {
        arr$group <- survationmrp:::onscode_to_name(arr$group)
    }
    
    cat("Top five\n===\n\n")
    inflation <- ifelse(obj$type == "binary", 100, 1)
    for (i in 1:5) {
        cat(paste0(i, ") "))
        cat(arr$group[i])
        cat(": ")
        cat(round(inflation * arr$mean[i], 1))
        cat(" [")
        cat(round(inflation * arr[i, "5%"], 1))
        cat(", ")
        cat(round(inflation * arr[i, "95%"], 1))
        cat("]\n")
    }
    cat("\nBottom five\n===\n\n")
        for (i in (nrow(arr)-4):nrow(arr)) {
        cat(paste0(i, ") "))
        cat(arr$group[i])
        cat(": ")
        cat(round(inflation * arr$mean[i], 1))
        cat(" [")
        cat(round(inflation * arr[i, "5%"], 1))
        cat(", ")
        cat(round(inflation * arr[i, "95%"], 1))
        cat("]\n")
    }
}

#' @export
plot.mrp <- function(mrpobj, type = "coef") {
    if (type == "coef") {
        ## Pass for the moment
    } else if (type == "constituency") {
        plot.df <- data.frame(constituency = rownames(mrpobj$smry),
                              mean = mrpobj$smry[, "mean"],
                              lo = mrpobj$smry[, "2.5%"],
                              hi = mrpobj$smry[, "97.5%"])
        plot.df$lpos <- ifelse(plot.df$mean > stats::median(plot.df$mean),
                               plot.df$hi,
                               plot.df$lo)
        plot.df$hjust <- ifelse(plot.df$mean > stats::median(plot.df$mean),
                               0,
                               1)
        plot.df$constituency <- factor(plot.df$constituency)
        plot.df$constituency <- stats::reorder(plot.df$constituency,
                                               plot.df$mean)
        p <- ggplot2::ggplot(plot.df, ggplot2::aes(x = constituency,
                                          y = mean,
                                          ymin = lo,
                                          ymax = hi)) +
            ggplot2::geom_pointrange() +
            ggplot2::scale_x_discrete() +
            ggplot2::geom_text(ggplot2::aes(label = constituency,
                                   y = lpos,
                                   hjust = hjust)) +
            ggplot2::coord_flip() +
            ggplot2::theme(axis.text.y = ggplot2::element_blank(),
                           axis.ticks.y = ggplot2::element_blank())
       
    } else {
        stop("'type' should be one of 'constituency' or 'coef'")
    }
    p
}



most_common <- function(x) {
    retval <- names(sort(table(x, useNA = "no"),
                         decreasing=TRUE))[1]
    retval <- ifelse(is.null(retval),
                     NA_character_,
                     retval)
}

check_formula <- function(f) {
    ## Checks that formula is two-sided formula in right format
    ## Check inputs
    if (!inherits(f, "formula")) {
        stop("You've not supplied a valid formula")
    }
    f <- Formula::Formula(f)
    if (length(f)[1] > 1) {
        stop("Package does not support multivariate responses")
    }
    if (length(f)[2] < 2) {
        stop(paste0("Formula must be a two-part formula",
                    " where poststratifcation variables precede",
                    " district variables"))
    }
    ## Check whether same term appears in both sides of formula
    ind_terms <- attr(terms(f, rhs = 1), "term.labels")
    const_terms <- attr(terms(f, rhs = 2), "term.labels")
    if (any(grepl(":", ind_terms))) {
        stop(paste0("No interaction terms can feature ",
                    "amongst the poststratification variables. ",
                    "Create interaction variables manually ",
                    "in the survey and post-stratification ",
                    "data frames using interaction(a, b)"))
    }
    
    if (length(intersect(ind_terms, const_terms)) > 0) {
        stop("Terms must appear only in one part of the formula")
    }

    f
}

check_constids <- function(df, my_levels, const) {
    ## Checks constituency IDs present w/ right levels
    dfconst <- factor(as.character(df[, const]),
                      levels = my_levels)
    if (any(is.na(dfconst))) {
        errmsg <- paste0("There were constituency identifiers ",
                         "in the data which did not match ",
                         "constituency identifiers in the ",
                         "post-stratification data.")
        missing_consts <- as.character(df[, const])[is.na(dfconst)]
        missing_consts <- unique(missing_consts)
        errmsg <- paste0(errmsg,
                         " These were: ",
                         paste(missing_consts, collapse = "; "))
        stop(errmsg)
    }
    df[, const] <- dfconst
    df
}

check_aux <- function(df, f, const, constids) {
    ## Checks auxiliary data frame has all necessary variables
    ## no surplus variables
    ## no missingness

    ## Get the formula for the auxiliary side
    auxf <- formula(f, lhs = 0, rhs = 2)
    auxf <- as.character(auxf)[2]
    auxf <- paste0("~",
                   auxf,
                   "+",
                   const)
    auxf <- stats::formula(auxf)
    mf <- model.frame(auxf, data = df)
    
    ## Check all const IDs present
    mpos <- charmatch(constids, mf[, const])
    if (any(mpos == 0 | is.na(mpos))) {
        missing <- constids[which(mpos == 0 | is.na(mpos))]
        errmsg <- paste0("Auxiliary data frame ",
                         "lacks any data on ",
                         paste(missing, sep = ", "))
        stop(errmsg)
    }
    ## Check data is complete
    incompletes <- sapply(mf, function(x) any(is.na(x)))
    if (any(incompletes)) {
        errmsg <- paste0("Auxiliary data frame ",
                         "has missing data on ",
                         paste(names(mf)[incompletes], sep = ", "))
        stop(errmsg)
    }
    
    mf
}

check_surv <- function(df, f, const) {
    ## Get the formula for the individual side
    indf <- formula(f, lhs = 1, rhs = 1)
    indf <- as.character(indf)
    indf[3] <- paste0(indf[3], " + ", const)
    indf <- stats::as.formula(paste0(indf[2],
                                     " ~ ",
                                     indf[3]))
    mf <- model.frame(indf, data = df)
    mf
}


check_ps <- function(df, f, const, weight.var) {
    ## Get the formula for the individual side
    psf <- formula(f, lhs = 0, rhs = 1)
    psf <- as.character(psf)[2]
    psf <- paste0("~",
                   psf,
                   "+",
                  const,
                  " + ",
                  weight.var)
    psf <- stats::as.formula(psf)
    mf <- model.frame(psf, data = df)
    ## Go over the different factor variables
    ## Recode to most common as baseline
    for (v in setdiff(names(psf), c(const, weight.var))) {
        new_base <- most_common(psf[, v])
        psf[, v] <- stats::relevel(psf[, v],
                                   ref = new_base)
    }
    
    ## What about grouping_by?
    gvars <- attr(terms(psf), "term.labels")
    gvars <- setdiff(gvars, weight.var)
    ### gvars <- c(gvars, const)
    mf <- mf %>%
        dplyr::group_by_(.dots = gvars) %>%
        dplyr::summarize_all(sum)
    class(mf) <- "data.frame"
    mf
}

create_stanf <- function(f, const) {
    ## Take a two-part formula
    ## replace first part with random terms
    ## keep second part as if
    if (length(f)[2] < 2) {
        stop(paste0("Formula must be a two-part formula",
                    " where demographic variables precede",
                    " district variables"))
    }
    randintercepts <- paste0("(1|",
                 attr(terms(f, rhs = 1), "term.labels"),
                 ")",
                 collapse = " + ")
    randintercepts <- paste0(randintercepts,
                             " + (1|",
                             const,
                             ")")
    constfx <- paste0(attr(terms(f, rhs = 2), "term.labels"),
                      collapse = " + ")
    outf <- paste0(as.character(f)[2],
                   " ~ ",
                   randintercepts,
                   " + ",
                   constfx)
    outf
}

## Following function taken from https://github.com/rtrangucci/class_20170809/blob/master/helper_funs.R
intervals_of_interest <- function(ps, group_facs, cell_counts, ps_reps, probs) { 
  
    cell_N <- ps[, cell_counts] %>%
        data.matrix()
    ps$cell_N <- cell_N
    gpd_ps <- ps %>%
        dplyr::ungroup() %>%
        dplyr::group_by_(.dots = group_facs)
    gp_ind <- gpd_ps %>%
        dplyr::group_indices()
    gpd_ps$gp_ind <- gp_ind
    
    gp_sums <- dplyr::summarise_(gpd_ps,
                                 tot_pop = sum(cell_N),
                                 gp_ind = dplyr::first(gp_ind))
    gp_nms <- dplyr::select_(gp_sums, .dots = group_facs)
    gp_nms <- apply(gp_nms, 1, function(x) paste(x, collapse = "_"))
  
    gp_sums$lo <- NA_real_
    gp_sums$med <- NA_real_
    gp_sums$hi <- NA_real_
    gp_num <- sort(unique(gp_ind))
    gp_map <- data.frame(nm = gp_nms,
                         num = gp_num,
                         stringsAsFactors = F)
    n_gps <- nrow(gp_map)
    ps_reps <- sweep(x = ps_reps,
                     MARGIN = 2,
                     STATS = ps$cell_N,
                     FUN = "*")
  
    intervals <- as.data.frame(matrix(NA, nrow = n_gps, ncol = length(probs) + 1))
    dists <- list()
    for (gp_i in seq_along(gp_num)) {
        gp <- gp_map[gp_i,]
        gp_n <- gp$num
        gp_nm <- gp$nm
        sel <- which(gpd_ps$gp_ind == gp_n)
        weight_tot <- sum(ps$cell_N[sel])
        sub_vote <- ps_reps[, sel]
        vote_vec <- rowSums(sub_vote)/weight_tot
        intervals[gp_n,] <- round(c(stats::quantile(vote_vec, probs = probs),
                                    mean(vote_vec)),3)
        dists[[gp_nm]] <- vote_vec
    }
    colnames(intervals) <- c(paste0(round(100*probs), '%'), "mean")
    intervals$group <- gp_map$nm
    return(list(intervals = intervals,
                dists = dists))
}

is_onscode <- function(code) {
    code <- as.character(code)
    ## check it matches
    if (any(nchar(code) != 9)) {
        return(FALSE)
    }
    
    initial <- substr(code, 0, 1)
    remainder <- substr(code, 2, 9)

    if (any(!is.element(initial, c("E", "S", "W", "N")))) {
        return(FALSE)
    }
    if (any(grepl("[^0-9]", remainder))) {
        return(FALSE)
    }
    return(TRUE)
}

   
onscode_to_name <- function(code) {
    lu <- structure(list(PCON16CD = c("E14000530", "E14000531", "E14000532",
                                      "E14000533", "E14000534", "E14000535", "E14000536", "E14000537",
                                      "E14000538", "E14000539", "E14000540", "E14000541", "E14000542",
                                      "E14000543", "E14000544", "E14000545", "E14000546", "E14000547",
                                      "E14000548", "E14000549", "E14000550", "E14000551", "E14000552",
                                      "E14000553", "E14000554", "E14000555", "E14000556", "E14000557",
                                      "E14000558", "E14000559", "E14000560", "E14000561", "E14000562",
                                      "E14000563", "E14000564", "E14000565", "E14000566", "E14000567",
                                      "E14000568", "E14000569", "E14000570", "E14000571", "E14000572",
                                      "E14000573", "E14000574", "E14000575", "E14000576", "E14000577",
                                      "E14000578", "E14000579", "E14000730", "E14000731", "E14000732",
                                      "E14000733", "E14000734", "E14000735", "E14000736", "E14000737",
                                      "E14000738", "E14000739", "E14000740", "E14000741", "E14000742",
                                      "E14000743", "E14000744", "E14000745", "E14000746", "E14000747",
                                      "E14000748", "E14000749", "E14000750", "E14000751", "E14000752",
                                      "E14000753", "E14000754", "E14000755", "E14000756", "E14000757",
                                      "E14000758", "E14000759", "E14000760", "E14000761", "E14000762",
                                      "E14000763", "E14000764", "E14000765", "E14000766", "E14000767",
                                      "E14000768", "E14000769", "E14000770", "E14000771", "E14000772",
                                      "E14000773", "E14000774", "E14000775", "E14000776", "E14000777",
                                      "E14000778", "E14000780", "E14000779", "E14000580", "E14000630",
                                      "E14000631", "E14000581", "E14000632", "E14000781", "E14000582",
                                      "E14000633", "E14000782", "E14000583", "E14000634", "E14000783",
                                      "E14000584", "E14000635", "E14000784", "E14000585", "E14000636",
                                      "E14000785", "E14000586", "E14000637", "E14000786", "E14000587",
                                      "E14000638", "E14000787", "E14000588", "E14000639", "E14000788",
                                      "E14000589", "E14000640", "E14000789", "E14000590", "E14000641",
                                      "E14000790", "E14000591", "E14000642", "E14000791", "E14000592",
                                      "E14000643", "E14000792", "E14000593", "E14000644", "E14000793",
                                      "E14000594", "E14000645", "E14000794", "E14000595", "E14000646",
                                      "E14000795", "E14000596", "E14000647", "E14000796", "E14000597",
                                      "E14000648", "E14000797", "E14000598", "E14000649", "E14000798",
                                      "E14000599", "E14000650", "E14000799", "E14000600", "E14000651",
                                      "E14000800", "E14000601", "E14000652", "E14000801", "E14000602",
                                      "E14000653", "E14000802", "E14000603", "E14000654", "E14000803",
                                      "E14000604", "E14000655", "E14000804", "E14000605", "E14000656",
                                      "E14000805", "E14000606", "E14000657", "E14000806", "E14000607",
                                      "E14000658", "E14000807", "E14000608", "E14000659", "E14000808",
                                      "E14000609", "E14000660", "E14000809", "E14000610", "E14000661",
                                      "E14000810", "E14000611", "E14000662", "E14000811", "E14000612",
                                      "E14000663", "E14000812", "E14000613", "E14000664", "E14000813",
                                      "E14000614", "E14000615", "E14000665", "E14000666", "E14000667",
                                      "E14000668", "E14000616", "E14000669", "E14000814", "E14000617",
                                      "E14000815", "E14000670", "E14000618", "E14000816", "E14000671",
                                      "E14000619", "E14000817", "E14000672", "E14000620", "E14000818",
                                      "E14000673", "E14000621", "E14000819", "E14000674", "E14000622",
                                      "E14000820", "E14000675", "E14000623", "E14000821", "E14000676",
                                      "E14000624", "E14000822", "E14000677", "E14000625", "E14000823",
                                      "E14000678", "E14000626", "E14000824", "E14000679", "E14000627",
                                      "E14000825", "E14000628", "E14000826", "E14000629", "E14000827",
                                      "E14000828", "E14000829", "E14000880", "E14000881", "E14000882",
                                      "E14000883", "E14000884", "E14000885", "E14000886", "E14000887",
                                      "E14000888", "E14000889", "E14000890", "E14000891", "E14000892",
                                      "E14000893", "E14000894", "E14000895", "E14000896", "E14000897",
                                      "E14000898", "E14000899", "E14000900", "E14000901", "E14000902",
                                      "E14000903", "E14000904", "E14000905", "E14000906", "E14000907",
                                      "E14000908", "E14000909", "E14000910", "E14000911", "E14000912",
                                      "E14000913", "E14000914", "E14000915", "E14000916", "E14000917",
                                      "E14000918", "E14000919", "E14000920", "E14000921", "E14000922",
                                      "E14000923", "E14000924", "E14000925", "E14000926", "E14000927",
                                      "E14000928", "E14000929", "E14000680", "E14000830", "E14000831",
                                      "E14000832", "E14000833", "E14000834", "E14000835", "E14000836",
                                      "E14000837", "E14000838", "E14000839", "E14000840", "E14000841",
                                      "E14000842", "E14000843", "E14000844", "E14000845", "E14000846",
                                      "E14000847", "E14000848", "E14000849", "E14000850", "E14000851",
                                      "E14000852", "E14000853", "E14000854", "E14000855", "E14000856",
                                      "E14000857", "E14000858", "E14000859", "E14000860", "E14000861",
                                      "E14000862", "E14000863", "E14000864", "E14000865", "E14000866",
                                      "E14000867", "E14000868", "E14000869", "E14000870", "E14000871",
                                      "E14000872", "E14000873", "E14000874", "E14000875", "E14000876",
                                      "E14000877", "E14000878", "E14000879", "E14000681", "E14000682",
                                      "E14000683", "E14000684", "E14000685", "E14000686", "E14000687",
                                      "E14000688", "E14000689", "E14000690", "E14000691", "E14000692",
                                      "E14000693", "E14000694", "E14000695", "E14000696", "E14000697",
                                      "E14000698", "E14000699", "E14000700", "E14000701", "E14000702",
                                      "E14000703", "E14000704", "E14000705", "E14000706", "E14000707",
                                      "E14000708", "E14000709", "E14000710", "E14000711", "E14000712",
                                      "E14000713", "E14000714", "E14000715", "E14000716", "E14000717",
                                      "E14000718", "E14000719", "E14000720", "E14000721", "E14000722",
                                      "E14000723", "E14000724", "E14000725", "E14000726", "E14000727",
                                      "E14000728", "E14000729", "E14001030", "S14000050", "E14001031",
                                      "E14001032", "E14001033", "E14001034", "S14000051", "E14001035",
                                      "E14000980", "N06000018", "S14000052", "E14001036", "E14000981",
                                      "S14000001", "S14000053", "E14001037", "E14000982", "S14000002",
                                      "S14000054", "E14001038", "E14000983", "S14000003", "S14000055",
                                      "E14001039", "E14000984", "S14000004", "S14000056", "E14001040",
                                      "E14000985", "S14000005", "S14000057", "E14001041", "E14000986",
                                      "S14000006", "S14000058", "E14001042", "E14000987", "S14000007",
                                      "S14000059", "E14001043", "E14000988", "S14000008", "W07000041",
                                      "E14001044", "E14000989", "S14000009", "W07000042", "E14001045",
                                      "E14000990", "S14000010", "W07000043", "E14001046", "E14000991",
                                      "S14000011", "W07000044", "E14001047", "E14000992", "S14000012",
                                      "W07000045", "E14001048", "E14000993", "S14000013", "W07000046",
                                      "E14001049", "E14000994", "S14000014", "W07000047", "E14001050",
                                      "E14000995", "S14000015", "W07000048", "E14001051", "E14000996",
                                      "S14000016", "W07000049", "E14001052", "E14000997", "S14000017",
                                      "W07000050", "E14001053", "E14000998", "S14000018", "W07000051",
                                      "E14001054", "E14000999", "S14000019", "W07000052", "E14001055",
                                      "E14001000", "S14000020", "W07000053", "E14001056", "E14001001",
                                      "S14000021", "W07000054", "E14001057", "E14001002", "W07000055",
                                      "S14000022", "W07000056", "E14001058", "E14001003", "W07000057",
                                      "E14001059", "S14000023", "E14001004", "W07000058", "E14001060",
                                      "S14000024", "E14001005", "W07000059", "E14001061", "S14000025",
                                      "E14001006", "W07000060", "E14001062", "S14000026", "E14001007",
                                      "W07000061", "N06000001", "S14000027", "E14001008", "W07000062",
                                      "N06000002", "S14000028", "E14001009", "W07000063", "N06000003",
                                      "S14000029", "E14001010", "W07000064", "N06000004", "S14000030",
                                      "E14001011", "W07000065", "N06000005", "S14000031", "E14001012",
                                      "W07000066", "N06000006", "S14000032", "E14001013", "W07000067",
                                      "N06000007", "S14000033", "E14001014", "W07000068", "N06000008",
                                      "S14000034", "E14001015", "W07000069", "N06000009", "S14000035",
                                      "E14001016", "W07000070", "N06000010", "S14000036", "E14001017",
                                      "W07000071", "N06000011", "S14000037", "E14001018", "W07000072",
                                      "N06000012", "S14000038", "E14001019", "W07000073", "N06000013",
                                      "S14000039", "E14001020", "W07000074", "N06000014", "S14000040",
                                      "E14001021", "W07000075", "N06000015", "S14000041", "E14001022",
                                      "W07000076", "N06000016", "S14000042", "E14001023", "W07000077",
                                      "N06000017", "S14000043", "E14001024", "W07000078", "S14000044",
                                      "E14001025", "W07000079", "S14000045", "E14001026", "W07000080",
                                      "S14000046", "E14001027", "S14000047", "E14001028", "E14001029",
                                      "E14000930", "S14000048", "S14000049", "E14000931", "E14000932",
                                      "E14000933", "E14000934", "E14000935", "E14000936", "E14000937",
                                      "E14000938", "E14000939", "E14000940", "E14000941", "E14000942",
                                      "E14000943", "E14000944", "E14000945", "E14000946", "E14000947",
                                      "E14000948", "E14000949", "E14000950", "E14000951", "E14000952",
                                      "E14000953", "E14000954", "E14000955", "E14000956", "E14000957",
                                      "E14000958", "E14000959", "E14000960", "E14000961", "E14000962",
                                      "E14000963", "E14000964", "E14000965", "E14000966", "E14000967",
                                      "E14000968", "E14000969", "E14000970", "E14000971", "E14000972",
                                      "E14000973", "E14000974", "E14000975", "E14000976", "E14000977",
                                      "E14000978", "E14000979"),
                         PCON16NM = c("Aldershot", "Aldridge-Brownhills", 
                                      "Altrincham and Sale West", "Amber Valley", "Arundel and South Downs", 
                                      "Ashfield", "Ashford", "Ashton-under-Lyne", "Aylesbury", "Banbury", 
                                      "Barking", "Barnsley Central", "Barnsley East", "Barrow and Furness", 
                                      "Basildon and Billericay", "Basingstoke", "Bassetlaw", "Bath", 
                                      "Batley and Spen", "Battersea", "Beaconsfield", "Beckenham", 
                                      "Bedford", "Bermondsey and Old Southwark", "Berwick-upon-Tweed", 
                                      "Bethnal Green and Bow", "Beverley and Holderness", "Bexhill and Battle", 
                                      "Bexleyheath and Crayford", "Birkenhead", "Birmingham, Edgbaston", 
                                      "Birmingham, Erdington", "Birmingham, Hall Green", "Birmingham, Hodge Hill", 
                                      "Birmingham, Ladywood", "Birmingham, Northfield", "Birmingham, Perry Barr", 
                                      "Birmingham, Selly Oak", "Birmingham, Yardley", "Bishop Auckland", 
                                      "Blackburn", "Blackley and Broughton", "Blackpool North and Cleveleys", 
                                      "Blackpool South", "Blaydon", "Blyth Valley", "Bognor Regis and Littlehampton", 
                                      "Bolsover", "Bolton North East", "Bolton South East", "Harrogate and Knaresborough", 
                                      "Harrow East", "Harrow West", "Hartlepool", "Harwich and North Essex", 
                                      "Hastings and Rye", "Havant", "Hayes and Harlington", "Hazel Grove", 
                                      "Hemel Hempstead", "Hemsworth", "Hendon", "Henley", "Hereford and South Herefordshire", 
                                      "Hertford and Stortford", "Hertsmere", "Hexham", "Heywood and Middleton", 
                                      "High Peak", "Hitchin and Harpenden", "Holborn and St Pancras", 
                                      "Hornchurch and Upminster", "Hornsey and Wood Green", "Horsham", 
                                      "Houghton and Sunderland South", "Hove", "Huddersfield", "Huntingdon", 
                                      "Hyndburn", "Ilford North", "Ilford South", "Ipswich", "Isle of Wight", 
                                      "Islington North", "Islington South and Finsbury", "Jarrow", 
                                      "Keighley", "Kenilworth and Southam", "Kensington", "Kettering", 
                                      "Kingston and Surbiton", "Kingston upon Hull East", "Kingston upon Hull North", 
                                      "Kingston upon Hull West and Hessle", "Kingswood", "Knowsley", 
                                      "Lancaster and Fleetwood", "Leeds Central", "Leeds East", "Leeds North West", 
                                      "Leeds North East", "Bolton West", "Cheltenham", "Chesham and Amersham", 
                                      "Bootle", "Chesterfield", "Leeds West", "Boston and Skegness", 
                                      "Chichester", "Leicester East", "Bosworth", "Chingford and Woodford Green", 
                                      "Leicester South", "Bournemouth East", "Chippenham", "Leicester West", 
                                      "Bournemouth West", "Chipping Barnet", "Leigh", "Bracknell", 
                                      "Chorley", "Lewes", "Bradford East", "Christchurch", "Lewisham East", 
                                      "Bradford South", "Cities of London and Westminster", "Lewisham West and Penge", 
                                      "Bradford West", "City of Chester", "Lewisham, Deptford", "Braintree", 
                                      "City of Durham", "Leyton and Wanstead", "Brent Central", "Clacton", 
                                      "Lichfield", "Brent North", "Cleethorpes", "Lincoln", "Brentford and Isleworth", 
                                      "Colchester", "Liverpool, Riverside", "Brentwood and Ongar", 
                                      "Colne Valley", "Liverpool, Walton", "Bridgwater and West Somerset", 
                                      "Congleton", "Liverpool, Wavertree", "Brigg and Goole", "Copeland", 
                                      "Liverpool, West Derby", "Brighton, Kemptown", "Corby", "Loughborough", 
                                      "Brighton, Pavilion", "Coventry North East", "Louth and Horncastle", 
                                      "Bristol East", "Coventry North West", "Ludlow", "Bristol North West", 
                                      "Coventry South", "Luton North", "Bristol South", "Crawley", 
                                      "Luton South", "Bristol West", "Crewe and Nantwich", "Macclesfield", 
                                      "Broadland", "Croydon Central", "Maidenhead", "Bromley and Chislehurst", 
                                      "Croydon North", "Maidstone and The Weald", "Bromsgrove", "Croydon South", 
                                      "Makerfield", "Broxbourne", "Dagenham and Rainham", "Maldon", 
                                      "Broxtowe", "Darlington", "Manchester Central", "Buckingham", 
                                      "Dartford", "Manchester, Gorton", "Burnley", "Daventry", "Manchester, Withington", 
                                      "Burton", "Denton and Reddish", "Mansfield", "Bury North", "Derby North", 
                                      "Meon Valley", "Bury South", "Derby South", "Meriden", "Bury St Edmunds", 
                                      "Derbyshire Dales", "Mid Bedfordshire", "Calder Valley", "Camberwell and Peckham", 
                                      "Devizes", "Dewsbury", "Don Valley", "Doncaster Central", "Camborne and Redruth", 
                                      "Doncaster North", "Mid Derbyshire", "Cambridge", "Mid Dorset and North Poole", 
                                      "Dover", "Cannock Chase", "Mid Norfolk", "Dudley North", "Canterbury", 
                                      "Mid Sussex", "Dudley South", "Carlisle", "Mid Worcestershire", 
                                      "Dulwich and West Norwood", "Carshalton and Wallington", "Middlesbrough", 
                                      "Ealing Central and Acton", "Castle Point", "Middlesbrough South and East Cleveland", 
                                      "Ealing North", "Central Devon", "Milton Keynes North", "Ealing, Southall", 
                                      "Central Suffolk and North Ipswich", "Milton Keynes South", "Easington", 
                                      "Charnwood", "Mitcham and Morden", "East Devon", "Chatham and Aylesford", 
                                      "Mole Valley", "East Ham", "Cheadle", "Morecambe and Lunesdale", 
                                      "Chelmsford", "Morley and Outwood", "Chelsea and Fulham", "New Forest East", 
                                      "New Forest West", "Newark", "Plymouth, Sutton and Devonport", 
                                      "Poole", "Poplar and Limehouse", "Portsmouth North", "Portsmouth South", 
                                      "Preston", "Pudsey", "Putney", "Rayleigh and Wickford", "Reading East", 
                                      "Reading West", "Redcar", "Redditch", "Reigate", "Ribble Valley", 
                                      "Richmond (Yorks)", "Richmond Park", "Rochdale", "Rochester and Strood", 
                                      "Rochford and Southend East", "Romford", "Romsey and Southampton North", 
                                      "Rossendale and Darwen", "Rother Valley", "Rotherham", "Rugby", 
                                      "Ruislip, Northwood and Pinner", "Runnymede and Weybridge", "Rushcliffe", 
                                      "Rutland and Melton", "Saffron Walden", "Salford and Eccles", 
                                      "Salisbury", "Scarborough and Whitby", "Scunthorpe", "Sedgefield", 
                                      "Sefton Central", "Selby and Ainsty", "Sevenoaks", "Sheffield Central", 
                                      "Sheffield South East", "Sheffield, Brightside and Hillsborough", 
                                      "Sheffield, Hallam", "Sheffield, Heeley", "Sherwood", "Shipley", 
                                      "Shrewsbury and Atcham", "Sittingbourne and Sheppey", "Skipton and Ripon", 
                                      "Sleaford and North Hykeham", "East Hampshire", "Newbury", "Newcastle upon Tyne Central", 
                                      "Newcastle upon Tyne East", "Newcastle upon Tyne North", "Newcastle-under-Lyme", 
                                      "Newton Abbot", "Normanton, Pontefract and Castleford", "North Cornwall", 
                                      "North Devon", "North Dorset", "North Durham", "North East Bedfordshire", 
                                      "North East Cambridgeshire", "North East Derbyshire", "North East Hampshire", 
                                      "North East Hertfordshire", "North East Somerset", "North Herefordshire", 
                                      "North Norfolk", "North Shropshire", "North Somerset", "North Swindon", 
                                      "North Thanet", "North Tyneside", "North Warwickshire", "North West Cambridgeshire", 
                                      "North West Durham", "North West Hampshire", "North West Leicestershire", 
                                      "North West Norfolk", "North Wiltshire", "Northampton North", 
                                      "Northampton South", "Norwich North", "Norwich South", "Nottingham East", 
                                      "Nottingham North", "Nottingham South", "Nuneaton", "Old Bexley and Sidcup", 
                                      "Oldham East and Saddleworth", "Oldham West and Royton", "Orpington", 
                                      "Oxford East", "Oxford West and Abingdon", "Pendle", "Penistone and Stocksbridge", 
                                      "Penrith and The Border", "Peterborough", "Plymouth, Moor View", 
                                      "East Surrey", "East Worthing and Shoreham", "East Yorkshire", 
                                      "Eastbourne", "Eastleigh", "Eddisbury", "Edmonton", "Ellesmere Port and Neston", 
                                      "Elmet and Rothwell", "Eltham", "Enfield North", "Enfield, Southgate", 
                                      "Epping Forest", "Epsom and Ewell", "Erewash", "Erith and Thamesmead", 
                                      "Esher and Walton", "Exeter", "Fareham", "Faversham and Mid Kent", 
                                      "Feltham and Heston", "Filton and Bradley Stoke", "Finchley and Golders Green", 
                                      "Folkestone and Hythe", "Forest of Dean", "Fylde", "Gainsborough", 
                                      "Garston and Halewood", "Gateshead", "Gedling", "Gillingham and Rainham", 
                                      "Gloucester", "Gosport", "Grantham and Stamford", "Gravesham", 
                                      "Great Grimsby", "Great Yarmouth", "Greenwich and Woolwich", 
                                      "Guildford", "Hackney North and Stoke Newington", "Hackney South and Shoreditch", 
                                      "Halesowen and Rowley Regis", "Halifax", "Haltemprice and Howden", 
                                      "Halton", "Hammersmith", "Hampstead and Kilburn", "Harborough", 
                                      "Harlow", "West Bromwich West", "Ochil and South Perthshire", 
                                      "West Dorset", "West Ham", "West Lancashire", "West Suffolk", 
                                      "Orkney and Shetland", "West Worcestershire", "Stroud", "West Tyrone", 
                                      "Paisley and Renfrewshire North", "Westminster North", "Suffolk Coastal", 
                                      "Aberdeen North", "Paisley and Renfrewshire South", "Westmorland and Lonsdale", 
                                      "Sunderland Central", "Aberdeen South", "Perth and North Perthshire", 
                                      "Weston-Super-Mare", "Surrey Heath", "Airdrie and Shotts", "Ross, Skye and Lochaber", 
                                      "Wigan", "Sutton and Cheam", "Angus", "Rutherglen and Hamilton West", 
                                      "Wimbledon", "Sutton Coldfield", "Argyll and Bute", "Stirling", 
                                      "Winchester", "Tamworth", "Ayr, Carrick and Cumnock", "West Aberdeenshire and Kincardine", 
                                      "Windsor", "Tatton", "Banff and Buchan", "West Dunbartonshire", 
                                      "Wirral South", "Taunton Deane", "Berwickshire, Roxburgh and Selkirk", 
                                      "Ynys Môn", "Wirral West", "Telford", "Caithness, Sutherland and Easter Ross", 
                                      "Delyn", "Witham", "Tewkesbury", "Central Ayrshire", "Alyn and Deeside", 
                                      "Witney", "The Cotswolds", "Coatbridge, Chryston and Bellshill", 
                                      "Wrexham", "Woking", "The Wrekin", "Cumbernauld, Kilsyth and Kirkintilloch East", 
                                      "Llanelli", "Wokingham", "Thirsk and Malton", "Dumfries and Galloway", 
                                      "Gower", "Wolverhampton North East", "Thornbury and Yate", "Dumfriesshire, Clydesdale and Tweeddale", 
                                      "Swansea West", "Wolverhampton South East", "Thurrock", "Dundee East", 
                                      "Swansea East", "Wolverhampton South West", "Tiverton and Honiton", 
                                      "Dundee West", "Aberavon", "Worcester", "Tonbridge and Malling", 
                                      "Dunfermline and West Fife", "Cardiff Central", "Workington", 
                                      "Tooting", "East Dunbartonshire", "Cardiff North", "Worsley and Eccles South", 
                                      "Torbay", "East Kilbride, Strathaven and Lesmahagow", "Rhondda", 
                                      "Worthing West", "Torridge and West Devon", "East Lothian", "Torfaen", 
                                      "Wycombe", "Totnes", "East Renfrewshire", "Monmouth", "Wyre and Preston North", 
                                      "Tottenham", "Newport East", "Edinburgh East", "Newport West", 
                                      "Wyre Forest", "Truro and Falmouth", "Arfon", "Wythenshawe and Sale East", 
                                      "Edinburgh North and Leith", "Tunbridge Wells", "Aberconwy", 
                                      "Yeovil", "Edinburgh South", "Twickenham", "Clwyd West", "York Central", 
                                      "Edinburgh South West", "Tynemouth", "Vale of Clwyd", "York Outer", 
                                      "Edinburgh West", "Uxbridge and South Ruislip", "Dwyfor Meirionnydd", 
                                      "Belfast East", "Na h-Eileanan an Iar", "Vauxhall", "Clwyd South", 
                                      "Belfast North", "Falkirk", "Wakefield", "Montgomeryshire", "Belfast South", 
                                      "Glasgow Central", "Wallasey", "Ceredigion", "Belfast West", 
                                      "Glasgow East", "Walsall North", "Preseli Pembrokeshire", "East Antrim", 
                                      "Glasgow North", "Walsall South", "Carmarthen West and South Pembrokeshire", 
                                      "East Londonderry", "Glasgow North East", "Walthamstow", "Carmarthen East and Dinefwr", 
                                      "Fermanagh and South Tyrone", "Glasgow North West", "Wansbeck", 
                                      "Brecon and Radnorshire", "Foyle", "Glasgow South", "Wantage", 
                                      "Neath", "Lagan Valley", "Glasgow South West", "Warley", "Cynon Valley", 
                                      "Mid Ulster", "Glenrothes", "Warrington North", "Merthyr Tydfil and Rhymney", 
                                      "Newry and Armagh", "Gordon", "Warrington South", "Blaenau Gwent", 
                                      "North Antrim", "Inverclyde", "Warwick and Leamington", "Bridgend", 
                                      "North Down", "Inverness, Nairn, Badenoch and Strathspey", "Washington and Sunderland West", 
                                      "Ogmore", "South Antrim", "Kilmarnock and Loudoun", "Watford", 
                                      "Pontypridd", "South Down", "Kirkcaldy and Cowdenbeath", "Waveney", 
                                      "Caerphilly", "Strangford", "Lanark and Hamilton East", "Wealden", 
                                      "Islwyn", "Upper Bann", "Linlithgow and East Falkirk", "Weaver Vale", 
                                      "Vale of Glamorgan", "Livingston", "Wellingborough", "Cardiff West", 
                                      "Midlothian", "Wells", "Cardiff South and Penarth", "Moray", 
                                      "Welwyn Hatfield", "Motherwell and Wishaw", "Wentworth and Dearne", 
                                      "West Bromwich East", "Slough", "North Ayrshire and Arran", "North East Fife", 
                                      "Solihull", "Somerton and Frome", "South Basildon and East Thurrock", 
                                      "South Cambridgeshire", "South Derbyshire", "South Dorset", "South East Cambridgeshire", 
                                      "South East Cornwall", "South Holland and The Deepings", "South Leicestershire", 
                                      "South Norfolk", "South Northamptonshire", "South Ribble", "South Shields", 
                                      "South Staffordshire", "South Suffolk", "South Swindon", "South Thanet", 
                                      "South West Bedfordshire", "South West Devon", "South West Hertfordshire", 
                                      "South West Norfolk", "South West Surrey", "South West Wiltshire", 
                                      "Southampton, Itchen", "Southampton, Test", "Southend West", 
                                      "Southport", "Spelthorne", "St Albans", "St Austell and Newquay", 
                                      "St Helens North", "St Helens South and Whiston", "St Ives", 
                                      "Stafford", "Staffordshire Moorlands", "Stalybridge and Hyde", 
                                      "Stevenage", "Stockport", "Stockton North", "Stockton South", 
                                      "Stoke-on-Trent Central", "Stoke-on-Trent North", "Stoke-on-Trent South", 
                                      "Stone", "Stourbridge", "Stratford-on-Avon", "Streatham", "Stretford and Urmston"
                                      )),
                    .Names = c("PCON16CD", "PCON16NM"),
                    row.names = c(NA, -650L),
                    class = "data.frame")
    pos <- charmatch(code, lu$PCON16CD)
    return(lu$PCON16NM[pos])
}
