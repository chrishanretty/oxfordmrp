library(rio)
library(tidyverse)
library(anesrake)
library(doParallel)

### S16000143
### Read in both the joint and targets
joint <- readRDS("working/joint.rds") %>%
    as.data.frame()
tgt <- readRDS("working/targets.rds")

cl <- makeCluster(4)
registerDoParallel(cl)

descr <- function(df, inputter) {
    holder <- list()
    for (i in names(inputter)) {
        what_i_got <- aggregate(df$weights, list(df[,i]), sum) %>%
            mutate(prop = x / sum(x)) %>%
            pull(prop)
        what_was_input <- inputter[[i]]
        holder[[i]] <- cbind(what_i_got, what_was_input)
    }
    holder
}

rake_to_const <- function(joint, targets) {
    ## results <- foreach(const = unique(targets$geogcode),
    ##                    .packages = c("tidyverse", "anesrake")) %dopar% {
    results <- list()
    for (const in unique(targets$geogcode)) {
        print(const)
        ## Set up the target object
        tgt <- subset(targets, geogcode == const)
        new_joint <- joint
        ## Check all the variables present in the targets
        ## are present in the joint distrib.
        thevars <- unique(tgt$variable)
        if (!all(thevars %in% names(new_joint))) {
            stop("Joint distribution lacks some variables")
        }

        ## Remove values which have zero props in the target
        ## in both the joint and the target
        for (v in thevars) {
            tmp <- subset(tgt, variable == v)
            zerovals <- subset(tmp, prop <= 0)
            killcat <- zerovals$category
            kill <- which(new_joint[, v] %in% killcat)
            if (length(kill) > 0) {
                new_joint <- new_joint[-kill, ]
            }
        }
        tgt <- subset(tgt, prop > 0)
        
        ## Ensure the factor levels are the same
        for (v in thevars) {
            my_levels <- tgt$category[which(tgt$variable == v)]
            tryval <- factor(as.character(new_joint[, v]),
                             levels = my_levels)
            if (any(is.na(tryval) & !is.na(new_joint[, v]))) {
                stop(paste0("Problem coercing factor levels ",
                            "for variable ", v))
            }
            
            new_joint[, v] <- tryval
        }
        ## Create a list of targets for anesrake
        inputter <- sapply(thevars, function(x) {

            tmp <- subset(tgt, variable == x)
            tmp <- subset(tmp, prop > 0)
            retval <- tmp$prop
            names(retval) <- tmp$category
            retval
        })

        ## Create caseid
        new_joint$caseid <- 1:nrow(new_joint)
        
        rake_solution <- try(anesrake(inputter = inputter,
                                  dataframe = new_joint,
                                  caseid = new_joint$caseid,
                                  weightvec = new_joint$w8,
                                  pctlim = 0.01,
                                  cap = 999,
                                  force1 = TRUE))

        if (inherits(class(rake_solution), "try-error")) {
            warning(const)
            caseweights <- data.frame(cases = unique(new_joint$caseid),
                                      weights = 1)
        } else { 
            caseweights <- data.frame(cases = rake_solution$caseid,
                                      weights = rake_solution$weightvec)
        }
        
        new_joint <- merge(new_joint, caseweights,
                       by.x = "caseid",
                       by.y = "cases",
                       all.x = TRUE,
                       all.y = FALSE)
        
        final <- new_joint %>%
            dplyr::group_by_at(vars(one_of(thevars))) %>%
            dplyr::summarize(w8 = sum(weights), .groups = "drop") %>%
            dplyr::ungroup() %>%
            dplyr::mutate(w8 = w8 / sum(w8))
        final$geogcode <- as.character(const)
        results[[const]] <- final
        ## final
    }
    results <- bind_rows(results)
    results
}




for (i in unique(tgt$variable)) {
    if (!is.element(i, names(joint))) {
        stop(paste0("Variable ", i, " is missing in joint"))
    }
    vars <- sort(unique(tgt$category[which(tgt$variable == i)]))
    jvars <- sort(unique(joint[, i]))
    if (!all(vars == jvars)) {
        stop(paste0("Not all values of ", i, " present in joint"))
    }
}

out <- rake_to_const(joint = joint, targets = tgt)

stopCluster(cl)
out.bak <- out

### Multiply by counts
out <- merge(out,
             tgt %>%
             filter(variable == "age") %>%
             group_by(geogcode) %>%
             dplyr::summarize(count = sum(count)),
             all = TRUE) %>%
    mutate(count = round(w8 * count))

nrow(out)
out <- out %>%
    filter(count > 0)
nrow(out)

saveRDS(out, file = "working/frame.rds")
