##' Plot Phenocam data
##' 
##' @param dat  dataframe of date, gcc_mean, gcc_std
##' @param ...  additional graphing parameters
plot_phenocam <- function(dat, pred = NULL){
  
  if(!is.null(dat)){
    
    dat <- dat %>% 
      mutate(ylo = gcc_mean - 1.96 * gcc_std,
             yhi = gcc_mean + 1.96 * gcc_std)
    
    ## 
    p <- ggplot(dat, aes(x = date)) +
      geom_ribbon(aes(ymin = ylo, ymax = yhi),
                  alpha = 0.70,
                  fill = "lightblue") +
      geom_point(aes(y = gcc_mean)) +
      labs(x = "Date", y = "GCC_90", title = "U. Illinois Tall Grass Prairie (2015)")
    
    if(!is.null(pred)){
      p <- p  +
        geom_line(data = pred, aes(x = date, y = pred))
      
    }
    
    p
    
  } else {
    print("plot_phenocam: input data not provided")
  }
  
}
```

5.  Follow the instructions above to Add, Commit, and Push the file back to your Github repository
6.  Next you want to perform a "pull request", which will send a request to the OWNER that they pull your new code into their mainline version. From your Github page for this project, click **New Pull Request**.
7.  Follow the instructions, creating a title, message, and confirming that you want to create the pull request

### OWNER

1.  Once the COLLABORATOR has created the pull request, you should get an automatic email and also be able to see the pull request under the "Pull Requests" tab on the Github page for the project.
2.  Read the description of the proposed changes and then click on "Files Changed" to view the changes to the project. New code should be in green, while deleted code will be in pink.
3.  The purpose of a pull request is to allow the OWNER to evaluate the code being added before it is added. As you read through the code, if you hover your mouse over any line of code you can insert an inline comment in the code. The COLLABORATOR would then have the ability to respond to any comments. In larger projects, all participants can discuss the code and decide whether it should be accepted or not. Furthermore, if the COLLABORATOR does any further pushes to Github before the pull request is accepted these changes will automatically become part of the pull request. While this is a very handy feature, it can also easily backfire if the COLLABORATOR starts working on something different in the meantime. This is the reason that experienced users of version control will use BRANCHES to keep different parts separate.
4.  Click on the "Conversation" page to return where you started. All participants can also leave more general comments on this page.
5.  If you are happy with the code, click "Merge Pull Request". Alternatively, to outright reject a pull request click "Close pull request"

## Task 4: Owner adds pred.logistic and fit.logistic

We are now past the 'set up' stage for both the OWNER and the COLLABORATOR, so for this task we'll explore the normal sequence of steps that the OWNER will use for day-to-day work

### OWNER

1.  Pull the latest code from Github. In RStudio this is done by clicking the light blue down arrow on the Git tab. This is equivalent to the command line *git pull origin main* where origin refers to where the where you did your original clone from and main refers to your main branch (if you use branches you can pull other branches)
2.  Next, open up a new R file, add the code below, and save as `03_logistic.R`

```{r}
##' Logistic model
##'
##' @param theta  parameter vector
##' @param x      vector of x values
##' @return vector of model predictions
pred_logistic <- function(theta, x){
  z <- exp(theta[3]+theta[4]*x)
  Ey <- theta[1]+theta[2]*z/(1+z) 
}

##' Fit logistic model
##' 
##' @param dat  dataframe of day of year (doy), gcc_mean, gcc_std
##' @param par  vector of initial parameter guess
##' @return  output from numerical optimization
fit_logistic <- function(dat, par){
  
  fit <- nls(gcc_mean ~ pred_logistic(theta = theta, x = doy), data = dat, start = list(theta = par))
  
  fit$m$getPars()
  
}

