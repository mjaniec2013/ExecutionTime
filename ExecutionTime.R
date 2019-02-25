
### Execution Time
##  MJ, 2019-02-24, v0.1 
##  inspired by 'timeR' package https://cran.r-project.org/web/packages/timeR/index.html
##  github: https://github.com/mjaniec2013/ExecutionTime

library(R6)
library(glue)
library(data.table)



common_dir <- "m:/Programming/R/COMMON/"
source( paste0(common_dir, "Round2.R") )



ET <- R6Class("ET",
                         
  public = list(
    
    start = function( this_timer_keeper_name="" ) {
      
      private$time_keeper_start <- Sys.time()
      
      if (this_timer_keeper_name!="") {
        
        private$time_keeper_name <- this_timer_keeper_name
        
      }
      
      if (private$is_verbose)
      
        cat(glue("Starting timer: [{private$time_keeper_name}] @ {private$time_keeper_start}."), "\n")
      
    },
    
    elapsed = function( verbose=TRUE ) {
      
      if (is.na(private$time_keeper_start)) {
        
        cat("Timer not started.\n")
        
        return(NULL)
        
      }
      
      if (!is.na(private$time_keeper_stop)) {
        
        cat("Timer stopped.\n")
        
      }
        
      current_time <- Sys.time()
        
      cat(glue( paste("Elapsed time for timer [{private$time_keeper_name}] started @ {private$time_keeper_start}",
                      "- {Round2(difftime(current_time, private$time_keeper_start, units='auto'), 4)}") ), 
          "\n")
      
    },
    
    stage = function( this_stage_name=NA ) {
      
      if (is.na(private$time_keeper_start)) {
        
        cat("Timer not started.\n")
        
        return(NULL)
        
      }
      
      if (!is.na(private$time_keeper_stop)) {
        
        cat("Timer stopped. Cannot add stage.\n")
        return(NULL)
        
      }
      
      current_time <- Sys.time()
      
      stages_num   <- length(private$time_keeper_stages)
      
      if (is.na(this_stage_name)) {
        
        this_stage_name <- glue("Stage {as.character(stages_num+1)}")
        
      }
      
      if (private$is_verbose)
      
        cat(glue( paste("Recording stage #{stages_num+1} [{this_stage_name}]",
                        "for [{private$time_keeper_name}] @ {current_time},",
                        "elapsed: {Round2(difftime(current_time, private$time_keeper_start, units='auto'), 4)}.") ), 
                  "\n")
      
      private$time_keeper_stages[[stages_num+1]] <- 
        
        list(
          
          stage_time = current_time,
          
          stage_name = this_stage_name
          
        )
      
    },
    
    
    # 'short' - show recorded stages only
    stages = function( short=FALSE ) {
      
      if (is.na(private$time_keeper_start)) {
        
        cat("Timer not started.\n")
        
        return(NA)
        
      } else {
        
        stages_num   <- length(private$time_keeper_stages)
        
        if (short) {
          
          cat( glue("{stages_num} stage(s) recorded:"), "\n\n" )
          
        } else {
        
          cat( glue("{stages_num} stage(s) recorded for [{private$time_keeper_name}] started @ {private$time_keeper_start}:"), "\n\n" )
          
        }
        
        if (stages_num>0) {
          
          data.table( 
            
            stage_name      = 
              
              sapply(private$time_keeper_stages, function(x) x$stage_name),
                      
            stage_time      = 
              
              sapply(private$time_keeper_stages, function(x) as.character(x$stage_time)),
                      
            time_from_start = 
                        
              sapply(private$time_keeper_stages, 
                        function(x) 
                            as.numeric(difftime(x$stage_time, private$time_keeper_start, unit="secs"))) 
            
          )
          
        }
        
      }
      
    },
    
    stop = function() {
      
      current_time             <- Sys.time()
      
      private$time_keeper_stop <- current_time
      
      cat( glue("Timer [{private$time_keeper_name}] started @ {private$time_keeper_start} stopped @ {private$time_keeper_stop} - time: {Round2(difftime(private$time_keeper_stop, private$time_keeper_start), 2)}"), "\n\n" )
      
      self$stages(short=TRUE)
      
    },
    
    verbose = function() {
      
      private$is_verbose <- !private$is_verbose
      
    }
    
  ),                         
                         
  private = list(
    
    time_keeper_name   = "",
    
    time_keeper_start  = NA,
    
    time_keeper_stages = list(),
    
    time_keeper_stop   = NA,
    
    is_verbose         = TRUE
    
  )                     
                         
)


### testing

et <- ET$new()

et$elapsed()

et$start("New timer")

et$elapsed()

# et$stages()

et$stage()

et$stage(5)

et$stages()

et$stop()

et$elapsed()

et$stage("additional")


