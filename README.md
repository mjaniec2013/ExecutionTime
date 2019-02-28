# ExecutionTime
Execution Time Stopwatch

ExecutionTime was inspired by the 'timeR' package (https://cran.r-project.org/web/packages/timeR/index.html).

It exteneds some functionality lacking in 'timeR'.

## Usage

Initialize the ExecutionTime (ET) class:

`et <- ET$new()`

Start timer named 'New timer': `et$start("New timer")`

Stop timer and show the time measurements: `et$stop()`

### Stages

While the timer is running, you can add stages. Stage names can be defined.

`et$stage()`

`et$stage("stage name")`

At any time you can check recorded stages: `et$stages()`.

### Interim checks

To check the running timer's status use `et$elapsed()`.


