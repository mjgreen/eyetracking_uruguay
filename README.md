# eyetracking_uruguay
Alex's Shiny App, clean version for server

## Works to upload and run an app into my user space

scp -r /home/matt/gits/eyetracking_uruguay mgreen@uruguay.bournemouth.ac.uk:/home/mgreen/ShinyApps/

https://uruguay.bournemouth.ac.uk/shiny/users/mgreen/eyetracking_uruguay/

## uploads to server's own space (no sudo so for packages that need to be built from source that have dependencies that aren't available on the server, we need to use renv to supply those packages within the app)

scp -r /home/matt/gits/eyetracking_uruguay mgreen@uruguay.bournemouth.ac.uk:/srv/shiny-server/sample-apps/

https://uruguay.bournemouth.ac.uk/shiny/sample-apps/eyetracking_uruguay/

## To rebuild from the venv lockfile, use `renv::rebuild()` on the server after having logged in using
>ssh -p 22 mgreen@uruguay.bournemouth.ac.uk

## goto server dir
setwd("/srv/shiny-server/sample-apps/eyetracking_uruguay/")

## launch Rstudio on the server
https://uruguay.bournemouth.ac.uk/server/auth-sign-in?appUri=%2F