# Getting Started (Mac OSX)

# Workstation Setup

- Install [Docker](https://docs.docker.com/docker-for-mac/install/)
- Install the [Haskell Platform](https://www.haskell.org/platform/mac.html)
    - GHC
    - Cabal build system
    - Stack tool

# Build and Run The Distributed Application

## Build

- Git clone this project
    ```bash
    git clone git@github.com:Eventuria/gsd.git
    ```
-  Go at the root of the clone :
    ```bash
    stack build
    ```
## Run

You are about to run 6 services :

- 1 user interface `gsd-cli` : command-line user interface to pilot the gsd application
- Relying on 5 services:
    - `eventstore-service` : database where all the data is persisted (embedded into a docker container)
    - `gsd-command-sourcer` : receives and store commands sent by the cli
    - `gsd-command-consumer` : consumes the commands from the sourcer and produces events
    - `gsd-read` : read the events and offers a read model for the cli
    - `gsd-monitoring` : shows what the command consumer produced.


In order to understand the technical aspects of the system, I recommend you to run 1 service per terminal as in this example :

![](https://github.com/Eventuria/media/raw/master/6-panels-demo.gif)

1) Run the [eventStore](https://eventstore.org/) on docker

    ```bash
    docker run --name eventstore-service -dit -p 2113:2113 -p 1113:1113 eventstore/eventstore
    ```
    This command `docker run` will :
    - [x] Download the docker image of the [EventStore](https://eventstore.org/)
       > *Warning* : This operation could take a while since you need to download the docker image...
    - [x] Run that docker image into a docker container named `eventstore-service` (in background mode)
    - [x] Open the docker container on the port 1113 for gsd services
    - [x] Open the docker container on the port 2113 for the [event store website](http://localhost:2113/web/index.html#/dashboard) (username: `admin`, password: `changeit` )

    Tips for handling that docker container created :
    - `docker stop eventstore-service` to stop the service.
    - `docker start eventstore-service` to start the service.
    - `docker rm eventstore-service` to delete the container.

2) run the command sourcer service
    ```
    stack run gsd-command-sourcer
    ```
3) run the command consumer service
    ```
    stack run gsd-command-consumer
    ```
4) run the read service
    ```
    stack run gsd-read
    ```
5) run the monitoring service
    ```
    stack run gsd-monitoring
    ```

6) run the command line interface
    ```
    stack run gsd-cli
    ```

Once all the services are up and running, the `gsd-cli` will give access to gsd commands, you should see something similar to the following :

```bash
###############################################
||          Service Health Checking          ||
###############################################
------------------------------------------
 [√] Service is up and running
------------------------------------------
###############################################
||       Welcome to the gsd client !         ||
###############################################
Workspaces
  - Learning Music > Todo : 4 goal(s)
  - Eventuria > Todo : 6 goal(s)
  - Sports > Todo : 3 goal(s)
------------------------------------------
Commands
   1- Create A Workspace
   2- Work On A Workspace
   3- Quit
```