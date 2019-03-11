
# GSD (Getting Stuffs Done)

GSD  is a basic todo list command-line application
- Organize your stuffs to be done by workspaces
    - e.g : Work, Sport, House, Vacations, Spanish ...
- Define the goals you wish to achieve within these workspaces
   - e.g : In the workspace "Spanish", you could add the goal "Mastering Spanish Grammar"
- Define the different actions to undertake for fulfilling these goals
   - e.g : Goal : "Mastering Spanish Grammar"
     - [x] Action 1 : Find a good book about Grammar
     - [ ] Action 2 : Learn the present tense of regular verbs, irregular verbs and verbs with spelling changes
     - [ ] Action 3 : Learn the present tense of verbs with stem changes
     - [ ] Action X : etc.

## Motivations

- Learning and Practicing the Haskell language and environment
- Demonstrating skills in the following areas
    - Distributed System and Domain Driven Design (D.D.D.D)
      - CQRS (Micro-service Architecture)
      - Event Sourcing
      - Command Sourcing
      - Domain Driven Design
    - Functional Programming
    - Functional Reactive Programming

For details about my experience, please visit :
https://www.linkedin.com/in/nhenin/

## Developer's Guide (Mac OSX)

### Workstation Setup

- Install the Haskell Platform (https://www.haskell.org/platform/mac.html)
    - GHC
    - Cabal build system
    - Stack tool
- Install docker (https://docs.docker.com/docker-for-mac/install/)

### Build and Run

- Git clone this project
- Build the executables
    - Go at the root of the clone :
    ```bash
    stack build
    ```
    - At the end, you should see something similar to :
    ```bash
      Installing library in /Users/nhenin/dev/gsdFlow/.stack-work/install/x86_64-osx/lts-13.0/8.6.3/lib/x86_64-osx-ghc-8.6.3/gsdFlow-0.1.0.0-I94do1kjiSNDk01E3dY4Vi
      Installing executable gsd-command-sourcer in /Users/nhenin/dev/gsdFlow/.stack-work/install/x86_64-osx/lts-13.0/8.6.3/bin
      Installing executable gsd-command-consumer in /Users/nhenin/dev/gsdFlow/.stack-work/install/x86_64-osx/lts-13.0/8.6.3/bin
      Installing executable gsd-read in /Users/nhenin/dev/gsdFlow/.stack-work/install/x86_64-osx/lts-13.0/8.6.3/bin
      Installing executable gsd-monitoring in /Users/nhenin/dev/gsdFlow/.stack-work/install/x86_64-osx/lts-13.0/8.6.3/bin
      Installing executable gsd-cli in /Users/nhenin/dev/gsdFlow/.stack-work/install/x86_64-osx/lts-13.0/8.6.3/bin
      Registering library for gsdFlow-0.1.0.0..
    ```
    - At this point, all the executables are installed on your Mac environment.
- run the distributed application
    - run the [eventStore](https://eventstore.org/) on docker :
      ```bash
        docker run --name eventstore-service -it -p 2113:2113 -p 1113:1113 eventstore/eventstore
      ```
      This command `docker run` will :
        - [x] Download the docker image of the [EventStore](https://eventstore.org/)
           > *Warning* : This operation could take a while since you need to download the docker image...
        - [x] Run that docker image into a docker container named `eventstore-service`
        - [x] Open the docker container on the port 1113

## High-Level documentation
### Project Structure
### Design Choices

## MVP Roadmap

- CommandTransactionDSL


------------------------



