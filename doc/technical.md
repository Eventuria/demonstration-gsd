
## Definitions

GSD is distributed application based on the following concepts :
- [Domain Driven Design](https://en.wikipedia.org/wiki/Domain-driven_design) (DDD)
    - Place the project's primary focus on the core domain and domain logic
    - Base complex designs on a model of the domain
- [CQRS](https://docs.microsoft.com/en-us/azure/architecture/patterns/cqrs)
    - It stands for Command Query Responsibility Segregation.
    - It also known as the distributed version of DDD (and then DDDD as Distributed Domain Driven Design)
- Command Sourcing ( [Event Sourcing](https://martinfowler.com/eaaDev/EventSourcing.html) ++)
    - It's Event Sourcing where commands are also persisted
        - Capture all changes to an application state as a sequence of events
        - Capture all the commands sent to the system
    - Commands sent to the system are stored and consumed asynchronously as opposed to Event Sourcing
        - The command loss when the service is down is reduced.
        - The data flow is pulled as opposed to pushed in Event Sourcing
        - you work in a stream from A-Z
    - This command consumption produces and stores in a single transaction containing
        - The Events
        - The Command Response
- [Functional Reactive Programing](https://en.wikipedia.org/wiki/Functional_reactive_programming)
    - it's a programming paradigm for reactive programming (asynchronous dataflow programming) using the building blocks of functional programming (e.g. map, reduce, filter).
    - Streams (dataflow) are ubiquitous in GSD and Logs (FIFO) are the persisted version of them.
- [Micro-service Architecture](https://en.wikipedia.org/wiki/Microservices)
    - Services are small in size
    - Messaging (Commands) enabled
    - Bounded by contexts
    - Autonomously developed
    - Independently deployable
    - Decentralized
    - Built and released with automated processes.

All these concepts are building blocks for implementing [Kahn process networks (KPNs, or process networks)](https://en.wikipedia.org/wiki/Kahn_process_networks)

it's a concurrent [model of computation](https://en.wikipedia.org/wiki/Model_of_computation) which can be also considered as a Pattern / Architecture for distributed systems)

For those who these concepts are new, I have gathered here a set of articles that could help you see clearer :

- https://martinfowler.com/eaaDev/EventSourcing.html
- http://www.cqrs.nu/faq
- https://thinkbeforecoding.com/post/2013/07/28/Event-Sourcing-vs-Command-Sourcing
- https://blog.jonathanoliver.com/cqrs-sagas-with-event-sourcing-part-i-of-ii/


<h1> </h1>


