# System development as an industrial process

## A useful analogy

Drawing analogies between the construction industry and computer related processes is not new. Spector and Gifford (1986), via an interview with an experienced bridge designer (Gerald Fox), have documented important analogies between the industrial process of bridge construction and corresponding computer related processes.

In order to provide rationality in all phases of building construction, it is essential that a well established philosophy (i.e. point of view) guides the work of all parties in the various activities of a building project.

By **architecture** of the construction approach, we mean a foundation of concepts and techniques, selected from a universe of potential foundations, that defines the characteristic structure of all buildings designed using the approach. As an example, we could consider the selection of an approach based upon the exploitation of building blocks and components. This approach can be compared with other alternatives, for example, an architecture based solely upon customized constructs.

The **method** makes explicit the step-by-step procedures to be followed in applying the architecture to projects.

The **process** provides for the scaling-up of the method, so that it can be applied on a large scale with many interacting activities and parties.

**Tools** are provided to support all aspects of the enterprise and explicitly the activities of architecture, method and process.

The method is more basic and is described as a project is described with its different activities. A project is ended when the last activity is performed and the building has been taken into operation.

Operation and maintenance are often described in a simplified way as a single phase. This is not fully correcct since maintenance often involves new projects, including all phases.

A process, on the other hand, lasts as long as the product lasts and describes how the different activities interact during the whole life of the product.

Note that it is important not to confuse the term architecture underlying the method with the architecture of a particular product (i.e. 'building') that may be realized by applying the architecture. These building architectures represent 'instances' employing the enterprise philosophy. Hence one architecture may be used for the building of various houses (instances), and also various architectures may be used for the building of a specific house.

The architecture may be based solely upon utilizing building blocks and components; or solely upon utilizing customized constructs by craftsmen; or any combination thereof.

For each possible approach, a variety of methods could be defined to describe how to work with these constructs. This leads to the definition of a variety of step-by-step procedures, namely methods, where for example appropriate combinations of building blocks and components are utilized. Furthermore, the method must be scaled and related also to other activities leading to various processes possible for each method defined. These activities could then be supported by various tools.

We now consider how various activities of building construction are supported. In fact, the model we have introduced earlier is applied during each activity of building construction. The activities are **creative design**, **construction** and **long term support**. Of course, to make the transition smoothly and seamlessly between the phases, well defined interfaces are needed.

### Creative design

The transformation from a set of requirements and vague notions of what is desired, to a structural plan of the building and a plan of action for its implementation are the creative activities of new development.

In some architectural approaches, a minitiarized scale model of the house may be constructed. However, when a series of houses is to be developed, where all of the houses have the same basic building architecture, a scale model, and most often one or more sample houses (prototypes), are constructed. The prototypes are useful for potential buyers to evaluate the functionality of the house in terms of their needs as well as serving as means of 'debugging' and improving the basic building architecture.

In creating a modern building, in addition to building standards and norms, significant attention is given to approaches which exploit large building blocks based upon sub-assemblies of modules and components. These practices make large scale building construction economically rational while at the same time insuring quality and safety in the final product.

Creative design thus takes place according to the architectural approach and follows step-by-step methods and processes with the assistance of tools used in reducing requirements to a viable building project architecture plan, including, when desirable, the creation of prototypes.

### Construction

The first activity in construction is to provide implementation details concerning the architectural and constructional plans. That is to go from the more abstract towards a more concrete plan. After a sufficiently concrete plan has evolved, the production (implementation) takes place. Production is thus the last phase of construction.  The number of people involved up to the point of production (eve for large scale projects) is quite small in comparison with the number of people involved in actual productioon.

Production is the result of manufacturing the more abstract construction plans as well as the detailed construction plan. Further, the production activity may take advantage of any related models and/or prototypes that may have been developed. Here we can differentiate between custom built houses and houses developed for mass production.

In the custom built case, the implementation is typically performed by artisans who are specialists in their particular field (woodworkers, plumbers, perhaps sculptors, etc.)

In the mass production case, we find the need to employ people with less skill, but who can carry out their detailed work in a cost effective manner.

Responsability for a large scale building projects is most often placed in the hands of an entrepreneur. The entrepeneur takes responsibility for the production according to the building architecture and construction documentation. The entrepreneur, in turn, elists the services of subcontractors who take responsibility for portions of the total project. In order to use subcontractors effectively, standards and norms and the usage of building blocks and components become vital so that subcontractors can be relied upon to perform their services properly. Further, we find once again the importance of the method, process and tools which explicitly define and document the procedures to be followed by entrepreneur and subcontractors.

### Long Term Support

Building construction projects, custom designed or mass produced, must take account of the fact that the products should exist for many years. Thus the architectural approach of this phase must thake account of 'life-cycle' requirements for maintenance, alteration and extension. In the software industry, due to inherent flexibility of alteration, the existence of a philosophy containing an architecture that permits long term support is absolutely essential.

### Conclusion

During all activities, from the original product requirements, through the creative design activities, construction, production and for long term support; documentation is a vital aspect of rational industrial activity. The documentation must be appropriate ('understandable') for the various parties having a vested interest in the building project.

Building blocks that have been identified and exploited must be well documented and understood so they can be applied to new projects. In this regard, we can differentiate between building blocks and components.

Building blocks are typically larger units which have evolved during specific projects (for example, prefabricated walls); whereas standard components (for example, windows and doors) may have been used as parts of the building blocks.

From our characterization of the building process being based upon an architecture, method, process and tools, we can make the following observations concerning the results of the scaling-up process from which direct analogies can be drawn with the software industry.

* The process must yield a foreseeble result, irrespective of what individual performed the job;
* The volume of output does not affect the process;
* It must be possible to spread out parts of the process to several manufacturers/subcontractors;
* It must be possible to make use of predefined building blocks and components;
* It must be possible to plan and calculate the process with a great precision;
* Persons trained for an operation must perform it in a similar manner.

## System development characteristics

### Part of a larger activity

System development does not take place in isolation. It is part of a larger activity, often aimed at developing a product in which software is an integrated part.

The main flow of activity typically passes directly from sales to production. The input consists of new orders from customers and the output consists of systems delivered to customers. (We use the term customer broadly = another department within the same company can also be viewed as a customer.) The sales department orders product configurations for delivery to customers and formulates requirements for new products.

An order should be formulated in such a way that it is immediately possible to identify the configuration of the final product. The production department delivers a complete system to the customer. Further, it should be possible to formulate an order in terms comprehenisble to the customer without the aid of the system development department. Thus no programmers should participate in the production process, only persons skilled in duplicating products, assembling and configuring systems as well as testing them prior to delivery.

Terminology comprehensible to customers should be employed so that the need for the participation of the development department in customer contacts is minimized. This is possible if products can be described and ordered as sets of packages of functionality services, or as we will call them in OOSE, **serivce packages**.

In the development department, new software items (i.e. source code and/or other documents, for the production of systems) are developed, based on the new product requirements. The sales department can be informed about the new service packages later.

It is in terms of service packages that the staff of the three subprocesses (sales, production and development) communicate. In order to achieve a rational return on investment, service packages should be designed so that they can be used in a number of different products. It will then be possible to build a large number of applications from a set of standard service packages.

A customer order corresponds to a product order, specified as a combination of service packages. The production department receives the order and assembles the finished product. To do so, they start from the source code for the service packages and transform the programs into the object code of a particular machine configuration. During these processes, it must also be possible to produce all other forms of documentation that are part of the finished system.

If these processes are to carried out properly, the service packages must be developed with great care so that they can be configured for a number of product variations. Resuse must be reflected in the way the software is designed. Thus the aim should be to provide software items with substantial reusability both within the system (building blocks) and between different types of systems (components). Remember our analogy of the prefabricated wall (building block) and components such as windows and doors.

The building blocks are highly application related and form the basis for adaptations of the product for different customer categories. A building block corresponds to a service package or part of a service package. Further, the system development department contains a special group of people who are responsible for the development and coordination of general and application related components. The component developers provide essential support to product developers.

Reuse occurs in many product related activities and is not limited to programs. The output of system development is, in fact, a set of **descriptions**. The descriptions include the source code which can be interpreted by humans and compilers, diagrams, flow charts, and so on. All of these descriptions must be framed (self-explained) for reusability. Further, the knowledge of how to organize and manage projects must also be documented in a framed manner and made reusable.

When proper framing is achieved, rationality of software development and product exploitation can be attained. Service packages form the basis for the configuration of a system for a particular customer. Each customer receives their special combination of service packages with relevant documentation which has been assembled from appropriate descriptions. For a new release of a system, it is possible to reuse descriptions from the previous release in a controlled manner so that multiple releases of the same document not be maintained.

### System development

As requirements change, the system changes. The changed system actually consists of altered development information (descriptions) to be used for the production process.

System development is carried out in a number of steps, each of which constitutes a more detailed and concrete development of earchier activities. Thus it can be observed that system development is a gradual transformation of a sequence of **models**. The first model describes the customer's requirements and the last step is the fully tested program. Between these two end points are a number of other models.

System development can be viewed as a process of producing model descriptions. This is true of all levels - analysis, design, implementation and testing. In this context, the source code is seen as a description that can be understood by programmers and also by the production process (a compiler and linker). The descriptions present models of different degrees of detail. Early models are quite abstract, focusing on external qualities of the system, whereas later models become more detailed and instructional in the sense that they describe how the system is to be built and how it is meant to function.

The aim is to divide the complicated development of a large system into a number of activities and make it possible for several designers to take part at the same time. Each partial model is an abstraction of the system which enables the designer to make the necessary decisions at this level in order to move closer to the final model, the tested source code. Each modeling step adds more structure to the system. Further, each new model is more formal than the previous one. To make the transition between the different models as simple and faultless as possible, it must be straightforward to relate the model in one activity to the model in the following activity.

We say that two models are **seamlessly** related to each other if notions which were introduced in one model are represented in the other model in a very simple and straightforward manner.

In essence, system development of three distinct phases that follow each other seamlessly - **Analysis**, **Construction** and **Testing**. In analysis an application-oriented specification is developed to specify what the system offers its users. At this early stage, when changes are still relatively inexpensive, the aim is also to find a good structure for the system, namely a structure that is robust against change and which is divided into clear, comprehensible and indivisible units that can be ordered (i.e. service packages). This specification, which we call the analysis model, specifies the functional behavior of the system under practically ideal circumstances and without regard to a particular implementation environment. It is important, however, to judge whether the analysis model can actually be realized under the given circumstances, for example with regard to performance requirements and/or development costs.

During construction, the idealized conditions of the analysis will gradually be replaced by requirements from the chosen implementation environment. In this phase, how the application-oriented analysis model will be realized with the aid of such components as system software, database management systems and user interfaces is defined. The construction activities constitute design and implementation. The design activities formilize the analysis model in terms of the implementation environment and specify the identified building blocks. The separate programs (blocks) identified in the design are then coded (i.e. implemented).

In testing, the system is checked to make sure that all of the service packages in the analysis model have been correctly implemented and that the performance of the system meets requirements. Testing takes palce at several levels, from specific functions to the system as a whole.

Our view must raise from the project to the product.

### The transition from analysis to construction

At present, formalism during the analysis phase should be restricted to the syntax and semantics of the static structure of the system. For this critical pahse, no sound, practical, strictly formal technique have come to our knowledge to satisfactoraly specify the system's dynamic behavior. A more practical, descriptive technique is therefore preferable to a mathematical, formal method that is not yet fully mature. A formal technique is better used later on, especially during implementation. Whenever the more formal techniques get mature, these will probably be preferred.

Even though the boundaries between analysis and construction may seem vague, there are certain guidelines for what should be described during analysis and what should be dealt with during construction:

* The analysis is indipendent of the implementation environment. Changes in the implementation requirements thus do not affect the analysis result. Even if an important part of the system, such as a database management system, is replaced during implementation, the analysis model is not affected.
* The analysis model is is application-oriented. The work is carried out in an ideal world; memory, performance and fault-tolerance requirements are set aside.
* The analysis model describes the elements of the application in application-related concepts such as service packages. Given this foundation, the structure of the implementation mirrors the structure of the problem, rather than the other way round.
* The analysis model should not be too elaborate, as some of this work must be adapted to the chosen implementation environment. Such adaptations may be difficult if the analysis model is too formal.

### Requirements are input to system development

The primary input for the development of a system is a requirements specification. This will have been developed from facts about the environment that the system is to serve.

For 'technical' applications, such as tactical command and control systems, process control systems or telecommunication systems, the role of the system in its environment is identified, and the requirements of the system are formulated in terms of the behavior of sensors and actuators.

For 'administrative' information systems, such as order-entry systems, personnel administration systems or reservation systems, the work usually begins with an analysis of the needs, problems and development tendencies of the enterprise.

Based on this analysis, a new enterprise model is built where the conputer-based information system forms an important part of the enterprise. In fact, the development of a new and changed enterprise is based on the existing enterprise.

This is fully analogous to changing an existing information system. Thus the same observations concerning changes which we made concerning the software system are also valid for the enterprise.

Although we here differentiate between techincal and administrative systems, we do not provide a prcise definition of either. In reality, most systems include aspects from both areas.

#### Enterprise development

Enterprise development can be viewed as a generalization of system development. Instead of developing a system, a whole enterprise is developed. In enterprise development, the company is seen from several different perspectives. The aim is to identify the problem areas and suggest alternative solutions. One result may be to introduce an information system. Enterprise development can be divided into this phases:

1. **Establish Current State**. The **current state** description is a survey of the framework of the present enterprise including its aims and problems.
