---
title: Rust as a platform for IoT
subtitle: Analyzing the readiness of the Rust ecosystem for IoT
authors:
- Yannik Sander
description: This essay presents a summary of the Rust ecosystem aiming at applications in IoT and an analysis of its current suitability for new projects at the time of writing in mid-2021.
tags: technical, rust, IoT, kth
date: 2021-12-12

bibliography: ./xxx/references.bib
image: https://unsplash.com/photos/LqKhnDzSF-8/download?ixid=MnwxMjA3fDB8MXxzZWFyY2h8NHx8aW90fHwwfHx8fDE2MzkwNDcwMzQ&w=1920
image-credits: |
    Made by Joshua Sortino on <a hrep=https://unsplash.com/photos/LqKhnDzSF-8>Unsplash</a>
status: published
...

:::{.note header="Background"}

This essay was written as part of the ID2012 Ubiquitous Computing Course at KTH Royal Institute of Technology Stockholm.

The task was to produce an analytic overview of a topic in the domain of Ubiquitous Computing or IoT. I chose Rust's application for IoT out of my personal interest in Rust and previous experiences diving into IoT and specifically embedded devices using Rust.

The essay was finished and handed in by **May 2021** and reviewing the filed today already shows some minor changes. Therefore be warned, the field is evolving quickly and might look better depending on when you will read this.

If you want to support my work or reach out feel free under the emails below:

**Yannik Sander** (yannik@kth.se / contact@ysndr.de)

*If you want to use this article, please consult me first. A PDF version is available [here](../rust-for-iot/Sander_Rust-for-IoT.pdf)*

:::


#  Introduction

The IoT and Ubiquitous Computing already makes for a large share of software being written today. To prove secure writing such software should be ergonomic while staying performant and efficient, the potential for human error should be minimized.

Rust is a young programming language with a lot of promises, including being a viable platform for IoT of Ubiquitous computing. This essay will introduce Ubiquitous Computing and IoT, then, draw a picture of the current Rust ecosystem relevant for these fields and finally analyze how well this ecosystem can provide for the demands of the field.

## Ubiquitous Computing and IoT

### Introduction to the character of ubiquitous computing

The notion of Ubiquitous Computing, often and in the following simply referred to as **Ubicomp**, has been established by Mark Weiser in his 1991 paper envisioning "The Computer for the 21st Century"[@weiser_computer_nodate]. In a time where computing was visible, immobile and rear compared to today's standards, Weiser spoke of a "disappearance" of technology. Surely, he did not speak about a decrease in computing, but the opposite. The disappearance was about the obvious presence of said technology. Weiser predicted a world where computing is omnipresent -- *ubiquitous* -- "weaved into everyday life". On the one hand, it was about the mobility of computers, i.e. being able to take computing everywhere. Yet, this doesn't cover it completely! Indeed, it was the unintrusive enhancement of people's life that defined Ubicomp.

In the modern day, Ubicomp has become an important part of human-computer interaction (HCI) implementation and research. Especially being aware of the user's context and acting upon that is an important aspect of Ubicomp relevant to HCI and carrying numerous opportunities for future advancements in computing [@dey_distributed_2002]. There have been great developments in technology since Weiser formulated the concept of Ubicomp that enable many of his ideas. Poppe et.al. [@poppe_evaluating_2007] pointed out critical developments in this regard.:

#### Context Awareness and Pro-Activeness

Multitudes of sensors both owned (or even worn) by the user or present in the public domain allow the greater possibility to take user context into account. This includes classifying user's actions, emotions, health and location and allows them to provide services related to that. This might be sports tracking e.g. analytics and recommendations in tennis [@sharma_wearable_2017] or granting access to public transport without any interaction, enabled by face detection [@li_you_2019]. Further, context awareness empowers services to be gradually more proactive, likewise reducing the amount of interaction required by a user, although "mixed-initiative" is said to be more appropriate to HCI. Siri [@noauthor_use_nodate] and Google Assistant [@noauthor_google_nodate] are examples of such context-sensitive, mixed-initiative services. They provide information when they are queried by a user, most notably facilitating non-physical interaction with distributed devices (i.e. through voice commands handled by a supported client device). They might also present information on their own, based on context and need.

#### Adaptability

*Need* is interesting on its own. It might mean external factors such as emergency warnings, but often employs a different concept important to Ubiquitous computing, namely, *adaptability*. Evers et.al.[@evers_user_2014] claim that "future computing systems must adjust to the user’s situations, habits, and intentions". Pro-activeness, as mentioned above, is much more useful, if not only useful, if it supports and anticipates the user's intentions and develops with their behaviour. As such, it's imperative to not only collect user context but also user feedback on the actions anticipated.

#### Intelligence

Tangential to the ability to learn from user feedback is the perceived intelligence of a Ubiquitous system. As humans, we are used to expecting decent levels of intelligence in natural interaction with each other. In effect, to be perceived as a *natural* part of one's environment, rather than being a tool, ubiquitous technology needs to show intelligence too. Adaptivity and pro-activeness, as discussed before, are some aspects of this. Additionally one might ask for additional criteria such as an ability to reflect and anticipate consequences, improve their behaviour and show diverse strategies as well as *natural* social competence.

Speaking of intelligence, one can discern different types of intelligence by the agent that shows it. Things or machines that display intelligence are typically considered robots. User software becomes a (software) agent or softbot. While softbots (for instance the aforementioned personal agents) can already be integrated into the environment and provide non-physical interaction, going a step further one can also separately distinguish smart environments. These are often referred to as implementations of ambient intelligence. We see intelligence embedded into objects in our environment [@cook_ambient_2009] such as appliances, thermostats and similar devices. Research might go even further exploring rooms, that sense the user's presence, can store and prepare different states for different users [@eliasson_secure_nodate, p.21] and be augmented virtually [@weiser_computer_nodate].

In fact, augmented reality was also envisioned by Weiser [@weiser_computer_nodate] as part of a ubiquitously computing future. Several projects are pushing the idea of AR implemented in projects of varying comfort in form of the Google Glasses and Microsoft Holo Lens or smartphone-based solutions like Google Lens.

#### Summary of the Dimensions of Ubicomp

Summarizing the nature of Ubiquitous computing one can distinguish advances on <!-- three --> different dimensions. First, implementations of Ubicomp might have varying *distance* to the user. It can be public domain face detection based access control, or intelligent rooms. Closer to the user there are wearables or smart fabrics. In recent days project like Neuralink [@pisarchik_novel_2019] picture a future with even closer integration of computing. Second, different grades of *artificial intelligence* are shown. Systems that interact close to the user are supposed to do so naturally, i.e. intelligently. The less direct the interaction the less the requirement for pro-activeness, talking about face detection as an example. Weiser himself coined another dimension, namely *size*:

> "Inch scale, foot scale and yard scale devices."

Computing nowadays can be as big or bigger than smart screens, down to tablet and smartphone/smartwatch size. Yet, even smaller computing is present in our environment. Credit cards, contactless keys, or glucose meters are examples of such smaller and certainly much more transparent areas of computation with medical devices reaching even greater records in smallness.

So far, Ubicomp has been mainly described as a form of HCI, providing ways for users to interact more or less directly with a greater service. Ubicomp itself while popular in academics (listing more than 1.5M search results on Google Scholar^[May 4th, 2021]) hasn't become nearly as present in the industry and everyday life as a term. Possibly due to its nature being more of a concept without clear and concrete borders as seen above. Instead, the term *Internet of Things* (IoT) experienced a phase of ubiquity in industry and consumer electronics. What is IoT then?

### The Internet of Things

While Ubicomp as a concept seems to concentrate more on the connection with humans and the possibilities it offers to them, IoT has a greater impact as a marketing term for connected devices. In a way, it's a more practical term than Ubicomp. Yet, that does not mean it is a more concrete one. Applications of IoT include Home Automatization, Smart Cities, Media Consumption and Transportation to name a few many of which are also part of Ubicomp. The difference to Ubicomp is that IoT describes *actual* systems/networks of devices that work together and communicate, as well as their protocols and standards.

As shown in an article in *Business Horizons* [@lee_internet_2015] numerous artefacts are part of the IoT. It is the nature of a system that qualifies it as part of *the* IoT.  Data is produced at one end by wirelessly connected sensors, send over a network, processed by specific middleware running *in the cloud* and driving IoT based applications.

Only considering consumer applications the market for IoT is enormous. Home automation for example has evolved rapidly over the last years, with multiple applications by competing vendors reaching from lighting overheating to property security and more. The great interest in this market led to a substantial fragmentation of the market on nearly every layer of IoT. In the pursuit of standard multiple communication protocols evolved. Nowadays, Zigbee, Z-Wave, Bluetooth LE and WiFi are the dominant standards to build networks[@elhadi_comparative_2018]. Yet, device protocols often remain largely incompatible still.

If building on open standards, the entrance to IoT has become quite simple. Hubs provide an interface to compatible connected devices or connect them to cloud-based services. Yet there is little standardization around the interaction between different devices' specially if coming from different vendors.

Looking at industrial contexts, an apparent difference is the predominance of sensors at the bottom of the network becomes apparent. IIoT is characterized by a multitude of wirelessly connected actuators and sensors [@foukalas_dependable_2019]. Unsurprisingly, IIoT generates a lot of data, which needs to be stored and processes or analyzed[@noauthor_what_nodate]. This fact strongly motivates cloud computing or even more immediate processing at the *edge of the network*. [@noauthor_real-time_nodate]. Additionally, the uniqueness of many applications implies that there are even incompatibilities between systems than in the consumer market.

To summarize, IoT consists of three main elements each of which can vary in complexity based on the application as crystallized by Jayavardhana Gubbi in a 2013 paper [@gubbi_internet_2013].

1. **Hardware** is the common term for sensors, actuators and communication drivers
2. **Middleware** provides intermediate analytics and data storage
3. **Presentation** conveys the findings to the end-user

One might additionally include **Software**, especially protocols, in the list. In the following, this essay focuses on software related to the first two points.




## Rust

Rust is a relatively modern programming language that was first introduced in 2010 by Mozilla as a basis for their experimental browser engine Servo[@graydon_project_nodate] parts of which are now driving the Firefox Browser[@noauthor_quantum_nodate]. Its trifecta of speed, safety and concurrency caught peoples interest early on. Since the beginning rust strived to provide greater safety through an advanced type system. By design, Rust disallows concurrent mutable access to the same data. Instead, it employs the concepts of data *ownership* and *borrowing*. At compile-time, Rust can resolve how long references are used and when they are cleared up. In effect, (modern) Rust does not implement a garbage collector. This and the fact that it is compiled to native code through LLVM put it in the same category as other unmanaged languages such as C/C++ and account for Rust's performance. The first stable version of Rust was released in 2015[@noauthor_rustreleasesmd_nodate]. Since then public interest grew starkly, due to its promises.

By now, Rust has been voted the "most-loved" language since 2016 by developers on StackOverflows yearly survey [@noauthor_stack_nodate]. Its today's most convincing features are summarized by Jake Goulding[@goulding_what_2020] in a blog post from January 2020. Firstly, its versatile and ergonomic type system enables very practical safety measures, for example, replacing `null` pointers for more expressive and safe `Option<T>` types and enforcing the handling of errors through a `Result<Error, T>`. Additionally, these are also examples of Rust's capability of algebraic data types. The aforementioned garbage collection model -- or the lack thereof -- is as well highly appreciated by users of the language as it decreases the applications memory footprint dramatically. The possibility of *safe* direct memory access has likewise driven Rust to be an aspiring candidate for embedded{@} devices as well as recently becoming an officially supported option for Linux kernel module development[@noauthor_rust_nodate-2], not at last because sticking to Rust's compiler enforced rules drastically reduces the possibility of segfaults. Segfaults, typically occurring when accessing invalid memory, are by default prevented by Rust's memory design.

Additionally to the language design, Rust has built a thriving ecosystem. Its standard build tool and package manager Cargo[@noauthor_introduction_nodate] is the pivoting point of this ecosystem. Using cargo one can easily manage dependencies, config feature flags, run tests and much more. It also offers great extensibility through custom commands and built-in integrability with IDEs [@noauthor_external_nodate]. Cargo links in and provides tools for publishing libraries on its package library [crates.io](https://crates.io)[@noauthor_cratesio_nodate]. These libraries referred to as *crates* are considered to be one of Rust's most important features on their own. Traditional languages such as C/C++ do not have any standard package manager, libraries are typically installed as precompiled binaries that need to be linked at compile time or runtime in case of shared objects. This requires the developer to include header files that are only resolved using a basic preprocessor, install these libraries separately and track/require them using third party tooling with little control over the actual version being used leaving many security issues to be dealt with by the user of the software and OS maintainers. C++ recently added support for modules[@corob-msft_overview_nodate] solving some problems related to header files but remains fragmented in general. Rust got inspired by more modern and ergonomic solutions of more recent languages such as NPM[@noauthor_npm_nodate].



# The Rust Ecosystem for IoT

In the introduction, Rust's ecosystem was outlined. Focusing on IoT one needs to take a deeper look into the accompanying tools and libraries. This essay will introduce key technologies and concepts that enable the development of IoT devices and related edge computing.

When speaking of IoT ARM is by far the leading manufacturer of Chipsets used at the edge of the IoT[@noauthor_who_2020] and embedded devices such as sensors. As such, to be a viable option to cover the IoT space as a language, support for ARM-based processors is imperative! Hence special focus will lie on ARM support in Rust.

## Rust on Microcontrollers

### Tools

A major component of the ecosystem of a programming language are tools that simplify or automate the development processes. These processes can become highly complex even for rather simple projects. For instance programming, a common microchip, requires a debugger, a connector to the on-chip debugger and the programmer and the build tool to work together. The latter also needs to be configured for the programmed chip. Several tools that have been developed try to shrink the associated learning curve and strive to allow for greater productivity quicker.

Being based on LLVM Rust supports a multitude of platforms [@noauthor_platform_nodate] including many ARM platforms. Cargo complements this by offering an interface to cross-compile to foreign architectures. Additionally, rustup[@noauthor_introduction_nodate-2] provides an interface to easily acquire toolchains for these architectures and simplifies keeping track of the fast-paced releases of the Rust language. Combining these tools, cross[@noauthor_rust-embeddedcross_2021] has been developed by the rust-embedded working group which uses isolated docker containers to minimize the efforts required and possible failures of setting up a development environment by providing a managed prepackaged solution. With this running and testing code for different architectures becomes as easy as

```bash
$ cross test \
  --target mips64-unknown-linux-gnuabi64
```

Targeting microcontrollers, in particular, the knurling project [@noauthor_knurling-rs_nodate] develops tools that make embedded development more seamless. `probe-run` is a project that integrates downloading binaries to controllers, and running code, as well as connecting debuggers with cargo and can therefore be easily integrated with IDE's. `defmt`  significantly reduces resource overhead of logging on microchips and has been found to offer a highly integrated debug process [@noauthor_using_nodate].

### Abstraction layers

Speaking about abstraction layers one must first understand why they are needed. Programming microcontrollers is flooded with hardware-level interaction, unsurprisingly. While rust is capable of doing these accesses, in many cases *some* of Rusts safety measures need to be disabled. While more is possible in these `unsafe` environments, obviously one strives to reduce the use of `unsafe`. Besides safety, ergonomics and compatibility are more reasons to ask for abstractions. Rust is known for its capabilities to bring these virtues to its users in other areas already due to expressive Generics and its trait system. In the context of embedded programming, this has enabled people to create various levels of abstractions on top of the lowest levels of interaction with the hardware.

#### Accessing the hardware

Peripherals on microcontrollers are configured through so-called memory-mapped registers. Manipulating the state of these registers changes how the external connectors to the chip behave, whether they are inputs or outputs, digital or analogue. Also, internal structures can be controlled this way, e.g. timers can be set and reacted upon. Unfortunately, there is no common interface to these registers not only due to the number of different manufacturers but also different chip design and application.

While configuration and layout differ, it does not do so undocumented. In fact for long SVD files [@noauthor_svd_nodate] are being made available by manufacturers describing the chip layout formally. In Rust, this is made use of to create so-called *peripheral access crates* (PAC). Using a `svd2rust` [@noauthor_rust-embeddedsvd2rust_2021] one can *generate* a rust library that implements a **safe interface** to all of the specified registers including context-based functions, such as being able to write or read from pins, or start timers using a method rather than setting bits manually.

#### Abstracting hardware functions

PACs do a great job making raw hardware accessible by Rust in an automated and safe way. Building on top of this, one might perform standard operations such as communicating to peripherals connect to USB, enable timers and so on. While building this functionality from the ground up based on peripheral access, a safer and more portable solution is building on a shared abstraction. Such an abstraction is provided by the `embedded_hal`[@noauthor_embedded_hal_nodate] crate.

The functionality provided by `embedded_hal` fulfils some important requirements:

1.  It is independent of any specific chip
2. Does not make restricting assumptions about how it is used on a specific chip
3. Provides low-cost abstractions that are compassable into higher-order abstractions (note that `embedded_hal` is still a *very* low-level abstraction)
4. Stemming from the previous point: Offers sufficient freedom and capabilities to base device-independent drivers upon.

Note, that `embedded_hal` does not implement most of the functionality, but defines interfaces that are eventually implemented for a specific chip or family of devices.

#### <!--Building on specific devices-->

 <!--Device Support-->

### Drivers

Apart from accessing mere hardware, the most important aspect of embedded development is, as in non-embedded scenarios, processing data, and providing functionality. In the context of IoT data is typically produced by the periphery, and communicated over some network channel, it is still the *internet* of things. Enabling this, one finds themselves at a gap. So far, the discussed abstractions merely provide hardware access. Yet, communication, in particular, requires conformity to often complex protocols (i.e. IEEE802.11/WLAN[@noauthor_ieee_2018]). Implementations for these protocols readily exist in C, less so in Rust, often because the modems are using more niche platforms, to begin with. Instead of reimplementing the existing C <!--might come--> implementation for those modems, Rust focuses on offloading this functionality. <!--expensive--> Offloading means to employ a second chip running a firmware that drives a communication module and exposing the data access through a firmware specific (serial) interface. Drivers have been implemented for all sorts of such devices and often make use of the aforementioned `embedded_hal` to be usable from any host device.

While serial protocols such as USB, RS232 or i2C can be part of a HAL, data protocols like AT[@noauthor_hayes_2021] are implemented separately. Crates like `atat`[@noauthor_blackbirdhqatat_2021] transparently offer access to these protocols. Building on that, driver crates for popular networking modems are already available. With IoT in mind, we can find drivers for cellular access[@noauthor_blackbirdhqublox-cellular-rs_2021] or short-range networks[@noauthor_blackbirdhqublox-short-range-rs_2021] that connect to u-blox[@noauthor_u-blox_nodate] devices. The drogue IoT project [@noauthor_drogue_nodate] not only brings support for common network standards like WiFi or LoRaWAN but also abstracts these to a transparent network interface, such that from Rust each of these network gateways can be used the same way providing TCP/UDP sockets. Building on this network abstraction the project also implements an MQTT and HTTP client.

### Notable Mentions

LoRaWAN[@noauthor_what_nodate] is known for its application as the basis for IoT. Especially The Things Network[@noauthor_things_nodate] plays a major role in pushing LoRa by providing a shared infrastructure that is energy efficient, yet reliant, open and secure. Incidentally, crates to create clients to this network already exist.

## Leaving Microcontrollers

At this point, Rust's support for microcontrollers was comprehensively presented... Although a lot of the IoT is implemented on the smallest of processors, often one has more resources to spare. Devices that could be described as "raspberry pi sized", can run a supported operating system (e.g. GNU/Linux) on a higher architecture, such as `aarch64`. Consequently, they profit from full Rust-Support. These offer more versatile tools and capabilities to connect to complex technologies such as Bluetooth, or processing greater amounts of data, such as camera feeds.



## Rust on the Edge

In recent years WebAssembly [@noauthor_webassembly_nodate] (WASM) has been growing as an OS-independent platform, meant to run programs in web-browsers at near-native speed in secure sandboxes. Rust as a language has been pushing this development forward, by language support and tooling. Not only have the three biggest freestanding WASM runtimes adopted Rust as their implementing language[@noauthor_wasmeriowasmer_2021, @noauthor_bytecodealliancewasmtime_2021, @noauthor_bytecodealliancelucet_2021], but also there have evolved standard tools to integrate WASM into your JavaScript Codebase [@noauthor_rustwasmwasm-pack_2021] and library support to narrow the gap between the Rust/WASM world and the JavaScript runtime[@noauthor_rustwasmwasm-bindgen_2021].

Companies like *fastly* and *Cloudflare* have developed services that facilitate this platform to offer easy entrance to Edge Computing. Cloudflare Workers [@noauthor_cloudflare_nodate] offers the infrastructure for reliable functions on the web that can act as an ingress point for IoT devices. Workers run code compiled to WebAssembly which makes them a ready target for Rust.



# Rust as a platform

In the previous section, a multitude of applications and capabilities of the Rust Language has been presented. While the ecosystem is large, it is important to also analyze it with an eye on qualitative factors to come up with a convincing conclusion about the usability of Rust as a Platform for IoT today. As rust is still evolving, many things will still improve. This essay aims to summarize the current developments and make an educated guess where Rust is heading.

## Today

Rust is known for assessing its performance in many areas publicly in the form of "are we X yet" websites[@noauthor_areweyet_nodate, @noauthor_are_nodate]. Unfortunately, for the domain of embedded/IoT, the community has not yet started such a project. Yet, as that form has proven very informative, this essay will adopt a similar approach.

++
  ~ Rust has stable and mature support. You can use Rust for this

+
  ~ Rust offers some support/development. Think twice.

±
  ~ Ideas are there but little has evolved from it.

-
  ~ Close to nothing has been developed. You are on your own, not recommended to use Rust here.



### The good

In some regards, rust can already shine, although it might need some polish in some places. Especially the strong features of Rust, tooling and performance, can shine too in the IoT context.

#### Performance and Ergonomics (++)

One of the strongest points to make about Rust is probably its performance. This does not mean solely its runtime performance but also its development process.

Rust is often hailed for the high-level elements that make it look and act like a general-purpose language in many regards. At the same time, it embraces the concept of "Zero-Cost Abstractions" that let it produce highly optimized code without accepting drawbacks on its high-level features.

Yet in areas where every byte counts, with rust one, has to trust on the optimizer to produce sufficiently small binaries. Which becomes harder given how easy it is to add dependencies to a project. Projects exist to help monitor the size of binaries but the main problem remains.

Looking at it in another way, given how high-level rust can be, memory accesses are not as obvious as they are with C. Clearly, this can cause unexpected problems in performance, especially on microcontrollers with limited memory speeds.



#### Tools (++)

Rust is known for its great tooling. This doesn't stop in the world of embedded systems and IoT. For one, Rust is pioneering the world of WebAssembly. Additionally, the entry into Embedded systems is made greatly easier given the rust tooling.

As we saw with the abstractions above, embedded Rust does not exclude the possibility to use cargo and its package management. In fact, it even provides a measure to tell whether a library can run on embedded devices or not, precisely as long as it does not use the standard library that is built on top of specific operating systems. Such crates are marked as `#[no_std]`.
It doesn't stop there. We saw projects automatizing the whole process of downloading binaries and running/debugging them through standard cargo invocations that integrate well with IDEs.

Of course, not everything is perfect in this regard area yet. As a lot of Rust's tooling is automatized, as a user one faces a rather high-level view of the process. While this can be desirable, it reduces the account one has to tell what parts are causing errors. Also, due to abstraction over multiple interfaces, the functionality provided might be less than what would theoretically be possible using those tools directly, at which point using Rust might become more of a burden as configuring these tools to fit Rust might not be trivial in every case.

A similar thing can be said about Rust for Edge Computing using WebAssembly. We have seen it as the driving language for modern runtimes and offering rich library support[@noauthor_webassembly_nodate-1]. As such given its tools, it is easy to integrate them into existing platforms or important in the domain of Edge Computing write high performant workers in it.

#### Frameworks and Libraries (+)

The presentation shed light on some of the most influential projects. While on the WASM front Rust has already developed a mature environment. In the space of embedded devices, libraries while bringing support for many devices, do not enjoy the same kinds of maturity and maintenance.

The `embedded-hal` is a great leap toward a unified API on microcontrollers and is already heavily used. Yet, many projects have implemented drivers independently or base on incompatible versions. Drivers are generally added more as an implementation to tick the boxes for a specific use case and therefore do usually not cover the available functionality.

`drogue-wifi` for instance implements a driver for the WiFi breakout board ESP8266. Yet, while the target chip is capable of a whole range of functionality, the driver only implements a limited subset of that such that it fits the drogue project. More extensive support would be desirable to use rust more ergonomically and ease the development.

### The bad

As a comparatively young language, especially compared to its contenders in the embedded world, naturally, Rust has a set of drawbacks mainly connected to its development pace and lack of maturity and experience in the industry.

#### Documentation (+/±)

Generally, Rust is known for its great documentation. There is even specialized tooling around it. Rustdoc[@noauthor_what_nodate-2] is the standard tool to generate documentation from rust source code. Docs.rs[@noauthor_docsrs_nodate] adds to this hosting documentation for the whole crates.io index of packages. The fact that all documentation is entangled this tightly is a major win to the whole community and aids development dramatically.

Rust has also evolved the mdBook[@noauthor_mdbook_nodate] tool. It is used throughout the rust community to assemble more high-level documentation and tutorials. For embedded the rust Community has published multiple of these books that aim to introduce programming Rust for low power hardware. Unfortunately, while covering the basics, once diverting from these or using newer technologies the amount of documentation is very sparse. At that point, it is mostly up to the developer of certain libraries to provide decent documentation. Yet, often development is faster than documentation, leading to outdated tutorials and examples. Additionally, many parts of the rust ecosystem are still mostly documented through blog posts. Although this is certainly not a bad thing in itself, as it shows that the interest is big, it is difficult to tell the recency or relevance of such articles. Likewise, there is little being written about how different projects are supposed to work together.

#### Stability (±)

As mentioned earlier, incompatibility especially at the abstraction level can be a problem. Indeed, `embedded-hal` is still considered unstable and while sticking to a semantic versioning protocol, must still be considered to break existing code without previous warning. Consequently, this means that the whole ecosystem around it is at most as stable as the abstraction layer they built upon. In reality, most libraries are yet to be considered incidental, meaning they were built to fit a need of someone at some time and are therefore not actively maintained or feature complete.

A few projects trying to build a stable foundation for certain functional units, such as the IoT network/cloud project `drogue`. Still, the interfaces that come to existence due to these projects aim foremost for internal compatibility.

#### Framework Interoperability (±)

Extending on the internal compatibility, it would be desirable to have projects interoperate cleanly. The aforementioned stability issues and fragmented ecosystem can often not be equalized by the overarching abstraction layer.

Moreover, documentation to integrate one library within the other is often hard to find as well. RTIC[@noauthor_preface_nodate] for example promises to provide a framework that works concurrently by managing interrupts and resources. Yet, it brings in a very different way to set up projects that complicate how to get started with the whole system especially as examples are scarce here too.

### The ugly

Lastly, some tasks are so far virtually impossible to achieve with Rust or require a lot of work to be invested by the user.

#### ~~ARM~~ (-)

We have seen that on ARM Rust offers great support. Indeed the stm32-rs group, for example, tries to provide embedded-has implementations for all chips manufactured by STM. Other ARM vendors have similar good support.

It is once one asks to write software for other architectures. First, they are limited by support for these architectures of the LLVM backend. The popular IoT platform Arduino for instance runs on AVR based chips which require a fork of LLVM to be programmed for In Rust. Extensa, the architecture employed by the ESP8266/ESP32 chips also requires additional care. These reasons drive the current disinterest in writing software for these chips (as it is not possible or difficult) which in turn affects the motivation to bring support for the platforms to Rust in the first place.

| Ecosystem element              | Maturity |
|--------------------------------|----------|
| Performance and Ergonomics     | ++       |
| Tools                          | ++       |
| Frameworks and Libraries       | +        |
| Documentation (Rust)           | +        |
| Documentation (Libraries)      | ±        |
| Stability                      | ±        |
| Framework Interoperability     | ±        |
| Architecture Support (not ARM) | -        |

: Summary of Rust's ecosystem today

## In the future

Today, Rust's ecosystem is not all roses. A lot of things are missing still. Yet, observing the community fundamental steps are being taken. Looking at the future there are a few clear indicators that Rust will become a growing influence for not and ubiquitous computing.

### Rust Foundation

More generically, the advent of the Rust Foundation [@noauthor_laying_nodate] will eventually also benefit the embedded section of the language. With greater structural organization and backing from industry-leading companies such as Google and Amazon, Microsoft, Huawei and Facebook, Rust manifests itself as a credible choice.

### Embedded-WG

The embedded working group has been mentioned throughout this essay. They have proven themselves as the originators of remarkable work that has brought rust a long way. Its projects will for sure continue to improve the experience of embedded development in Rust.

### Ferrous

Ferrous Systems, the leading force behind the excellent knurling project, is committed to further invest in Rusts embedded future. Ferrous already contributes to the Rust Open Source community in many ways, and with knurling still being a young project, its influence in the embedded world is still to be expanded.

### Libraries/Tools

Finally, with more companies and individuals committing to Rust as their language of choice, albeit its current pitfalls will eventually populate the language with more helpful libraries and tools. Especially, once fundamental libraries such as the `embedded-hal` a point of stability, libraries building on these are expected to follow.

Taking into account WebAssembly, we see that with its adoption in all major browsers and the stabilization of its specification, WASM is there to stay. Rust has been influencing its development a lot until today

# Conclusion

Throughout this essay, the domain of Ubiquitous Computing and IoT has been described in detail. Building on that the programming language Rust has been closely examined on its capabilities to fulfil the needs of this field. We have seen which infrastructure drives Rust's support of embedded programming, how WebAssembly enables Rust to play a leading role as a language to implement functions on the Edge. Apart from this descriptive part, we put the available ecosystem into perspective, pointing out its strengths and current weaknesses.

Finally, we can conclude that Rust is perfectly capable of doing specific tasks, in the area of Embedded Computing and more so on higher levels of the Internet of Things, such as lightweight computing on the edge and the implementation of backend services. Yet, it shows that Rust is a fairly recent language. As such parts of its ecosystem, relevant to IoT, are still evolving, lack even some significant foundations and are far from stable. For early adopters and the generally curious Rust still offers the foundations on which one can build their own solutions, albeit without providing the maturity of decades of development. On the bright side, we see several companies like Drogue, Ferrous Systems and other independent groups, doing exactly that. Crucial foundations are in active development and promise a brighter future for Rust.

Condensing this essay into one sentence

> Rust shows the potential to become the IoT platform of choice in the future, providing speed, ergonomics and safety, but does not show the maturity to be readily used as such without thorough consideration.

... Wow! You made it until here. Thank you a lot for your time and interest. If you have any thoughts about this post after all, please feel free to reach out to me.

# References
