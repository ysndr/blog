---
title: Agent Communication
subtitle: Welcome to this Blog
tags:
date: 2020-01-02
description: Introduction and what this is about
image: https://unsplash.com/photos/6HdfrD20rTk/download?w=1600
status: published
image-credits: |
    Image taken by <a style="background-color:black;color:white;text-decoration:none;padding:4px 6px;font-family:-apple-system, BlinkMacSystemFont, &quot;San Francisco&quot;, &quot;Helvetica Neue&quot;, Helvetica, Ubuntu, Roboto, Noto, &quot;Segoe UI&quot;, Arial, sans-serif;font-size:12px;font-weight:bold;line-height:1.2;display:inline-block;border-radius:3px" href="https://unsplash.com/@ericmuhr?utm_medium=referral&amp;utm_campaign=photographer-credit&amp;utm_content=creditBadge" target="_blank" rel="noopener noreferrer" title="Download free do whatever you want high-resolution photos from Eric Muhr"><span style="display:inline-block;padding:2px 3px"><svg xmlns="http://www.w3.org/2000/svg" style="height:12px;width:auto;position:relative;vertical-align:middle;top:-2px;fill:white" viewBox="0 0 32 32"><title>unsplash-logo</title><path d="M10 9V0h12v9H10zm12 5h10v18H0V14h10v9h12v-9z"></path></svg></span><span style="display:inline-block;padding:2px 3px">Eric Muhr</span></a> on Unsplash
...

[hello](./resource.txt)

> **Why** do agents need to be coordinated and **how to do so**? Discuss common **coordination techniques** such as
>
> - **organizational structures**
> - **meta-level information exchange**
> - **Multi-agent planning**
> - **explicit analysis and synchronization**
> - **Coordination inspired by human teamwork**
> - **commitments and conventions**

# Coordination

> Reason about **own actions** and **actions of others** to keep the **community act coherently**

- Prevent **chaos**
- Meet global **constraints**
- Distribute **information**, **resources**
- **Dependent actions**
- Greater **efficiency** due to **less redundancy**

## Coherent behaviour

> Coherent behavior can be achieved through **complete knowledge** where all agents **know about every other** and **mutually adjust** each others actions. Agents can also be **directly supervised** by one **central** entity or multiple **distributed entities**.

## Control decisions

> Decisions **what actions to take** from a **choice** of actions, informed by **control knowledge**. Outcome depends on **control regime** which takes into account **alternatives**, **criteria** and **procedure**.

> If control choice becomes more complicated decisions become more **numerous**, and need to become **asynchronous** and **decentralized** to ensure efficiency.

### Kinds of control

> **network/cooperative control** will lead to good overall performance taking into account **communicated information**, while **local control** leads to good **local decisions** based on **local information**,

### Network Control

- Aggregated from multiple agents
- Concerns relationships between agents

Utilized to influence ...

- Set of action alternatives
- Decision criteria
- Decision procedure

#### Example: allocation of search-space regions to agents

> Transfer the set of goals into a tree structure that describes the **dependencies between goals and goal as well as goals and resources**.
>
> The tree is used to find a desired state of the environment that ultimately build on related goals.
>
> Subtrees of this problem can be allocated to different agents where each agents tries to solve its part of the tree.

### Local Control

Concerning only single nodes with their own search space region.

**network** can influence control uncertainty but better local control may uncover information to focus network control decisions on.



### Control uncertainty

![image-20210101162513724](assets/image-20210101162513724.png)

#### Solution

Reduce degree of uncertainty and its impact by reducing common dependencies.

## Coordination steps

- defining the goal graph
- assigning regions of search space
- controlling decisions about which areas of the graph to explore
- traversing the goal structure satisfying dependencies
- ensuring report of the successful traversal

## Coordination techniques

![image-20210101163154296](assets/image-20210101163154296.png)

- Organisational Structures
  - Product hierarchy
  - Functional hierarchy
- Meta-level Information Exchange
  - e.g. Partial Global Planning (PGP), *(Durfee)*
- Multi-agent Planning
- Explicit Analysis and Syncronization
- Norms and social laws
- Coordination Models based on human teamwork:
  - Joint commitments *(Jennings)*
  - Mutual Modelling

### Organizational structures

> Associate agents with problem types, agents are responsible for problem instances of that type.

#### Product Hierarchie

![image-20210101165357727](assets/image-20210101165357727.png)

> Individual structures for all products so failures do not affect other products but lots of roles are set multiple times

#### Functional Hierarchy

![image-20210101165633239](assets/image-20210101165633239.png)

>  **Pool agents** of similar type into functional departments with **functional managers**.  Failures in task agents can be reallocated but mangers pose **single points of failure**

#### Decentralized Markets

![image-20210101170114623](assets/image-20210101170114623.png)

#### Centralized Markets

![image-20210101170453984](assets/image-20210101170453984.png)

> **Fewer messages** than decentralized markets (4)
>
> Similar to functional model

#### Comparison

![image-20210101170928803](assets/image-20210101170928803.png)

### Meta-level information exchange

> Share control level information about priorities and focus. Control level information can **change decisions of agents** but is transient and does n**ot specify which goals are considered**. They are therefore **imprecise** and only **valid for a limited amount of time**.

#### Partial Global Planning

1. Each agent decides what its own goals are and generates short-term plans in order to achieve them.
2. Agents exchange information to determine where plans and goals interact.
3. Agents alter local plans in order to better coordinate their own activities.

> Reach **common conclusions** by **exchanging local information**. Generate plans only for the local subproblem and merge local plans through exchange

> **Dynamically revise plans** cost-effectively in uncertain worlds.
>
> Yields **no guaranteed global coordination**

### Multi-Agent planning

> Generate **explicit plans**, that are **synchronized** and **arranged apriori**.

> **High amount of communication** required to exchange plans and local information. Hence, the **computational effort** is far greater as is the demand for **communication resources**.
>
> Requires agents to reach **mutual agreement** before they start operating, which is not the case fo PGP but comes with certain **guarantees**

#### Centralized planning for distributed plans

> Central instance generates plans for distributed agents that work together to fulfill a goal by synchronizing as needed.

#### Distributed Planning for centralised plans

> Central plan such as flight control rail traffic or manufacturing processes. Planning **happens on the distributed layer** and is **merged to one central plan**.

#### Distributed Planning for distributed plans

> Individuals generate their own local plan, or are assigned to indivisdually generated plans. Therefore, there is **no individual is aware of all other agents' actions** which makes it more difficult to detect and resolve undesirable interactions

### Explicit analysis and synchronization

- [ ] todo

### Social Norms and Laws

> **Norms** are **patterns of behavior** that are expected but **not enforced**
>
> **Social laws** are norms that **are enforced** and carry **authority**



### Coordination Models based on human teamwork

#### Mutual modelling

> Build a **model of other agents** and **coordinate own activities** based on that model.
>
> Cooperation without communication

#### Coordination with(-out) cooperation

**Without**: acting similarly due to an external reason

**With**: acting similarly because you have the goal to do so

#### Joint Intentions model

Based on teamwork model, requiring **joint commitment** for the global Ain as well s **individual commitment**.

Pursue own assigned task **and** the global goal.

##### **Teamwork:**

> **Cooperative effort** by team members towards a **common goal**

##### Commitment

> **Bound to a reason**, promise pursued until unfounded
>
> Therefore if the environment changes, **commitment may change** as well.
>
> **But**: Requires knowledge about each others state.

##### Conventions

Describes when to modify commitment.

- **Keep** - (retain)
- **Revise** - (rectify)
- **Remove** - (abandon)

````
If Commitment Satisfied OR
	 Commitment Unattainable OR
	 Motivation for Commitment no longer present
then
   terminate Commitment.
````

## Summary

- Ensure coherent behavior
- Coordination Techniques: Organisational structures
  - Meta-level information exchange
  - Multi-agent Planning
  - Social norms and laws
  -  Mutual Modelling
  - Joint Intentions
