# README #

This is a haskell based RFXCom to MQTT bridge. It converts to and from the RFXCom Messages to JSON structures and publishes or subscribes on them from the MQTT broker.

**Work still in progress!**


### What is this repository for? ###

* This contains the source code for the RFXCom to MQTT Bridge and also known issues and plans for current and future releases of the application.

### How do I get set up? ###

* Clone the current repository to your computer.
* Get the stack tool.
* Set up the stack environment by issuing **stack setup** in the RFXCom directory.
* Run **stack build**
* Execute it with **stack exec rfxcom**

### Contribution guidelines ###

_Toolchain:_

* I use emacs and I have installed **intero** and **magit**
* I have setup so emacs can use **cabal, hasktags, hlint** and **stylish-haskell** in the search path. For instance in **~/.local/bin.**

_What to think consider:_

* Before checking in run **stylish-haskell** ( from emacs **M-x haskdell-mode-stylish-buffer**)

### Who do I talk to? ###

* Repo owner or admin, tomas.stenlund@telia.com.

# Klassdiagram

```mermaid
classDiagram
Class01 <|-- AveryLongClass : Cool
Class03 *-- Class04
Class05 o-- Class06
Class07 .. Class08
Class09 --> C2 : Where am i?
Class09 --* C3
Class09 --|> Class07
Class07 : equals()
Class07 : Object[] elementData
Class01 : size()
Class01 : int chimp
Class01 : int gorilla
Class08 <--> C2: Cool label
```

# Sekvensdiagram

```mermaid
sequenceDiagram
    participant Alice
    participant Bob
    Alice->>John: Hello John, how are you?
    loop Healthcheck
        John->>John: Fight against hypochondria
    end
    Note right of John: Rational thoughts <br/>prevail!
    John-->>Alice: Great!
    John->>Bob: How about you?
    Bob-->>John: Jolly good!
```

# Flödesdiagram


```mermaid
graph TD;
    A-->B;
    A-->C;
    B-->D;
    C-->D;
```


# Statediagram
```mermaid
stateDiagram-v2
    [*] --> Still
    Still --> [*]
%% this is a comment
    Still --> Moving
    Moving --> Still %% another comment
    Moving --> Crash
    Crash --> [*]
```


```mermaid
erDiagram
          CUSTOMER }|..|{ DELIVERY-ADDRESS : has
          CUSTOMER ||--o{ ORDER : places
          CUSTOMER ||-->{ INVOICE : "liable for"
          DELIVERY-ADDRESS ||--o{ ORDER : receives
          INVOICE ||--|{ ORDER : covers
          ORDER ||--|{ ORDER-ITEM : includes
          PRODUCT-CATEGORY ||--|{ PRODUCT : contains
          PRODUCT ||--o{ ORDER-ITEM : "ordered in"
  ```
