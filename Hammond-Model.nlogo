globals [
  choices           ;; The list of choices for an action
]

turtles-own[
  honesty           ;; The agents predisposition to honesty
  network           ;; The agents social network of friends
  memory            ;; List of past interactions for each agent
  jail-remaining    ;; Amount of time an agent has left in jail
  corrupt-prev?     ;; If the agent was corrupt in the last round
  reports           ;; Total number of times reported to the authorities since last jailed
]

bureaucrats-own[
  chosen?           ;; If the bureaucrat is already playing a citizen that round
]

breed [citizens citizen] ;; Define the citizens
breed [bureaucrats bureaucrat] ;; Define the bureaucrats

;; Set up the model
to setup
  clear-all
  set-default-shape turtles "person"
  resize-world 0 9 0 (number-of-agents / 10) - 1 ;; Resize world so it is large enough to accommodate every turtle on its own patch
  setup-globals
  setup-turtles
  reset-ticks
  calculate-decision
end

;; Create the global variable containing the choices for an action
to setup-globals
  set choices ["corrupt" "honest"]
end

;; Create and initialize the turtles
to setup-turtles

  ;; Create the citizen turtles
  create-citizens number-of-agents / 2 [
    set color white
    move-to-empty-location
  ]

  ;; Create the bureaucrat turtles
  create-bureaucrats number-of-agents / 2 [
    set color grey
    move-to-empty-location
    set chosen? false
  ]

  ;; Set up all turtle variables
  ask turtles[
    set honesty random-float 1
    set memory n-values size-of-memory [one-of choices]
    set jail-remaining 0
    set corrupt-prev? false
    set reports 0
    set label who
    set label-color black
  ]

  setup-networks
end

;; Create the network for each turtle out of its peers
to setup-networks

  ;; Create citizen networks
  ask citizens[
    set network[]

    ;; Correct for illogical user input for network size
    if size-of-network > count other citizens[
      set size-of-network count other citizens
    ]

    let friends n-of size-of-network other citizens
    foreach sort friends [ the-friend ->
      set network lput the-friend network
    ]
  ]

  ;; Create bureaucrat networks
  ask bureaucrats[
    set network[]
    let friends n-of size-of-network other bureaucrats
    foreach sort friends [ the-friend ->
      set network lput the-friend network
    ]
  ]
end

;; Space out turtles on the grid so that each has its own patch
to move-to-empty-location
  move-to one-of patches
  while [any? other turtles-here] [
    move-to one-of patches
  ]
end

;; To run the model itself
to go
  generate-links
  play
  enforce
  clear-links
  calculate-decision
  tick
end

;; To handle calculating the decision for a turtle on a particular go
to calculate-decision

  ;; Calculate the decision of each turtle in turn
  foreach sort turtles [ the-turtle ->
      ask the-turtle[
        ifelse jail-remaining != 0[set pcolor red] ;; If turtle is jailed, set its patch to red and disregard any further decision making
        [
          ;; Calculate the weighted corruption payoff based on a turtles honesty
          let xi ( 1 - honesty ) * corruption-base-payoff

          ;; Find the number of corrupt agents in a turtles memory
          let memory-corrupt occurrences "corrupt" memory

          ;; Variable holding a trtles percieved probability of encountering a corrupt agent
          let A 0

          ;; Sets probability, avoiding potential divide-by-zero errors when memory is set to zero
          if size-of-memory != 0
          [set A memory-corrupt / size-of-memory]

          ;; Hold the amount of turtles in network that are jailed/corrupt
          let friends-jailed 0
          let friends-corrupt 0

          ;; Scan through network updating jailed/corrupt values
          foreach network [ friend-turtle ->
            ask friend-turtle [
              if jail-remaining != 0 [
                set friends-jailed friends-jailed + 1
              ]
              if corrupt-prev? = true [
                set friends-corrupt friends-corrupt + 1
              ]
            ]
          ]

          ;; Variable holding the turtles percieved chance of being jailed
          let B 0

          ;; Sets probability, avoiding potential divide-by-zero errors if no friends are corrupt
          if friends-corrupt != 0
          [set B friends-jailed / friends-corrupt]

          ;; Calculate the turtles corruption payoff for the round based on A, B, x, y and k
          let corruption-payoff (1 - B) * ((A * xi) + (1 - A) * honesty-base-payoff) + B * (honesty-base-payoff - jail-term * honesty-base-payoff)

          ;; Reports on the intermediate and final values for each turtle
          ;;type who type " " type A type " " type B type " " type corruption-payoff type "\n"

          ;; Determine whether the agent will be corrupt or honest in the next round and set values accordingly
          ifelse honesty-base-payoff > corruption-payoff
          [
            set pcolor blue
          ]
          [
            set pcolor yellow
          ]

        ]
    ]
  ]
end

;; To create links between each citizen and a bureaucrat each round to symbolize the playing of the game
to generate-links

  ;; Only use citizens that are not in jail
  ask citizens with [jail-remaining = 0][
    let current who

    ;; Get a partner from the list of bureaucrats that is not jailed and has not already been picked
    let partner one-of bureaucrats with [chosen? = false and jail-remaining = 0]

    ;; If suitable bureaucrat has been found
    if (partner != nobody) [
      ask partner [
        ;; Link the two turtles
        create-link-with citizen current
        ask link who current[
          hide-link
        ]
        ;; Update bureaucrat so they cannot be picked again this round
        set chosen? true
      ]
    ]

  ]

  ;; Once all links are created, reset the bureaucrats for the next round
  ask bureaucrats[
    set chosen? false
  ]
end

;; Using the links created, play the game for each free turtle
;; As the citizens are created before the bureaucrats, they will always be end1 in the link
to play

  ;; Go through each link individually
  ask links[

    ;; Variables to hold the strategy of each player
    let citizen-decision "null"
    let bureaucrat-decision "null"

    ;; Get the decisions of the two payers from the patch colour
    ask end1[
      ifelse pcolor = blue[set citizen-decision "honest"][set citizen-decision "corrupt"]
    ]
    ask end2[
      ifelse pcolor = blue[set bureaucrat-decision "honest"][set bureaucrat-decision "corrupt"]
    ]

    ;; Find mismatches
    ifelse citizen-decision = "honest" and bureaucrat-decision = "honest"
    [
      ;;show "Honest transaction"
      ask both-ends[set corrupt-prev? false]
    ]
    [
      ifelse citizen-decision = "corrupt" and bureaucrat-decision = "corrupt"
      [
        ;;show "Corrupt transaction"
        ask both-ends[set corrupt-prev? true]
      ]
      [
        ifelse citizen-decision = "corrupt"
        [
          ;;show "Corrupt citizen"
          ;; Bureaucrat reports citizen to authorities
          ask end1[
            set reports reports + 1
            set corrupt-prev? true
          ]
          ask end2[
            set corrupt-prev? false
          ]
        ]
        [
          ;;show "Corrupt bureaucrat"
          ;; Citizen reports bureaucrat to authorities
          ask end1[
            set corrupt-prev? false
          ]
          ask end2[
            set reports reports + 1
            set corrupt-prev? true
          ]
        ]
      ]
    ]

    if size-of-memory > 0[
      ;; Update the memory for each agent to remove the oldest and add on the latest opponent decision
      ask end1[
        set memory but-first memory
        set memory lput bureaucrat-decision memory
      ]
      ask end2[
        set memory but-first memory
        set memory lput citizen-decision memory
      ]
    ]

  ]
end

;; Where the authorities update jail for corrupt turtles
to enforce
  ask turtles[

    ;; Decrease time remaining in jail for all incarcerated agents
    if jail-remaining > 0[
      set jail-remaining jail-remaining - 1
    ]

    ;; Arrest any agents that have exceeded the report threshold
    if reports = reports-for-jail[
      set jail-remaining jail-term
      set reports 0
    ]
  ]
end

;; To compose a report on the entire turtle agentset
to turtles-report
  type "Round " type ticks type "\n"
  foreach sort turtles [ the-turtle ->
    ask the-turtle [
      turtle-report
    ]
  ]
end

;; To compose a report on one turtle
to turtle-report
  type "Turtle " type who type ", "
  type "Network: " type network type ", "
  type "Honesty: " type honesty type ", "
  type "Memory: " type memory type ", "
  type "Jail Time: " type jail-remaining type ", "
  type "Corrupt Previously?: " type corrupt-prev? type ", "
  type "# of Reports: " type reports type "\n"
end

;; To compose a report on a specified turtle (as chosen in the input window)
to show-info
  let target turtle show-info-on-turtle
  ifelse target != nobody[
    ask target[
      type "Round " type ticks type "\n"
      turtle-report
    ]
  ]
  [type "No such turtle found\n"]
end

;; Reporter to count the number of occurrences of a word in a list
;; Used for calculating the number of corrupt agents in memory
to-report occurrences [x the-list]
  report reduce
    [ [occurrence-count next-item] -> ifelse-value (next-item = x) [occurrence-count + 1] [occurrence-count] ] (fput 0 the-list)
end
