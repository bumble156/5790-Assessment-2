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
@#$#@#$#@
GRAPHICS-WINDOW
201
10
676
486
-1
-1
46.7
1
10
1
1
1
0
0
0
1
0
9
0
9
1
1
1
ticks
30.0

BUTTON
8
61
108
94
Setup Model
setup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
8
302
71
335
Go
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
8
97
187
130
corruption-base-payoff
corruption-base-payoff
0
100
20.0
1
1
NIL
HORIZONTAL

SLIDER
8
131
187
164
honesty-base-payoff
honesty-base-payoff
0
100
1.0
1
1
NIL
HORIZONTAL

SLIDER
8
165
187
198
size-of-memory
size-of-memory
0
100
5.0
1
1
NIL
HORIZONTAL

SLIDER
8
199
187
232
size-of-network
size-of-network
0
200
10.0
1
1
NIL
HORIZONTAL

SLIDER
8
233
187
266
jail-term
jail-term
1
100
7.0
1
1
NIL
HORIZONTAL

SLIDER
8
267
187
300
reports-for-jail
reports-for-jail
1
100
6.0
1
1
NIL
HORIZONTAL

BUTTON
124
302
187
335
Go
go
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

INPUTBOX
7
357
186
417
show-info-on-turtle
3.0
1
0
Number

BUTTON
7
418
187
451
Show Info For Selected Turtle
show-info
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
7
452
187
485
Show Info For All Turtles
turtles-report
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

PLOT
681
10
1315
247
Total Corruption
Time
Corrupt Agents
0.0
100.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -2674135 true "" "plot count patches with[pcolor = yellow]"

PLOT
681
249
1314
486
Endogenous Transition Behaviour in Corruption
Iteration
Number of Agents
0.0
100.0
0.0
5.0
true
false
"" ""
PENS
"Corrupt Citizens" 1.0 0 -14730904 true "" "plot count citizens with [pcolor = yellow]"
"Corrupt Bureaucrats" 1.0 0 -1184463 true "" "plot count bureaucrats with [pcolor = yellow]"
"Jailed Citizens" 1.0 0 -5825686 true "" "plot count citizens with [pcolor = red]"
"Jailed Bureaucrats" 1.0 0 -11221820 true "" "plot count bureaucrats with [pcolor = red]"

SLIDER
7
10
179
43
number-of-agents
number-of-agents
10
1000
100.0
10
1
NIL
HORIZONTAL

@#$#@#$#@
## WHAT IS IT?

(a general understanding of what the model is trying to show or explain)

## HOW IT WORKS

(what rules the agents use to create the overall behavior of the model)

## HOW TO USE IT

(how to use the model, including a description of each of the items in the Interface tab)

## THINGS TO NOTICE

(suggested things for the user to notice while running the model)

## THINGS TO TRY

(suggested things for the user to try to do (move sliders, switches, etc.) with the model)

## EXTENDING THE MODEL

(suggested things to add or change in the Code tab to make the model more complicated, detailed, accurate, etc.)

## NETLOGO FEATURES

(interesting or unusual features of NetLogo that the model uses, particularly in the Code tab; or where workarounds were needed for missing features)

## RELATED MODELS

(models in the NetLogo Models Library and elsewhere which are of related interest)

## CREDITS AND REFERENCES

(a reference to the model's URL on the web if it has one, as well as any other necessary credits, citations, and links)
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

sheep
false
15
Circle -1 true true 203 65 88
Circle -1 true true 70 65 162
Circle -1 true true 150 105 120
Polygon -7500403 true false 218 120 240 165 255 165 278 120
Circle -7500403 true false 214 72 67
Rectangle -1 true true 164 223 179 298
Polygon -1 true true 45 285 30 285 30 240 15 195 45 210
Circle -1 true true 3 83 150
Rectangle -1 true true 65 221 80 296
Polygon -1 true true 195 285 210 285 210 240 240 210 195 210
Polygon -7500403 true false 276 85 285 105 302 99 294 83
Polygon -7500403 true false 219 85 210 105 193 99 201 83

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

wolf
false
0
Polygon -16777216 true false 253 133 245 131 245 133
Polygon -7500403 true true 2 194 13 197 30 191 38 193 38 205 20 226 20 257 27 265 38 266 40 260 31 253 31 230 60 206 68 198 75 209 66 228 65 243 82 261 84 268 100 267 103 261 77 239 79 231 100 207 98 196 119 201 143 202 160 195 166 210 172 213 173 238 167 251 160 248 154 265 169 264 178 247 186 240 198 260 200 271 217 271 219 262 207 258 195 230 192 198 210 184 227 164 242 144 259 145 284 151 277 141 293 140 299 134 297 127 273 119 270 105
Polygon -7500403 true true -1 195 14 180 36 166 40 153 53 140 82 131 134 133 159 126 188 115 227 108 236 102 238 98 268 86 269 92 281 87 269 103 269 113

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.0.1
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180
@#$#@#$#@
0
@#$#@#$#@
