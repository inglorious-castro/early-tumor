extensions [rnd]

breed [cancer-cells cancer-cell]
breed [immune-cells immune-cell]

globals [ticks-without-cancer kills-this-turn base-grey
  cytokine-diffusion-rate cytokine-degradation-rate
  min-pdiv min-pev min-muta max-pdiv max-pev max-muta init-pdiv init-pev init-muta]
cancer-cells-own [muta base-pdiv pev]
immune-cells-own [kills failed] ; yet to put to good use
patches-own [cytokines]

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to define-globals
  set min-pdiv 0.0    set max-pdiv 0.3    set init-pdiv 0.02
  set min-pev 0.0     set max-pev 0.9     set init-pev 0.01
  set min-muta 0.001  set max-muta 0.5    set init-muta 0.03

  set cytokine-diffusion-rate 0.5
  set cytokine-degradation-rate 0.02

  set base-grey 30

  set ticks-without-cancer 0 ;; to register how many times elimination was successful

  set-default-shape cancer-cells cancer-shape
  set-default-shape immune-cells immune-shape
end


;; distribution employed for mutations (truncated normal with rejection sampling)
to-report random-normal-trunc [m sd llim rlim]
  let value random-normal m sd
  while [value < llim or value > rlim] [
    set value random-normal m sd
  ]
  report value
end


to-report rb-color [r-attr b-attr]
  let r-factor (255 - base-grey) / (max-pdiv - min-pdiv)
  let b-factor (255 - base-grey) / (max-pev - min-pev)
  let r-val round (base-grey + r-factor * r-attr)
  let b-val round (base-grey + b-factor * b-attr)
  report (list r-val base-grey b-val)
end


to-report mutate-pdiv [ref mut]
  let eff-muta mut * (max-pdiv - min-pdiv)  ; normalized for the feature range (does it make sense?)
  report precision (random-normal-trunc ref eff-muta min-pdiv max-pdiv) 5
end

to-report mutate-pev [ref mut]
  let eff-muta mut * (max-pev - min-pev)  ; normalized for the feature range (does it make sense?)
  report precision (random-normal-trunc ref eff-muta min-pev max-pev) 5
end

to-report mutate-muta [ref mut]
  let eff-muta mut * (max-muta - min-muta)  ; normalized for the feature range (does it make sense?)
  report precision (random-normal-trunc ref eff-muta min-muta max-muta) 5
end


to mutate-attributes
  let parent-pdiv [base-pdiv] of myself
  let parent-pev [pev] of myself
  let parent-muta [muta] of myself

  set base-pdiv mutate-pdiv parent-pdiv parent-muta
  set pev mutate-pev parent-pev parent-muta
  set muta mutate-muta parent-muta parent-muta
end


to seed-cancer
  let empty-patches patches with [not any? turtles-here]
  let relevant-patch ifelse-value (seed-cancer-randomly?) [one-of empty-patches] [patch 0 0]

  ask relevant-patch [
    sprout-cancer-cells 1 [
      set base-pdiv init-pdiv
      set pev init-pev
      set muta init-muta
      set color rb-color base-pdiv pev
    ]
  ]
end


to sprout-immune [how-many]
  let empty-patches patches with [not any? turtles-here]
  let available min (list how-many (count empty-patches))
  ask n-of available empty-patches [
    sprout-immune-cells 1 [
      set color yellow
      set kills 0
      set failed 0
    ]
  ]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to setup
  clear-all
  define-globals
  ask patches [set cytokines 0.0]
  seed-cancer
  sprout-immune immune-baseline
  reset-ticks
end


to go
  set kills-this-turn 0
  keep-things-interesting
  update-cytokines
  resolve-complexes
  move-immune
  form-complexes
  remove-immune
  recruit-immune
  proliferate-cancer
  tick
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to proliferate-cancer
  ask cancer-cells [
    let empty-neighbors neighbors with [not any? turtles-here]
    let pdiv base-pdiv * (1 - count cancer-cells / max-size)

    if any? empty-neighbors and random-float 1.0 < pdiv [
      hatch 1 [
        mutate-attributes
        set color rb-color base-pdiv pev
        move-to one-of empty-neighbors
      ]
    ]
  ]
end


to form-complexes
  let free-immune-cells immune-cells with [not any? link-neighbors]

  ask free-immune-cells [
    let cancer-neighbors cancer-cells-on neighbors
    let free-cancer-neighbors cancer-neighbors with [not any? link-neighbors]

    if any? free-cancer-neighbors [
      let target one-of free-cancer-neighbors
      if random-float 1.0 > ([pev] of target) [
        create-link-with target
      ]
    ]
  ]
end


to move-immune
  let free-immune-cells immune-cells with [not any? link-neighbors]

  ask free-immune-cells [
    let empty-neighbors neighbors with [not any? turtles-here]
    if any? empty-neighbors [
      let chosen-patch rnd:weighted-one-of empty-neighbors [cytokines]
      move-to chosen-patch
    ]
  ]
end

to resolve-complexes
  let linked-immune-cells immune-cells with [any? link-neighbors]

  ask linked-immune-cells [
    ifelse random-float 1.0 < pkill [
      ask link-neighbors [die]
      ask patch-here [
        set cytokines cytokines + release-per-kill
      ]
      set kills kills + 1
      set kills-this-turn kills-this-turn + 1
    ] [
      ask my-links [die]
      set failed failed + 1
    ]
  ]
end


to update-cytokines
  let diffusion-rate cytokine-diffusion-rate
  let degradation-rate cytokine-degradation-rate

  diffuse cytokines diffusion-rate
  ask patches [
    set cytokines precision (cytokines * (1 - degradation-rate)) 7

    ifelse visualize-cytokines? [
      set pcolor 40 + 2.5 * (cytokines / release-per-kill)
    ] [
      set pcolor black
    ]
  ]
end


to recruit-immune
  let empty-patches patches with [not any? turtles-here]

  let below-baseline immune-baseline - count immune-cells
  if below-baseline > 0 [
    sprout-immune below-baseline
  ]

  let requested kills-this-turn
  sprout-immune requested
end


to remove-immune
  ask immune-cells [
    if random-float 1.0 < removal-factor [die]
  ]
end


to keep-things-interesting
  if not any? cancer-cells [
    ifelse ticks-without-cancer > 5 [
      seed-cancer
      set ticks-without-cancer 0
    ] [
      set ticks-without-cancer ticks-without-cancer + 1
    ]
  ]
end
@#$#@#$#@
GRAPHICS-WINDOW
211
11
624
425
-1
-1
5.0
1
10
1
1
1
0
1
1
1
-40
40
-40
40
1
1
1
ticks
30.0

BUTTON
28
14
94
47
NIL
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
96
14
159
47
NIL
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
6
296
179
329
immune-baseline
immune-baseline
0
100
10.0
1
1
NIL
HORIZONTAL

PLOT
6
54
206
204
plot 1
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"pen-cancer" 1.0 0 -11783835 true "" "plot count cancer-cells"
"pen-immune" 1.0 0 -7171555 true "" "plot count immune-cells"

MONITOR
14
210
103
255
immune
count immune-cells
17
1
11

MONITOR
107
210
197
255
cancer
count cancer-cells
17
1
11

PLOT
632
10
832
160
plot 2
NIL
NIL
0.0
10.0
0.0
1.0
true
false
"" ""
PENS
"default" 1.0 0 -14730904 true "" "plot mean [pev] of cancer-cells / (max-pev - min-pev)"
"pen-1" 1.0 0 -8053223 true "" "plot mean [base-pdiv] of cancer-cells / (max-pdiv - min-pdiv)"

SLIDER
6
263
178
296
max-size
max-size
0
2000
2000.0
10
1
NIL
HORIZONTAL

SLIDER
5
368
177
401
pkill
pkill
0.0
1.0
1.0
0.01
1
NIL
HORIZONTAL

SLIDER
5
403
177
436
removal-factor
removal-factor
0
0.01
0.01
0.0005
1
NIL
HORIZONTAL

MONITOR
631
165
800
210
NIL
mean [muta] of cancer-cells
4
1
11

SLIDER
6
332
178
365
release-per-kill
release-per-kill
0
5.0
3.0
0.1
1
NIL
HORIZONTAL

SWITCH
632
214
813
247
visualize-cytokines?
visualize-cytokines?
0
1
-1000

CHOOSER
633
293
814
338
cancer-shape
cancer-shape
"dot" "square"
1

CHOOSER
633
341
814
386
immune-shape
immune-shape
"dot" "square"
0

SWITCH
632
249
814
282
seed-cancer-randomly?
seed-cancer-randomly?
0
1
-1000

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
NetLogo 6.4.0
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
