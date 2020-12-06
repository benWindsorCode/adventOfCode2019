∇Out←p8second;In;Shaped;Colour;Coords;Image
 In←⊃⊃⎕NGET'input/p8.txt' 1
 Shaped←100 6 25⍴In
 ⍝ Given an array, remove the twos then pull the 'top' layer value remaining
 Colour←{{⊃⍵[⍸⍵≠'2']}Shaped[;⍺;⍵]}
 Coords←(⍳6)∘.,(⍳25)
 ⍝ Apply the colour function accross each coord, then convert the 0,1 into space and quad characters for readability
 Image←(⊃¨Coords)Colour¨((⊃⌽)¨Coords)
 Out←' ⎕'[1+⍎¨Image]
∇
