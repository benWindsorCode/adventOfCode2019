∇Out←p8;In;Width;Height;Layers;Shaped;Zeros;ChosenLayer;NumOf
 In←⊃⊃⎕NGET'input/p8.txt' 1
 ⍝ Reshape to layers, height, width. Where layers = (⍴In)÷(width × height)÷
 Shaped←100 6 25⍴In
 ⍝ Number of zeroes on the alpha layer of omega
 Zeros←+/[2]+/[3]Shaped='0'
 ChosenLayer←Shaped[(⍸Zeros=⌊/Zeros);;]
 ⍝ Function taking a matrix, and returning number of elements equal to ⍺
 NumOf←{((+/)⍣2)⍵=⍺}
 Out←('1'NumOf ChosenLayer)×('2'NumOf ChosenLayer)
∇
