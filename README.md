# crest18-puzzle
This is repository to solve puzzle that is crest from 18 sticks.  

## Solved puzzle
![Solved 18 parts crest](./solved.jpg "Solved 18 parts crest puzzle")

## Parts
![Parts](./parts.jpg "Parts")

On the image with parts parts without number (second rows) should have number 17 on them.  
For each part there is (n+8) on the other side.  

## Results
File [result.txt](./result.txt) contains all results program found. And proper result is `Variant 126`:
```
Variant: 126
Top
25 A2	8 A1	17 A2
13 A1	4 A0	17 A2
Left
25 A2	2 A3	1 A2
17 A0	7 A3	25 A0
Right
17 A1	25 A3
14 A0	11 A2
25 A1	17 A3
End
```
I didn't try varaiants 127-138.

## How to read variants

Check [image](./crest-18-puzzle.png), so top, left and right are just as it's on the image. And numbers are from [this image](./parts.jpg).  
So A0, A1, A2, A3 and A4 are angles. A0 is no rotation. A1 90 clockwise and so on.
