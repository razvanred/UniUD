% demijohns(Number, DCapacity)
% batch(Capacity, BHeight, Diameter, Quantity)
% boxSize(Width, Length, OHeight, MinOccupancy)

bottleId(1..I) :-
I = #sum{Quantity, Capacity, BHeight, Diameter : batch(Capacity, BHeight, Diameter, Quantity)}.

bottle(I, Capacity, BHeight, Diameter) : batch(Capacity, BHeight, Diameter, Quantity) :-
bottleId(I).

:- Quantity < #count{I : bottle(I, Capacity, BHeight, Diameter)},
batch(Capacity, BHeight, Diameter, Quantity).

box(I, Width, Length, OHeight, MinOccupancy) : boxSize(Width, Length, OHeight, MinOccupancy) :-
bottleId(I).

overlap(Bottle1, Bottle2) :-
X1 < X2 + Diameter2,
Y1 < Y2 + Diameter2,
X1 + Diameter1 > X2,
Y1 + Diameter1 > Y2,
Bottle1 != Bottle2,
bottle(Bottle1, Capacity1, BHeight1, Diameter1),
bottle(Bottle2, Capacity2, BHeight2, Diameter2),
placed(Bottle1, Box, X1, Y1),
placed(Bottle2, Box, X2, Y2).

:- overlap(Bottle1, Bottle2).

{
    placed(Bottle, Box, X, Y) :
        X = 0..(Width - Diameter),
        Y = 0..(Length - Diameter),
        BHeight <= OHeight,
        box(Box, Width, Length, OHeight, MinOccupancy)
} 1 :-
bottle(Bottle, Capacity, BHeight, Diameter).

:- Occupancy*100/(Width*Length*OHeight) < MinOccupancy,
Occupancy > 0,
Occupancy = #sum{
    Capacity, Bottle : bottle(Bottle, Capacity, _, _), placed(Bottle, Box, _, _)
},
box(Box, Width, Length, OHeight, MinOccupancy).

:- UsedWine > AvailableWine,
UsedWine = #sum{Capacity, Bottle : bottle(Bottle, Capacity, _, _), placed(Bottle, _, _, _)},
AvailableWine = #sum{Number*DCapacity : demijohns(Number, DCapacity)}.

#maximize {Capacity@1, Bottle : bottle(Bottle, Capacity, _, _), placed(Bottle, _, _, _)}.

#minimize {1, Box : placed(_, Box, _, _)}.

#show bottle/4.
#show box/5.
#show placed/4.
