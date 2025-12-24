% demijohns(Number, DCapacity)
% batch(Capacity, BHeight, Diameter, Quantity)
% boxSize(Width, Length, OHeight, MinOccupancy)

bottle(1..Quantity, Capacity, BHeight, Diameter) :- batch(Capacity, BHeight, Diameter, Quantity).

box(1..I, Width, Length, OHeight, MinOccupancy) :-
I = #sum{Quantity, Capacity, BHeight, Diameter : batch(Capacity, BHeight, Diameter, Quantity)},
boxSize(Width, Length, OHeight, MinOccupancy).

eqBottles(B, Capacity, BHeight, Diameter, B, Capacity, BHeight, Diameter) :-
bottle(B, Capacity, BHeight, Diameter).

overlap(B1, Capacity1, BHeight1, Diameter1, B2, Capacity2, BHeight2, Diameter2) :-
X1 < X2 + Diameter2,
Y1 < Y2 + Diameter2,
X1 + Diameter1 > X2,
Y1 + Diameter1 > Y2,
not eqBottles(B1, Capacity1, BHeight1, Diameter1, B2, Capacity2, BHeight2, Diameter2),
placed(B1, Capacity1, BHeight1, Diameter1, O, Width, Length, OHeight, MinOccupancy, X1, Y1),
placed(B2, Capacity2, BHeight2, Diameter2, O, Width, Length, OHeight, MinOccupancy, X2, Y2).

:- overlap(B1, Capacity1, BHeight1, Diameter1, B2, Capacity2, BHeight2, Diameter2).

{
    placed(B, Capacity, BHeight, Diameter, O, Width, Length, OHeight, MinOccupancy, X, Y) :
        X = 0..(Width - Diameter),
        Y = 0..(Length - Diameter),
        BHeight <= OHeight,
        box(O, Width, Length, OHeight, MinOccupancy)
} 1 :-
bottle(B, Capacity, BHeight, Diameter).

:- Occupancy*100/(Width*Length*OHeight) < MinOccupancy,
Occupancy > 0,
Occupancy = #sum{
    Capacity, B, BHeight, Diameter : placed(B, Capacity, BHeight, Diameter, O, Width, Length, OHeight, MinOccupancy, _, _)
},
box(O, Width, Length, OHeight, MinOccupancy).

:- UsedWine > AvailableWine,
UsedWine = #sum{Capacity, B, BHeight, Diameter : placed(B, Capacity, BHeight, Diameter, _, _, _, _, _, _, _)},
AvailableWine = #sum{Number*DCapacity : demijohns(Number, DCapacity)}.

#maximize {Capacity@1, B, BHeight, Diameter : placed(B, Capacity, BHeight, Diameter, _, _, _, _, _, _, _)}.

#minimize {1, O, Width, Length, OHeight, MinOccupancy : placed(_, _, _, _, O, Width, Length, OHeight, MinOccupancy, _, _)}.

#show placed/11.
