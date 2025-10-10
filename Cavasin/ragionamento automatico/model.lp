% batch(capacity, height, diameter, quantity)



{ bottle(Capacity, Height, Diameter, 0..X, 0..Y) } < Quantity :-
    X = #max { Width : boxSize(Width, Length, Height, MinCoverage) },
    Y = #max { Length : boxSize(Width, Length, Height, MinCoverage) },
    batch(Capacity, Height, Diameter, Quantity).

0 {  } 1 :- bottle(Capacity, Height, Diameter, X, Y).
