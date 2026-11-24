is_left_of(waldo,odlaw).
is_left_of(odlaw,swi).
is_left_of(swi,wilma).
is_left_of(whitebeard,woof).
is_left_of(woof,watchers).
is_below_of(whitebeard,waldo).
is_below_of(woof,odlaw).
is_below_of(watchers,swi).
is_right_of(X,Y) :- is_left_of(Y,X).
is_above_of(X,Y) :- is_below_of(X,Y).
