:- dynamic am/2, pm/2, before/2.

%am(1, 50).
%am(2, 50).
%
%pm(1,50).
%pm(2,50).

before(am(_,_), pm(A,B)) :- true.
before( pm(A,B),am(_,_)) :- false.

before( pm(H1,M1),pm(H2,M2)) :-
    H1 < H2 -> true;
    H2 < H1 -> false;
    M1 < M2 -> true; false.


next(am(_,_), pm(A,B)) :- false.
next( pm(A,B),am(_,_)) :- false.

next( pm(H1,M1),pm(H2,M2)) :-
    M1 < M2 -> false;
    M2 < M1 -> false;
    H1 is (H2 - 1) -> true; false.

next( am(H1,M1),am(H2,M1)) :-
    H1 is (H2 - 1) -> true; false.

next_hour(am(12,X),am(1,X)).
next_hour(am(N,X),am(N1,X)) :- N>0, N<11, N1 is N+1.
next_hour(am(11,X),pm(12,X)).
next_hour(pm(12,X),pm(1,X)).
next_hour(pm(N,X),pm(N1,X)) :- N>0, N<11, N1 is N+1.


same([], []).

same([H1|R1], [H2|R2]):-
    H1 = H2,
    same(R1, R2).

del1(E, [], []) :- false.
del1(E, E, []).
del1(E, _, []) :- false.
del1(E, [], _) :- false.
del1(E , [H|T], [H2|T2]) :-
    H = H2 -> del1(E, T, T2);
    E = H -> same(T, T2); del1(E, T, T2).
%    false.


replace(_, _, [], []).
replace(Old, New, [Old | L], [New | R]) :-
    replace(Old, New, L, R).
replace(Old, New, [Head | L], [Head | R]) :-
    dif(Old,Head),
    replace(Old, New, L, R).
