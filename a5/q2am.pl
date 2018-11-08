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

