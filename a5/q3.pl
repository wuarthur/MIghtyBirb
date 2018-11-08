% a)

lookup(X, Env, V) :- check(Env,X,V).


check([],X,V) :-false.
check([val(W,N)|T],X,V) :-
    X \= W -> check(T, X,V);
    N \= V -> check(T, X,V);
    true.



% b)

checkVar([],X,V) :-false.
checkVar([val(W,N)|T],X,V) :-
    number(X) -> true;
    X \= W -> check(T, X,V);
    true.

eval(A*B, Env, V):-
    eval(A, Env,V), eval(B, Env, V).

eval(A+B, Env, V):-
    eval(A, Env,V), eval(B, Env, V).


eval(Single, Env,V):-
    checkVar(Env, Single, V).
