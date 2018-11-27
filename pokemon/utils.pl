
%% Here be domain agnostic func

rep_list(X, N, List):-
  length(List, N),
  maplist(=(X), List).


take(N, List, Res):-
  length(Res, N),
  append(Res, _, List).

count(E, List, N):-
  include(=(E), List, Tmp),
  length(Tmp, N).


product_list(List, Product):-
  foldl(foldl_product, List, 1, Product).

foldl_product(Curr, Acc, Next):-
  Next is Curr * Acc.

zip(X, Y, Z):-
  maplist(zip_helper, X, Y, Z).

zip_helper(X, Y, Z):-
  Z = [X,Y].

sort_zipped(S, Sorted):-
  predsort(compare_zipped, S, Sorted).

compare_zipped(R, [A, _], [B, _]):-
  compare(R, A, B).
