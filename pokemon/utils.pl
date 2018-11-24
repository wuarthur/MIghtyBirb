
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
