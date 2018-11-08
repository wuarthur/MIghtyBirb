%%% Prolog program for the simplified plumbing example....

% Copyright David L Poole 2018.
% This work is licensed under a Creative Commons
% Attribution-NonCommercial-ShareAlike 4.0 International License.
% See: http://creativecommons.org/licenses/by-nc-sa/4.0/deed.en_US.

% The dynamic declaration prevents undefined predicate errors.
:- dynamic on_t1/0, on_t2/0, off_t1/0, off_t2/0, plugged_bath/0, pressurized_/1, on_t3,
	   unplugged_bath/0.

% See assignment description for the intended interpretation of the symbols
pressurized_p1.
pressurized_(p2) :- on_t1, pressurized_p1.
pressurized_(p3) :- on_t1, pressurized_p1.

%% hot water pipes
%pressurized_Hp1.
%pressurized_(hp2) :- on_Ht1, pressurized_Hp1.
%pressurized_(hp3) :- on_Ht1, pressurized_Hp1.
%
%%hot water flows
%flow_(shower) :- on_t2, pressurized_(hp2).
%flow_(sink) :- on_t3, pressurized_(hp3).

flow_(shower) :- on_t2, pressurized_(p2).
flow_(sink) :- on_t3, pressurized_(p3).
flow_(d2) :- wet_(bath), unplugged_bath.
flow_(d3) :- wet_(sink), unplugged_sink.
flow_(d1) :- flow_(d2).
flow_(d1) :- flow_(d3).

wet_(bath) :- flow_(shower).
wet_(bath) :- overFlow_(bath).
wet_(floor) :- flow_(sink).
wet_(floor) :- overFlow_(sink).

%overFlow_i means if i overflows
overFlow_(sink) :- plugged_sink, flow_(sink).
overFlow_(bath) :-plugged_bath, flow_(shower).



% You can change the status of taps and plugs by adding or removing (commenting out) statements:
on_t1.
on_t2.
on_t3.
unplugged_bath.

