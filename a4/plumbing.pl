%%% Prolog program for the simplified plumbing example....

% Copyright David L Poole 2018.
% This work is licensed under a Creative Commons
% Attribution-NonCommercial-ShareAlike 4.0 International License.
% See: http://creativecommons.org/licenses/by-nc-sa/4.0/deed.en_US.

% The dynamic declaration prevents undefined predicate errors.
:- dynamic on_t1/0, on_t2/0, off_t1/0, off_t2/0, plugged_bath/0,
	   unplugged_bath/0.

% See assignment description for the intended interpretation of the symbols
pressurized_p1.
pressurized_p2 :- on_t1, pressurized_p1.
pressurized_p3 :- on_t1, pressurized_p1.

% hot water pipes
pressurized_Hp1.
pressurized_Hp2 :- on_Ht1, pressurized_Hp1.
pressurized_Hp3 :- on_Ht1, pressurized_Hp1.

%hot water flows
flow_shower :- on_t2, pressurized_Hp2.
flow_sink :- on_t3, pressurized_Hp3.

flow_shower :- on_t2, pressurized_p2.
flow_sink :- on_t3, pressurized_p3.
flow_d2 :- wet_bath, unplugged_bath.
flow_d3 :- wet_sink, unplugged_sink.
flow_d1 :- flow_d2.
flow_d1 :- flow_d3.

wet_bath :- flow_shower.
wet_bath :- overFlow_bath.
wet_floor :- flow_sink.
wet_floor :- overFlow_sink.

%overFlow_i means if i overflows
overFlow_sink :- plugged_sink, flow_sink.
overFlow_bath :-plugged_bath, flow_shower.



% You can change the status of taps and plugs by adding or removing (commenting out) statements:
on_t1.
on_t2.
unplugged_bath.

