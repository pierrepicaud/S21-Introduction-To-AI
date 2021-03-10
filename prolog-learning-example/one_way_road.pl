% town1----->-----town2---->----town3---->----town4--->----town5---->---town6
% A one way road links 6 towns. Write a program that can work out if you can travel on that road. For example. Here are two sample program behaviours. 


% add facts
is_one_way_to(town1, town2).
is_one_way_to(town2, town3).
is_one_way_to(town3, town4).
is_one_way_to(town4, town5).
is_one_way_to(town5, town6).


% basecase
can_get(X, Y) :-
	is_one_way_to(X, Y).


% recusion
can_get(X, Y) :-
	is_one_way_to(X, Z),
	can_get(Z, Y).
