% Given the following list of cities and features


% Write a program called member, that would be able to see if 
% something was a member of a list. The program would inform me 
% that sydney_opera_house was not on list by failing,
% yet confirm all the above were on the list by succeeding
% (answering yes).  

% created a new rule instead of just passing it through the funtor as 
% an argument.
run :-
	X = [london_buckingham_palace,
	paris_eiffel_tower, york_minster, 
	pisa_leaning_tower, athens_parthenon],
	member1(H|X).
	
member1(H, [H|T]).
member1(H, [_|T]):-
	 member1(H, T).


