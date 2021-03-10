% Given facts:
taller(bob, mike).
taller(mike, jim).
taller(jim, george).

%  Write a recursive program that will determine that Bob's height
%  is greater than George's. 

% base case
taller_than(X, Y) :- taller(X, Y).

taller_than(X,Y) :-
	taller(X,Z),
	taller_than(Z,Y).

% note: Worked after adding the base case.
