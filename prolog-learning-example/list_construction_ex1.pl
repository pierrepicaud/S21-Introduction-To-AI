% Write a program to delete all reference of a particular item from a list.
% It should have three arguments.
% The list you wish to use, the item to delete, and the resulting list. 
% Here are some example of it behaviour:
% ?- delete_all([a,b,a,c,a,d],a,Result).
% Result = [b,c,d]
% ?- delete_all([a,b,a,c,a,d],b,Result).
% Result = [a,a,c,a,d]
% ?- delete_all([a,b,a,c,a,d],prolog,Result).
% Result = [a,b,a,c,a,d]

% list searching
on(Item,[Item|Rest]). 
on(Item,[DisregardHead|Tail]):-
	on(Item,Tail).

% Basecase
delete_all([], _, []).


% if fisrt element is not equal to the element to be deleted find
% it in the tail.
delete_all([H|T], X, [H|Result]):- H \= X,
        delete_all(T, X, Result).


% dis-regard the head
delete_all([_|Tail], X, Result) :-
        delete_all(Tail, X, Result).

% Was having trouble with the basecase, put the "!" in the wrong place:
% delete_all([], _, [])!.
% Need to think in terms of matching, not in terms of functions.
