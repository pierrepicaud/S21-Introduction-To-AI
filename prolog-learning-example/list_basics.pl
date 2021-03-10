
% this is a pattern that separate head and tail, it's ... somthing special.
% try to run a few query to see what happens... like p([a, b, c], X, Z).
p([H|T], H, T).


% got 7/7 right :D
% [a, d, z, c] and [H|T]
% H = a, T = [d, z, c]
%
% [apple,pear,grape] and [A,pear|Rest]
% A = apple, Rest = [grape]
%
% [a|Rest] and [a,b,c]
% Rest = [b, c]
%
% [a,[]] and [A,B|Rest]
% A = a, B = [], Rest = []
%
% [One] and [two|[]]
% One = two
%
% [one] and [Two]
% Two = one
%
% [a,b,X] and [a,b,c,d] 
% doesn't match, for it to match, put a "|" infront of X




list_member(X,[X|_]).
list_member(X,[_|TAIL]) :- list_member(X,TAIL).

/*
This is a recursive call.

Say we want to find out whether b is a member of [a, b, c], we will make the
following query:
list_member(b, [a, b, c]).

first the program will check whether b is head or not, since b is not head,
the program will check whether b is in the body or not it will run:

list_member(b, [_|[b, c]]).

and the statement above is true when the the statement list_member(b, [b,c]). is true.
the program recursively remove the head until the list is empty. This is so fucking cool :)

*/









