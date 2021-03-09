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
