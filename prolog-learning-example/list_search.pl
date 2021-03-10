% Is it in the list.

% basecase it's the head
on(Item, [Item|Rest]).

% otherwise, recursively pops the head
on(Item, [_IgnoreHead|Tail]) :- on(Item, Tail).

