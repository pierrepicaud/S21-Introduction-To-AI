fun(X) :- red(X), car(X).            /* and a car */

fun(X) :- blue(X), bike(X).           /* and a bike */

/* database of red items */
red(apple_1).
red(block_1).
red(car_27).



/* database of cars */
car(desoto_48).
car(edsel_57).



/* database of blue items */
blue(flower_3).
blue(glass_9).
blue(honda_81).



/*Db of bikes*/
bike(iris_8).
bike(my_bike).
bike(honda_81).

on_route(rome).

on_route(Place):-
    move(Place,Method,NewPlace),
    on_route(NewPlace).

move(home,taxi,halifax).
move(halifax,train,gatwick).
move(gatwick,plane,rome).
