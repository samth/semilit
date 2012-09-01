#lang semilit datalog

Semi-literate programming works for datalog as well

This is the data portion -- we just declare some basic facts.
> parent(john, douglas)

The `ancestor` relation computes the transitive closure of the 
`parent` relation.

> ancestor(A, B) :-
>   parent(A, B)
> ancestor(A, B) :-
>   parent(A, C),
>   ancestor(C, B)

And that's all she wrote.