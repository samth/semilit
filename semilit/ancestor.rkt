#lang semilit datalog

Semi-literate programming works for datalog as well

This is the data portion -- we just declare some basic facts.
> parent(john, douglas).
> parent(mary, john).

The `ancestor` relation computes the transitive closure of the 
`parent` relation.

> ancestor(A, B) :-
>   parent(A, B).
> ancestor(A, B) :-
>   parent(A, C),
>   ancestor(C, B).

Now we can ask a question.

> ancestor(mary,A)?

And then we get answers.