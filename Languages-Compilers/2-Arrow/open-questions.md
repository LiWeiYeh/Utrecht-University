
# Open questions

## Exercise 4
Happy is more efficient at parsing left-recursive rules because they result in a constant stack-space parser, whereas right-recursive rules require
stack space proportional to the length of the list being parsed. So Happy uses left-recursive rules so that it doesn't run out of stack space.

Trying to parse a left-recursive grammar does not work at all in parser combinators. This is because a left-recursive grammar refers to itself e.g. 'a = a <*> z'. 'a' Calls a itself without consuming anything, so we will get an infinite loop.


## Exercise 10
When calls are in the middle of a command, it's creating more stack space due to commands not being
handled yet. Whereas calls in the end of the sequence uses less, due to commands that are already
handled being removed from the sequence.
