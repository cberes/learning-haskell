# Dijkstra's algorithm

Implementation of Dijkstra's algorithm with some extra stuff to compare two parallel highways with several intersections.

Say there is a highway like this:

    0 --A-- 2 --D-- 4
            |       |
            C       F
            |       |
    1 --B-- 3 --E-- 5

We will start at either 0 or 1 and find the best solution to either 4 or 5. For input, provide a file with one path cost per line. Costs are listed in groups of threes in order of top highway, bottom highway, and intersection. So for the above example, the input costs will correspond to the edges in this order:

    A
    B
    C
    D
    E
    F

Compile

    ghc --make dijkstra

Run

    ./dijkstra edges.txt

