# VRP Project

This project is one of the assignment of my IA course at the university of bologna. The objective is to build a simple program that, through a Genetic Algorithm and/or an Ant Colony Optimization Algorithm, find the best solution for a VRP/TSP problem. In addiction i want to explore a little more the functional programming paradigm. That's why the choice of Haskell as programming language.

In this case i used some the instance in the `files` folder and consider only the distance between the points.
In the following there're some intruction for building the project and set it up.

## Building and Executing the Project

For the building of this project i suggest the [stack tool](http://haskellstack.org), it will intall all the haskell requirements and dependecies for this project. 

For building, run: `stack build` from the root of the project.
For executing, run: `stack exec VRP` from the root of the project.

## Set the Parameters

If you want to change some of the parameters of the algorithms go to `Parameters.hs` file and rebuild the project. In particular pay attention to set the right absolute path of the `files` folder or the project will rise a runtime error. If something in the types is wrong inside the parameter file, the project will not compile.
