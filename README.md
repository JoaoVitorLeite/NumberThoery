# NumberThoery

### Description

Implementation in Scala of some concepts of Number Theory

### About 

This class was implemented using a generic type, which was implemented [here](https://github.com/JoaoVitorLeite/NumberThoery/blob/master/src/main/scala/numtheory/Num.scala), which supports some basic algebraic operations and is defined for types Int, Float, Double, Long. The tests made are of simple character, and for this the Scala Test was used. In total 23 tests were performed.

### Project Structure

```bash
src
|
+---.gitignore
|   build.sbt
|   LICENSE
|   README.md
|                               
+---src
|   +---main
|   |   \---scala
|   |       \---numtheory
|   |               ChineseAux.scala
|   |               Num.scala
|   |               NumberTheory.scala
|   |               
|   \---test
|       \---scala
|           \---numtheory
|                   NumberTheorySuite.scala
|                   

```

### Compile

To compile the files it is necessary to be in the root directory(where 
the `build.sbt` file is), the use the `sbt compile` command.

### Test

To perform the tests it is necessary to be in the root 
directory(where the `build.sbt` file is), then use the `sbt test` 
command. The tests used FunSuite and assert's.

### IDE

The IDE used was [Intellij Idea](https://www.jetbrains.com/idea/).

### References



### License

The LICENSE used is [Apache-2.0](https://github.com/JoaoVitorLeite/NumberThoery/blob/master/LICENSE).
