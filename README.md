# Art Gallery Problem
## Scenario Week at UCL-CS (Spring 2016)

This project contains all the sources of the scenario week dedicated
to solving the Art Gallery Problem.

It also provides, as a part of the development, the source of the
framework for generation of random polygons with specific "features"
and QuickCheck-style testing of geometric procedure. Check the test
files and the section **Generating random polygons** below for more
examples.

The overall experience of organising the competition and using the
testing framework is described in the ICFP'16 experience report
**Experience Report: Growing and Shrinking Polygons for Random Testing
of Computational Geometry Algorithms**.



### Resources

Some relevant reading includes the following books, papers and links:

#### Description of the scenario for the participants and slides

See the folder `docs`.

#### Mathematical background

* [Art Gallery Problem](https://en.wikipedia.org/wiki/Art_gallery_problem)
* [Art Gallery Book](http://cs.smith.edu/~orourke/books/ArtGalleryTheorems/)
* [Visibility of Simple Polygon from a Point](https://cs.uwaterloo.ca/research/tr/1985/CS-85-38.pdf)
* [Computational Geometry: Algorithms and Applications](http://www.cs.uu.nl/geobook/)

#### Scala programming and related frameworks

* [Scala Programming Language](http://www.scala-lang.org/)
* [Scala Building Tool (SBT)](http://www.scala-sbt.org/)
* [ScalaCheck](https://scalacheck.org/)
* [ScalaTest](http://www.scalatest.org/)
* [Spray Framework](http://spray.io/)
* [SBT Revolver](https://github.com/spray/sbt-revolver)

### Requirements and building

Requirements: **sbt** (version >0.13.0), **JDK 1.8**.

To compile and test, execute

```
sbt test 
```

To run the visualizer, execute

 ```
 sbt "runMain ucl.scenario.geometry.runners.ArtGalleryPainter filepath"
 ```

 where ``filepath`` points to a test file, e.g.,

```
sbt "runMain ucl.scenario.geometry.runners.ArtGalleryPainter ./src/test/resources/testdata/lshaped.agp"
```

If the set of guards (blue dots) doesn't cover the whole gallery with their visibility areas (yellow), a counterexample position will be located (red dot).

Check ``ucl.scenario.geometry.ArtGalleryPainter`` object for details.

### AGP test file format

 The files are formatted as follows:

 ```
comma-separated list of polygon vertices in a counter-clockwise order
###
comma-separated list of guards
 ```

 In the counter-clockwise order the polygon stays on the left while walking its the boundary. For instance:

 ```
 (0, 0), (5, 0), (5, 2), (4, 1), (1, 1), (0, 2)
 ###
 (0, 2), (5, 2)
 ```

 The following extended format allows to input the auxiliary triangulation:

 ```
 (0, 0), (5, 0), (5, 2), (4, 1), (1, 1), (0, 2)
 ###
 (0, 2), (5, 2)
 ###
 true
 ```

More examples can be found in ``./src/test/resources/testdata/``.

### Running the scenario server

To run the server with the simple test scenario project locally (located in ``./instances/test``), run the following command from terminal:

```
sbt "runMain ucl.scenario.server.Boot ./instances/test"
 ```

You can change some parameters of the scenario (e.g., host address or port) by modifying the `config` file in the root
of the folder instance, passed as an argument to `Boot`.

To check the stub page, navigate to ``http://127.0.0.1:8083`` (or the host you put in the config file).

You can add your email address by changing some of the ``emails`` files of the test instances. For example, you can add
 your email into ``./instances/test/teams/albatros``, therefore getting all notifications for the `albatros` team to your
 email address.

You can find some examples of good and bad solutions for different teams for this specific `test` scenario instance
in the folders `./resources/test_scenarios/test/solutions`.

### Bootstrapping a new scenario instance


All necessary instances of the scenario are already create, but if you want to create a new one, run

```
sbt "runMain ucl.scenario.util.SetupScenario <filepath> <scnearioname>"
```

where `filepath` will be the root folder of your scenario, for instance

```
sbt "runMain ucl.scenario.util.SetupScenario ./instances myscenario"
```

This will create team accounts, default task files and confing file.

## Generating random polygons

In order to generate series of different random polygons, run one of the following scripts:

For rectilinear polygons:
```
sbt "runMain ucl.scenario.geometry.runners.RectilinearPolygonGenerator"
```

For triangular-like polygons:
```
sbt "runMain ucl.scenario.geometry.runners.TriangularPolygonGenerator"
```

For quasi-convex polygons:
```
sbt "runMain ucl.scenario.geometry.runners.QuasiConvexPolygonGenerator"
```

For totally random polygons:
```
sbt "runMain ucl.scenario.geometry.runners.CrazyPolygonGenerator"
```

You can also run them in the form

```
sbt "runMain ucl.scenario.geometry.runners.X path num"
```

Where `X` is the corresponding class name (one of the four above), `path` is the desired output file path and
 `num` is a number of polygons generated. If `path` is absent, the default file with extension
 `.pol` will be put to the project root folder.

Use the visualiser described above in order to render polygons.

## Checking solutions for the Art Gallery problem

Run from the command line:

```
sbt "runMain ucl.scenario.geometry.runners.CheckVisibility inputPath"
```
where `inputPath` is a path to your file with solutions to the Guards problem, formated as follows:

```
polygon_number: polygon_vertices ; guards
```

For instance, you can try it on the problem file from Part 2 as follows:

```
sbt "runMain ucl.scenario.geometry.runners.CheckVisibility ./resources/tasks/check/check.pol"
```
