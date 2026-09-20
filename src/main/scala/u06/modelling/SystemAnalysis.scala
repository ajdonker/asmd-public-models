package u06.modelling

import scala.annotation.tailrec

// Basical analysis helpers
object SystemAnalysis:

  type Path[S] = List[S]

  extension [S](system: System[S])

    def normalForm(s: S): Boolean = system.next(s).isEmpty

    def complete(p: Path[S]): Boolean = normalForm(p.last)

    // paths of exactly length `depth`
    def paths(s: S, depth: Int): Seq[Path[S]] = depth match
      case 0 => LazyList()
      case 1 => LazyList(List(s))
      case _ =>
        for
          path <- paths(s, depth - 1)
          next <- system.next(path.last)
        yield path :+ next

    // complete paths with length '<= depth' (could be optimised)
    def completePathsUpToDepth(s: S, depth:Int): Seq[Path[S]] =
      (1 to depth).to(LazyList) flatMap (paths(s, _)) filter complete

    // compute reachable markings up to depth (bounded reachability)
    def reachableMarkings(s: S, maxDepth: Int): Set[S] =
      @tailrec
      def iter(current: Set[S], explored: Set[S], depth: Int): Set[S] =
        if depth == 0 || current.isEmpty then explored
        else
          val next = current.flatMap(system.next)
          val newMarkings = next -- explored
          iter(newMarkings, explored ++ newMarkings, depth - 1)
      
      iter(Set(s), Set(s), maxDepth)
